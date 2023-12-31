# long.R | stock_EMA | long stock EMA model

# LS <- "Long"
l_s <- ifelse(LS=="Long", 1, -1)

start_time <- Sys.time()               # for internal monitoring
run_time <- paste0(" ", get_hour(start_time), "-", get_minute(start_time))

###################### optimization sequence #############################
for (j in seq_len(nrow(runs))) {
  # j <- 550
  
  df <- df_og
  # exponential moving averages
  fast_lag <- runs$fast[j]
  slow_lag <- runs$slow[j]
  df$Efast <- ewmaRcpp(df$close, fast_lag)    
  df$Eslow <- ewmaRcpp(df$close, slow_lag)  +1e-6

    # time management for chunk runs 
  # df<- df|>
  #   slice(13502:20253)    # [13502:20253,]
  
  start_date <- min(df$time) ; end_date <- max(df$time)
  date_range <- as.numeric(difftime(end_date, start_date, units = "days"))
  
  # calculate cross trade signal 
  df <- df |>
    mutate(time = as.POSIXct(time, tz = "UTC"), # Ensure time is in POSIXct format
           # dfast = if_else(Efast-lag(Efast)==0, 1e-7, Efast-lag(Efast)),
           # dslow = if_else(Eslow-lag(Eslow)==0, 1e-7, Eslow-lag(Eslow)),
           # Hfast = HMA(close, fast_lag),
           # Hslow = (HMA(close, slow_lag) +1e-6), 
           cross = (Efast - Eslow) * l_s,      #      cross moving avg selection
           on = if_else(cross >= 0 & lag(cross) < 0, 1, 0), 
           off = if_else(cross < 0 & lag(cross) >= 0, -1, 0),
           signal = on + off) |>
    drop_na(on) |>
    drop_na(off) 
  
  if(df$cross[1] > 0) {  # First row trade signal set up
    df$on[1] <- 1
    df$signal[1] <- 1
  } else {
    df <- df |>
      slice(which.max(on):nrow(df)) # start with first trade
  }
     
  if(fast_lag == slow_lag | sum(df$on) == 0) {  # avoid errors where no trades
    results[j,1:19] <- as_tibble_row(
      c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=0, drawdown=0,
        bliss=0, bliss2=0, lake=0, end_value=0, trade_test=0,
        trade_count=0, wins=0, losses=0, win_rate=0,
        trade_total_pnl=0, won=0, lost=0, dollar_won=0, LS=l_s),
      .name_repair = "universal")
    next
  }
  
  df <- df |>
    mutate(open_trade = cumsum(signal),
           open_date = if_else(on == 1, as_datetime(lead(time), 
                                 tz="America/New_York"), NA),
           close_date = if_else(off == -1, as_datetime(lead(time), 
                                  tz="America/New_York"), NA),
           open_price = if_else(on == 1, lead(open) + skid * l_s, NA),
           close_price = if_else(off == -1, lead(open) - skid * l_s, 0),
           amount = NA,
           trade_pnl = 0,  
           closed_pnl = 0,
           open_pnl = 0,
           equity = NA) |>
    fill(open_price) |>
    fill(open_date)  |>
    drop_na(open_price)
  
  # Close out last trade if trade is open when data file runs out
  if(df$on[nrow(df)] == 1) {
    df$on[nrow(df)] = 0 ; df$signal[nrow(df)] = 0
  } else if (df$cross[nrow(df)] > 0 | df$signal[nrow(df)] == -1) {
    df$off[nrow(df)] <- -1
    df$signal[nrow(df)] <- -1
    df$open_trade[nrow(df)] <- 0
    df$close_date[nrow(df)] <- df$time[nrow(df)]
    df$close_price[nrow(df)] <- df$close[nrow(df)]
  }
  
  trade_test <- sum(df$signal)     # Trade test
  if(trade_test != 0 | min(df$open_trade) !=0) warning(
    "HOLY SHIT! THE TRADES ARE FUCKED!")
  
  tpnl <- df |>                  # Calc trade pnl 
    select(off, open_date:closed_pnl) |>
    filter(off == -1) |>
    mutate(off=NULL)
  
  opens <- match(tpnl$open_date,  df$open_date)
  closes <- match(tpnl$close_date,  df$close_date)
  tpnl <- tpnl  |>
    mutate(
      open = opens,
      close = closes
    )
  # calc the first row values
  tpnl$amount[1] <- floor(start_value / tpnl$open_price[1])
  tpnl$trade_pnl[1] <- tpnl$amount[1] * (tpnl$close_price[1] - 
              tpnl$open_price[1]) * l_s
  tpnl$closed_pnl[1] <- start_value + tpnl$trade_pnl[1]
  df$amount[tpnl$open[1]] <- tpnl$amount[1] 
  df$trade_pnl[tpnl$close[1]] <- tpnl$trade_pnl[1]
  df$closed_pnl[tpnl$close[1]] <- tpnl$closed_pnl[1]
  
  # loop through the rest of the trades
  for (i in seq2(2, nrow(tpnl))) {
    tpnl$amount[i] <- floor(tpnl$closed_pnl[i-1] /tpnl$open_price[i])
    tpnl$trade_pnl[i] <- tpnl$amount[i] * (tpnl$close_price[i] - 
              tpnl$open_price[i]) * l_s
    tpnl$closed_pnl[i] <- tpnl$closed_pnl[i-1] + tpnl$trade_pnl[i]
    
    df$amount[tpnl$open[i]] <- tpnl$amount[i] 
    df$trade_pnl[tpnl$close[i]] <- tpnl$trade_pnl[i]
    df$closed_pnl[tpnl$close[i]] <- tpnl$closed_pnl[i]
  }
  
  # cume trade metrics
  df <- df |>
    fill(amount) |>
    mutate(
      open_pnl = (close - open_price) * amount * open_trade * l_s,
      closed_pnl = cumsum(trade_pnl) + start_value,
      equity = open_pnl + closed_pnl,
      win_lose = as.factor(if_else(trade_pnl<0, "loser", "winner")),
      highwater = cummax(equity),
      lake = highwater - equity,
      drawdown = lake / highwater)
  
  trades <- df |>   # create the trade table
    select(off, open_date:drawdown) |>
    filter(off == -1) |>
    mutate(
      off=NULL, open_trade=NULL)
  
  #  MAE  MFE calc
  trades <- trades |>
    mutate(MAE = map_dbl(1:n(), ~ {
      row <- .x
      df |>
        filter(time >= trades$open_date[row], time <= trades$close_date[row]) |>
        pull(low) |>
        min(na.rm = TRUE)
    }), .after=win_lose,
    MAE_mark = MAE - open_price,
    MAE_percent = (MAE / open_price) - 1,
    MFE = map_dbl(1:n(), ~ {
      row <- .x
      df |>
        filter(time >= trades$open_date[row], time <= trades$close_date[row]) |>
        pull(high) |>
        max(na.rm = TRUE)
    }),
    MFE_mark = MFE - open_price,
    MFE_percent = (MFE / open_price) - 1,
    trade_pnl_percent = trade_pnl / open_price)
  
  # risk and return calculation
  end_value <- df$equity[nrow(df)]  
  ratio <- end_value/ start_value
  ICAGR <- if_else(ratio <= 0, 0, log(ratio)/(date_range/365.25))
  drawdown <- max(df$drawdown)
  lake <- sum(df$lake) / sum(df$equity)
  bliss <- ICAGR / drawdown
  bliss2 <- ICAGR / (drawdown + lake)
  trade_count <- nrow(trades)
  trade_total_pnl <- sum(trades$trade_pnl)
  zz <- rep(0, 16) ; dim(zz) <- c(2,8)
  zz <- split_fun(trades, trade_pnl)
  if (dim(zz)[1] == 2) {
    wins <- zz[[2,3]] ; losses <- zz[[1,3]] ; won <- zz[[2,2]]; lost <- zz[[1,2]];
    win_rate <- wins/trade_count ; dollar_won <- -zz[[2,4]]/zz[[1,4]]
  } else if (trade_total_pnl > 0) {
    wins <- zz[[1,3]]; losses <- 0;  won <- zz[[1,2]]; lost <- 0; 
    win_rate <- wins/trade_count; dollar_won <- 0
  } else {
   wins <- 0; losses <- zz[[1,3]]; won <- 0; lost <- zz[[1,2]]; 
   win_rate <- wins/trade_count; dollar_won <- 0
  }
  results[j,1:19] <- as_tibble_row(          
    c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=ICAGR, drawdown=drawdown, 
      bliss=bliss, bliss2=bliss2, lake=lake, end_value=end_value,
      trade_test=trade_test, trade_count=trade_count, wins=wins, losses=losses, 
      win_rate=win_rate, trade_total_pnl=trade_total_pnl, won=won, lost=lost, 
      dollar_won=dollar_won, LS=l_s),
    .name_repair = "universal")
  
  # accumulate the trades into the global trade table
  trade_tmp <- trades |>
    mutate(MA_slow = slow_lag,
           MA_fast = fast_lag)
  trades_global <- trades_global |>
    bind_rows(trade_tmp)

  
  ###################################
  #                                 # 
  #  Yes, it's running now...       #
  #                                 # 
} ###################################
#  optimization loop end  
# j <- j +1

epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)


# save the results and trades_global files
run_id <- paste0( " ", candles," fast ", min(runs$fast), "-", max(runs$fast), 
                  " slow ",min(runs$slow), "-", max(runs$slow), " ", epoch)
results_file_name <- paste0(here("output", "results "), ticker, " ", LS, " ",
                    nrow(results), " runs ", run_id, run_time, ".csv", sep="")
write_csv(results, results_file_name)
trade_file_name <- paste0(here("output", "trades "), ticker, " ", LS, " ",
                    nrow(runs), " runs", run_id, run_time, ".csv", sep="")
write_csv(trades_global, trade_file_name)

ggsave(paste0(here("output", "run "), run_id, run_time, ".pdf"), 
       width=14, height=11, units="in", dpi=300)


df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "lm", linewidth = 25, alpha = 0.1) +
  geom_line(aes(y=Efast, alpha = 0.2), color = "GREEN") +
  geom_line(aes(y=Eslow, alpha = 0.2), color = "RED") +
  labs(title=sprintf(
   "Run completed. %s, %s, %s,  %1.1f%% buy'n'hold return - Fast: %d-%d Slow: %d-%d", 
                     ticker, LS, epoch, buy_n_hold(df) *100, min(runs$fast), 
                     max(runs$fast), min(runs$slow), max(runs$slow)),
       subtitle=sprintf(
  "%s, %d days, Open: %1.f, High: %1.f, Low: %1.f, Close: %1.f, %d runs,  %d rows",
         candles, round(date_range, 0), df$open[1], max(df$high), min(df$low),  
         df$close[nrow(df)], nrow(runs), nrow(df))) +
  theme(legend.position = "none")

forever <- Sys.time() - start_time
secs <- ifelse(attr(forever, "units")=="secs", 1, 60) * forever / nrow(runs)
sprintf("Yo, %1.2f %s,  %1.2f per run, %i runs, %s records, over %1.0f days of data", 
        forever, attr(forever, "units"), secs, nrow(results), format(nrow(df), 
                                           big.mark=","), date_range)



