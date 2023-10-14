#  short.R | stock_EMA | Single stock EMA buy and sell signals

rm(list=ls()) ###############
# library(conflicted)
library(tidyverse, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(tidyquant, quietly = TRUE)
library(Rcpp, quietly = TRUE)
library(TTR, quietly = TRUE)
library(plotly, quietly = TRUE)
library(rlang, quietly = TRUE)
library(clock, quietly = TRUE)
library(chron)
library(here)
library(ggrepel)
options(ggrepel.max.overlaps = Inf)      # ggrepel options for ggplot2
theme_set(theme_light())                # ggplot theme or _bw()
# conflicts_prefer(dplyr::lag)
# conflicts_prefer(dplyr::filter)

start_time <- Sys.time()               # for internal monitoring
run_time <- paste0(" ", get_hour(start_time), "-", get_minute(start_time))

# calculates pnl summary statistics
split_fun <- function(data, column_name, factor_name) {
  summary_stats <- data |>
    mutate(category = ifelse({{ column_name }} >= 0, "Positive", "Negative")) |>
    group_by({{ factor_name }}, category) |>
    summarise(
      sum = sum({{ column_name }}),
      count = n(),
      mean = mean({{ column_name }}),
      median = median({{ column_name }}),
      sd = sd({{ column_name }}),
      min = min({{ column_name }}),
      max = max({{ column_name }})
    ) |>
    select({{ factor_name }}, category, sum, count, everything())
  return(summary_stats)
}
################ C code for EMA calculation with no leading NA
sourceCpp(
  code =
    "
     #include <Rcpp.h>
     // [[Rcpp::export]]
     Rcpp::NumericVector ewmaRcpp(Rcpp::NumericVector x, double a){
       int n = x.length();
       Rcpp::NumericVector s(n);
       s[0] = x[0];
       if (n > 1) {
         for (int i = 1; i < n; i++) {
           s[i] =  s[i-1] + (x[i] - s[i-1])/(( a + 1)/2);
         }
       }
       return s;
     }
    ")     ###################
LS <- "Short"
ticker <- "ULTA"  
fast_high <- 500
fast_low <- 20
fast_step <- 20
slow_high <- 500
slow_low <- 20
slow_step <- 20
drying_paint <- (fast_high/fast_step - fast_low/fast_step +1) * 
  (slow_high/slow_step - slow_low/slow_step +1)
runs <- expand.grid(slow=seq(slow_low, slow_high, slow_step), 
                    fast=seq(fast_low, fast_high, fast_step))

df_orig <- read_csv("BATS_ULTA, 5_ec9c3.csv", col_names = TRUE)
df <- df_orig |>
  select(time:close) 

# discern time interval from input file
first_row_time <- df$time[1] ; second_row_time <- df$time[2] ; third_row_time <- df$time[3]
interval <- min(as.numeric(difftime(second_row_time, first_row_time, units = "mins")),
                as.numeric(difftime(third_row_time, second_row_time, units = "mins")))
if(interval <= 0) Warning("HOLY SHIT! WE HAVE REACHED THE END OF TIME!") # file sorted backwards?

# candles is a string for in graph titles
candles <- if(interval>=1440) {
  sprintf("%.0f day", interval/1440) 
} else if (interval>=60){
  sprintf("%.0f hr", interval/60)
} else {
  sprintf("%.0f min", interval)
}

# time management for market 
start_date <- min(df$time) ; end_date <- max(df$time)
date_range <- as.numeric(difftime(end_date, start_date, units = "days"))

the_high <- max(df$high) ; the_low <- min(df$low) ; the_range <- the_high - the_low
trades_global = tibble() # one big file for all teh trades on each run.
results <- tibble() # create the file to collect the results of each run
epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)

start_value <- 1e4
skid <- 0   # skid is expected loss on trade execution, set to ZERO for building the model!
buy_n_hold <- log(df$close[nrow(df)]/df$open[1])/(date_range/365.25)

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  labs(title=sprintf("Running next calculation...  ICAGR: %1.2f%% buy and hold - %s model", 
                     buy_n_hold *100, LS),
       subtitle=paste0(candles, " periods, ",epoch, 
                       "       High: ", the_high, "  Low: ", the_low, "      ",
                       nrow(df), " rows    ", drying_paint, " runs")) +
  theme(legend.position = "none")

# heat <- 0.1 # risk budget, 10% of book equity for each trade
# ATR_multiplier <- 5 # dynamic risk sizing based on current volatility via ATR
df_og <- df


###################### optimization sequence #############################


for (j in seq_len(nrow(runs))) {
  # j <- 1024
  
  df <- df_og
  fast_lag <- runs$fast[j]
  slow_lag <- runs$slow[j]
  
  if(fast_lag == slow_lag) {  # avoid error where fast=slow for same MA
    results[j,1:17] <- as_tibble_row(
      c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=0, drawdown=0,
        bliss=0, lake=0, end_value=0, trade_test=0,
        trade_count=0, wins=0, losses=0, win_rate=0,
        trade_total_pnl=0, won=0, lost=0, dollar_won=0),
      .name_repair = "universal")
    next
  }
  
  # exponential moving averages
  df$Efast <- ewmaRcpp(df$close, fast_lag)    
  df$Eslow <- ewmaRcpp(df$close, slow_lag)  +1e-6
  
  # calculate HMA, Hull Moving Avg, cross trade signal and ATR, average true range
  df <- df |>
    # select(time:close, Volume, Efast, Eslow) |>
    mutate(time = as.POSIXct(time, tz = "UTC"), # Ensure time is in POSIXct format
           dfast = if_else(Efast-lag(Efast)==0, 1e-7, Efast-lag(Efast)),
           dslow = if_else(Eslow-lag(Eslow)==0, 1e-7, Eslow-lag(Eslow)),
           # Hfast = HMA(close, fast_lag),
           # Hslow = (HMA(close, slow_lag) +1e-6), 
           cross =  Eslow - Efast,              #      cross moving avg selection
           on = if_else(cross >= 0 & lag(cross) < 0, 1, 0), 
           off = if_else(cross < 0 & lag(cross) >= 0, -1, 0),
           signal = on + off) |>
    drop_na(on) |>
    drop_na(off)
  df$off[1] <- 0      
  # start_value <- df$open[match(1, df$on) +1] * 100
  
  df <- df |>
    mutate(open_trade = cumsum(signal),
           open_date = if_else(on == 1, as_datetime(lead(time), tz="America/New_York"), NA),
           close_date = if_else(off == -1, as_datetime(lead(time), tz="America/New_York"), NA),
           open_price = if_else(on == 1, lead(open) + skid, NA),
           close_price = if_else(off == -1, lead(open) - skid, 0),
           amount = NA,
           trade_pnl = 0,  
           closed_pnl = 0,
           open_pnl = 0,
           equity = NA) |>
    fill(open_price) |>
    fill(open_date)  |>
    drop_na(open_price)
  
  # First row set up
  if(df$cross[1] > 0) {     # trade signal details
    df$on[1] <- 1
    df$signal[1] <- 1
    # df$amount <- floor((start_value * heat)  / (ATR_multiplier * df$atr_EMA[[1]]))
  }
  
  # Close out last trade if long when data file runs out
  if(df$on[nrow(df)] == 1) {
    df$on[nrow(df)] = 0 ; df$signal[nrow(df)] = 0
  } else if (df$cross[nrow(df)] > 0 | df$signal[nrow(df)] == -1) {
    df$off[nrow(df)] <- -1
    df$signal[nrow(df)] <- -1
    df$open_trade[nrow(df)] <- 0
    df$close_date[nrow(df)] <- df$time[nrow(df)]
    df$close_price[nrow(df)] <- df$close[nrow(df)]
  }
  
  # Calc trade pnl 
  trade_test <- sum(df$signal) 
  if(trade_test != 0) warning("HOLY SHIT! THE TRADES ARE FUCKED!")
  tpnl <- df |>
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
  
  tpnl$amount[1] <- floor(start_value / tpnl$open_price[1])  # short
  tpnl$trade_pnl[1] <- tpnl$amount[1] * (-tpnl$close_price[1] + tpnl$open_price[1]) 
  tpnl$closed_pnl[1] <- start_value + tpnl$trade_pnl[1]
  df$amount[tpnl$open[1]] <- tpnl$amount[1] 
  df$trade_pnl[tpnl$close[1]] <- tpnl$trade_pnl[1]
  df$closed_pnl[tpnl$close[1]] <- tpnl$closed_pnl[1]
  
  for (i in seq2(2, nrow(tpnl))) {
    tpnl$amount[i] <- tpnl$closed_pnl[i-1] /tpnl$open_price[i] # short
    tpnl$trade_pnl[i] <- tpnl$amount[i] * (-tpnl$close_price[i] + tpnl$open_price[i])
    tpnl$closed_pnl[i] <- tpnl$closed_pnl[i-1] + tpnl$trade_pnl[i]
    
    df$amount[tpnl$open[i]] <- tpnl$amount[i] 
    df$trade_pnl[tpnl$close[i]] <- tpnl$trade_pnl[i]
    df$closed_pnl[tpnl$close[i]] <- tpnl$closed_pnl[i]
  }
  
  # cume trade metrics
  df <- df |>
    fill(amount) |>
    mutate(       #  short
      open_pnl = (-close + open_price) * amount * open_trade,
      closed_pnl = cumsum(trade_pnl) + start_value,
      equity = open_pnl + closed_pnl,
      win_lose = as.factor(if_else(trade_pnl<0, "loser", "winner")),
      highwater = cummax(equity),
      lake = highwater - equity,
      drawdown = lake / highwater)
  
  # summary trade table 
  trade_test <- sum(df$signal) 
  if(trade_test != 0) warning("HOLY SHIT! THE TRADES ARE FUCKED!")
  trades <- df |>
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
  trade_count <- nrow(trades)
  trade_total_pnl <- sum(trades$trade_pnl)
  zz <- split_fun(trades, trade_pnl)
  if (dim(zz)[1] == 2) {
    wins <- zz[[2,3]] ; losses <- zz[[1,3]] ; won <- zz[[2,2]]; lost <- zz[[1,2]]
    win_rate <- wins/trade_count ; dollar_won <- -zz[[2,4]]/zz[[1,4]]
  } else {
    wins <- 0; losses <- 0; win_rate <- 0; won <- 0; lost <- 0; dollar_won <- 0
  }
  results[j,1:17] <- as_tibble_row(          
    c(j=j, fast_lag=fast_lag, slow_lag=slow_lag, ICAGR=ICAGR, drawdown=drawdown, 
      bliss=bliss, lake=lake, end_value=end_value, trade_test=trade_test, 
      trade_count=trade_count, wins=wins, losses=losses, win_rate=win_rate,
      trade_total_pnl=trade_total_pnl, won=won, lost=lost, dollar_won=dollar_won),
    .name_repair = "universal")
  
  # accumulate the trades into the global trade table
  trade_tmp <- trades |>
    mutate(MA_slow = slow_lag,
           MA_fast = fast_lag)
  trades_global <- trades_global |>
    bind_rows(trade_tmp)
  
  #  Yes, it's running...
}       #################### optimization loop end    ##########################
# j <- j +1


# save the results and trades_global files
run_id <- paste0( " ", candles," fast ", min(runs$fast), "-", max(runs$fast), 
                  " slow ",min(runs$slow), "-", max(runs$slow), " fr ", epoch)
results_file_name <- paste0(here("output", "results "), nrow(results), " runs ", 
                            run_id, run_time, ".csv", sep="")
write_csv(results, results_file_name)
trade_file_name <- paste0(here("output", "trades "), nrow(runs), " runs", run_id,
                          run_time, ".csv", sep="")
write_csv(trades_global, trade_file_name)

##########################

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "lm", linewidth = 25, alpha = 0.1) +
  # geom_line(aes(y=fast, alpha = 0.2)) +
  # geom_line(aes(y=slow, alpha = 0.2)) +
  geom_line(aes(y=Efast, alpha = 0.2), color = "GREEN") +
  geom_line(aes(y=Eslow, alpha = 0.2), color = "RED") +
  labs(title=paste("Run completed!"),
       subtitle=paste0(candles, " periods, ", round(date_range, 0),
                       "D of data, ", epoch,",   fast: ", min(runs$fast), "-", 
                       max(runs$fast), " slow: ",min(runs$slow), "-", 
                       max(runs$slow),", ",  "    High: ", the_high, " Low: ",
                       the_low)) +
  theme(legend.position = "none")



forever <- Sys.time() - start_time
secs <- 60 *forever  / nrow(runs)
sprintf("Yo, %1.2f %s,  %1.2f per run, %i runs, %s records, over %1.0f days of data", 
        forever, attr(forever, "units"), secs, nrow(results), format(nrow(df), 
                                                                     big.mark=","), date_range)



