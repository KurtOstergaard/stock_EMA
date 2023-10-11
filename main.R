#  Single stock exponential moving average buy and sell signals

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

ticker <- "ULTA"  
fast_high <- 15
fast_low <- 2
fast_step <- 1
slow_high <- 200
slow_low <- 25
slow_step <- 5
drying_paint <- (fast_high - fast_low +1)/fast_step * (slow_high-slow_low+1)/slow_step
runs <- expand.grid(slow=seq(slow_low, slow_high, slow_step), 
                    fast=seq(fast_low, fast_high, fast_step))

df_orig <- read_csv("BATS_ULTA, 1D_8ba0c.csv", col_names = TRUE)
df <- df_orig
# calculate ATR, average true range, for risk sizing the trades
df <- df |>
  select(time:close) |>
  mutate(hc_yest = high - lag(close),
         lc_yest = lag(close) - low,
         range = high - low) |>
  mutate(ATR = pmax(range, hc_yest, lc_yest, na.rm = TRUE))
df$atr_EMA <- ewmaRcpp(df$ATR, 20)

# discern time interval from input file
first_row_time <- df$time[1] ; second_row_time <- df$time[2] ; third_row_time <- df$time[3]
interval <- min(as.numeric(difftime(second_row_time, first_row_time, units = "mins")),
                as.numeric(difftime(third_row_time, second_row_time, units = "mins")))
if(interval <= 0) Warning("HOLY SHIT! WE HAVE REACHED THE END OF TIME!") # file sorted backwards?

# candles is a string for in graphs
candles <- if(interval>=1440) {
  sprintf("%.0f hr", interval/1440) 
} else {
  sprintf("%.0f min", interval/60)
}
# time management for market 
start_date <- min(df$time) ; end_date <- max(df$time)
date_range <- as.numeric(difftime(end_date, start_date, units = "days"))
dates_run <- floor_date(start_date, unit="days"):floor_date(end_date, unit = "days")

the_high <- max(df$high) ; the_low <- min(df$low) ; the_range <- the_high - the_low
trades_global = tibble() # one big file for all teh trades on each run.
results <- tibble() # create the file to collect the results of each run
epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)


start_value <- df$close[1]  # start with 1 share at the opening price
skid <- 0   # skid is expected loss on trade execution, set to ZERO for building the model!

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  labs(title=paste("Running next calculation..."),
       subtitle=paste0(candles, " periods, ",epoch, 
                       "       High: ", the_high, "  Low: ", the_low, "      ",
                       nrow(df), " rows    ", drying_paint, " runs")) +
  theme(legend.position = "none")

df_og <- df
###################### optimization sequence #############################


for (j in seq_len(nrow(runs))) {
  # j <- 1
  
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
  df$Eslow <- ewmaRcpp(df$close, slow_lag)  
  
  # calculate HMA, Hull Moving Avg, cross trade signal and ATR, average true range
  df <- df |>
    # select(time:close, Volume, Efast, Eslow) |>
    mutate(time = as.POSIXct(time, tz = "UTC"), # Ensure time is in POSIXct format
           Hfast = HMA(close, fast_lag),
           Hslow = (HMA(close, slow_lag) +1e-6), 
           # dfast = if_else(fast-lag(fast)==0, 1e-7, fast-lag(fast)),
           # dslow = if_else(slow-lag(slow)==0, 1e-7, slow-lag(slow)),
           cross = Efast - Eslow,              #      cross moving avg selection
           on = if_else(cross > 0 & lag(cross) < 0, 1, 0), 
           off = if_else(cross < 0 & lag(cross) > 0, -1, 0),
           signal = on + off) |>
    drop_na(on) |>
    drop_na(off)
  
    df$off[1] <- 0
    if(df$cross[1] > 0) df$on[1] <- 1    # trade signal details
    
  df <- df |>
    mutate(open_trade = cumsum(signal),
           buy_date = if_else(on == 1, as_datetime(lead(time), tz="America/New_York"), NA),
           sell_date = if_else(off == -1, as_datetime(lead(time), tz="America/New_York"), NA),
           buy_price = if_else(on == 1, lead(open) + skid, NA),
           sell_price = if_else(off == -1, lead(open) - skid, 0),
           buy_amount = 1,) |>  # no work on trade sizing yet, important though it is
    fill(buy_price) |>
    fill(buy_date)  
  
  # Close out last trade if long when data file runs out
  if(df$on[nrow(df)] == 1) {
    df$on[nrow(df)] = 0 ; df$signal[nrow(df)] = 0
  } else if (df$cross[nrow(df)] > 0 | df$signal[nrow(df)] == -1) {
    df$off[nrow(df)] <- -1
    df$signal[nrow(df)] <- -1
    df$sell_date[nrow(df)] <- df$time[nrow(df)]
    df$sell_price[nrow(df)] <- df$close[nrow(df)]
  }
  
  # cume trade metrics
  df <- df |>
    drop_na(buy_price) |>
    mutate(
      trade_pnl = if_else(signal == -1, (sell_price - buy_price) * buy_amount, 0),
      win_lose = as.factor(if_else(trade_pnl<0, "loser", "winner")),
      closed_pnl = cumsum(trade_pnl) + start_value,
      open_pnl = (close - buy_price) * buy_amount * open_trade,
      equity = open_pnl + closed_pnl,
      highwater = cummax(equity),
      lake = highwater - equity,
      drawdown = lake / highwater)
  
  # summary trade table 
  trade_test <- sum(df$signal) 
  if(trade_test != 0) warning("HOLY SHIT! THE TRADES ARE FUCKED!")
  trades <- df |>
    select(off, buy_date:drawdown) |>
    filter(off == -1) |>
    mutate(
      off=NULL, open_trade=NULL, buy_amount=NULL)
  
  #  MAE  MFE calc
  trades <- trades |>
    mutate(MAE = map_dbl(1:n(), ~ {
      row <- .x
      df |>
        filter(time >= trades$buy_date[row], time <= trades$sell_date[row]) |>
        pull(low) |>
        min(na.rm = TRUE)
    }), .after=win_lose,
    MAE_mark = MAE - buy_price,
    MAE_percent = (MAE / buy_price) - 1,
    MFE = map_dbl(1:n(), ~ {
      row <- .x
      df |>
        filter(time >= trades$buy_date[row], time <= trades$sell_date[row]) |>
        pull(high) |>
        max(na.rm = TRUE)
    }),
    MFE_mark = MFE - buy_price,
    MFE_percent = (MFE / buy_price) - 1,
    trade_pnl_percent = trade_pnl / buy_price)
  
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
  wins <- zz[[2,3]] ; losses <- zz[[1,3]] ; won <- zz[[2,2]]; lost <- zz[[1,2]]
  win_rate <- wins/trade_count ; dollar_won <- -zz[[2,4]]/zz[[1,4]]
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
  
  
}       #################### optimization loop end    ##########################
# j <- j +1  


# save the results and trades_global files
run_id <- paste0( " ", candles," fast ", min(runs$fast), "-", max(runs$fast), 
                  " slow ",min(runs$slow), "-", max(runs$slow), " fr ", epoch)
results_file_name <- paste0(here("output", "results "), nrow(results), " runs ", 
                            run_id, run_time, ".csv", sep="")
write_csv(results, results_file_name)
trade_file_name <- paste0(here("output", "trades "), nrow(runs), " runs", run_id, run_time, ".csv", sep="")
write_csv(trades_global, trade_file_name)

forever <- Sys.time() - start_time
secs <- forever  / nrow(runs)
sprintf("Yo, %1.2f min,  %1.2f per run, %i runs, %s records, over %1.2f days of data", 
        forever, secs, nrow(results), format(nrow(df), big.mark=","), date_range)


##########################

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "lm", linewidth = 10) +
  # geom_line(aes(y=fast, alpha = 0.2)) +
  # geom_line(aes(y=slow, alpha = 0.2)) +
  geom_line(aes(y=Efast, alpha = 0.2)) +
  geom_line(aes(y=Eslow, alpha = 0.2)) +
  labs(title=paste("Run completed!"),
       subtitle=paste0(candles, " periods, ", round(date_range, 0),
                       "D of data, ", epoch,",   fast: ", min(runs$fast), "-", 
                       max(runs$fast), " slow: ",min(runs$slow), "-", 
                       max(runs$slow),", ",  "    High: ", the_high, " Low: ",
                       the_low)) +
  theme(legend.position = "none")




