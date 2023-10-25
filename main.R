#  main.R | stock_EMA | setup for single stock EMA buy and sell signals

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
################

LS <- "Short"
ticker <- "MPC"  
fast_high <- 200
fast_low <- 5
fast_step <- 5
slow_high <- 500
slow_low <- 10
slow_step <- 10

df_raw <- read_csv("BATS_MPC, 5_1994c.csv", col_names = TRUE)
df <- df_raw |>
  select(time:close) 

runs <- expand.grid(slow=seq(slow_low, slow_high, slow_step), 
                    fast=seq(fast_low, fast_high, fast_step))

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
epoch <- paste0(get_month(start_date),"-", get_day(start_date), "-",
                get_year(start_date)-2000," to ", get_month(end_date), 
                "-", get_day(end_date), "-", get_year(end_date)-2000)

trades_global = tibble() # one big file for all the trades on each run.
results <- tibble() # create the file to collect the results of each run

start_value <- 1e4
skid <- 0   # skid is expected loss on trade execution, set to ZERO for building the model!

buy_n_hold <- log(df$close[nrow(df)]/df$open[1])/
  (as.numeric(difftime(max(df$time), min(df$time), units = "days"))/365.25)

df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  labs(title=sprintf("%s, %s, %s,  %1.1f%% buy & hold return - Fast: %d-%d Slow: %d-%d", 
                     ticker, LS, epoch, buy_n_hold *100, min(runs$fast), 
                     max(runs$fast), min(runs$slow), max(runs$slow)),
      subtitle=sprintf(
        "%s periods, %d days, Open: %1.f, High: %1.f, Low: %1.f, Close: %1.f, %d runs,  %d rows",
           candles, round(date_range, 0), df$open[1], max(df$high), min(df$low),  
        df$close[nrow(df)], nrow(runs), nrow(df))) +

    theme(legend.position = "none")

df_og <- df

# Not running yet, kick off the runs
