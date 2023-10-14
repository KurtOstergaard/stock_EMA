#viz3.R - individual moving average pair graphs for examining trades and details
library(tidyverse)
library(lubridate)
library(Rcpp, quietly = TRUE)
library(here)

###### C code for EMA calculation with no leading NA ################
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

process_csv_file <- function(file_path) {
  data <- read_csv(file_path, show_col_types = FALSE)
  
  num_rows <- nrow(data)
  file_name <- basename(file_path)
  return(tibble(file_name, num_rows))
}  

#   ################ find files  ############
# csv_files <- list.files(path=here("output"), pattern = "\\.csv$", full.names = TRUE)
csv_files <- list.files(path=here("output"), pattern = "^trades.", full.names = TRUE)
trade_files <- lapply(csv_files, process_csv_file) |>
  bind_rows()
colnames(trade_files) <- c("File Name", "Rows")
# View(trade_files)

#   ################ load files  ############
big_trades <- read_csv(trade_file_name, col_names = TRUE)
# big_trades <- read_csv(here("output", "trades 1 runs 1 sec fast 580-580 slow 2680-2680 fr 9-18-23 to 9-19-23 21-51.csv"), col_names = TRUE)
big_results <- read_csv(results_file_name, col_names = TRUE)

mkt <- df_og |> select(time:close)
# mkt <- read_csv("CME_MINI_ES1!, 1S_9e97e.csv", col_names = TRUE) |>  select(time:close)
mkt_high <- max(mkt$high) ; mkt_low <- min(mkt$low) ; mkt_range <- mkt_high - mkt_low

fast_MA <- 120
slow_MA <- 420

mkt$Efast <- ewmaRcpp(mkt$close, fast_MA)    
mkt$Eslow <- ewmaRcpp(mkt$close, slow_MA)  

these_trades <- big_trades |>
  filter(MA_fast == fast_MA) |>
  filter(MA_slow == slow_MA)

these_results <- big_results |>
  filter(fast_lag == fast_MA, slow_lag == slow_MA)

pnl <- sum(these_trades$trade_pnl)

mkt |>     
  ggplot(aes(x = time, y=close), alpha=0.1) +
  geom_line() +
  # geom_smooth(method = "lm", linewidth = 15, alpha = 0.1) +
  geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "band"))+
  scale_fill_manual("", values="gray20") +
  geom_segment(data=these_trades, aes(x=open_date, y=open_price, xend=close_date,
                                  yend=close_price, color=factor(win_lose)), linewidth = 2) +
  scale_color_manual(values= c("red", "green3")) +
  labs(title=sprintf("%s: %0.i-%0.i, %.0f trades, ICAGR: %0.2f%%, DD: %0.2f%%, Bliss: %.2f%%, Lake: %.2f%%",  
                     ticker, fast_MA, slow_MA, nrow(these_trades), these_results$ICAGR[1]*100,
                     these_results$drawdown[1]*100, these_results$bliss[1]*100, these_results$lake[1]*100),
       subtitle=paste0(candles, " chart, ", floor(date_range), "D of data, ", epoch))+
  xlab("Date")+
  ylab(ticker) +
  theme(legend.position = "none")




mkt |>        
  ggplot(aes(x = time)) +
  geom_line(aes(x=time, y=Eslow), alpha=0.8, color="tomato") +
  geom_line(aes(x=time, y=Efast), alpha=0.8, color="limegreen") +
  geom_ribbon(aes(ymin=low, ymax=high, x=time, fill = "band"))+
  scale_fill_manual("", values="gray20") +
  geom_segment(data=these_trades, aes(x=open_date, y=open_price, xend=close_date,
                                  yend=close_price, color=factor(win_lose)), linewidth = 2) +
  scale_color_manual(values= c("red", "green3")) +
  labs(title=sprintf("%s: %0.i-%0.i, %.0f trades, ICAGR: %0.2f%%, DD: %0.2f%%, Bliss: %.2f%%, Lake: %.2f%%",  
                     ticker, fast_MA, slow_MA, nrow(these_trades), these_results$ICAGR[1]*100,
                     these_results$drawdown[1]*100, these_results$bliss[1]*100, these_results$lake[1]*100),
       subtitle=paste0(candles, " chart, ", floor(date_range), " days of data, ", epoch))+
  xlab("Date")+
  ylab(ticker) +
  theme(legend.position = "none")

ggsave(paste0(here("output", "run "), ticker, " ", candles," ", fast_MA, 
              "-", slow_MA, " ", epoch, run_time, ".pdf"), 
       width=11, height=8.5, units="in", dpi=300)

# scale_y_continuous(sec.axis=sec_axis(~ . *(max(df$high)/min(df$low))-(min(df$low)) )) +
# geom_line(aes(x=time, y=equity *(max(high)/min(low)) + min(low)-min(equity)), linewidth=2, alpha=0.7, color="deepskyblue") +

these_trades |>   # Stop analysis
  ggplot(aes(MAE_percent, trade_pnl_percent,  color=factor(win_lose))) +
  geom_point(shape=3, size=2,) +
  scale_color_manual(values= c("red","green3")) + 
  labs(title=sprintf("%s: %0.i-%0.i, %.0f trades, ICAGR: %0.2f, DD: %0.2f, Bliss: %.2f, Lake: %.2f",  
                     ticker, fast_MA, slow_MA, nrow(these_trades), these_results$ICAGR[1],
                     these_results$drawdown[1], these_results$bliss[1], these_results$lake[1]),
       subtitle=paste0(candles, " chart, ", floor(date_range), "D of data, ", epoch))+
  xlab("Maximum Adverse Excursion") + 
  ylab("Trade P&L") +
  theme(legend.position = "none")

ggsave(paste0(here("output", "stop "), candles," ", runs$fast[j], 
              "-", runs$slow[j], " ", epoch, run_time, " ", j, ".pdf"), 
       width=11, height=8.5, units="in", dpi=300)



# Cover
df |>
  ggplot(aes(x = time, y = close)) +
  geom_line(alpha = 0.4) +
  labs(title=sprintf("%s:    buy and hold ICAGR: %1.2f%% ", ticker, buy_n_hold *100),
       subtitle=paste0(candles, " periods, ",epoch, 
                       "       High: ", the_high, "  Low: ", the_low, "      ",
                       nrow(df), " rows    ", drying_paint, " runs")) +
  theme(legend.position = "none")

ggsave(paste0(here("output", "run "), ticker, " ", candles," ", fast_MA, 
              "-", slow_MA, " ", epoch, run_time, ".pdf"), 
       width=11, height=8.5, units="in", dpi=300)




#   DON'T Use this until better numbers 
#   DON'T Use this until better numbers 
#   DON'T Use this until better numbers 
df |>        # lake over time with equity line
  ggplot(aes(x = time)) +
  geom_ribbon(aes(ymin=equity, ymax=highwater, x=time, fill = "band"), alpha = 0.9)+
  scale_color_manual("", values="grey12")+
  scale_fill_manual("", values="red") +
  geom_line(aes(y = highwater), size = 1, alpha = 0.6) +
  geom_line(aes(x=time, y=drawdown *max(highwater)/max(drawdown)), alpha=0.2) +
  labs(title=sprintf("Lake Ratio: %s: %0.i-%0.i, %.0f trades, ICAGR: %0.2f, DD: %0.2f, Bliss: %.2f, Lake: %.2f",  
                     ticker, fast_MA, slow_MA, nrow(these_trades), these_results$ICAGR[1],
                     these_results$drawdown[1], these_results$bliss[1], these_results$lake[1]),
       subtitle=paste0(candles, " chart, ", floor(date_range), "D of data, with unscaled drawdown in the background", epoch),
       x="Year", y="Ending equity in points, FIX THIS!!!") +
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0,NA)) +
  theme(legend.position = "none")

ggsave(paste0(here("output", "lake over time "), candles," ", runs$fast[j], 
              "-", runs$slow[j], " ", epoch, run_time, " ", j, ".pdf"), 
       width=10, height=8, units="in", dpi=300)



 