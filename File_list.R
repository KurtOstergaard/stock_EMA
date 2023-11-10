# library of data files, with start and ends dates and time intervals
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

process_csv_file <- function(file_path) {
  data <- read_csv(file_path, show_col_types = FALSE)
  
  if (!("time" %in% colnames(data))) {
    message("Skipping file: '", file_path, "' - No 'time' column found.")
    return(NULL)
  }
  data$time <- as.POSIXct(as.numeric(data$time), origin = "1970-01-01", tz = "UTC")
  first_row_time <- data$time[1] ; second_row_time <- data$time[2]
  time_difference_minutes <- as.numeric(difftime(second_row_time, first_row_time, units = "mins"))
  start_time <- min(data$time, na.rm = TRUE)
  end_time <- max(data$time, na.rm = TRUE)
  price <- mean(data$close, na.rm = TRUE)
  num_rows <- nrow(data)
  file_name <- basename(file_path)
  return(tibble(file_name, start_time, end_time, time_difference_minutes, price, num_rows))
}

# Process each CSV file and create a summary data frame
files_galore <- lapply(csv_files, process_csv_file) |>
  bind_rows()
colnames(files_galore) <- c("File Name", "Start", "Stop", "Int in mins", "Price",  "Rows")
View(files_galore)

# print(files_galore)

# periodicity() from quantmod does pretty much the same thing
# unclass(periodicity()) to get a list of values 
