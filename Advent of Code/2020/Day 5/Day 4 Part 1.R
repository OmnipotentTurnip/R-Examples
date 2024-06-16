library("tidyverse")

boarding_passes <- read.delim("input.txt", header = FALSE)[,1] %>%
  strsplit("", fixed = TRUE) %>%
  do.call(rbind.data.frame, .)

names(boarding_passes) <- c('FB1', 'FB2', 'FB3', 'FB4', 'FB5', 'FB6', 'FB7', 'RL1', 'RL2', 'RL3')

row_lower <- as.numeric(rep(1, times = nrow(boarding_passes)))
row_upper <- as.numeric(rep(128, times = nrow(boarding_passes)))
column_lower <- as.numeric(rep(1, times = nrow(boarding_passes)))
column_upper <- as.numeric(rep(8, times = nrow(boarding_passes)))

for (i in 1:7){
  front <- boarding_passes[, paste0("FB", i)] == "F"
  back <- boarding_passes[, paste0("FB", i)] == "B"
  row_lower[back] <- (row_lower[back] + row_upper[back] + 1)/2
  row_upper[front] <- (row_lower[front] + row_upper[front] - 1)/2
}

for (i in 1:3){
  left <- boarding_passes[, paste0("RL", i)] == "L"
  right <- boarding_passes[, paste0("RL", i)] == "R"
  column_lower[right] <- (column_lower[right] + column_upper[right] + 1)/2
  column_upper[left] <- (column_lower[left] + column_upper[left] - 1)/2
}

boarding_passes$seat_id <- (row_upper - 1) * 8 + column_upper - 1

print(paste0("Answer is ", max(boarding_passes$seat_id)))