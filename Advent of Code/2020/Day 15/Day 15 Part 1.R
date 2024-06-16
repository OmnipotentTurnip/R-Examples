library(readr)
library(tidyverse)

start_time <- Sys.time()

numbers <-read_lines("example.txt") %>% str_split(",") %>% unlist()

for (i in (length(numbers) + 1):2020){
  if (numbers[i-1] %in% numbers[1:(i-2)]){
    numbers[i] <- Position(function(x) x == numbers[i-1], rev(numbers[1:(i-2)]))
  } else {
    numbers[i] <- "0"
  }
}

print(paste0("Answer is ", numbers[2020]))

print(Sys.time() - start_time)
