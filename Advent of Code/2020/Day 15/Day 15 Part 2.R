library(readr)
library(tidyverse)

start_time <- Sys.time()

numbers <-read_lines("example.txt") %>% str_split(",") %>% unlist()
l <- length(numbers)
seen <- list(1,2,3)
names(seen) <- numbers[1:(l-1)]
current <- numbers[l]
f <- 2020

for (i in l:(f-1)){
  if(current %in% names(seen)){
    new <- as.character(i - seen[[current]])
    seen[current] <- i
  } else {
    new <- "0"
    seen[current] <- i
  }
  current <- new
  if(i %% 100000 == 0) {print(i)}
}

print(paste0("Answer is ", current))

print(Sys.time() - start_time)
