library(readr)
library(stringr)
library(tidyverse)
library(purrr)

input <- read_lines("input.txt")
line_breaks <- (1:length(input))[input == ""]

rules <- input[1:(line_breaks[1]-1)]
my_ticket <- as.integer(input[line_breaks[2] - 1] %>% str_split(.,",") %>% unlist())
nearby_tickets <- input[(line_breaks[2]+2):length(input)] %>% str_split(., ",")
categories <- str_extract(rules, "[[:alpha:]]*\\s*[[:alpha:]]+:") %>% gsub(":", "", .)
rules <- str_extract_all(rules, "[[:digit:]]+")
rules <- map(rules, function(x){
  c(x[1]:x[2], x[3]:x[4])
})

names(rules) <- categories

valid_values <- c(unlist(rules))

valid_tickets <- map(nearby_tickets, function(x){
  as.integer(x) %in% valid_values
})

ticket_error <- map(1:length(nearby_tickets), function(x){
  error <- 0
  if(all(valid_tickets[[x]]) != TRUE){
  error <- sum(as.integer(nearby_tickets[[x]][!valid_tickets[[x]]]))
  }
  error
})

print(paste0("Answer is ", sum(unlist(ticket_error))))

