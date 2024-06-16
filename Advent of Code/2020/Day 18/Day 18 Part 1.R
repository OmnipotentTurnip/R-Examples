library(tidyverse)
library(readr)
example1 <- "1 + 2 * 3 + 4 * 5 + 6"
example2 <- "1 + (2 * 3) + (4 * (5 + 6))"
example3 <- "2 * 3 + (4 * 5)"
example4 <- "5 + (8 * 3 + 9 + 3 * 4 * 3)"
example5 <- "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
example6 <- "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
input <- readLines("input.txt")

# calculation <- c(example1, example2, example3, example4, example5, example6)
calculation <- input
calculation<- c(paste0(calculation[1:(length(calculation)-1)], ", "), input[length(calculation)])


`%plus%` <- `+`

`%times%` <- `*`

calculation <- gsub("\\+", "%plus%", calculation) %>% gsub("\\*", "%times%", .)
calculation[1] <- paste0("c(", calculation[1])
calculation[length(calculation)] <- paste0(calculation[length(calculation)], ")")

fileConn<-file("output.R")
writeLines(calculation, fileConn)
close(fileConn)


#copy and paste what's in the output.txt file into sum(c())


x <- source("output.R")[[1]]
print(paste0("Answer is ", sum(x)))

      