library(numbers)
library(VeryLargeIntegers)

schedule <- read.delim("input.txt", header = FALSE)[2,] %>%
  str_split(",")

bus <- c(unlist(schedule))
bus_index <- 1:length(bus)
bus_index <- bus_index[bus != "x"]
bus <- as.numeric(bus[bus != "x"])
bus_index <- as.character(-bus_index %% bus)

N = prod(bus)
Nni = as.character(N/bus)
bus <- as.character(bus)

a <- c()
b <- c()
for (i in 1:length(bus)){
  coefficients <- exteuclid(as.vli(bus[i]), as.vli(Nni[i]))
  a[i] <- as.character(coefficients[[2]])
  b[i] <- as.character(coefficients[[3]])
}

xi <- c()
for (i in 1:length(bus)){
  xi[i] <- as.character(as.vli(bus_index[i]) * as.vli(b[i]) * as.vli(Nni[i]))
}

x <- "0"
for (i in 1:length(bus)){
  x <- as.character(as.vli(x) + as.vli(xi[i]))
}

x <- as.character(as.vli(x) %% as.vli(as.character(N)) + as.vli("1"))


print(paste0("Answer is ", x))

