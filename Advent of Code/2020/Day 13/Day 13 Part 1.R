library(stringr)
schedule <- read.delim("input.txt", header = FALSE)

earliest <- as.numeric(schedule[1,])
bus <- schedule[2,] %>%
  str_split(",")
bus <- c(unlist(bus))
bus <- as.numeric(bus[bus != "x"])
arrival <- ceiling(earliest/bus) * bus

schedule <- data.frame(bus, arrival)
schedule <- schedule[order(schedule$arrival),]

print(paste0("Answer is ", (schedule$bus[1]) * (schedule$arrival[1] - earliest)))

      
