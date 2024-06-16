library(stringr)
library(purrr)

instructions <- read.delim("input.txt", header = FALSE)[[1]]

instructions <- data.frame(map_chr(instructions, function(x){str_extract(x, "[[:alpha:]]{1}")}),
                           map_dbl(instructions, function(y){as.numeric(str_extract(y, "[[:digit:]]+"))}))
names(instructions) <- c("action", "value")

compass <- c("N" = 0, "E" = 90, "S" = 180, "W" = 270)

instructions$direction <- instructions$action
instructions$direction[1] <- "E"
for (i in 2:nrow(instructions)){
  if (instructions$action[i] == "R"){
    new_direction <- (compass[instructions$direction[i-1]] + instructions$value[i]) %% 360
    instructions$direction[i] <- names(compass[compass == new_direction])
  } else if (instructions$action[i] == "L"){
    new_direction <- (compass[instructions$direction[i-1]] - instructions$value[i]) %% 360
    instructions$direction[i] <- names(compass[compass == new_direction])
  } else {
    instructions$direction[i] <- instructions$direction[i-1]
  }
}


x <- sum(instructions$value[instructions$action == "E"]) - sum(instructions$value[instructions$action == "W"])
x <- x + sum(instructions$value[instructions$action == "F" & instructions$direction == "E"])
x <- x - sum(instructions$value[instructions$action == "F" & instructions$direction == "W"])

y <- sum(instructions$value[instructions$action == "N"]) - sum(instructions$value[instructions$action == "S"])
y <- y + sum(instructions$value[instructions$action == "F" & instructions$direction == "N"])
y <- y - sum(instructions$value[instructions$action == "F" & instructions$direction == "S"])

manhattan_distance <- abs(x) + abs(y)

print(paste0("Answer is ", manhattan_distance))

