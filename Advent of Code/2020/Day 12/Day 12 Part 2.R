instructions <- read.delim("input.txt", header = FALSE)[[1]]

instructions <- data.frame(map_chr(instructions, function(x){str_extract(x, "[[:alpha:]]{1}")}),
                           map_dbl(instructions, function(y){as.numeric(str_extract(y, "[[:digit:]]+"))}))
names(instructions) <- c("action", "value")

instructions$waypoint_x <- 10
instructions$waypoint_y <- 1
instructions$ship_x <- 0
instructions$ship_y <- 0

if (instructions$action[1] == "N") {
  instructions$waypoint_y[1] <- instructions$waypoint_y[1] + instructions$value[1]
} else if (instructions$action[1] == "S") {
  instructions$waypoint_y[1] <- instructions$waypoint_y[1] - instructions$value[1]
} else if (instructions$action[1] == "E") {
  instructions$waypoint_x[1] <- instructions$waypoint_x[1] + instructions$value[1]
} else if (instructions$action[1] == "") {
  instructions$waypoint_x[1] <- instructions$waypoint_x[1] - instructions$value[1]
} else if (instructions$action[1] == "R") {
  relative <- c(instructions$waypoint_x[1] - instructions$ship_x[1],
                instructions$waypoint_y[1] - instructions$ship_y[1])
  theta <- instructions$value[1]
  R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, ncol = 2)
  new_relative <- round(R, 0) %*% relative
  instructions$waypoint_x[1] <- instructions$ship_x[1] + new_relative[1]
  instructions$waypoint_y[1] <- instructions$ship_y[1] + new_relative[1]
} else if (instructions$action[1] == "L") {
  relative <- c(instructions$waypoint_x[1] - instructions$ship_x[1],
                instructions$waypoint_y[1] - instructions$ship_y[1])
  theta <- instructions$value[1]
  L <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2, ncol = 2)
  new_relative <- round(L, 0) %*% relative
  instructions$waypoint_x[1] <- instructions$ship_x[1] + new_relative[1]
  instructions$waypoint_y[1] <- instructions$ship_y[1] + new_relative[2]
} else if (instructions$action[1] == "F") {
  relative <- c(instructions$waypoint_x[1] - instructions$ship_x[1],
                instructions$waypoint_y[1] - instructions$ship_y[1])
  instructions$ship_x[1:nrow(instructions)] <- instructions$ship_x[1] + instructions$value[1] * relative[1]
  instructions$ship_y[1:nrow(instructions)] <- instructions$ship_y[1] + instructions$value[1] * relative[2]
  instructions$waypoint_x[1:nrow(instructions)] <- instructions$ship_x[1] + relative[1]
  instructions$waypoint_y[1:nrow(instructions)] <- instructions$ship_y[1] + relative[2]
}


for (i in 2:nrow(instructions)){
  if (instructions$action[i] == "N") {
    instructions$waypoint_y[i:nrow(instructions)] <- instructions$waypoint_y[i-1] + instructions$value[i]
  } else if (instructions$action[i] == "S") {
    instructions$waypoint_y[i:nrow(instructions)] <- instructions$waypoint_y[i-1] - instructions$value[i]
  } else if (instructions$action[i] == "E") {
    instructions$waypoint_x[i:nrow(instructions)] <- instructions$waypoint_x[i-1] + instructions$value[i]
  } else if (instructions$action[i] == "W") {
    instructions$waypoint_x[i:nrow(instructions)] <- instructions$waypoint_x[i-1] - instructions$value[i]
  } else if (instructions$action[i] == "R") {
    relative <- c(instructions$waypoint_x[i-1] - instructions$ship_x[i-1],
                  instructions$waypoint_y[i-1] - instructions$ship_y[i-1])
    theta <- instructions$value[i]
    R <- matrix(c(cos(theta * pi/180), -sin(theta * pi/180), sin(theta * pi/180), cos(theta * pi/180)), nrow = 2, ncol = 2)
    new_relative <- round(R, 0) %*% relative
    instructions$waypoint_x[i:nrow(instructions)] <- instructions$ship_x[i-1] + new_relative[1]
    instructions$waypoint_y[i:nrow(instructions)] <- instructions$ship_y[i-1] + new_relative[2]
  } else if (instructions$action[i] == "L") {
    relative <- c(instructions$waypoint_x[i-1] - instructions$ship_x[i-1],
                  instructions$waypoint_y[i-1] - instructions$ship_y[i-1])
    theta <- instructions$value[i] 
    L <- matrix(c(cos(theta * pi/180), sin(theta * pi/180), -sin(theta * pi/180), cos(theta * pi/180)), nrow = 2, ncol = 2)
    new_relative <- round(L, 0) %*% relative
    instructions$waypoint_x[i:nrow(instructions)] <- instructions$ship_x[i-1] + new_relative[1]
    instructions$waypoint_y[i:nrow(instructions)] <- instructions$ship_y[i-1] + new_relative[2]
  } else if (instructions$action[i] == "F") {
    relative <- c(instructions$waypoint_x[i-1] - instructions$ship_x[i-1],
                  instructions$waypoint_y[i-1] - instructions$ship_y[i-1])
    instructions$ship_x[i:nrow(instructions)] <- instructions$ship_x[i-1] + instructions$value[i] * relative[1]
    instructions$ship_y[i:nrow(instructions)] <- instructions$ship_y[i-1] + instructions$value[i] * relative[2]
    instructions$waypoint_x[i:nrow(instructions)] <- instructions$ship_x[i] + relative[1]
    instructions$waypoint_y[i:nrow(instructions)] <- instructions$ship_y[i] + relative[2]
  }
}

print(paste0("Answer is ", abs(instructions$ship_x[nrow(instructions)]) + abs(instructions$ship_y[nrow(instructions)])))


instructions$waypoint_x[i-1] - instructions$ship_x[i-1]
instructions$waypoint_y[i-1] - instructions$ship_y[i-1]
round(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, ncol = 2),0)
round(matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2, ncol = 2),0)

