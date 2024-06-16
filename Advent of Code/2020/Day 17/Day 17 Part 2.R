## this took 8.585408 hours because it's lazy and can definitely be done in a quicker way

library(readr)
library(tidyverse)
library(purrr)
library(stringr)

start_time <- Sys.time()

z0 <- read_lines("input.txt") %>% strsplit("") %>% unlist()
z0 <- matrix(z0,nrow = sqrt(length(z0)), ncol = sqrt(length(z0)), byrow = TRUE)

space <- list()

for (i1 in 1:(nrow(z0)+14)){
  for (i2 in 1:(nrow(z0)+14)){
    for (i3 in 1:(nrow(z0)+14)){
      for (i4 in 1:(nrow(z0) + 14)){
        space[paste0("x", i1, "y", i2, "z", i3, "w", i4)] <- "."
      }
    }
  }
  print(paste0("i1 = ", i1))
}

colnames(z0) <- 1:nrow(z0) + 7
rownames(z0) <- nrow(z0):1 + 7

for (j1 in colnames(z0)){
  for (j2 in rownames(z0)){
    space[[paste0("x", j1, "y", j2, "z", floor((nrow(z0)+14)/2), "w", floor((nrow(z0)+14)/2))]] <- z0[j1, j2]
  }
}

surrounding_boxes <- function(x, y, z, w){
  output <- c()
  for (i1 in -1:1){
    for (i2 in -1:1){
      for (i3 in -1:1){
        for (i4 in -1:1){
          output <- c(output, space[[paste0("x", x + i1, "y", y + i2, "z", z + i3, "w", w + i4)]])
        }
      }
    }
  }
  output
}

new_space <- space

for (i in 1:6){
  for (k1 in (8-i):(nrow(z0) + 7 + i)){
    for (k2 in (8-i):(nrow(z0) + 7 + i)){
      for (k3 in (8-i):(nrow(z0) + 7 + i)){
        for(k4 in (8-i):(nrow(z0) + 7 + i )){
          boxes <- surrounding_boxes(k1, k2, k3, k4)
          if ((space[[paste0("x", k1, "y", k2, "z", k3, "w", k4)]] == "#") & (!(length(boxes[boxes == "#"]) %in% c(3, 4)))){
            new_space[[paste0("x", k1, "y", k2, "z", k3, "w", k4)]] <- "."
            } else {
              if (length(boxes[boxes == "#"]) == 3){
                new_space[[paste0("x", k1, "y", k2, "z", k3, "w", k4)]] <- "#"
              }
            }
        }
      }
    }
  }
  space <- new_space
  print(paste0("i = ", i))
}


space <- map(space, function(x){
  if (x == "#"){
    x <- 1
  } else {
    x <- 0
  }
  x
})

print(paste0("Answer is ", sum(unlist(space))))

print(Sys.time() - start_time)


