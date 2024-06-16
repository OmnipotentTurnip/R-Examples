library(readr)
library(tidyverse)
library(purrr)
library(stringr)

start_time <- Sys.time()

z0 <- read_lines("input.txt") %>% strsplit("") %>% unlist() %>% matrix(.,nrow = 8, ncol = 8, byrow = TRUE)

pocket_dimension <- list("z(0)" = z0)

surrounding_boxes <- function(x, y, z){
  output <- c()
  for (i1 in -1:1){
    for (i2 in -1:1){
      for (i3 in -1:1){
        output <- c(output, expanded_pocket_dimension[[paste0("z(", z + i1, ")")]][x + 1 + i2, y + 1 + i3])
      }
    }
  }
  output
}

for (i in 1:6){

pocket_dimension <- map(pocket_dimension, function(x){
  rbind(".", x, ".") %>% cbind (".", .,".")
})
pocket_dimension[paste0("z(", -2*i+1,")")] <- list(matrix(data = rep(".", times = (4*i+6)^2), nrow = 4*i + 6, ncol = 4*i + 6))
pocket_dimension[paste0("z(", 2*i-1,")")] <- list(matrix(data = rep(".", times = (4*i+6)^2), nrow = 4*i + 6, ncol = 4*i + 6))


expanded_pocket_dimension <- map(pocket_dimension, function(x){
  rbind(".", x, ".") %>% cbind (".", .,".")
})
expanded_pocket_dimension[paste0("z(", -2*i,")")] <- list(matrix(data = rep(".", times = (4*i + 8)^2), nrow = 4*i + 8, ncol = 4*i + 8))
expanded_pocket_dimension[paste0("z(", 2*i,")")] <- list(matrix(data = rep(".", times = (4*i + 8)^2), nrow = 4*i + 8, ncol = 4*i + 8))


new_pocket_dimension <- expanded_pocket_dimension
for (o in as.numeric(str_extract(names(pocket_dimension), "-*[[:digit:]]"))){
  for (p in 1:nrow(pocket_dimension[[paste0("z(", o, ")")]])){
    for(q in 1:ncol(pocket_dimension[[paste0("z(", o, ")")]])){
      boxes <- surrounding_boxes(p, q, o)
  if ((pocket_dimension[[paste0("z(", o, ")")]][p, q] == "#") * (!(length(boxes[boxes == "#"]) %in% c(3, 4)))){
      new_pocket_dimension[[paste0("z(", o, ")")]][p + 1, q + 1] <- "."
  } else {
    if (length(boxes[boxes == "#"]) == 3){
      new_pocket_dimension[[paste0("z(", o, ")")]][p + 1, q + 1] <- "#"
    }
  }
    }
  }
}

pocket_dimension <- new_pocket_dimension
}

pocket_dimension <- map(pocket_dimension, function(y){apply(y, c(1,2), function(x){
  if (x == "#"){
    x <- 1
  } else {
    x <- 0
  }})})

print(paste0("Answer is ", sum(unlist(map(pocket_dimension, function(x){rowSums(x)})))))
print(Sys.time() - start_time)
                                                   