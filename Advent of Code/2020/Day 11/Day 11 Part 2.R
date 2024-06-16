layout <- read.delim("input.txt", header = FALSE)[,1] %>%
  str_split("")
layout <- do.call(rbind.data.frame, layout) %>%
  rbind(".", ., ".") %>%
  cbind(".", ., ".")
names(layout) <- 1:ncol(layout)

surrounding_seats <- function(x, y){
  
  #N
  i = x
  j = y
  while (old_layout[i - 1,j] == "." & i - 1 > 1){
    i = i-1
  }
  seats <- old_layout[i - 1, j]
  
  #NE
  i = x
  j = y
  while (old_layout[i - 1,j + 1] == "." & i - 1 > 1 & j + 1 < ncol(old_layout)){
    i = i-1
    j = j+1
  }
  seats <- c(seats, old_layout[i - 1, j + 1])
  
  #E
  i = x
  j = y
  while (old_layout[i,j + 1] == "." & j + 1 < ncol(old_layout)){
    j = j+1
  }
  seats <- c(seats, old_layout[i, j + 1])
  
  #SE
  i = x
  j = y
  while (old_layout[i + 1,j + 1] == "." & i + 1 < nrow(old_layout) & j + 1 < ncol(old_layout)){
    i = i+1
    j = j+1
  }
  seats <- c(seats, old_layout[i + 1, j + 1])
  
  #S
  i = x
  j = y
  while (old_layout[i + 1,j] == "." & i + 1 < nrow(old_layout)){
    i = i+1
  }
  seats <- c(seats, old_layout[i + 1, j])
  
  #SW
  i = x
  j = y
  while (old_layout[i + 1,j - 1] == "." & i + 1 < nrow(old_layout) & j - 1 > 1){
    i = i+1
    j = j-1
  }
  seats <- c(seats, old_layout[i + 1, j - 1])
  
  #W
  i = x 
  j = y
  while (old_layout[i,j - 1] == "." & j - 1 > 1){
    j = j-1
  }
  seats <- c(seats, old_layout[i, j - 1])

  #NW
  i = x
  j = y
  while (old_layout[i - 1,j - 1] == "." & i - 1 > 1 & j - 1 > 1){
    i = i-1
    j = j-1
  }
  seats <- c(seats, old_layout[i - 1, j - 1])
  seats
}

old_layout <- as.data.frame(matrix(data = rep("L", nrow(layout)*ncol(layout)), nrow = nrow(layout), ncol = ncol(layout)))
k = 1
while(!all(layout == old_layout)){
  old_layout <- layout
  for (i in 2:(nrow(old_layout)-1)){
    for (j in 2:(ncol(old_layout)-1)){
      if (old_layout[i,j] == "L"){
        if (all(surrounding_seats(i,j) != "#")){
          layout[i,j] <- "#"
        }
      } else if (layout[i,j] == "#"){
        if (length(surrounding_seats(i,j)[surrounding_seats(i,j) == "#"]) >= 5){
          layout[i,j] <- "L"
        }
      }
      
    }
  }
  print(k)
  k = k+1
}


print(paste0("Answer is ", sum(layout == "#")))
