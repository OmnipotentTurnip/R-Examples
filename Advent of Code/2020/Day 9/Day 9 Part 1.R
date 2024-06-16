library(tidyverse)

cypher <- read.delim("input.txt", header = FALSE)[[1]]

calculate_sums <- function(x){
  last_25 <- cypher[(x-25):(x-1)]
  sums <- numeric()
  
  for (m in 1:25) {
    for (n in 1:25) {
      if (last_25[m] != last_25[n]) {
        sums <- unique(c(sums, last_25[m] + last_25[n]))
      }
    }
  }
  return(sums)
}



i = 26
sums <- calculate_sums(i)
possible <- TRUE

while (possible == TRUE) {
  if (cypher[i] %in% sums) {
    i = i + 1
    sums <- calculate_sums(i)
  } else {
    possible = FALSE
  }
}

print(paste0("Answer is ", cypher[i]))

