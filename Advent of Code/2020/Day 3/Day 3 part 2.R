#--- Part Two ---
#  Time to check the rest of the slopes - you need to minimize the probability of a sudden arboreal stop, after all.
#
#Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:
#  
#  Right 1, down 1.
#Right 3, down 1. (This is the slope you already checked.)
#Right 5, down 1.
#Right 7, down 1.
#Right 1, down 2.
#In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.
#
#What do you get if you multiply together the number of trees encountered on each of the listed slopes?

right <- c(1, 3, 5, 7, 1)
left <- c(1, 1, 1, 1, 2)

toboggan_run <- function(x, y){
  i <- 1 # col number
  j <- 1 # row number
  tree_count <- 0
  
  while (j < nrow(slope)){
    i <- (i+x) %% 31
    j <- j+y
    if (i == 0) {
      i <- 31
      }
    if(slope[j, i] == "#"){
      tree_count <- tree_count + 1
    }
  }
  return(tree_count)
}

print(paste0("Answer is ", prod(map2_dbl(right, left, toboggan_run))))

