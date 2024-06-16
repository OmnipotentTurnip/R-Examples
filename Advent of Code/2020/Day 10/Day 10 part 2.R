library(purrr)
library(gtools)

jolt_diff_1 <- jolt_diff[jolt_diff == 1]
jolt_diff_2 <- jolt_diff[jolt_diff == 2]
jolt_diff_3 <- jolt_diff[jolt_diff == 3]

one_groups <- double(length = sum(jolt_diff_3)/3 + 1)
j = 1

for (i in 1:length(jolt_diff)){
  if (jolt_diff[i] == 1) {
    one_groups[j] <- one_groups[j] + 1
  } else if (jolt_diff[i] == 3) {
    j = j + 1
  } else {
    print("ERROR")
  }
}

lines <- map(1:4, function(x){
  line_combos <- as.data.frame(combinations(4, x, 1:4))
  gaps <- apply(line_combos, 1, function(y){max(c(y, 4) - c(0,y)) <= 3})
  line_combos[unlist(gaps),]
})

combinations <- map(one_groups, function(x){
  if (x %in% c(0,1)) {
    combinations <- 1
  } else {
    test <- map(1:(x-1), function(y){
    line_combos <- as.data.frame(combinations((x-1), y, 1:(x-1)))
    gaps <- apply(line_combos, 1, function(z){max(c(z, x) - c(0,z)) <= 3})
    nrow(as.data.frame(line_combos[unlist(gaps),]))
  })
  if (x %in% c(2,3)){
    combinations <- 1 + sum(unlist(test))
  } else if (x > 3) {
    combinations <- sum(unlist(test))
  }
  }
  })


print(paste0("Answer is ", prod(unlist(combinations))))
