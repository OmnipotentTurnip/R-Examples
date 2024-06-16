library(stringr)

boot_code <- read.delim("input.txt", header = FALSE)[[1]] %>%
  str_split(" ")

boot_code <- data.frame(do.call(rbind.data.frame, boot_code))
names(boot_code) <- c("operation", "argument")
boot_code$visited <- FALSE
boot_code$argument <- as.numeric(boot_code$argument)

test_code <- function(x){
  i = 1
  acc = 0
  
  while (x$visited[i] == FALSE){
    x$visited[i] <- TRUE
    if (i == 624){
      print(paste0("Accumulator is ", acc, " and j is ", j))
      } else {
        if (x$operation[i] == "acc"){
          acc <- acc + x$argument[i]
          i <- i + 1
          } else if (x$operation[i] == "jmp"){
            i <- i + x$argument[i]
            } else {
              i <- i + 1
            }
      }
  }
}

for (j in 1:(nrow(boot_code) - 1)){
  if (boot_code$operation[j] == "nop") {
    bug_fix <- boot_code
    bug_fix$operation[j] <- "jmp"
    test_code(bug_fix)
  } else if (boot_code$operation[j] == "jmp") {
    bug_fix <- boot_code
    bug_fix$operation[j] <- "nop"
    test_code(bug_fix)
  } else {}
}
