library(stringr)

boot_code <- read.delim("input.txt", header = FALSE)[[1]] %>%
  str_split(" ")

boot_code <- data.frame(do.call(rbind.data.frame, boot_code))
names(boot_code) <- c("operation", "argument")
boot_code$visited <- FALSE
boot_code$argument <- as.numeric(boot_code$argument)

code_test <- 
i = 1
acc = 0

while (boot_code$visited[i] == FALSE){
  boot_code$visited[i] <- TRUE
  if (boot_code$operation[i] == "acc"){
    acc <- acc + boot_code$argument[i]
    i <- i + 1
  } else if (boot_code$operation[i] == "jmp"){
    i <- i + boot_code$argument[i]
  } else {
    i <- i + 1
  }
}

print(paste0("Answer is ", acc))