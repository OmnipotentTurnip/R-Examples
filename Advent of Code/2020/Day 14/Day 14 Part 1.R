library(readr)
library(stringr)

programme <- read_lines("input.txt") %>% str_split(" = ")
programme <- do.call(rbind.data.frame, programme)
names(programme) <- c("action", "value")
mask <- c()
mem <- c()
powers.of.two <- 2^(0:35)

for (i in 1:nrow(programme)){
  if (programme$action[i] == "mask"){
    mask <- as.integer(rev(c(unlist(str_split(programme$value[i], "")))))
  } else {
    mem_id <- as.numeric(str_extract(programme$action[i], "\\d+"))
    asbinary <- c(as.integer(head(intToBits(programme$value[[i]]), 32)), rep.int(0, times = 4))
    asbinary[!is.na(mask)] <- mask[!is.na(mask)]
    mem[mem_id] <- as.numeric(asbinary %*% powers.of.two)
  }
}

print(paste0("Answer is ", sum(as.numeric(mem), na.rm = TRUE)))
