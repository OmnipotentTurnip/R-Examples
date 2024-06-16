library (purrr)
programme <- read_lines("input.txt") %>% str_split(" = ")
programme <- do.call(rbind.data.frame, programme)
names(programme) <- c("action", "value")
mask <- c()
mem <- c()
powers.of.two <- 2^(0:35)
ids <- numeric()
values <- numeric()
assigned <- data.frame(ids, values)

for (i in 1:nrow(programme)){
  if (programme$action[i] == "mask"){
    mask <- rev(c(unlist(str_split(programme$value[i], ""))))
  } else {
    mem_id <- as.numeric(str_extract(programme$action[i], "\\d+"))
    asbinary <- c(as.character(as.integer(head(intToBits(mem_id), 32))), rep("0", times = 4))
    address <- asbinary
    address[mask != "0"] <- mask[mask != "0"]
    if (!("X" %in% address)){
      addresses <- list(address)
    } else {
      incomplete_addresses <- list(address)
      while(length(incomplete_addresses) > 0){
        addresses <- unlist(map(incomplete_addresses, function(x){
          id = Position(function(y) y == "X", x)
          address_0 <- x
          address_0[id] <- "0"
          address_1 <- x
          address_1[id] <- "1"
          list(address_0, address_1)
        }), recursive = FALSE)
        addresses_lgl <- map_lgl(addresses, function(x){
          "X" %in% x
        })
        incomplete_addresses <- addresses[addresses_lgl]
      }
    }
    mem_id <- map_dbl(addresses, function(x){
      as.numeric(as.numeric(x) %*% powers.of.two)
    })
    
    assigned$values[assigned$ids %in% mem_id] <- programme$value[i]
    if(all(mem_id %in% assigned$ids)){
    } else {
    assigned <- rbind(assigned,
                      data.frame("ids" = mem_id[!(mem_id %in% assigned$ids)], "values" = programme$value[i]))
    }
  }
}

print(paste0("Answer is ", sum(as.numeric(assigned$values))))




  