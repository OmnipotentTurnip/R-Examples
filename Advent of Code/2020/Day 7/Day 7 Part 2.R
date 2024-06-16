library(stringr)

rules <- read.delim("input.txt", header = FALSE)[, 1] %>%
  gsub("bag[s]*", "", .) %>%
  gsub("\\s*\\.+", "", .) %>%
  gsub("no other", "0 no other", .) %>%
  strsplit("  contain ", fixed = TRUE) %>%
  do.call(rbind.data.frame, .) %>%
  rbind(c("no other", "0 no other"))

names(rules) <- c("outer", "inner")

colour <- c("shiny gold")
amount <- c("shiny gold" = 1)
bag_count <- 0
i = 0
while (sum(amount) != 0){
  colour_update <- lapply(colour, function(x){
    str_extract_all(rules$inner[rules$outer == x], "[[:alpha:]]+\\s[[:alpha:]]+")
  })
  amount_update <- lapply(colour, function(x){
    amount[x] * as.numeric(str_extract_all(rules$inner[rules$outer == x], "[[:digit:]]+")[[1]])
  })
  colour <- c(unlist(colour_update))
  amount <- c(unlist(amount_update))
  update <- data.frame(colour, amount) %>%
    group_by(colour) %>%
    summarise(amount = sum(amount), .groups = 'drop')
  amount <- update$amount
  colour <- update$colour
  names(amount) <- colour
  bag_count <- bag_count + sum(amount)
  i = i + 1
}

print(paste0("Answer is ", bag_count))

