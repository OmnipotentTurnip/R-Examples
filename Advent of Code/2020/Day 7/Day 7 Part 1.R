library(tidyverse)

rules <- read.delim("input.txt", header = FALSE)[, 1] %>%
  gsub("bag[s]*", "", .) %>%
  gsub("\\s*\\.+", "", .) %>%
  strsplit("  contain ", fixed = TRUE) %>%
  do.call(rbind.data.frame, .)

names(rules) <- c("outer", "inner")

#first set
bag_colours <- c("shiny gold")
outer_bags <- map(bag_colours, function(colour){
  rules$outer[grepl(colour, rules$inner)]
})
update_bag_colours <- c(unlist(outer_bags))

# remaining
while (length(bag_colours) != length(update_bag_colours)) {
  bag_colours <- update_bag_colours
  outer_bags <- map(bag_colours, function(colour){
    rules$outer[grepl(colour, rules$inner)]
  })
  update_bag_colours <- unique(c(bag_colours, unlist(outer_bags)))
}

print(paste0("Answer is ", length(bag_colours)))

