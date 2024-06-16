source('~/R/Advent of Code/Day 16/Day 16 Part 1.R', echo=TRUE)

valid_tickets <- map(valid_tickets, function(x){all(x)})
valid_tickets <- nearby_tickets[unlist(valid_tickets)]
valid_tickets <- do.call(rbind.data.frame, valid_tickets)
colnames(valid_tickets) <- paste0("cat", 1:length(categories))
tickets_with_cats <- valid_tickets

while (ncol(valid_tickets) > 0){
possible_cats <- map(categories, function(x){
  apply(valid_tickets, 2,  function(y){all(y %in% rules[[x]])})
})

names(possible_cats) = categories

possible_count <- map_int(possible_cats, function(x){sum(x)})
names(possible_count) <- categories
print(min(possible_count))
explicit_cats <- names(possible_count[possible_count == min(possible_count)])  


for (x in explicit_cats){
tickets_with_cats <- rename(tickets_with_cats, !!x := names(which(possible_cats[[x]] == TRUE)))
}
valid_tickets <- select(valid_tickets, -names(which(possible_cats[[explicit_cats]] == TRUE)))
categories <- categories[categories != explicit_cats]
i = i + 1
}

names(my_ticket) <- colnames(tickets_with_cats)

print(paste0("Answer is ", prod(my_ticket[grepl("departure", names(my_ticket))])))

