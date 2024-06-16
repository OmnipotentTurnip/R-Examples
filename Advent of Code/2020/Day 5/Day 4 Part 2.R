one_lower <- boarding_passes$seat_id - 1
one_lower <- one_lower[one_lower != min(one_lower)]
one_higher <- boarding_passes$seat_id + 1
one_higher <- one_higher[one_higher != max(one_higher)]

my_seat <- c(one_lower, one_higher)
my_seat <- my_seat[!(my_seat %in% boarding_passes$seat_id)]

print(paste0("Answer is ", my_seat[1], " which is row ", floor(my_seat[1]/8), " column ", my_seat[1] %% 8))
