adapters <- read.delim("input.txt", header = FALSE)[[1]]
adapter_order <- c(0, sort(adapters), max(adapters) + 3)
jolt_diff <- adapter_order[2:length(adapter_order)] - adapter_order[1:(length(adapter_order)-1)]

print(paste0("Answer is ", sum(jolt_diff[jolt_diff == 1]) * (sum(jolt_diff[jolt_diff == 3])/3)))
