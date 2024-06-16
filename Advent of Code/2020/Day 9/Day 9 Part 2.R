invalid_number <- cypher[i]

for (a in 1:1000) {
  b = 1
  contiguous_sum <- cypher[a]
  while (contiguous_sum <= invalid_number & (a + b) <= 1000) {
    contiguous_sum <- contiguous_sum + cypher[a + b]
    if (contiguous_sum == invalid_number) {
      encryption_weakness <- list(c(a, b), c(min(cypher[a:(a+b)]), max(cypher[a:a+b])), min(cypher[a:(a+b)]) + max(cypher[a:(a+b)]))
      names(encryption_weakness) <- c("Indexes", "Numbers", "Sum")
      print(paste0("Answer is ", encryption_weakness["Sum"]))
    } else {
      b = b + 1
    }
  }
}
