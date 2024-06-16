library(purrr)
library(stringr)
example1 <- "1 + 2 * 3 + 4 * 5 + 6"
example2 <- "1 + (2 * 3) + (4 * (5 + 6))"
example3 <- "2 * 3 + (4 * 5)"
example4 <- "5 + (8 * 3 + 9 + 3 * 4 * 3)"
example5 <- "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
example6 <- "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

input <- readLines("input.txt")

calculations <- input %>%
  gsub(" ", "", .)


calculation <- map(calculations, function(calculation){
while (!is.na(str_extract(calculation, "\\("))){
  brackets <- str_extract(calculation, "\\([\\d\\+*]+\\)")
  brackets_replacement <- brackets
  while(!is.na(str_extract(brackets_replacement, "\\+"))){
    addition <- str_extract(brackets_replacement, "\\d+\\+\\d+")
    brackets_replacement <- gsub(addition, as.character(eval(parse( text = paste((addition))))), brackets_replacement, fixed = TRUE)
  }
  while(!is.na(str_extract(brackets_replacement, "\\*"))){
    multiplication <- str_extract(brackets_replacement, "\\d+\\*\\d+")
    brackets_replacement <- gsub(multiplication, as.character(eval(parse( text = paste((multiplication))))), brackets_replacement, fixed = TRUE)
  }
  brackets_replacement <- brackets_replacement %>%
    gsub("\\(", "", .) %>%
    gsub("\\)", "", .)
  
  calculation <- gsub(brackets, brackets_replacement, calculation, fixed = TRUE)
}

while(!is.na(str_extract(calculation, "\\+"))){
  addition <- str_extract(calculation, "\\d+\\+\\d+")
  calculation <- gsub(addition, as.character(eval(parse( text = paste((addition))))), calculation, fixed = TRUE)
}
while(!is.na(str_extract(calculation, "\\*"))){
  multiplication <- str_extract(calculation, "\\d+\\*\\d+")
  calculation <- gsub(multiplication, as.character(eval(parse( text = paste((multiplication))))), calculation, fixed = TRUE)
}
calculation
})


print(paste0("Answer is ", sum(as.numeric(unlist(calculation)))))
