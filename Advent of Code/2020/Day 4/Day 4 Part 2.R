#--- Part Two ---
#  The line is moving more quickly now, but you overhear airport security 
#talking about how passports with invalid data are getting through. Better add 
#some data validation, quick!
#  
#  You can continue to ignore the cid field, but each other field has strict 
#rules about what values are valid for automatic validation:
  
#  byr (Birth Year) - four digits; at least 1920 and at most 2002.
#iyr (Issue Year) - four digits; at least 2010 and at most 2020.
#eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
#hgt (Height) - a number followed by either cm or in:
#  If cm, the number must be at least 150 and at most 193.
#If in, the number must be at least 59 and at most 76.
#hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
#ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
#pid (Passport ID) - a nine-digit number, including leading zeroes.
#cid (Country ID) - ignored, missing or not.
#Your job is to count the passports where all required fields are both present 
#and valid according to the above rules. Here are some example values:
#  
#  byr valid:   2002
#byr invalid: 2003
#
#hgt valid:   60in
#hgt valid:   190cm
#hgt invalid: 190in
#hgt invalid: 190
#
#hcl valid:   #123abc
#  hcl invalid: #123abz
#  hcl invalid: 123abc
#
#
#ecl valid:   brn
#ecl invalid: wat
#
#pid valid:   000000001
#pid invalid: 0123456789
#Here are some invalid passports:
#  
#  eyr:1972 cid:100
#hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
#  
#  iyr:2019
#hcl:#602927 eyr:1967 hgt:170cm
#  ecl:grn pid:012533040 byr:1946
#
#hcl:dab227 iyr:2012
#ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
#
#hgt:59cm ecl:zzz
#eyr:2038 hcl:74454a iyr:2023
#pid:3556412378 byr:2007
#Here are some valid passports:
#  
#  pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
#hcl:#623a2f
#  
#  eyr:2029 ecl:blu cid:129 byr:1989
#iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
#  
#  hcl:#888785
#  hgt:164cm byr:2001 iyr:2015 cid:88
#pid:545766238 ecl:hzl
#eyr:2022
#
#iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
#  Count the number of valid passports - those that have all required fields and
#valid values. Continue to treat cid as optional. In your batch file, how many 
#passports are valid?

complete_passports <- fields_lgl_df
complete_passports$ID <- 1:nrow(complete_passports)
complete_passports <- complete_passports[complete_passports$complete == 1,]

for(x in fields){
  for (i in 1:nrow(complete_passports)){
    doc_ID <- complete_passports[i, "ID"]
    complete_passports[i, x] <- str_match(documents[doc_ID], paste0(x, ":[[:graph:]]+")) %>%
      gsub(paste0(x, ":"), "", .)
  }
}

### byr tests ###
length_check <- grepl("\\d{4}", complete_passports$byr)
complete_passports <- complete_passports[length_check, ]
complete_passports$byr <- as.double(complete_passports$byr)
range_check <- (complete_passports$byr >= 1920 & complete_passports$byr <= 2002)
complete_passports <- complete_passports[range_check, ]

### iyr tests ###
length_check <- grepl("\\d{4}", complete_passports$iyr)
complete_passports <- complete_passports[length_check, ]
complete_passports$iyr <- as.double(complete_passports$iyr)
range_check <- (complete_passports$iyr >= 2010 & complete_passports$iyr <= 2020)
complete_passports <- complete_passports[range_check, ]

### eyr tests ###
length_check <- grepl("\\d{4}", complete_passports$eyr)
complete_passports <- complete_passports[length_check, ]
complete_passports$eyr <- as.double(complete_passports$eyr)
range_check <- (complete_passports$eyr >= 2020 & complete_passports$eyr <= 2030)
complete_passports <- complete_passports[range_check, ]

### hgt tests ###
format_check <- grepl("[[:digit:]]*cm", complete_passports$hgt) | grepl("[[:digit:]]*in", complete_passports$hgt)
complete_passports <- complete_passports[format_check, ]
range_check <- logical(length = nrow(complete_passports))
for (i in 1:nrow(complete_passports)){
  value <- as.double(str_match(complete_passports[i, "hgt"], "[[:digit:]]+")) 
  measure <- str_match(complete_passports[i, "hgt"], "[[:alpha:]]+")
  if (measure == "cm" & value >= 150 & value <= 193){
    range_check[i] <- TRUE
  } else if (measure == "in" & value >= 59 & value <= 76){
    range_check[i] <- TRUE
  } else {
    range_check[i] <- FALSE
  }
}
complete_passports <- complete_passports[range_check, ]

### hcl tests ###
length_check <- str_length(complete_passports$hcl) == 7
complete_passports <- complete_passports[length_check, ]
format_check <- grepl("#[0-9A-Fa-f]{6}", complete_passports$hcl)
complete_passports <- complete_passports[format_check, ]

### ecl tests ###
range_check <- complete_passports$ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
complete_passports <- complete_passports[range_check, ]

### pid tests ###
length_check <- str_length(complete_passports$pid) == 9
complete_passports <- complete_passports[length_check, ]
format_check <- grepl("[[:digit:]]{9}", complete_passports$pid)
complete_passports <- complete_passports[format_check, ]

print(paste0("Answer is ", nrow(complete_passports)))
