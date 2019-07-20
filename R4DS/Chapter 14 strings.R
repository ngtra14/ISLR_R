# Chapter 14 strings
library(tidyverse)
library(stringr)
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

x <- c("\"", "\\")
x
writeLines(x)

x <- "\u00b5"
x

# string length
str_length(c("a", "R for data science", NA))

# combine strings
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix") # shorter matches longer

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
) # drop length = 0

str_c(c("x", "y", "z"), collapse = ", ") # collapse

# subsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

# locales
str_to_upper(c("i", "i"))
str_to_upper(c("i", "i"), locale = "tr")

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x, locale = "haw")
