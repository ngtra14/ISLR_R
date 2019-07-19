# Chapter 11 data import
library(tidyverse)

# inine csv file
read_csv("a, b, c
         1, 2, 3
         4, 5, 6")

# skip lines
read_csv("The first line of metadat
the second line of metadata
        a, b, c
         1, 2, 3
         4, 5, 6", skip = 2)

read_csv("# The first line of metadat
# the second line of metadata
        a, b, c
         1, 2, 3
         4, 5, 6", comment = "#")

# no column names
read_csv("1, 2, 3\n4, 5, 6", col_names = FALSE)

read_csv("1, 2, 3\n4, 5, 6", 
         col_names = c("x", "y", "z"))

# dealing with NAs
read_csv("a, b, c\n4, 5, .", 
         na = ".")

# alternave 
# data.table::fread()

# quiz
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")

# parsing a vector
str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_integer(c("1", "2", "3")))

str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45")) # warning
x
problems(x)

# numbers
parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")

parse_number("20%")

parse_number("It cost $123.45")

# Used in America
parse_number("$123,456,789")

# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

# strings
charToRaw("Hadley")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))

parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))

guess_encoding(charToRaw(x2))

# factors
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

# dates and time
parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

parse_date("2010-10-01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr")) # french

# parsing a file
# write into a file