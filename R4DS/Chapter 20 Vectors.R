# Chapter 20 Vectors

# Intro -------------------------------------------------------------------

library(tidyverse)
typeof(letters)
typeof(1:10)
length(letters)

x <- list("a", "b", 1:10)
length(x)


# atomic vector -----------------------------------------------------------

# logic
1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)

# numeric
typeof(1)
typeof(1L)

# character
x <- "this is a reason"
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)

# missing values
NA
NA_integer_
NA_real_


# using atomic vectors ----------------------------------------------------

# coercion
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)
mean(y)

# the most complex type always wins.
typeof(c(1.5, "a"))

# scalars and recycling rules
sample(10) + 100
runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3

tibble( x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

# naming vectors
c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

# subsetting
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)] # "-" inside, not inside

x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]

x[x %% 2 == 0]

NA%%2

x <- c(abc = 1, def = 2, xyz = 5) # like a dictionary in python
x[c("xyz", "def")]


# recursive vectors - lists -----------------------------------------------
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

# visualizing lists
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

# subsetting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])


str(a[[1]])
str(a[[4]])
str(a[4])

a$a
a[["a"]] # equivalent

# The distinction between [ and [[ is really important for lists, 
# because [[ drills down into the list while [ returns a new, smaller list.


# attributes --------------------------------------------------------------

x <- 1:10
attr(x, "greeting")

attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
x

as.Date
methods("as.Date")
getS3method("as.Date", "default")


# augmented vectors -------------------------------------------------------

# factors
# Factors are built on top of integers, and have a levels attribute:
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)

# dates and date-times
x <- as.Date("1971-01-01")
unclass(x)

typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)

typeof(x)
attributes(x)

attr(x, "tzone")

y <- as.POSIXlt(x)
typeof(y)
attributes(y)

# tibbles
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)

attributes(tb)

# The difference between a tibble and a list is that all the elements of a 
# data frame must be vectors with the same length. 
df <- data.frame(x = 1:5, y = 5:1)
typeof(df)

attributes(df)




