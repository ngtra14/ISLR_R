# chapter 04 workflow : basics
# coding basics

# do math
sin(pi/2)

# new objects
x <- 3 * 4

# object_name <- value

# --- snake_case ---
# i_use_snake_case
# otherPeopleUseCamelCase
# some.people.use.periods
# And_aFew.People_RENOUNCEconvention

r_rocks <- 2 ^ 3
r_rock
R_rocks # Error: object 'R_rocks' not found

# calling functions
seq(1, 10)
x <- "hello, world"

y <- seq(1, 10, length.out = 5)
y
(y <- seq(1, 10, length.out = 5))

# quiz
my_variable <- 10
my_variable

library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

# Press Alt + Shift + K