# Chapter 10 tibble

library(tidyverse)
as.tibble(iris)
tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

# no syntatic names - not working !
tb <- tibble(
  `:)` = "smile",
  `` = "space",
  `2000` = "number"
)

# tribble
tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
  print(n = 10, width = Inf)

# subsetting
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df
df$x
df["x"]
df[["x"]] # difference
df[[1]]

df %>% .$x # placehoder .

# convert back to data.frame
class(as.data.frame(df))

# quiz
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]
str(df)
