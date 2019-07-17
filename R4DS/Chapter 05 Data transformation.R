# Chapter 05 Data transformation
library(nycflights13)
library(tidyverse)
flights
names(flights)

# dplyr basics
# filter rows with filter()
filter(flights, month ==1, day == 1)
jan1 <- filter(flights, month ==1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))

# comparisons
filter(flights, month = 1)
sqrt(2)^2 == 2
1/49 * 49 == 1

near(sqrt(2)^2, 2)
near(1/49*49, 1)

# logical operators
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# missing values
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

# confusion example
x <- NA
y <- NA
x == y

is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x)|x>1)

# quiz
NA^0
NA|TRUE
FALSE&NA
NA * 0

# arrange rows with arrange()
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# select columns with select()
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

# rename
rename(flights, tail_num = tailnum)
names(flights)
select(flights, time_hour, air_time, everything())

# quiz
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
select(flights, contains("TIME"))

# add new variables with mutate()
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
                      )
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

# to keep the new variables only
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hour)

# more creation functions
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

(x <- 1:10)
lag(x)
lead(x)

x
cumsum(x)
cummean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)
