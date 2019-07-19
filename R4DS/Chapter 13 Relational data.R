# Chapter 13 Relational data
library(tidyverse)
library(nycflights13)
# four tibbles
airlines
airports
planes
weather
flights

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

# mutating joins
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>% 
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

# understanding joins
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# -- inner join --
x %>% 
  inner_join(y, by = "key")

# -- outer join --
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

# another example
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key") # error ?

# define the key columns
flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = 'faa'))

# quiz
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# -- filtering joins --
top_dest <- flights %>%
  count(dest, sort = TRUE) %>% 
  head(10)
top_dest

flights %>%
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)

flights %>%
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

# -- join problems --
airports %>% 
  count(alt, lon) %>% 
  filter(n > 1)

# -- set operations --
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)

df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)
