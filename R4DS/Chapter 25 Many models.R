# Chapter 25 Many models
# Prerequisites
library(modelr)
library(tidyverse)
# for detailed text
# https://r4ds.had.co.nz/many-models.html
# gapminder dataset
library(gapminder)
gapminder

gapminder %>% 
  ggplot(aes(year, lifeExp, group=country)) +
  geom_line(alpha = 1/3)

# New Zealand as example
nz <- filter(gapminder, country=="New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>% 
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, colour="white", size=3) +
  geom_line() +
  ggtitle("Remaining pattern")

# nested data
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country # country, continent are factors now, data consists of tibbles

by_country$data[[1]]

# list-columns
country_model <- function(df){
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model) # purr:: map()

by_country <- by_country %>% 
  mutate(model=map(data, country_model))

by_country

by_country %>% 
  filter(continent=="Europe")

by_country %>% 
  arrange(continent, country)

# unnesting
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )

by_country

resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group=country), alpha=1/2)+
  geom_smooth(se=FALSE)

resids %>% 
  ggplot(aes(year, resid, group=country)) +
  geom_line(alpha = 1/3) +
  facet_wrap( ~ continent)

# model quality
broom::glance(nz_mod)

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) # suppress list-columns
glance

glance %>% 
  arrange(r.squared)

glance %>% 
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)
gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

# When applied to a grouped data frame, nest() keeps the grouping columns 
# as is, and bundles everything else into the list-column:
gapminder %>% 
  group_by(country, continent) %>% 
  nest()

gapminder %>% 
  nest(year:gdpPercap)

# from vectorized functions
df <- tribble(
  ~x1,
  "a, b, c",
  "d, e, f, g"
)

df %>% 
  mutate(x2 = stringr::str_split(x1, ","))

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

sim <- tribble(
  ~f, ~params,
  "runif", list(min=-1, max=1),
  "rnorm", list(sd=5),
  "rpois", list(lambda=10)
)

sim %>% 
  mutate(sims = invoke_map(f, params, n = 10))

# from multivalued summaries
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg)) # error

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) # correct one

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p=list(probs), q=list(quantile(mpg, probs))) %>% 
  unnest()

# from a named list
x <- list(
  a = 1:5,
  b = 3:4,
  c = 5:6
)

df <- enframe(x)
df
# Now if you want to iterate over names and values in parallel, 
# you can use map2():
df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

# quiz
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) %>% 
  unnest()

mtcars %>% 
  group_by(cyl) %>% 
  summarise_each(funs(list))

# simplifying list-columns
# list to vector
df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)
df

df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)

df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)

df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

# unnesting
tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

df1 <- tribble(
  ~x, ~y,           ~z,
  1, c("a", "b"), 1:2,
  2, "c",           3
)
df1

df1 %>% unnest(y, z)
# Doesn't work because y and z have different number of elements
df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)
df2

df2 %>% unnest(y, z)

# make tidy data with broom
broom::augment(nz_mod)
