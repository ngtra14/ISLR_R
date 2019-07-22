# Chapter 14 Strings - regular expressions
# --- basic matches ---
library(tidyverse)

x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

# --- anchors ---
# ^ to match the start of the string.
# $ to match the end of the string.
x <- c("apple", "banana", "pear")
str_view(x, "^a")

str_view(x, "a$")

# To force a regular expression to only match a complete string, 
# anchor it with both ^ and $:
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$")

# --- Character classes and alternatives ---
# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.
# Look for a literal character that normally has special meaning in a regex
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

str_view(c("grey", "gray"), "gr(e|a)y")

# --- Repetition ---
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')

# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

# You can make them "lazy", matching the shortest string possible by putting a ?
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

# --- Grouping and backreferences ---
str_view(fruit, "(..)\\1", match = TRUE)

# -- Tools ---
#  Detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with t?
sum(str_detect(words, "^t"))

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")

# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

# A common use of str_detect() is to select the elements that match a pattern.
words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(
  word = words, 
  i = seq_along(word)
)
df

df %>% 
  filter(str_detect(word, "x$"))

# count string
x <- c("apple", "banana", "pear")
str_count(x, "a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
str_view(words, "[aeiou]")

# pay attention to "^"
df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

# Note that matches never overlap
str_count("abababa", "aba")
str_view_all("abababa", "aba")

# Extract matches
length(sentences)
head(sentences)

# to find all sentences that contain a colour
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

str_extract(more, colour_match)

str_extract_all(more, colour_match) # return a list

str_extract_all(more, colour_match, simplify = TRUE) # return a matrix

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

# --- grouped matches ---
# extract nouns from the sentences
noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)

has_noun %>% 
  str_extract(noun) # complete match

has_noun %>% 
  str_match(noun) # give individual components

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )

# --- replacing matches --- 
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-") # replace all

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

# flip the order of the second and third words
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

# --- splitting ---
sentences %>%
  head(5) %>% 
  str_split(" ")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))

str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

# --- find matches ---
str_locate(x, "e")


# --- other tpyes of pattern ---
# The regular call:
str_view(fruit, "nana")

# Is shorthand for
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)

microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1 == a2

str_detect(a1, fixed(a2))

str_detect(a1, coll(a2))

# That means you also need to be aware of the difference
# when doing case insensitive matches:
i <- c("I", "I", "i", "i")
i

str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))

# --- Other uses of regular expressions ---
apropos("replace")

# # find all the R Markdown files in the current directory with
head(dir(pattern = "\\.Rmd$")) 

# --- stringi vs stringr ---
# stringi has 234 functions to stringr's 46.

