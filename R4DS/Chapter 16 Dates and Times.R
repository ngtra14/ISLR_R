# Chapter 16 Dates and Times
library(tidyverse)
library(lubridate)
library(nycflights13)

# --- create date and time ---
today()
now()

# from strings
ymd("2017-01-31")
mdy("January, 31st, 2017")
dmy("31-Jan-2017")

ymd(20170131)

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd(20170131, tz = "UTC") # force time zone

# from different columns in tables
flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # across a year

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 10 mins

# make date and time from other types
as_datetime(today())
as_date(now())

as_datetime(60*60*10) # 10 hours later
as_date(365*10 + 2) # ten years later on Jan 2nd

# --- date time components ---
datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

# departure time
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

# rounding
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

# setting components
(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime

month(datetime) <- 01
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
datetime

ymd("2015-02-01") %>% 
  update(mday = 30) # roll over

ymd("2015-02-01") %>% 
  update(hour = 400) # roll over

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

# --- time spans ---
# duration - in seconds !
h_age <- today() - ymd(19791014)
h_age
as.duration(h_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(2)
dyears(1) + dweeks(12) + dhours(15)

tomorrow <- today() + dday(1)
last_year <- today() - dyears(1)

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm

one_pm + ddays(1)

# periods
one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# A leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
one_pm + ddays(1)
one_pm + days(1)

# flights case study
flights_dt %>% 
  filter(arr_time < dep_time) 

flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>% 
  filter(overnight, arr_time < dep_time)

# intervals
years(1) / days(1)

(today() %--% next_year) %/% days(1)

# --- time zones ---
Sys.timezone()
length(OlsonNames())
head(OlsonNames())

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1 - x2
x1 - x3

x4 <- c(x1, x2, x3)
x4

x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4
