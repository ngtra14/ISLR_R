
# Group data points by month ----------------------------------------------
# https://www.r-bloggers.com/2022/06/arrange-data-by-month-in-r-with-example/

df <- data.frame(date=as.Date(c('1/5/2022', 
                                '1/10/2022', 
                                '2/12/2022', 
                                '2/18/2022',
                                '3/15/2022', 
                                '3/20/2022', 
                                '3/30/2022'), 
                                 '%m/%d/%Y'),
                 sales=c(22, 11, 32, 14, 15, 22, 33))

library(tidyverse)
df %>% 
  group_by(month = lubridate::floor_date(date, 'month')) %>% 
  summarise(sum_of_sales = sum(sales))
