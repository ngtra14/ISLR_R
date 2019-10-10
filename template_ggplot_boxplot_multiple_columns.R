# How to use reshape to reshuffle data ------------------------------------
# Your sample data...
df <- data.frame(id = 1:10,
               var1 = rnorm(10),
               var2 = rnorm(10),
               var3= rnorm(10),
               factor.col= LETTERS[1:10]
)

head(df)

# Use the reshape2 package to merge the columns by id and factor.col
library(reshape2)
df_long <- melt(df, id=c("id","factor.col"))
head(df_long)

# And now plot the boxplots
library(ggplot2)
ggplot(df_long,aes(x=factor.col,y=value)) + 
  geom_boxplot()

# https://stackoverflow.com/questions/26164676/ggplot-boxplot-for-multiple-columns-with-a-factor/26165302