# Load libraries ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
## ----scatter-plot, fig.cap="Scatter plot."-------------------------------
x <- rnorm(50)
y <- rnorm(50)
plot(x, y)

## ----cars-scatter-plot,"Scatter plot of speed and distance for cars."----
cars %>% plot(speed, dist, data =., main = "cars data", xlab = "speed",
              ylab = "stopping distance")
with(data = cars,
     plot(speed, dist, main="Cars data",
              xlab="Speed", ylab="Stopping distance")
)

## ----cars-histogram-  "Histogram plot of speed and distance for cars."----
with(data = cars,
     plot(speed, dist, main="Cars data", type="h",
              xlab="Speed", ylab="Stopping distance")
)

## ----cars-speed-histogram-plot, fig.cap="Histogram for cars speed."------
with(data = cars, hist(speed)
)

## ----"Longley data showing Unemployed and Armed.Forces. 
## The y-axis doesn't cover all of the Armed.Forces variable."----

longley %>% plot(Unemployed ~ Year, data = ., type = 'l')
longley %>% lines(Armed.Forces ~ Year, data = ., col = "blue")

## ----longley-corrected-y, fig.cap="Longley data showing Unemployed and 
## Armed.Forces. The y-axis is wide enough to hold all the data."----
with(data = longley,
     plot(Unemployed ~ Year, type = 'l',
                 ylim = range(c(Unemployed, Armed.Forces))),
     lines(Armed.Forces ~ Year, col = "blue")
)


## ----"The cars data points annotated with the best fitting line."----
cars %>% plot(dist ~ speed, data = .)
cars %>% lm(dist ~ speed, data = .) %>% abline(col = "red")

## ----iris-species-coloured, fig.cap="Iris data plotted with different 
## colours for different species."----
color_map <- c("setosa" = "red",
               "versicolor" = "green",
               "virginica" = "blue")
# some of the codes needs to add "data=.," statement, ODD
iris %>% plot(Petal.Length ~ Petal.Width, data = .,
              col = color_map[Species])


## ----cars-points-, "Plot of the cars data using qplot (ggplot2)."----
cars %>% qplot(speed, dist, data = .) # also need 'data=.'

## ----iris-coloured-according-to-species-qplot, "Plot of iris data 
## with colours determined by the species. 
iris %>% qplot(Petal.Width, Petal.Length , 
               color = Species, data = .)

## ----cars-speed-histogram-ggplot, fig.cap="Histogram of car speed 
## created using qplot (ggplot2)."----
cars %>% qplot(speed, data = ., bins = 10)

## ----cars-speed-density-ggplot, fig.cap="Density of car speed created 
## using qplot (ggplot2)."----
cars %>% qplot(speed, data = ., geom = "density")

## ----iris-red-ggplot, fig.cap="Iris data where the colour of the 
# points is hardwired."----
iris %>% ggplot +
  geom_point(aes(x = Petal.Width, y = Petal.Length), 
               color = "red")

## ----cars-combined-histogram-and-density----
cars %>% ggplot(aes(x = speed, y = ..count..)) +
  geom_histogram(bins = 10) +
  geom_density()

## ----"Cars data plotted with a linear model smoothing."----
cars %>% ggplot(aes(x = speed, y = dist)) + 
  geom_point() + geom_smooth(method = "lm")

## ----"Cars data plotted with a loess smoothing."----
cars %>% ggplot(aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth()

## ----"Longley data plotted with ggplot2."----
longley %>% ggplot(aes(x = Year)) +
  geom_line(aes(y = Unemployed)) + 
  geom_line(aes(y = Armed.Forces), color = "blue")

## ----"Longley data plotted with ggplot2 using both points and lines."----
longley %>% ggplot(aes(x = Year)) +
  geom_point(aes(y = Unemployed)) + 
  geom_point(aes(y = Armed.Forces), color = "blue") +
  geom_line(aes(y = Unemployed)) + 
  geom_line(aes(y = Armed.Forces), color = "blue")

## ----"Longley data plotted using tidy data."----
longley %>% gather(key, value, Unemployed, Armed.Forces) %>%
  ggplot(aes(x = Year, y = value, color = key)) + 
  geom_line()

## ----"Longley data plotted using facets."----
longley %>% gather(key, value, Unemployed, Armed.Forces) %>%
  ggplot(aes(x = Year, y = value)) + 
  geom_line() +
  facet_grid(key ~ .)

## ----"Iris measures plotted on the same y-axis."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value)) + 
  geom_boxplot() +
  facet_grid(Measurement ~ .)

# how it looks like after 'gather'
iris %>% gather(Measurement, Value, -Species) %>% 
  head()

## ---- "Iris measures plotted on different y-axes."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value)) + 
  geom_boxplot() +
  facet_grid(Measurement ~ ., scale = "free_y")

## ---- "Iris measures with measure labels adjusted."----
label_map <- c(Petal.Width = "Petal Width",
               Petal.Length = "Petal Length",
               Sepal.Width = "Sepal Width", 
               Sepal.Length = "Sepal Length")

iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value)) + 
  geom_boxplot() +
  facet_grid(Measurement ~ ., scale = "free_y",
             labeller = labeller(Measurement = label_map))

## ----"Iris data plotted with a factor on the x-axis."----
iris %>% ggplot(aes(x = Species, y = Petal.Length)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, height = 0.1)

## ---- "Iris data plotted with default fill colours."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  facet_grid(Measurement ~ ., scale = "free_y",
               labeller = labeller(Measurement = label_map))

## ---- "Iris data plotted with custom fill colours."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "green", "blue")) +
  facet_grid(Measurement ~ ., scale = "free_y",
               labeller = labeller(Measurement = label_map))

## ---- "Iris data plotted with a brewer fill colours."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  facet_grid(Measurement ~ ., scale = "free_y",
               labeller = labeller(Measurement = label_map))

## ---- "Iris with flipped coordinates and switched facet labels."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("setosa" = "Setosa",
                              "versicolor" = "Versicolor",
                              "virginica" = "Virginica")) +
  scale_fill_brewer(palette = "Greens") +
  facet_grid(Measurement ~ ., switch = "y",
             labeller = labeller(Measurement = label_map)) +
  coord_flip()
# many layers effect !

## ---- "Iris data with theme modifications."----
iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("setosa" = "Setosa",
                              "versicolor" = "Versicolor",
                              "virginica" = "Virginica")) +
  scale_fill_brewer(palette = "Greens") +
  facet_grid(Measurement ~ ., switch = "y",
             labeller = labeller(Measurement = label_map)) +
  coord_flip() +
  theme(strip.background = element_blank()) +
  theme(legend.position="top")

## ---- "Final version of iris plot"----

label_map <- c(Petal.Width = "Petal Width",
               Petal.Length = "Petal Length",
               Sepal.Width = "Sepal Width", 
               Sepal.Length = "Sepal Length")
species_map <- c(setosa = "Setosa",
                 versicolor = "Versicolor",
                 virginica = "Virginica")

iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_x_discrete(labels = species_map) +
  scale_fill_brewer(palette = "Greens", labels = species_map) +
  facet_grid(Measurement ~ ., switch = "y",
             labeller = labeller(Measurement = label_map)) +
  coord_flip() +
  theme(strip.background = element_blank()) +
  theme(legend.position="top")

## ------------------------------------------------------------------------
petal <- iris %>% ggplot() +
  geom_point(aes(x = Petal.Width, y = Petal.Length, 
                   color = Species)) +
  theme(legend.position="none")

sepal <- iris %>% ggplot() +
  geom_point(aes(x = Sepal.Width, y = Sepal.Length, 
                   color = Species)) +
  theme(legend.position="none")

## ---- warning=FALSE, echo=FALSE------------------------------------------
suppressPackageStartupMessages(library(gridExtra, quietly = TRUE))

## ----"Combining two plots of the iris data using grid.arrange."----
grid.arrange(petal, sepal, ncol = 2)

## ---- warning=FALSE, echo=FALSE------------------------------------------
suppressPackageStartupMessages(library(cowplot, quietly = TRUE))

## ----"Combining two plots of the iris data using cowplot."----
plot_grid(petal, sepal, labels = c("A", "B"))

## ---- echo=FALSE---------------------------------------------------------
theme_set(theme_bw())
