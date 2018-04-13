# Dat_1.R
# The First day of the stats class 
# Purpose: to practice some of the concepts that we will encounter
# 12 April 2018

#Set-up

library(tidyverse)
library(tibble)

# Integers ----------------------------------------------------------------

#Generate integer data
Integer_r <- as.integer(seq(5, 14, by = 1))

#List data
Integer_r 

#summarise data
summary(Integer_r)


# Continuous --------------------------------------------------------------

#Generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10)


# Dates -------------------------------------------------------------------

as.Date("2005-12-31") - as.Date("2005-12-12")
# or
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
#summarise data
summary(dates_r)


# Dataframes --------------------------------------------------------------

#Create  the base dataframe
df_r <- data.frame(integers = Integer_r,
                   numeric = numeric_r,
                   dates = dates_r)
#Then upgrade it to a tibble
df_r <- as_tibble(df_r)
summary(df_r)


# Categories --------------------------------------------------------------


#Electronics
elec_r <- as.factor(c("laptops",
                      "desktops", 
                      "cell phones"))

#People
people_r <- as.factor(c("funny",
                        "beautiful pepole",
                        "beanies"))

#Colours
colour_r <- as.factor(c("red","blue"))


# Ordianal data -----------------------------------------------------------

#Here we still have qualitative data
#bit with some sort of order

colour_qual <- ordered(c("blue", "green",
                         "yellow", "orange",
                         "red"),
                       levels = c("blue", "green",
                                  "yellow", "orange",
                                  "red"))


# Binary ------------------------------------------------------------------

#These are generally represented as : TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


# Characters --------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gamsbaai", "Sea Point")


# Missing values ----------------------------------------------------------

# Number of eggs recorded in a nest

# The NA shows a nest that was not able to be sampled
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)

# summarize data
summary(chicks_nest)

#mean
mean(chicks_nest)

#standard deviation
sd(chicks_nest)


# Viewing Data ------------------------------------------------------------

#List data
ChickWeight

#summarized data
summary(ChickWeight)

#to view either the top or bottom of the dataset use head or tail and the number of values to display

head(ChickWeight, 15)

tail(ChickWeight, 15)


# Descriptive statistics --------------------------------------------------

# First create a dataframe
chicks <- as_tibble(ChickWeight)

#Count the data
chicks %>%
  summarise(chicken_count = n())

#or
nrow(chicks)


# Measures of central tendancy --------------------------------------------

#Calculate mean weight
chicks %>%
  summarise(mean_weight = mean(weight))

#be more specific
central_chicks <- chicks %>%
  filter(Time == 21) %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight), 
            median_weight = median(weight))

#Visualize the density of the data
ggplot(data = filter(chicks, Time == 21),
       aes (x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = central_chicks,
             aes(xintercept =mean_weight,
                 colour =Diet), size = 1.5) +
  geom_vline(data = central_chicks,
             aes(xintercept =median_weight,
                 colour =Diet), size = 1.5, linetype = "dashed")
# Wo!Such plot. Many lines. Good job :)

# Skewness ----------------------------------------------------------------

#calculate the numeric value

library(e1071)

#Compare difference in mean and median against skewness

chicks %>%
  filter(Time == 21) %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight), 
            median_weight = median(weight),
            skew_weight = skewness(weight))


# kurtosis ----------------------------------------------------------------

chicks %>%
  filter(Time == 21) %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight), 
            median_weight = median(weight),
            skew_weight = skewness(weight),
            kurt_weight =kurtosis(weight))

#Kurtosis has no tails

exp_r <- data.frame(dat =rexp(n = 500),
                    sample = "A")

ggplot(data = exp_r, aes(x = dat)) +
  geom_density()

kurtosis(exo_r$dat)

# Measures of variablity --------------------------------------------------

#Below is a summary of many different statistical properties

weight_summary <- chicks %>%
  filter(Time == 21) %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight), 
            median_weight = median(weight),
            weight_variance = var(weight),
            weight_sd = sd(weight),
            weight_min =min(weight),
            weight_quart1 = quantile(weight, 0.25),
            weight_quart2 = quantile(weight, 0.5),
            weight_quart3 = quantile(weight, 0.75))




