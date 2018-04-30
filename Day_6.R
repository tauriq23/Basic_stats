# Day_6.R
# Tauriq Jamalie
# 26 April 2018
# Confidence intervals and data transformations


# Confidence intervals ----------------------------------------------------

#Load data

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

#Set-up

library(rcompanion)
library(tidyverse)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)
# steps as a function of 1 calculates the average number of steps in the entire class

# one-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data
groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

#produce a graph of the mean and CI and display the effect of teacher and sex on mean 

data_groupwiseMean <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

ggplot(data_groupwiseMean, aes(x = Sex, y = Mean, fill = Teacher)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper), position = "dodge", width = 0.95) +
  facet_wrap(~Teacher)

# Suggested Graph ------------------------------------------------------------

library(ggplot2)
 
ggplot(data_groupwiseMean, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax = Mean + Trad.upper,
                    colour = Teacher)) +
  facet_wrap(~Teacher)
  
# by bootstrapping
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)



# Testing assumptions -----------------------------------------------------

#Remember these important assumptions that need to be met before running t-tests, ANOVA or linear regressions
  #The dependent variable must be continuous.
  #The data must be independent of each other.
  #The data most be normally distributed.
  #The data must be homoscedastic

# Data transformation -----------------------------------------------------------

mutated_data <- data %>% 
  mutate(ln.steps = log(Steps), 
         log10.steps = log10(Steps),
         cube.steps = Steps^(1/3),
         sqrt.steps = sqrt(Steps)) %>% 
  select(-Student, -Rating) %>% 
  gather (key = "data.type", value = "trans.data",
        -Sex, -Teacher) %>% 
  mutate(data.type = as.factor(data.type))

# Visialization of transformed data- Histograms ---------------------------------

Hist1 <- ggplot(data = filter(mutated_data, data.type == "cube.steps"), aes(x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge") 

Hist2 <- ggplot(data = filter(mutated_data, data.type == "ln.steps"), aes(x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge") 

Hist3 <- ggplot(data = filter(mutated_data, data.type == "log10.steps"), aes(x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge") 

Hist4 <- ggplot(data = filter(mutated_data, data.type == "sqrt.steps"), aes(x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge") 


library(ggpubr)
ggarrange(Hist1, Hist2, Hist3, Hist4, nrow = 2, ncol = 2, labels = "AUTO")


# Revision: iris data ANOVA ---------------------------------------------------------

iris.dat <- as.tibble(iris)

# H0: There is no significant difference in the petal length between three iris species
# H1: There is a significnat difference in the petal length between three iris species

# Test assumptions

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Length)[2]))

# We find that some of the species have non-normal data

#Do a Kruskal-Wallis test instead of an ANOVA

kruskal.test(Petal.Length ~ Species, data = iris)

# In conclusion p < 0.05, thus the null hypothesis is not accepted.
# There is a signifant difference between the petal length between three iris species
