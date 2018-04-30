# Day_5.R
# Tauriq Jamalie
# 19 April 2018
# ANOVA, simple linear regressions and correlations


# Set-up ------------------------------------------------------------------

library(tidyverse)
# library(Rmisc) # Unfortunately this overrides many dplyr functions


# Load data ---------------------------------------------------------------

snakes <- read_csv("snakes.csv")
snakes$day = as.factor(snakes$day)

# Summarise the data -----------------------------------------------------

snakes.summary <- snakes %>% 
  dplyr::group_by(day) %>% 
  dplyr::summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

# Formulate a hypothesis --------------------------------------------------

#H0: There is no difference in the number of openings from day to day
#H1: There is a difference in the number of openings from day to day


# Test a hypothesis --------------------------------------------------------

#Fiest calculate SE and CI

snakes.summary2 <- Rmisc::summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

#Then visualise the data

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#But wait, we have two factors, so we need another null hypothesis
#H0: There is no difference between snakes with respect to  
  #the number of openings at which they habituate
#H0: There is no difference between days in terms of
  #the number if openings at which the snakees habituate

#Test just the days hypothesis

snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)

#Test both hypothesis

snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)


# Test assumptions afterwards ---------------------------------------------

#First visualise normality

snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

#The visualize homoscedasticicity of results

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))
     
#Check Tukey results

snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
snakes.tukey
plot(snakes.tukey)

#Visualise the factor interaction

ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3)+
  geom_point(size = 4)


# Exercise  ---------------------------------------------------------------

#Get the moth data from Github
#Run a two-way ANOVA on them

moth_trap <- read_csv("moth_trap.csv") %>% 
  gather(key = "trap", value = "count", -Location)
moth_trap$trap = as.factor(moth_trap$trap)

moth_trap.summary <- moth_trap %>% 
  dplyr::group_by(trap) %>% 
  dplyr::summarise(mean_openings = mean(count),
            sd_openings = sd(count)) %>% 
  ungroup()
moth_trap

moth_trap.summary2 <- Rmisc::summarySE(data = moth_trap, measurevar = "count", groupvars = c("trap"))

plt1 <- ggplot(moth_trap, aes(x = Location, y = count)) +
  geom_boxplot()+
  geom_jitter(width = 0.05, shape = 21)

plt2 <- ggplot(data = moth_trap, aes(x = trap, y = count)) +
  geom_segment(data = moth_trap.summary2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

plt3 <- ggplot(moth_trap, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap)) +
  geom_jitter(width =0.05, shape = 21)

library(ggpubr)
ggarrange(plt1, plt2, plt3, nrow = 2, ncol = 2, labels = "AUTO")

moth_trap.aov <- aov(count ~ trap * Location, data = moth_trap)
summary(moth_trap.aov)

# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
moth_trap.res <- residuals(moth_trap.aov)
hist(moth_trap.res)

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(moth_trap.aov), residuals(moth_trap.aov))

#Difference?

moth_trap.tukey <- TukeyHSD(moth_trap.aov, which = "trap")
plot(moth_trap.tukey)


# Regressions -------------------------------------------------------------

#For the explanation of this statistical analysis
#We are going to use eruption data from ol' faithful

head(faithful)

#Scatterplot

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")

#Hypothesis

#H0: The waitning time does not influence the duration of an eruption
#H1: The waiting time does influence the duration of an eruption


# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data =  faithful)
summary(faithful_lm)

?lm


# Correlations ------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(corrplot)

#Load data
ecklonia <- read_csv("data/ecklonia.csv")


# Formulate a hypothesis --------------------------------------------------

#H0: There is no relationship between frond length and frond mass
#for the kelp Ecklonia maxima
#H1: There is relationship between frond length and frond mass
#for the kelp Ecklonia maxima


# Test a hypothesis -------------------------------------------------------

cor.test(ecklonia$frond_length, ecklonia$frond_mass)


# Visualise the data ------------------------------------------------------

ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass)) +
  geom_point()


# Run multiple tests at once ----------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(frond_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor


# Spearman rank test ------------------------------------------------------

#First create an ordnial column
ecklonia$length <-  as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))
ecklonia$length

#Then run a Spearman test

cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman") #ordinal


# Kendall rank test -------------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall") #not normal


# Visualize all the things ------------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

corrplot(ecklonia_pearson, method = "circle")


# Exercise 1 --------------------------------------------------------------

#Here is bunch of data for pigs raised on different diets. The experiment is similar to the chicken one. 
#Does feed type have an effect on the mass of pigs at the end of the experiment?

# H0: Feed type does not significantly affect pig mass
# H1: Feed type does significantly affect pig mass

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

#making the dataframe
chitlins <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

# Run ANOVA

chitlins.aov1 <- aov(mass ~ feed, data = chitlins)

# Data summary

summary(chitlins.aov1)

# In conclusion Pr < 0.0.5, thus we do not accept the null hypothesis. 
# Feed type does significantly effect pig mass.

# Exercise 2 --------------------------------------------------------------

#Construct suitable null and alternative hypotheses for the built-in ToothGrowth data, 
#and test your hypotheses using an ANOVA.

# H0: There is no significant correlation bewteen tooth length and supplement application
# H1: There is a difference correlation between tooth length and supplement application

#Load Data

toothgrowth <- ToothGrowth

# Run ANOVA
toothgrowth.aov <- aov(len ~ supp, data = toothgrowth)

# Data summary
summary(toothgrowth.aov)

# In conclusion Pr < 0.05, thus we do not accept the null hypothesis, 
# there is a significant correlation between tooth length and supplement application

