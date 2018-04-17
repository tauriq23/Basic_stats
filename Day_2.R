# Day_2.R
# 13 April 2018
# The day in which we discuss data visualizations and distributions


# Set-up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# Manual calculations -----------------------------------------------------

# generate random data

r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), 
               Sample = "A")
#list data

r_dat

#Visualize the data

ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

#The mean which is the sum of all the points divided by the number of all the points

r_dat %>%
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))

#The median via base R (Brute force)

order(r_dat$dat)[length(r_dat$dat)/2]

#The median via tidy

r_dat %>%
  arrange(dat) %>%
  slice(n()/2)

#The median via the tidy automagic way, this is the recommended method

r_dat %>%
  summarise(r_median = median(dat))

# Calculate variance by taking each value minus the mean squared divided by the the count of the sample minus one

r_dat %>%
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>%
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            r_var_func =var(dat))


#The standard deviation
r_dat %>%
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))


# Exercise 1 --------------------------------------------------------------

summary(ChickWeight$weight)

ChickWeight %>%
  summarise(min_weight = min(weight),
             quart_1 = quantile(weight, 0.25),
             med_weight = median(weight),
             mean_weight = mean(weight),
             quart_3 = quantile(weight, 0.75),
             max_weight = max(weight))


# Visualizations ----------------------------------------------------------

# Set-up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

#Load Data

sa_times <- read_csv("SA_Times.csv")

#Edit our data

sa_times <- sa_times %>% 
  mutate(human = seq(1, n(),1))
         #geo = c(rep(c("Cape Town", "George", "PE"), times = 6), 
                 #rep("Joburg", 2)))

sa_long <- sa_times %>% 
  #group_by(Human) %>% 
  gather(key = "time_type", value = "minutes", -human)

# Qualitative  ------------------------------------------------------------

#Create a count of qualitative values


sa_count <- sa_long %>% 
  count(time_type) %>% 
  mutate(prop = n/sum(n))

#Stacked bar graohs

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

#Stacked proportion bar graph

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

#A pie chart (considered unprofessional)

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "not cool",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()


# Continuous data ---------------------------------------------------------

#clean the data

sa_clean <- sa_long %>% 
  filter(minutes < 300)

#Histograms

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#Relative proportion histogram

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y = ..density.. , fill = time_type),
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#Boxplots

ggplot(data = sa_clean, aes(x=  time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

#Notched boxplots

ggplot(data = sa_clean, aes(x=  time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

#Calculate summary stats for plotting over the boxplots

sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

#plot these means over the boxclot

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 3, shape = 11,
             aes(y = time_type_mean), colour = "goldenrod")


# Relationships -----------------------------------------------------------

#Basic scatterplot

ggplot(data = sa_times, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

#Adding a trendline 

# Consider adding a geo column so you can visualise this as well

#ggplot(data = sa_times, aes(y = now_now, x = just_now)) +
 # geom_point(aes(colour = geo)) +
  #geom_smooth(aes(colour = geo), method = "lm") +
  #coord_equal(xlim = c(0, 60), ylim = c(0, 60))
