# Day_3.R
# 17 April 2018
# Distributions and Inferences about one or two populations

#Set-up

library(tidyverse)
library(fitdistrplus)
library(logspline)

# Generate log-normal data

r_norm <- rnorm(n = 1000, mean = 13, sd = 1) 

#Generate a histogram

hist(r_norm)

#Generate a Cullen and Frey graph

descdist(r_norm, discrete = FALSE, boot = 100)

#uniform data

y <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

#  Inferences about one or two populations --------------------------------

# Set-up ------------------------------------------------------------------

library(tidyverse)
library(plotly)

# Generate random data ----------------------------------------------------

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Check assumptions -------------------------------------------------------

#To determine normality we use the Shapiro-Wilk test

shapiro.test(r_dat$dat) #this tests all the data together
shapiro.test(r_dat$dat)[1] #Only output w value
shapiro.test(r_dat$dat)[2] #Only output p value

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) 
# data normal when p > 0.05, non- normal when p <- 0.05

# Check homoscedasticity --------------------------------------------------

#There are many ways to check homoscedasticity
#which is the similarity of variance between sample sets
#for now we will simply say that the assumptions are met when the variance of the samples are not more than 2 - 4 times 
#greater than one another

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))


# One sample t-test -------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")
#Histogram

ggplot(data = r_one, aes(x = dat, fill = sample)) +
  geom_histogram(colour = "black", position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")

#Run the test

t.test(r_dat$dat, mu = 20)

#A test that will produce a significant result

t.test(r_dat$dat, mu = 30)


# Pick a side -------------------------------------------------------------

#Are these data SMALLER/LESS than the population mean

t.test(r_one$dat, mu = 20, alternative = "less")

#or greater 

t.test(r_one$dat, mu = 20, alternative = "greater")

#for larger population mean are the samples < population of 30

t.test(r_one$dat, mu = 30, alternative = "less")

#or greater

t.test(r_one$dat, mu = 30, alternative = "greater")


# Two sample t-test -------------------------------------------------------

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))


#Histogram

ggplot(data = r_two, aes(x = dat, fill = sample)) +
  geom_histogram(colour = "black", position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")

#Run a default/ basic test

t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#Pick a side

#Is A less than B?

t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

#Is A greater than B?

t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")

# A t-test workflow -------------------------------------------------------

# Set-up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

# Load data ---------------------------------------------------------------

ecklonia <- read_csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)
ecklonia

# Box-plot ----------------------------------------------------------------

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

#  Formulate a hypothesis -----------------------------------------------

#The stipe masses at Batsata Rock is greater than at Boulders Beach
#this hypothesis is based on the following visualization

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure- another boxplot

ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
#Thus
#H0: Stipe mass at Batsata Rock is not greater than at Boulders Beach.
#H1: Stipe mass at Batsata Rock is greater than at Boulders Beach.

# Choosing a test ---------------------------------------------------------

#As we may see in the above figure, we have two sample sets that we are comparing
#thus  we will likely be using a t-test. 
#For our hypothesis we want to see if the stipe mass at Batsata Rock 
#is greater than the stipe mass at Boulders Beach, not just that they are different.
#thus we will need a one-sided t-test

#  Checking assumptions ---------------------------------------------------

#determine if the data are normally distributed, and
#that the data are homoscedastic, and that there are no outliers, this can be done automagically

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = var(value)[1],
            stipe_mass_norm = as.numeric(shapiro.test(value)[2]))

# Run the analysis -----------------------------------------------------

# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

# Conclusion --------------------------------------------------------------

#The stipe mass (kg) of the kelp Ecklonia maxima was found to be significantly 
#greater at Batsata Rock than at Boulders Beach (p = 0.03, t = 1.87, df = 24).


# Exercise 1 --------------------------------------------------------------

#Find or create your own normally distributed data and 
#think of a hypothesis you could use a t-test for. 
#Write out the hypothesis, test it, and write a one sentence conclusion for it. 
#Provide all of the code used to accomplish this.

# Set-up ------------------------------------------------------------------

library(tidyverse)
library(plotly)

# Generate random data ----------------------------------------------------

random_data <- data.frame(dat = c(rnorm(n = 69, mean = 7, sd = 4),
                            rnorm(n = 69, mean = 6, sd = 3)),
                    sample = c(rep("A", 69), rep("B", 69)))


# Formulate a hypothesis --------------------------------------------------

#Hypothesis was based on the following visualization

ggplot(data = random_data, aes(x = dat, fill = sample)) +
  geom_histogram(colour = "black", position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")

#Thus
#H0: Sample A is not greater than Sample B
#H1: Sample A is greater than Sample B

# Check assumptions -------------------------------------------------------

random_data %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

#Run a t-test

t.test(dat ~ sample, data = random_data, var.equal = TRUE)

#Two Sample t-test
#data:  dat by sample
#t = 1.2998, df = 136, p-value = 0.1959
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -0.4470251  2.1615985
#sample estimates:
 # mean in group A mean in group B 
#7.010969        6.153682 

#In conclusion sample A is not significantly greater than sample B (t = 1.2998, df = 136, p-value = 0.1959)
#The data used in this exercise is random thus results obtained when conducting the t-test will vary





