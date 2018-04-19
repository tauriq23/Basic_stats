# Day_4.R
# Tauriq Jamalie
# 19 April 2018
# ANOVA

# Set-up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

# t-test ------------------------------------------------------------------

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# t-test

t.test(weight ~ Diet, data = chicks_sub)
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")

#In conclusion there is no significant difference in weight and diet (t = -1.2857, df = 15.325, p-value = 0.2176)

# 1-Way ANOVA  ------------------------------------------------------------

#Research question: Is there a difference in chicken mass attained after 21 dyas 
#after chickens having been fed four different diets?

#H0: There is no difference in chicken mass at 21 days after having been fed one of four diets
#H1: There is a difference in chicken mass at 21 days after having been fed one of four diets

chicks_21 <- chicks %>% 
  filter(Time == 21)
#The same as running 'chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21)'

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

#In conclusion we do not accept the null hypothesis, 

#This can be determined visually as well...

ggplot(data = chicks_21, aes(x = Time, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE) #When bands overlap theres no significant difference, when they dont overlap 
#there is a significant difference

# Tukey HSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)

#ANOVA

summary(aov(weight ~ Diet, data = chicks_21))

#Tukey

TukeyHSD(aov(weight ~ Diet, data = chicks_21))

#Box-plot

ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

#Segments showing confidence interbals
#Dataframe of segments

chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))
chicks_tukey

?TukeyHSD

plot(TukeyHSD(chicks.aov1, "Diet"))         

#OR

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))
                                
# Multiple factor ANOVA ---------------------------------------------------

#H0: There is no change in chicken mass (kg) from day 0 to day 21

#Create a dataframe with just those days

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))

#Visualize the data

ggplot(data = chicks_0_21, aes (x = as.factor(Time), y = weight)) +
  geom_boxplot(notch = T, aes (fill = as.factor(Time)))

#Run an ANOVA

summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

#Perform a Tukey post-hoc test

TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

#Look at the confidence intervals

plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

#Look only at day 0 and 21 for both time and diet

summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

#Or simply look at all of the Time
#...which is not the hypothesis 

summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
#Note the increase in the degrees of freedom for the time factor
#But no increase for the d.f. for Diet

#Now to look at interactios between factors
summary(aov(weight ~ Diet * as.factor(Time), data =filter(ChickWeight, Time %in% c(0,21))))

#Lets look at the Tukey results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data =filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0,21)))))

#Create a line graph to help explain this concept
#Fist create values by time and diet

chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm =T))

#Visualization

ggplot(data = chicks_mean, aes(x= Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)

# non-parametic tests -----------------------------------------------------

#This is avoided are results arent considered to be robust
#What happens in instances we dont have normal data?

# For a t-test we use a Wilcox rank sum test
wilcox.test() #filled in the same way a t.test() is filled in 

#Kruskall-Wallis 

kruskal.test(weight ~ Diet, data = chicks_0_21)

#Load thsi for a non-parametric post-hoc test

library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)
