
library(tidyverse)
library(haven)
library(PerformanceAnalytics)
library(corrplot)
library(car)
library(effects)
library(dplyr)
library(ggplot2)


ceodata <- read_dta(file = "ceosal.dta")

## Data Analysis and Exploration


summary(ceodata)

  
  #### Following Plots and Analyses are About the "ceosal" data.
  

Salary = ggplot(aes(x=salary), data=ceodata)+
  geom_histogram(binwidth = 150, fill='darkred', color='black')+
  ggtitle("Salary Distribution")
Salary


outliers <- boxplot.stats(ceodata$salary)$out

boxplot(ceodata$salary,
        height = 800,
        width= 800,
        ylim = c(0,4500),
        ylab = "Salary",
        main = "Boxplot of Salary With Outliers")

out <- which(ceodata$salary %in% c(outliers))



## Project Questions



scatterplotMatrix ( ~ lsalary + lsales + roe + ros , data = ceodata )


Since salary and sales variables right-skewed (positive skew distribution) for this used their log-transformed type. If we take the log of a variable outliers will come closer to the mean, so that we can have a normal distribution on our data. For the regression analyses, we should use the log transformation of the data. It can be seen that there is a positive relationship between sales and salary. As sales increase the salary of the CEOs are increasing. On the other hand, there is a negative trend between sales and the return on the firm's stock (ros). It might be wrong but from my point of view, as a firm's stocks lose return in their stock, their sales are negatively impacted. 


salary.mod <- lm(lsalary ~  lsales + roe + pcroe + ros + indus + finance + consprod  , data = ceodata)
summary(salary.mod)


salary.mod2 <- lm(lsalary ~  lsales + roe + ros  , data = ceodata)
summary(salary.mod2)

plot(predictorEffects(salary.mod2)

 


