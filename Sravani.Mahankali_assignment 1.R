## Reading the file

Performance.Ratings <- read.csv("california-hospital-performance-ratings-for-coronary-artery-bypass-graft-cabg-surgery-2011-2018.csv")

## Installing the packages and loading them

install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

## Getting the column names

names(Performance.Ratings)

## Checking the summary to change the variables to categorical and numeric

summary(Performance.Ratings)

## Selecting the necessary variables

Performance.Ratings.Cleaned <- Performance.Ratings %>%
  select(County, Performance.Measure, X..of.Cases, X..of.Adverse.Events, Risk.adjusted.Rate)

## Changing the variables to categorical and numeric types

Performance.Ratings.Cleaned$County <- as.factor(Performance.Ratings.Cleaned$County)
Performance.Ratings.Cleaned$Performance.Measure <- as.factor(Performance.Ratings.Cleaned$Performance.Measure)
Performance.Ratings.Cleaned$X..of.Cases <- as.numeric(Performance.Ratings.Cleaned$X..of.Cases)
Performance.Ratings.Cleaned$X..of.Adverse.Events <- as.numeric(Performance.Ratings.Cleaned$X..of.Adverse.Events)
Performance.Ratings.Cleaned$Risk.adjusted.Rate <- as.numeric(Performance.Ratings.Cleaned$Risk.adjusted.Rate)

## checking the summary

summary(Performance.Ratings.Cleaned)

## dropping NA_values

Performance.Ratings.Cleaned <- Performance.Ratings.Cleaned%>%
  drop_na(X..of.Cases) %>%
  drop_na(X..of.Adverse.Events)%>%
  drop_na(Risk.adjusted.Rate)

## re-checking the summary

summary(Performance.Ratings.Cleaned)

## Writing the data frame to csv file

write.csv(Performance.Ratings.Cleaned, "CABG.csv")

## Building the hierarchical models

### model of risk adjusted rate and number of cases

lm.model.1 <- lm(Risk.adjusted.Rate ~ X..of.Cases, data = Performance.Ratings.Cleaned)
summary(lm.model.1)

### model of risk adjusted rate, county and number of cases

lm.model.2 <- lm(Risk.adjusted.Rate ~ X..of.Cases+ County, data = Performance.Ratings.Cleaned)
summary(lm.model.2)

### model of risk adjusted rate, county,performance measure and number of cases

lm.model.3 <- lm(Risk.adjusted.Rate ~ X..of.Cases+ County+ Performance.Measure, data = Performance.Ratings.Cleaned)
summary(lm.model.3)

### model of risk adjusted rate, county,performance measure, number of adverse events and number of cases

lm.model.4 <- lm(Risk.adjusted.Rate ~ X..of.Cases+ County+ Performance.Measure+ X..of.Adverse.Events, data = Performance.Ratings.Cleaned)
summary(lm.model.4)

### comparing all the models

anova(lm.model.1, lm.model.2, lm.model.3, lm.model.4)

### model of risk adjusted rate, number of adverse events and performance measure 

lm.model.final <- lm(Risk.adjusted.Rate ~ X..of.Adverse.Events+ Performance.Measure, data = Performance.Ratings.Cleaned)
summary(lm.model.final)

cor.test(Performance.Ratings.Cleaned$Risk.adjusted.Rate, Performance.Ratings.Cleaned$X..of.Adverse.Events)

### comparing all the models

anova(lm.model.3, lm.model.final)

