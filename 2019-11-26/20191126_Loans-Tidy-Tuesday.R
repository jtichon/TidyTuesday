## Packages
library(tidyverse)
library(tidytuesdayR)

## Import Data
tuesdata <- tidytuesdayR::tt_load("2019-11-26")
tuesdata <- tidytuesdayR::tt_load(2019, week = 48)

loans <- tuesdata$loans

## Find NA's
count.na <- function(i){sum(is.na(loans[,i]))}
2:10 %>%
map(count.na)

## Initial Exploration

loans2 <-loans %>%
  filter(is.na(voluntary_payments) == FALSE) %>%
  filter(voluntary_payments >= 300000, wage_garnishments >= 300000) %>%
  filter(year != 15)
 

loans.gg <- ggplot(loans2)

loans.lm <- lm(sqrt(wage_garnishments) ~ sqrt(voluntary_payments), data = loans2)
plot(fitted.values(loans.lm), resid(loans.lm))
qqnorm(rstandard(loans.lm))
qqline(rstandard(loans.lm))

loans.gg + geom_point(aes(x=sqrt(voluntary_payments), y=sqrt(wage_garnishments), col = as.factor(year))) + 
  labs(color = "Year") +
  geom_abline( intercept = loans.lm$coefficients[[1]], slope = loans.lm$coefficients[[2]])

loans.gg +
  geom_boxplot(aes(x = as.factor(year), y = wage_garnishments, col = as.factor(year))) +
    xlab("Year") + 
    labs(color = "Year")

loans.gg +
  geom_boxplot(aes(x = as.factor(year), y = voluntary_payments, col = as.factor(year))) +
  xlab("Year") + labs(color = "Year")

table(loans2$year)
