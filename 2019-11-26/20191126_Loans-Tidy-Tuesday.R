### Packages
library(tidyverse)
library(tidytuesdayR)

### Import Data
tuesdata <- tidytuesdayR::tt_load("2019-11-26")
tuesdata <- tidytuesdayR::tt_load(2019, week = 48)

loans <- tuesdata$loans

### Find NA's
count.na <- function(i){sum(is.na(loans[,i]))}
2:10 %>%
map(count.na)

## Initial Exploration

## Regression lines by year comparing voluntary payments to wage garnishments
loans2 <-loans %>%
  filter(is.na(voluntary_payments) == FALSE)

loans.gg <- ggplot(loans2)

loans15 <- loans2 %>%
  filter(year == 15)

loans16 <-loans2 %>%
  filter(year == 16)

loans17 <- loans2 %>%
  filter(year == 17)

loans18 <- loans2 %>%
  filter(year == 18)

loans15.lm <- lm(wage_garnishments ~ voluntary_payments, data = loans15)
plot(fitted.values(loans15.lm), resid(loans15.lm))
qqnorm(rstandard(loans15.lm))
qqline(rstandard(loans15.lm))

loans16.lm <- lm(wage_garnishments ~ voluntary_payments, data = loans16)
plot(fitted.values(loans16.lm), resid(loans16.lm))
qqnorm(rstandard(loans16.lm))
qqline(rstandard(loans16.lm))

loans17.lm <- lm(wage_garnishments ~ voluntary_payments, data = loans17)
plot(fitted.values(loans17.lm), resid(loans17.lm))
qqnorm(rstandard(loans17.lm))
qqline(rstandard(loans17.lm))

loans18.lm <- lm(wage_garnishments ~ voluntary_payments, data = loans18)
plot(fitted.values(loans18.lm), resid(loans18.lm))
qqnorm(rstandard(loans18.lm))
qqline(rstandard(loans18.lm))

loans.gg + geom_point(aes(x=voluntary_payments, y=wage_garnishments, col = as.factor(year))) +
  ggtitle("Rate of Wage Garnishment as Voluntary Payments Increase") +
  labs(color = "Year") +
  ylab("Wage Garnishments") +
  xlab("Voluntary Payments") +
  geom_abline( intercept = loans15.lm$coefficients[[1]], slope = loans15.lm$coefficients[[2]], 
               color = "#F8766D") +
  geom_abline( intercept = loans16.lm$coefficients[[1]], slope = loans16.lm$coefficients[[2]], 
               color = "#7CAE00") +
  geom_abline( intercept = loans17.lm$coefficients[[1]], slope = loans17.lm$coefficients[[2]], 
               color = "#00BFC4") +
  geom_abline( intercept = loans18.lm$coefficients[[1]], slope = loans18.lm$coefficients[[2]], 
               color = "#C77CFF")

loans.gg +
  geom_boxplot(aes(x = as.factor(year), y = wage_garnishments, col = as.factor(year))) +
    xlab("Year") + 
    labs(color = "Year")

loans.gg +
  geom_boxplot(aes(x = as.factor(year), y = voluntary_payments, col = as.factor(year))) +
  xlab("Year") + labs(color = "Year")

table(loans2$year)

### Consolidated by agency
loans2 <- select(loans2, agency_name, total, voluntary_payments, wage_garnishments)

loans.byagency <- loans2 %>%
  group_by(agency_name) %>%
  summarize(sum_voluntary_payments = sum(voluntary_payments),
            sum_wage_garnishments = sum(wage_garnishments),
            sum_total = sum(total)) %>%
  mutate(percent_wage_garnishments = sum_wage_garnishments / sum_total * 100, 
         percent_voluntary_payments = s-tary_payments / sum_total * 100)


library(reshape2)
loans.byagency.melt <- melt(loans.byagency[,c("agency_name", "percent_voluntary_payments", "percent_wage_garnishments")], id.vars = 1)
names(loans.byagency.melt)[2] <- "payment_type"
names(loans.byagency.melt)[3] <- "percentage"

loans.byagency.gg <- ggplot (loans.byagency.melt)

loans.byagency.gg +
  geom_point(aes(x = agency_name, y = percentage, colour = payment_type)) +
               scale_color_manual(values=c("blue", "red"), labels = c("Voluntary Repayment", "Wage Garnishment")) +
  geom_smooth(se = FALSE, method = loess, aes(x = agency_name, y = percentage, group = payment_type, colour = payment_type)) +
  labs( colour = "Repayment Source") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Company") +
  ylab("Percentage of Total Dollars Repaid") +
  ggtitle("Percentage of Total Student Loan Debt Repaid \n Voluntary vs Garnishment by Company")


