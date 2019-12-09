### Packages
library(tidyverse)
library(tidytuesdayR)
library(reshape2)
library(here)

### Import Data
tuesdata <- tidytuesdayR::tt_load("2019-11-26")
tuesdata <- tidytuesdayR::tt_load(2019, week = 48)

loans <- tuesdata$loans

### Find NA's
count.na <- function(i){sum(is.na(loans[,i]))}
2:10 %>%
map(count.na)

# Regression lines by year comparing voluntary payments to wage garnishments
loans2 <-loans %>%
  filter(is.na(voluntary_payments) == FALSE)

# Select variables agency_name, total, voluntary_payments, wage_garnishments
loans2 <- select(loans2, agency_name, total, voluntary_payments, wage_garnishments)

# Eliminate year by summing across all years within each agency. Re-express voluntary_payments
# and wage_garnishments as percentage of total repaid
loans.byagency <- loans2 %>%
  group_by(agency_name) %>%
  summarize(sum_voluntary_payments = sum(voluntary_payments),
            sum_wage_garnishments = sum(wage_garnishments),
            sum_total = sum(total)) %>%
  mutate(percent_wage_garnishments = sum_wage_garnishments / sum_total * 100, 
         percent_voluntary_payments = s-tary_payments / sum_total * 100)

# Melt data to make repayment method a factor 
loans.byagency.melt <- melt(loans.byagency[,c("agency_name", "percent_voluntary_payments", 
                                              "percent_wage_garnishments")], id.vars = 1)
names(loans.byagency.melt)[2] <- "payment_type"
names(loans.byagency.melt)[3] <- "percentage"

# Make ggplot object on melted data set
loans.byagency.gg <- ggplot (loans.byagency.melt)


# Plot for each company the percent repaid with each method
loans.byagency.gg +
  geom_point(aes(x = agency_name, y = percentage, colour = payment_type)) +
  scale_color_manual(values=c("blue", "red"), labels = c("Voluntary Repayment", "Wage Garnishment")) +
  geom_smooth(se = FALSE, method = loess, aes(x = agency_name, y = percentage, group = payment_type, colour = payment_type)) +
  labs( colour = "Repayment Source") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Company") +
  ylab("Percentage of Total Dollars Repaid") +
  ggtitle("Percentage of Total Student Loan Debt Repaid \n Voluntary vs Garnishment by Company")

ggsave(here("2019-11-26", "20191126_tidy_tuesday_plot-JGT.png"))


