library(tidytuesdayR)
library(tidyverse)
library(here)

tuesdata <- tidytuesdayR::tt_load("2019-12-10")
tuesdata <- tidytuesdayR::tt_load(2019, week = 50)

diseases <- tuesdata$diseases

table(diseases$disease)
table(diseases$state)
table(diseases$year)

# > table(diseases$disease)
# 
# Hepatitis A     Measles       Mumps   Pertussis       Polio     Rubella    Smallpox 
# 2346        3876        1836        3774        3774        1938        1326 

#Filter to only measles, keep state, year, and percentage of population with disease
measles <-
  diseases %>%
  filter(disease == "Measles") %>%
  mutate(state, year, percentage = count/population)

#save to .rda
save(measles, file = "measles.rda")

### For Shiny App by State

#Filter out NA's
bystate<- diseases %>%
  filter(is.na(count) == FALSE) %>%
  select(state, year, count, disease)

save(bystate, file = "bystate.rda")
