## Load in dataset

library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

## Packages
library(tidyverse)

## Get Brand Names with at least 150 products
sephoraTemp <- tuesdata$sephora
ulta <- tuesdata$ulta

head(sephoraTemp)

brandsTemp<-as.data.frame(table(sephoraTemp$brand))
brandsTemp

brands <- brandsTemp %>%
  rename(Brand = Var1) %>%
  filter(Freq >= 150)

brands

## Limit Sephora to those with at least 150 products

sephora <- sephoraTemp %>%
  filter(is.element(brand, brands$Brand)) %>% 
  group_by(product) %>%
  filter(n()>=50)

