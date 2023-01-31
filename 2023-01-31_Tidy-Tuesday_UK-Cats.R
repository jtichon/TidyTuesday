# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2023-01-31')
tuesdata <- tidytuesdayR::tt_load(2023, week = 5)

cats_uk <- tuesdata$cats_uk
cats_uk_reference <- tuesdata$cats_uk_reference

# Load Packages
library(tidyverse)
library(patchwork)
library(stringr)
library(ggalt)


## Find max and min for each cat

# Sort by tag to match full data set
cats_uk_reference <- cats_uk_reference %>%
  arrange(tag_id) %>%
  mutate(tag_id = str_remove_all(tag_id, "-Tag"))

# Sort by tag, add gender, find max and min speeds
cats_speed <- cats_uk %>%
  group_by(tag_id, ) %>%
  arrange(tag_id) %>%
  mutate(tag_id = str_remove_all(tag_id, "-Tag")) %>%
  filter(ground_speed !=0) %>%
  summarise(min_speed = min(ground_speed), max_speed = max(ground_speed)) %>%
  mutate(animal_sex = cats_uk_reference$animal_sex)

# Speed data for females
cats_speed_m <- cats_speed %>%
  filter(animal_sex == "m")

# Speed data for males
cats_speed_f <- cats_speed %>%
  filter(animal_sex == "f")


# Create Plot
theme_set(theme_classic())

gf <- ggplot(cats_speed_f, aes(x=min_speed, xend=max_speed, y=tag_id, group=tag_id))

p1 <- gf + geom_dumbbell(color="#844b8d", 
              size=0.75) +
  labs(x = "Range Non-Resting Speed",
       y = NULL,
       title = "Female Cats") +
  xlim(0, max(cats_speed_m$max_speed)) +
  theme() +
  geom_label(
    label="Zzzzzz", 
    x=10000,
    y=17,
    label.padding = unit(0.1, "lines"), # Rectangle size around label
    label.size = 0.15,
    color = "black",
    fill = "#CBC3E3"
  )

gm <- ggplot(cats_speed_m, aes(x=min_speed, xend=max_speed, y=tag_id, group=tag_id))

p2 <- gm + geom_dumbbell(color="#a3c4dc", 
                         size=0.75) +
  labs(x = "Range Non-Resting Speed", 
       y = NULL,
       title = "Males Cats") +
  xlim(0, max(cats_speed_m$max_speed))

(p1 | p2) +
  plot_annotation(title = 'Range in Non-Resting Speeds for UK Cats by Sex',
                  theme = theme(plot.title = element_text(hjust = 0.5)))

