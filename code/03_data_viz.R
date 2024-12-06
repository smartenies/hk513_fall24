#' -----------------------------------------------------------------------------
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' Project: HK 513 Reproducibility Deliverable Example
#' 
#' Description: This script is used to generate Figure 1 in the mansucript 
#' presenting the number of students who reported meeting their semester goals
#' for the course.
#' -----------------------------------------------------------------------------

library(tidyverse)
library(here)
library(ggplot2)

#' Read in the clean data set
data <- read_csv(here("data", "clean_data.csv"))
glimpse(data)
summary(data)

#' bar chart for the number of students who report meeting their 
#' semester goals

#' Set missing values to 0 to include in the bar chart
data$met_goals[is.na(data$met_goals)] <- 0
data$met_goals_fac <- factor(data$met_goals,
                             levels = c(1, 2, 3, 0),
                             labels = c("Yes", "Somewhat", "No", "Missing"),
                             ordered = T) 

#' Bar chart counting students in each response group
goal_bar <- ggplot(data) +
  geom_bar(aes(x = met_goals_fac),
           position = position_dodge2(preserve = "single")) +
  scale_x_discrete(drop = F, na.value = "Missing") +
  labs(x = "Do you feel like you've reached your goals for the course?",
       y = "Number of students") +
  theme_minimal()
goal_bar
ggsave(goal_bar, filename = here("figs", "fig1.jpeg"),
       units = "in", height = 4, width = 6)
