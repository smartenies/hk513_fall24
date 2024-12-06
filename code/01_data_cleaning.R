#' -----------------------------------------------------------------------------
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' Project: HK 513 Reproducibility Deliverable Example
#' 
#' Description: This script is used to read in the raw data set, clean it,
#' calculate the difference in self-reported confidence variables, and save the
#' clean data set for other data analysis steps.
#' ----------------------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)

#' Read in the raw data set
#' Note: The raw data are identifiable and not publicly available
data <- read_xlsx(here("raw_data", "raw_survey_data.xlsx"))
glimpse(data)

#' Add a random ID to deidentify these data
data2 <- mutate(data, id = sample(x = 1:200, size = nrow(data), replace = F))
glimpse(data2)

#' Select variables of interest:
#' - Level of R experience before the class
#' - Self-reported confidence before the class
#' - Self-reported confidence after the class
#' - Self-reported met goals

data3 <- data2 %>%
  select(id, exp_r_pre, starts_with("comp"), met_goals)
glimpse(data3)

#' Stratify by level of R experience
#' No experience (1) vs. any experience (2,3,4,5)
table(data3$exp_r_pre)
data3$any_r_exp <- ifelse(data3$exp_r_pre > 1, 1, 0)

table(data3$exp_r_pre, data3$any_r_exp)

#' Write out the clean data set
write_csv(data3, here("data", "clean_data.csv"))
