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
library(ggpubr)
library(rstatix)
library(viridis)

#' Read in the clean data set
data <- read_csv(here("data", "clean_data.csv"))
glimpse(data)
summary(data)

#' ----------------------------
#' Figure 1: Bar chart showing changes in median confidence
#' ----------------------------

#' Function for format the median and IQR
med_iqr <- function(x) {
  med <- median(x, na.rm = T)
  iqr <- IQR(x, na.rm = T)
  miss <- sum(is.na(x))
  med_iqr <- paste0(format(round(med, digits = 1), nsmall = 1), " (", format(round(iqr, digits = 1), nsmall = 1), ")")
  return(med_iqr)
}

#' Long data set with pre- and post scores for each question
data_long_post <- data %>%
  pivot_longer(cols = ends_with("_post"), 
               names_to = "comp", values_to = "conf_score") %>%
  select(id, comp, conf_score) %>%
  mutate(comp = str_remove(comp, "_post"),
         test = "post")
data_long_pre <- select(data, -exp_r_pre) %>%
  pivot_longer(cols = ends_with("_pre"), 
               names_to = "comp", values_to = "conf_score") %>%
  select(id, comp, conf_score) %>%
  mutate(comp = str_remove(comp, "_pre"),
         test = "pre")
data_long <- bind_rows(data_long_post, data_long_pre) %>%
  mutate(test = factor(test, levels = c("pre", "post"),
                       labels = c("Pre-course", "Post-course")),
         comp = factor(comp, labels = c("Task A*", "Task B*", "Task C",
                                        "Task D", "Task E", "Task F",
                                        "Task G", "Task H")))

#' calculate medians
data_long_med <- data_long %>%
  group_by(comp, test) %>%
  summarise(median = median(conf_score, na.rm =T),
            pct25 = quantile(conf_score, probs = 0.25, na.rm = T),
            pct75 = quantile(conf_score, probs = 0.75, na.rm = T))

med_plot <- ggplot() +
  geom_col(data = data_long_med, aes(x = comp, y = median, 
                                      fill = test),
           position = position_dodge(0.9),
           color = "black", width = 0.85, show.legend = T) +
  geom_errorbar(data = data_long_med, aes(x = comp, y = median,
                                           ymin = pct25,
                                           ymax = pct75,
                                           group = test),
                position = position_dodge(0.9), color = "black", width = 0.2) +
  scale_fill_viridis(name = "Questionnaire", discrete = T,  option = "plasma") +
  labs(
    x = NULL,
    y = "Median level of confidence",
    caption = "Error bars indicate the inter-quartile range.\n
    *Statistically significant change based on the paired Wilcoxson Ranked Sign test."
  ) +
  theme_minimal()
med_plot
ggsave(med_plot, filename = here::here("figs", "fig1.jpeg"),
       width = 8, height = 6, units = "in", dpi = 500)

#' ----------------------------
#' Figure 2: Bar chart for the number of students who report meeting their 
#' semester goals
#' ----------------------------

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
ggsave(goal_bar, filename = here("figs", "fig2.jpeg"),
       units = "in", height = 4, width = 6)
