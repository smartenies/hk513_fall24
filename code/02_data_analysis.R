#' -----------------------------------------------------------------------------
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' Project: HK 513 Reproducibility Deliverable Example
#' 
#' Description: This script is used to analyze the change in confidence related
#' to data management skills reported by students enrolled in HK 513 (Fall 2024)
#' 
#' Guidance on the analysis of Likert scale questionnaire responses can be found
#' here: 
#' Sullivan GM, Artino AR. 2013. Analyzing and Interpreting Data From Likert-
#' Type Scales. J Grad Med Educ 5:541–542; doi:10.4300/JGME-5-4-18.
#' -----------------------------------------------------------------------------

library(tidyverse)
library(here)

#' Read in the clean data set
data <- read_csv(here("data", "clean_data.csv"))
glimpse(data)

#' Summarize the level of experience
#' Summarize the pre- and post scores
#' Summarize the "met goals" question

nrow(data) # 12 pre-course responses
sum(!is.na(data$met_goals)) # 9 post-course responses

#' ----------------------------
#' Level of experience with R 
#' 1 = no experience, 5 = A great deal of experience
#' ----------------------------
summary(data$exp_r_pre)
hist(data$exp_r_pre) #' responses are not normally distributed
table(data$exp_r_pre)
prop.table(table(data$exp_r_pre))

#' Not normally distributed, summarizing as median and IQR
sum(!is.na(data$exp_r_pre))
median(data$exp_r_pre)
IQR(data$exp_r_pre)

#' Summarize pre- and post- scores
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
  mutate(test = factor(test, levels = c("pre", "post")))
  
score_sum <- data_long %>%
group_by(comp, test) %>%
  summarise(
    count = sum(!is.na(conf_score)),
    score1_n = sum(conf_score == 1, na.rm = T),
    score1_pct = round((score1_n / count)*100, 1),
    score2_n = sum(conf_score == 2, na.rm = T),
    score2_pct = round((score2_n / count)*100, 1),
    score3_n = sum(conf_score == 3, na.rm = T),
    score3_pct = round((score3_n / count)*100, 1),
    score4_n = sum(conf_score == 4, na.rm = T),
    score4_pct = round((score4_n / count)*100, 1),
    median = median(conf_score, na.rm = TRUE),
    IQR = IQR(conf_score, na.rm = TRUE),
    n_missing = sum(is.na(conf_score)),
    score1_n_pct = paste0(score1_n, " (", score1_pct, ")"),
    score2_n_pct = paste0(score2_n, " (", score2_pct, ")"),
    score3_n_pct = paste0(score3_n, " (", score3_pct, ")"),
    score4_n_pct = paste0(score4_n, " (", score4_pct, ")"),
    med_iqr = paste0(median, " (", IQR, ")")
  )
score_sum

score_sum2 <- score_sum %>%
  select(comp, test, score1_n_pct:med_iqr)
write_csv(score_sum2, file = here("results", "comp_score_summary.csv"))

#' ----------------------------
#' Differences in confidence pre vs. post
#' Using paired Wilcoxson Signed Rank Test
#' ----------------------------

#' A) Use software to organize quantitative data
hist(data$comp_a_post)
hist(data$comp_a_pre)
wilcox.test(data$comp_a_post, data$comp_a_pre, paired = TRUE, 
            alternative = "two.sided")

#' B) Use software to clean quantitative data
hist(data$comp_b_post)
hist(data$comp_b_pre)
wilcox.test(data$comp_b_post, data$comp_b_pre, paired = TRUE, 
            alternative = "two.sided")

#' C) Use software to analyze quantitative data
hist(data$comp_c_post)
hist(data$comp_c_pre)
wilcox.test(data$comp_c_post, data$comp_c_pre, paired = TRUE, 
            alternative = "two.sided")

#' D) Use software to visualize quantitative data
hist(data$comp_d_post)
hist(data$comp_d_pre)
wilcox.test(data$comp_d_post, data$comp_d_pre, paired = TRUE, 
            alternative = "two.sided")

#' E) Write a methods section
hist(data$comp_e_post)
hist(data$comp_e_pre)
wilcox.test(data$comp_e_post, data$comp_e_pre, paired = TRUE, 
            alternative = "two.sided")

#' F) Write a results section
hist(data$comp_f_post)
hist(data$comp_f_pre)
wilcox.test(data$comp_f_post, data$comp_f_pre, paired = TRUE, 
            alternative = "two.sided")

#' G) Share work for reproducibility
hist(data$comp_g_post)
hist(data$comp_g_pre)
wilcox.test(data$comp_g_post, data$comp_g_pre, paired = TRUE, 
            alternative = "two.sided")

#' H) Find answers
hist(data$comp_h_post)
hist(data$comp_h_pre)
wilcox.test(data$comp_h_post, data$comp_h_pre, paired = TRUE, 
            alternative = "two.sided")

#' ----------------------------
#' Summarizing students who met their goals
#' ----------------------------

table(data$met_goals)
prop.table(table(data$met_goals))
