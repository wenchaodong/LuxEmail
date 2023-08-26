library(dplyr)
library(readr)
library(sjmisc)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggthemes)
library(psych)
library(caret)
library(car)
library(corrplot)
library(plotrix) 
library(QuantPsyc)
library(emmeans)
library(margins)
library(cowplot)
library(mediation) 
mediate <- mediation::mediate
library(e1071)
library(stargazer)


# ================================================================================== Table 1: description for numerical variables  ====================================================================================

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "salary_bonus_13","potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", "num_weekday_total", "num_weekend_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# potential pay < 0 === filter out negative pay changes
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)

#median value for total wage in 2013 by gender
non_neg_potential_pay_diff %>%
  group_by(gender_13) %>%
  summarise(median_value = median(salary_bonus_13))


# group size by gender and rank
non_neg_potential_pay_diff %>%
  group_by(gender_13, manager_13) %>%
  summarise(number = n())


# by gender and rank
gender_rank_summary <- non_neg_potential_pay_diff %>%
  group_by(gender_13, manager_13) %>%
  summarise_at(.vars = vars(salary_bonus_13, potential_pay_diff, age_13, per_warmth_total, per_competence_total, num_email_total, num_targets_total),
               .funs = c(mean="mean", sd = "sd", min="min", max="max")); gender_rank_summary

#by gender only
gender_summary <- non_neg_potential_pay_diff %>%
  group_by(gender_13) %>%
  summarise_at(.vars = vars(salary_bonus_13, potential_pay_diff, age_13, per_warmth_total, per_competence_total, num_email_total, num_targets_total),
               .funs = c(mean="mean", sd = "sd", min="min", max="max")); gender_summary

#overall summary
overall_summary <- non_neg_potential_pay_diff %>%
  summarise_at(.vars = vars(salary_bonus_13, potential_pay_diff, age_13, per_warmth_total, per_competence_total, num_email_total, num_targets_total),
               .funs = c(mean="mean", sd = "sd", min="min", max="max")); overall_summary


# ================================================================================== Table 2: Total pay ~ demographic variables (baseline for analysis) ====================================================================================
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) 

cols_13 <- c("gender_13", "age_13", "manager_13", "company_13", "salary_bonus_13", "func_13")
individual_df_13 <- na.omit(individual_communication[,cols_13])

cols_14 <- c("gender_14", "age_14", "manager_14", "company_14", "salary_bonus_14", "func_14")
individual_df_14 <- na.omit(individual_communication[,cols_14])


# --- preprocess ---
# log transformation
individual_df_13["log_salary_bonus_13"] <- log1p(individual_df_13["salary_bonus_13"])
individual_df_14["log_salary_bonus_14"] <- log1p(individual_df_14["salary_bonus_14"])


# center
individual_df_13$age_13 <- c(scale(individual_df_13$age_13))
individual_df_13$log_salary_bonus_13 <- c(scale(individual_df_13$log_salary_bonus_13))


individual_df_14$age_14 <- c(scale(individual_df_14$age_14))
individual_df_14$log_salary_bonus_14 <- c(scale(individual_df_14$log_salary_bonus_14))


# factor
categorical_col_13 <-c('gender_13', 'manager_13', 'company_13', "func_13")
individual_df_13[categorical_col_13] <- lapply(individual_df_13[categorical_col_13], as.factor)
individual_df_13$gender_13 <- relevel(individual_df_13$gender_13, ref = "1")
individual_df_13$manager_13 <- relevel(individual_df_13$manager_13, ref = '0')
individual_df_13$company_13 <- relevel(individual_df_13$company_13, ref = "Standard")
individual_df_13$func_13 <- unclass(individual_df_13$func_13)



categorical_col_14 <-c('gender_14', 'manager_14', 'company_14', 'func_14')
individual_df_14[categorical_col_14] <- lapply(individual_df_14[categorical_col_14], as.factor)
individual_df_14$gender_14 <- relevel(individual_df_14$gender_14, ref = "1")
individual_df_14$manager_14 <- relevel(individual_df_14$manager_14, ref = '0')
individual_df_14$company_14 <- relevel(individual_df_14$company_14, ref = "Standard")
individual_df_14$func_14 <- unclass(individual_df_14$func_14)


# potential_pay ~ demo attributes
table_1_lm1 <- lm(log_salary_bonus_13 ~ gender_13 + age_13 + manager_13 + company_13 + func_13, data = individual_df_13); summary(table_1_lm1)


table_1_lm2 <- lm(log_salary_bonus_14 ~ gender_14 + age_14 + manager_14 + company_14 + func_14, data = individual_df_14); summary(table_1_lm2)


stargazer(table_1_lm1, table_1_lm2, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')






# ================================================================================== Table 3: t-test ====================================================================================
# ----- t-test by gender -----

rm(list = ls())
individual_AgencyCommunion_hr_2013 <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))

individual_AgencyCommunion_hr_2013 <- individual_AgencyCommunion_hr_2013 %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)


cols_gender <- c("gender_13", "potential_pay_diff", "per_warmth_total", "per_competence_total", "num_email_total", "num_targets_total")
individual_gender_df <- na.omit(individual_AgencyCommunion_hr_2013[,cols_gender]) 


non_neg_non_neg_gender_df <- individual_gender_df %>% filter(potential_pay_diff > -1) 



categorical_gender <-c('gender_13')
non_neg_non_neg_gender_df[categorical_gender] <- lapply(non_neg_non_neg_gender_df[categorical_gender], as.factor)
non_neg_non_neg_gender_df$gender_13 <- relevel(non_neg_non_neg_gender_df$gender_13, ref = "1")


# stats 
table(non_neg_non_neg_gender_df$gender_13)
#    1        0 
#   297      189 


# potential pay - sig
# Welch Two Sample t-test
var.test(potential_pay_diff~gender_13, data = non_neg_non_neg_gender_df)$p.value
potential_pay_diff_t_test <- t.test(potential_pay_diff~gender_13, data = non_neg_non_neg_gender_df, alternative = "two.sided", var.equal = FALSE); potential_pay_diff_t_test
non_neg_non_neg_gender_df %>%
  group_by(gender_13) %>%
  summarise(se_potential_pay_diff = sd(potential_pay_diff))



# warmth 
# Two Sample t-test
var.test(per_warmth_total~gender_13, data = non_neg_non_neg_gender_df)$p.value
warmth_percent_t_test <- t.test(per_warmth_total~gender_13, data = non_neg_non_neg_gender_df, alternative = "two.sided", var.equal = TRUE); warmth_percent_t_test
non_neg_non_neg_gender_df %>%
  group_by(gender_13) %>%
  summarise(se_warmth_percent = sd(per_warmth_total))



# competence - sig
# Two Sample t-test
var.test(per_competence_total~gender_13, data = non_neg_non_neg_gender_df)$p.value
competence_percent_t_test <- t.test(per_competence_total~gender_13, data = non_neg_non_neg_gender_df, alternative = "two.sided", var.equal = TRUE); competence_percent_t_test
non_neg_non_neg_gender_df %>%
  group_by(gender_13) %>%
  summarise(se_competence_percent = sd(per_competence_total))



# num_email_total - sig
# Welch Two Sample t-test
var.test(num_email_total~gender_13, data = non_neg_non_neg_gender_df)$p.value
num_email_total_t_test <- t.test(num_email_total~gender_13, data = non_neg_non_neg_gender_df, alternative = "two.sided", var.equal = FALSE); num_email_total_t_test
non_neg_non_neg_gender_df %>%
  group_by(gender_13) %>%
  summarise(se_num_email = sd(num_email_total))



# num_targets_total - sig
# Welch Two Sample t-test
var.test(num_targets_total~gender_13, data = non_neg_non_neg_gender_df)$p.value
num_targets_total_t_test <- t.test(num_targets_total~gender_13, data = non_neg_non_neg_gender_df, alternative = "two.sided", var.equal = FALSE); num_targets_total_t_test
non_neg_non_neg_gender_df %>% 
  group_by(gender_13) %>%
  summarise(se_num_targets = sd(num_targets_total))





# ----- t-test by rank -----


rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)


cols_manager <- c("manager_13", "potential_pay_diff", "per_warmth_total", "per_competence_total", "num_email_total", "num_targets_total")
individual_manager_df <- na.omit(individual_communication[,cols_manager]) #533


non_neg_manager_df <- individual_manager_df %>% filter(potential_pay_diff > -1) # 524



categorical_manager <-c('manager_13')
non_neg_manager_df[categorical_manager] <- lapply(non_neg_manager_df[categorical_manager], as.factor)
non_neg_manager_df$manager_13 <- relevel(non_neg_manager_df$manager_13, ref = "0")

# stats 
table(non_neg_manager_df$manager_13)
#  0       1 
# 435     89 


# potential pay - sig
# Welch Two Sample t-test
var.test(potential_pay_diff~manager_13, data = non_neg_manager_df)$p.value
potential_pay_diff_t_test <- t.test(potential_pay_diff~manager_13, data = non_neg_manager_df, alternative = "two.sided", var.equal = FALSE); potential_pay_diff_t_test
non_neg_manager_df %>%
  group_by(manager_13) %>%
  summarise(se_potential_pay_diff = sd(potential_pay_diff))



# warmth - sig
# Welch Two Sample t-test
var.test(per_warmth_total~manager_13, data = non_neg_manager_df)$p.value
warmth_percent_t_test <- t.test(per_warmth_total~manager_13, data = non_neg_manager_df, alternative = "two.sided", var.equal = FALSE); warmth_percent_t_test
non_neg_manager_df %>%
  group_by(manager_13) %>%
  summarise(se_warmth_percent = sd(per_warmth_total))



# competence - not sig
# Two Sample t-test
var.test(per_competence_total~manager_13, data = non_neg_manager_df)$p.value
competence_percent_t_test <- t.test(per_competence_total~manager_13, data = non_neg_manager_df, alternative = "two.sided", var.equal = TRUE); competence_percent_t_test
non_neg_manager_df %>%
  group_by(manager_13) %>%
  summarise(se_competence_percent = sd(per_competence_total))




# num_email_total - sig
# Two Sample t-test
var.test(num_email_total~manager_13, data = non_neg_manager_df)$p.value
num_email_total_t_test <- t.test(num_email_total~manager_13, data = non_neg_manager_df, alternative = "two.sided", var.equal = TRUE); num_email_total_t_test
non_neg_manager_df %>%
  group_by(manager_13) %>%
  summarise(se_num_email = sd(num_email_total))



# num_targets_total - sig
# Two Sample t-test
var.test(num_targets_total~manager_13, data = non_neg_manager_df)$p.value
num_targets_total_t_test <- t.test(num_targets_total~manager_13, data = non_neg_manager_df, alternative = "two.sided", var.equal = TRUE); num_targets_total_t_test
non_neg_manager_df %>%
  group_by(manager_13) %>%
  summarise(se_num_targets = sd(num_targets_total))



# ================================================================================== Table 4: Communion/Agency (Overall + Same-legacy + Across-legacy) ~ demographic + communication ====================================================================================

# ----- regression 1 - overall: agency ~ gender -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", 'func_13')
individual_total <- na.omit(individual_communication[,total_cols])


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)



# preprocess 

# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_total"] <- log1p(non_neg_potential_pay_diff["per_warmth_total"])
non_neg_potential_pay_diff["log_per_competence_total"] <- log1p(non_neg_potential_pay_diff["per_competence_total"])
non_neg_potential_pay_diff["log_num_email_total"] <- log1p(non_neg_potential_pay_diff["num_email_total"])
non_neg_potential_pay_diff["log_num_targets_total"] <- log1p(non_neg_potential_pay_diff["num_targets_total"])

# center
non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_total <- c(scale(non_neg_potential_pay_diff$log_per_warmth_total))
non_neg_potential_pay_diff$log_per_competence_total <- c(scale(non_neg_potential_pay_diff$log_per_competence_total))
non_neg_potential_pay_diff$log_num_email_total <- c(scale(non_neg_potential_pay_diff$log_num_email_total))
non_neg_potential_pay_diff$log_num_targets_total <- c(scale(non_neg_potential_pay_diff$log_num_targets_total))

non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))



# factor
categorical_col <-c('gender_13', 'manager_13', 'company_13', 'func_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)


table_4_lm_1 <- lm(log_per_competence_total ~ gender_13 + age_13 + manager_13 + company_13 +func_13 +
                     log_num_email_total + log_num_targets_total, data = non_neg_potential_pay_diff); summary(table_4_lm_1) 

stargazer(table_4_lm_1, star.cutoffs = c(0.05, 0.01, 0.001), type='text')


# ----- regression 2 - overall: communion ~ gender -----
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

#test_neg <- individual_total %>% filter(potential_pay_diff < 0)


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", 'func_13')
individual_total <- na.omit(individual_communication[,total_cols]) #494


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)#486



# preprocess 

# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_total"] <- log1p(non_neg_potential_pay_diff["per_warmth_total"])
non_neg_potential_pay_diff["log_per_competence_total"] <- log1p(non_neg_potential_pay_diff["per_competence_total"])
non_neg_potential_pay_diff["log_num_email_total"] <- log1p(non_neg_potential_pay_diff["num_email_total"])
non_neg_potential_pay_diff["log_num_targets_total"] <- log1p(non_neg_potential_pay_diff["num_targets_total"])

# center
non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_total <- c(scale(non_neg_potential_pay_diff$log_per_warmth_total))
non_neg_potential_pay_diff$log_per_competence_total <- c(scale(non_neg_potential_pay_diff$log_per_competence_total))
non_neg_potential_pay_diff$log_num_email_total <- c(scale(non_neg_potential_pay_diff$log_num_email_total))
non_neg_potential_pay_diff$log_num_targets_total <- c(scale(non_neg_potential_pay_diff$log_num_targets_total))

non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))



# factor
categorical_col <-c('gender_13', 'manager_13', 'company_13', 'func_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)


table_4_lm_2 <- lm(log_per_warmth_total ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                     log_num_email_total + log_num_targets_total, data = non_neg_potential_pay_diff); summary(table_4_lm_2)

stargazer(table_4_lm_2, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')





# ----- regression 3 - within: agency ~ gender -----
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within", 'func_13')
individual_within <- na.omit(individual_communication[,within_cols]) #493


# potential pay change < 0
non_neg_potential_pay_diff <- individual_within %>% filter(potential_pay_diff >-1) # 485


# preprocess
# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_within"] <- log1p(non_neg_potential_pay_diff["per_warmth_within"])
non_neg_potential_pay_diff["log_per_competence_within"] <- log1p(non_neg_potential_pay_diff["per_competence_within"])
non_neg_potential_pay_diff["log_num_email_within"] <- log1p(non_neg_potential_pay_diff["num_email_within"])
non_neg_potential_pay_diff["log_num_targets_within"] <- log1p(non_neg_potential_pay_diff["num_targets_within"])

# center


non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_within <- c(scale(non_neg_potential_pay_diff$log_per_warmth_within))
non_neg_potential_pay_diff$log_per_competence_within <- c(scale(non_neg_potential_pay_diff$log_per_competence_within))
non_neg_potential_pay_diff$log_num_email_within <- c(scale(non_neg_potential_pay_diff$log_num_email_within))
non_neg_potential_pay_diff$log_num_targets_within <- c(scale(non_neg_potential_pay_diff$log_num_targets_within))

non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))



# factor

categorical_col <-c('gender_13', 'manager_13', 'company_13', 'func_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)






table_4_lm_3 <- lm(log_per_competence_within ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                     log_num_email_within + log_num_targets_within, data = non_neg_potential_pay_diff); summary(table_4_lm_3)

stargazer(table_4_lm_3, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')









# ----- regression 4 - within: communion ~ gender -----
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within", 'func_13')
individual_within <- na.omit(individual_communication[,within_cols]) #493


# potential pay change < 0
non_neg_potential_pay_diff <- individual_within %>% filter(potential_pay_diff >-1) # 485


# preprocess
# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_within"] <- log1p(non_neg_potential_pay_diff["per_warmth_within"])
non_neg_potential_pay_diff["log_per_competence_within"] <- log1p(non_neg_potential_pay_diff["per_competence_within"])
non_neg_potential_pay_diff["log_num_email_within"] <- log1p(non_neg_potential_pay_diff["num_email_within"])
non_neg_potential_pay_diff["log_num_targets_within"] <- log1p(non_neg_potential_pay_diff["num_targets_within"])

# center


non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_within <- c(scale(non_neg_potential_pay_diff$log_per_warmth_within))
non_neg_potential_pay_diff$log_per_competence_within <- c(scale(non_neg_potential_pay_diff$log_per_competence_within))
non_neg_potential_pay_diff$log_num_email_within <- c(scale(non_neg_potential_pay_diff$log_num_email_within))
non_neg_potential_pay_diff$log_num_targets_within <- c(scale(non_neg_potential_pay_diff$log_num_targets_within))

non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))



# factor

categorical_col <-c('gender_13', 'manager_13', 'company_13', 'func_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)






table_4_lm_4 <- lm(log_per_warmth_within ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                     log_num_email_within + log_num_targets_within, data = non_neg_potential_pay_diff); summary(table_4_lm_4)

stargazer(table_4_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')





# ---------- regression 5 - across: agency ~ gender -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



across_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_across", "num_targets_across", "num_words_across", "num_warmth_words_across", "num_competence_words_across",
                 "per_warmth_across", "per_competence_across", 'func_13')
individual_across <- na.omit(individual_communication[,across_cols]) 

# potential pay < 0
non_neg_potential_pay_diff <- individual_across %>% filter(potential_pay_diff >-1) 



# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_across"] <- log1p(non_neg_potential_pay_diff["per_warmth_across"])
non_neg_potential_pay_diff["log_per_competence_across"] <- log1p(non_neg_potential_pay_diff["per_competence_across"])
non_neg_potential_pay_diff["log_num_email_across"] <- log1p(non_neg_potential_pay_diff["num_email_across"])
non_neg_potential_pay_diff["log_num_targets_across"] <- log1p(non_neg_potential_pay_diff["num_targets_across"])

# center
non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_across <- c(scale(non_neg_potential_pay_diff$log_per_warmth_across))
non_neg_potential_pay_diff$log_per_competence_across <- c(scale(non_neg_potential_pay_diff$log_per_competence_across))
non_neg_potential_pay_diff$log_num_email_across <- c(scale(non_neg_potential_pay_diff$log_num_email_across))
non_neg_potential_pay_diff$log_num_targets_across <- c(scale(non_neg_potential_pay_diff$log_num_targets_across))
non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))




# factor
categorical_col <-c('gender_13', 'manager_13', 'company_13', 'func_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)


table_4_lm_5 <- lm(log_per_competence_across ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                     log_num_email_across + log_num_targets_across, data = non_neg_potential_pay_diff); summary(table_4_lm_5)

stargazer(table_4_lm_5, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')








# ---------- regression 6 - across: acommunion ~ gender -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



across_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_across", "num_targets_across", "num_words_across", "num_warmth_words_across", "num_competence_words_across",
                 "per_warmth_across", "per_competence_across", 'func_13')
individual_across <- na.omit(individual_communication[,across_cols]) 

# potential pay < 0
non_neg_potential_pay_diff <- individual_across %>% filter(potential_pay_diff >-1) 



# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_across"] <- log1p(non_neg_potential_pay_diff["per_warmth_across"])
non_neg_potential_pay_diff["log_per_competence_across"] <- log1p(non_neg_potential_pay_diff["per_competence_across"])
non_neg_potential_pay_diff["log_num_email_across"] <- log1p(non_neg_potential_pay_diff["num_email_across"])
non_neg_potential_pay_diff["log_num_targets_across"] <- log1p(non_neg_potential_pay_diff["num_targets_across"])

# center
non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_across <- c(scale(non_neg_potential_pay_diff$log_per_warmth_across))
non_neg_potential_pay_diff$log_per_competence_across <- c(scale(non_neg_potential_pay_diff$log_per_competence_across))
non_neg_potential_pay_diff$log_num_email_across <- c(scale(non_neg_potential_pay_diff$log_num_email_across))
non_neg_potential_pay_diff$log_num_targets_across <- c(scale(non_neg_potential_pay_diff$log_num_targets_across))
non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))




# factor
categorical_col <-c('gender_13', 'manager_13', 'company_13', 'func_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)


table_4_lm_6 <- lm(log_per_warmth_across ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                     log_num_email_across + log_num_targets_across, data = non_neg_potential_pay_diff); summary(table_4_lm_6)

stargazer(table_4_lm_6, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')



stargazer(table_4_lm_1, table_4_lm_2, table_4_lm_3, table_4_lm_4, table_4_lm_5, table_4_lm_6,align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')






# ================================================================================== Table 5: Pay Change ~ demographic + communication ====================================================================================

# ----- overall (Age <= 53):  agency moderation -----
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", "func_13")
individual_total <- na.omit(individual_communication[,total_cols]) 


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)


# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_total"] <- log1p(non_neg_potential_pay_diff["per_warmth_total"])
non_neg_potential_pay_diff["log_per_competence_total"] <- log1p(non_neg_potential_pay_diff["per_competence_total"])
non_neg_potential_pay_diff["log_num_email_total"] <- log1p(non_neg_potential_pay_diff["num_email_total"])
non_neg_potential_pay_diff["log_num_targets_total"] <- log1p(non_neg_potential_pay_diff["num_targets_total"])

# center


non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_total <- c(scale(non_neg_potential_pay_diff$log_per_warmth_total))
non_neg_potential_pay_diff$log_per_competence_total <- c(scale(non_neg_potential_pay_diff$log_per_competence_total))
non_neg_potential_pay_diff$log_num_email_total <- c(scale(non_neg_potential_pay_diff$log_num_email_total))
non_neg_potential_pay_diff$log_num_targets_total <- c(scale(non_neg_potential_pay_diff$log_num_targets_total))

non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))



# factor

categorical_col <-c('gender_13', 'manager_13', 'company_13', "func_13")
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)


mod_salary_competence <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                              log_num_email_total + log_per_warmth_total + log_per_competence_total + log_num_targets_total + gender_13*log_per_competence_total, data = non_neg_potential_pay_diff); summary(mod_salary_competence)


#slope analysis 

(competence_list <- list(log_per_competence_total=seq(-2,6,by=1),gender_source=c("0","1")))
emmeans_results_within_competence <- as.data.frame(emmeans(mod_salary_competence, ~ gender_13*log_per_competence_total,  by = 'gender_13', at=competence_list)); emmeans_results_within_competence


emmeans_mod_within_competence<- ggplot(data = emmeans_results_within_competence, aes(x = log_per_competence_total, y = emmean, colour = gender_13, group = gender_13, ymin = emmean-SE, ymax = emmean+SE))  + 
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = emmeans_results_within_competence, show.legend = F)+
  geom_line(data = emmeans_results_within_competence, size=1.1) + 
  geom_errorbar(width = 0.25, size = 1.5) +
  guides(size = "none", colour = guide_legend(nrow = 2, override.aes = list(size=8))) + 
  scale_y_continuous(breaks = pretty_breaks(5))+
  scale_x_continuous(breaks = pretty_breaks(5))+
  ylab("Predicted Pay Change") + xlab("Agency Words Percentage")  +
  ggtitle("Overall\n(Age <= 3rd. Qu)") +
  scale_color_manual(values=c("#0078b7", "#ff1315"), 
                     labels = c( 'Men', 'Women')) +
  guides(fill = "none", alpha = "none", colour = guide_legend(override.aes = list(size=8, shape = 15))) + 
  theme(axis.title = element_text(size=18),axis.text= element_text(size=15, colour = 'black'),plot.title= element_text(hjust = .5, size = 23), 
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        legend.position = 'None'); emmeans_mod_within_competence

interactions::sim_slopes(model = mod_salary_competence, 
                         pred = log_per_competence_total, modx = gender_13)

Table_4_lm_1 <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + func_13+
                     log_num_email_total + log_num_targets_total + log_per_warmth_total + log_per_competence_total, data = non_neg_potential_pay_diff); summary(Table_4_lm_1)

Table_4_lm_2 <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + func_13+
                     log_num_email_total + log_num_targets_total + log_per_warmth_total + log_per_competence_total + gender_13*log_per_competence_total, data = non_neg_potential_pay_diff); summary(Table_4_lm_2)


stargazer(Table_4_lm_1, Table_4_lm_2, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

# ----- within (Age <= 53): Agency moderation -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53)

individual_communication <- age_qu_3



individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)



within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", "func_13",
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within")
individual_within <- na.omit(individual_communication[,within_cols]) 


# potential pay < 0
non_neg_potential_pay_diff <- individual_within %>% filter(potential_pay_diff >-1) 



# log transformation

non_neg_potential_pay_diff["log_potential_pay_diff"] <- log1p(non_neg_potential_pay_diff["potential_pay_diff"])
non_neg_potential_pay_diff["log_per_warmth_within"] <- log1p(non_neg_potential_pay_diff["per_warmth_within"])
non_neg_potential_pay_diff["log_per_competence_within"] <- log1p(non_neg_potential_pay_diff["per_competence_within"])
non_neg_potential_pay_diff["log_num_email_within"] <- log1p(non_neg_potential_pay_diff["num_email_within"])
non_neg_potential_pay_diff["log_num_targets_within"] <- log1p(non_neg_potential_pay_diff["num_targets_within"])

# center
non_neg_potential_pay_diff$log_potential_pay_diff <- c(scale(non_neg_potential_pay_diff$log_potential_pay_diff))
non_neg_potential_pay_diff$log_per_warmth_within <- c(scale(non_neg_potential_pay_diff$log_per_warmth_within))
non_neg_potential_pay_diff$log_per_competence_within <- c(scale(non_neg_potential_pay_diff$log_per_competence_within))
non_neg_potential_pay_diff$log_num_email_within <- c(scale(non_neg_potential_pay_diff$log_num_email_within))
non_neg_potential_pay_diff$log_num_targets_within <- c(scale(non_neg_potential_pay_diff$log_num_targets_within))

non_neg_potential_pay_diff$age_13 <- c(scale(non_neg_potential_pay_diff$age_13))



# factor
categorical_col <-c('gender_13', 'manager_13', 'company_13', "func_13")
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")
non_neg_potential_pay_diff$func_13 <- unclass(non_neg_potential_pay_diff$func_13)

mod_salary_competence <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + func_13+
                              log_num_email_within + log_per_warmth_within + log_per_competence_within + log_num_targets_within + gender_13*log_per_competence_within, data = non_neg_potential_pay_diff); summary(mod_salary_competence)

# slope analysis 

(competence_list <- list(log_per_competence_within=seq(-2,6,by=1),gender_source=c("0","1")))
emmeans_results_within_competence <- as.data.frame(emmeans(mod_salary_competence, ~ gender_13*log_per_competence_within,  by = 'gender_13', at=competence_list)); emmeans_results_within_competence


emmeans_mod_within_competence_w_legend<- ggplot(data = emmeans_results_within_competence, aes(x = log_per_competence_within, y = emmean, colour = gender_13, group = gender_13, ymin = emmean-SE, ymax = emmean+SE))  + 
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = emmeans_results_within_competence, show.legend = F)+
  geom_line(data = emmeans_results_within_competence, size=1.1) + 
  geom_errorbar(width = 0.25, size = 1.5) +
  guides(size = "none", colour = guide_legend(nrow = 2, override.aes = list(size=8))) + 
  scale_y_continuous(breaks = pretty_breaks(5))+
  scale_x_continuous(breaks = pretty_breaks(5))+
  ylab("Predicted Pay Change") + xlab("Agency Words Percentage")  +
  ggtitle("Same-legacy\n(Age <= 3rd. Qu)") +
  scale_color_manual(values=c("#0078b7", "#ff1315"), 
                     labels = c( 'Men', 'Women')) +
  guides(fill = "none", alpha = "none", colour = guide_legend(override.aes = list(size=8, shape = 15))) + 
  theme(axis.title = element_text(size=18),axis.text= element_text(size=15, colour = 'black'),plot.title= element_text(hjust = .5, size = 23), 
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        legend.position = 'bottom', legend.text = element_text(size=15), legend.title = element_blank(),  
        legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.key.width = unit(0, "cm")); emmeans_mod_within_competence_w_legend
legend_gender <- get_legend(emmeans_mod_within_competence_w_legend)

emmeans_mod_within_competence_wo_legend<- ggplot(data = emmeans_results_within_competence, aes(x = log_per_competence_within, y = emmean, colour = gender_13, group = gender_13, ymin = emmean-SE, ymax = emmean+SE))  + 
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = emmeans_results_within_competence, show.legend = F)+
  geom_line(data = emmeans_results_within_competence, size=1.1) + 
  geom_errorbar(width = 0.25, size = 1.5) +
  guides(size = "none", colour = guide_legend(nrow = 2, override.aes = list(size=8))) + 
  scale_y_continuous(breaks = pretty_breaks(5))+
  scale_x_continuous(breaks = pretty_breaks(5))+
  ylab("Predicted Pay Change") + xlab("Agency Words Percentage")  +
  ggtitle("Same-legacy\n(Age <= 3rd. Qu)") +
  scale_color_manual(values=c("#0078b7", "#ff1315"), 
                     labels = c( 'Men', 'Women')) +
  guides(fill = "none", alpha = "none", colour = guide_legend(override.aes = list(size=8, shape = 15))) + 
  theme(axis.title = element_text(size=18),axis.text= element_text(size=15, colour = 'black'),plot.title= element_text(hjust = .5, size = 23), 
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        legend.position = 'None'); emmeans_mod_within_competence_wo_legend

interactions::sim_slopes(model = mod_salary_competence, 
                         pred = log_per_competence_within, modx = gender_13)


Table_4_lm_3 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + manager_13 + company_13 + func_13+
                     log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within , data = non_neg_potential_pay_diff); summary(Table_4_lm_3)

Table_4_lm_4 <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + func_13+
                     log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within + gender_13*log_per_competence_within, data = non_neg_potential_pay_diff); summary(Table_4_lm_4)

table_5_part_2 <- stargazer(Table_4_lm_3, Table_4_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

stargazer(Table_4_lm_1, Table_4_lm_2, Table_4_lm_3, Table_4_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

# plot
plot_grid(plot_grid(emmeans_mod_within_competence, emmeans_mod_within_competence_wo_legend,
                    ncol = 2,
                    labels = "AUTO",
                    label_size = 18,
                    align = "hv"
), legend_gender, 
ncol = 1,
#labels = "AUTO",
rel_heights = c(2,.2),
align = "hv")
























