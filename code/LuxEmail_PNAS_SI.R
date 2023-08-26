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
library(tidyverse)
library(scales)
library(ggthemes)
library(psych)
library(emmeans)
library(margins)
library(mediation) 
mediate <- mediation::mediate
library(e1071)
library(margins)
library(stargazer)
library(haven)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(quantreg)
library(broom)
library(purrr)



# ======================================================================== Table S1 ===================================================================



# skewness test and preprocessing


rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8")

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)



all_salary_change <- na.omit(individual_communication[,c("potential_pay_diff", "X")])
skewness(all_salary_change$potential_pay_diff, type = 2)
kurtosis(all_salary_change$potential_pay_diff, type = 2)

test_neg_change <- individual_communication %>% filter(potential_pay_diff < -1)



all_age <- na.omit(individual_communication[,c("age_13", "X")]) 
skewness(all_age$age_13, type = 2)
kurtosis(all_age$age_13, type = 2)



all_warmth <- na.omit(individual_communication[,c("per_warmth_total", "X")]) 
skewness(all_warmth$per_warmth_total, type = 2)
kurtosis(all_warmth$per_warmth_total, type = 2)

all_competence <- na.omit(individual_communication[,c("per_competence_total", "X")])
skewness(all_competence$per_competence_total, type = 2)
kurtosis(all_competence$per_competence_total, type = 2)



all_email_num <- na.omit(individual_communication[,c("num_email_total", "X")]) 
skewness(all_email_num$num_email_total, type = 2)
kurtosis(all_email_num$num_email_total, type = 2)



all_taegets <- na.omit(individual_communication[,c("num_targets_total", "X")]) 
skewness(all_taegets$num_targets_total, type = 2)
kurtosis(all_taegets$num_targets_total, type = 2)




total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) # 494

test_neg <- individual_total %>% filter(potential_pay_diff < -1)

# potential pay < 0
non_neg_salary_change <- individual_total %>% filter(potential_pay_diff >-1) # 486


# log transformation
non_neg_salary_change["log_potential_pay_diff"] <- log1p(non_neg_salary_change["potential_pay_diff"])
non_neg_salary_change["log_per_warmth_total"] <- log1p(non_neg_salary_change["per_warmth_total"])
non_neg_salary_change["log_per_competence_total"] <- log1p(non_neg_salary_change["per_competence_total"])
non_neg_salary_change["log_num_email_total"] <- log1p(non_neg_salary_change["num_email_total"])
non_neg_salary_change["log_num_targets_total"] <- log1p(non_neg_salary_change["num_targets_total"])


# center
non_neg_salary_change$log_potential_pay_diff <- c(scale(non_neg_salary_change$log_potential_pay_diff))
non_neg_salary_change$log_per_warmth_total <- c(scale(non_neg_salary_change$log_per_warmth_total))
non_neg_salary_change$log_per_competence_total <- c(scale(non_neg_salary_change$log_per_competence_total))
non_neg_salary_change$log_num_email_total <- c(scale(non_neg_salary_change$log_num_email_total))
non_neg_salary_change$log_num_targets_total <- c(scale(non_neg_salary_change$log_num_targets_total))
non_neg_salary_change$age_13 <- c(scale(non_neg_salary_change$age_13))

# test skewness again
skewness(non_neg_salary_change$log_potential_pay_diff, type = 2)
kurtosis(non_neg_salary_change$log_potential_pay_diff, type = 2)

skewness(non_neg_salary_change$age_13, type = 2)
kurtosis(non_neg_salary_change$age_13, type = 2)


skewness(non_neg_salary_change$log_per_warmth_total, type = 2)
kurtosis(non_neg_salary_change$log_per_warmth_total, type = 2)

skewness(non_neg_salary_change$log_per_competence_total, type = 2)
kurtosis(non_neg_salary_change$log_per_competence_total, type = 2)

skewness(non_neg_salary_change$log_num_email_total, type = 2)
kurtosis(non_neg_salary_change$log_num_email_total, type = 2)

skewness(non_neg_salary_change$log_num_targets_total, type = 2)
kurtosis(non_neg_salary_change$log_num_targets_total, type = 2)




# ======================================================================== Table S2 ===================================================================

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", "num_weekday_total", "num_weekend_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)


# women employee summary
women_employee_df <- non_neg_potential_pay_diff %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_df$num_email_total)
sd(women_employee_df$num_email_total)

summary(women_employee_df$num_weekday_total)
sd(women_employee_df$num_weekday_total)

summary(women_employee_df$num_weekend_total)
sd(women_employee_df$num_weekend_total)


# women manager summary
women_manager_df <- non_neg_potential_pay_diff %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")



summary(women_manager_df$num_email_total)
sd(women_manager_df$num_email_total)

summary(women_manager_df$num_weekday_total)
sd(women_manager_df$num_weekday_total)

summary(women_manager_df$num_weekend_total)
sd(women_manager_df$num_weekend_total)




# women summary
women_df <- non_neg_potential_pay_diff %>%
  filter(gender_13 == "0") 


summary(women_df$num_email_total)
sd(women_df$num_email_total)

summary(women_df$num_weekday_total)
sd(women_df$num_weekday_total)

summary(women_df$num_weekend_total)
sd(women_df$num_weekend_total)



# men employee summary
men_employee_df <- non_neg_potential_pay_diff %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_df$num_email_total)
sd(men_employee_df$num_email_total)

summary(men_employee_df$num_weekday_total)
sd(men_employee_df$num_weekday_total)

summary(men_employee_df$num_weekend_total)
sd(men_employee_df$num_weekend_total)


# men manager summary
men_manager_df <- non_neg_potential_pay_diff %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")


summary(men_manager_df$num_email_total)
sd(men_manager_df$num_email_total)

summary(men_manager_df$num_weekday_total)
sd(men_manager_df$num_weekday_total)

summary(men_manager_df$num_weekend_total)
sd(men_manager_df$num_weekend_total)

# men summary
men_df <- non_neg_potential_pay_diff %>%
  filter(gender_13 == "1") 


summary(men_df$num_email_total)
sd(men_df$num_email_total)

summary(men_df$num_weekday_total)
sd(men_df$num_weekday_total)

summary(men_df$num_weekend_total)
sd(men_df$num_weekend_total)

# overall summary



summary(non_neg_potential_pay_diff$num_email_total)
sd(non_neg_potential_pay_diff$num_email_total)

summary(non_neg_potential_pay_diff$num_weekday_total)
sd(non_neg_potential_pay_diff$num_weekday_total)


summary(non_neg_potential_pay_diff$num_weekend_total)
sd(non_neg_potential_pay_diff$num_weekend_total)


# ======================================================================== Table S3 ===================================================================



rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", "num_weekday_total", "num_weekend_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)

quantile(non_neg_potential_pay_diff$potential_pay_diff)
#        0%        25%          50%          75%          100% 
#   -0.004      1822.717      2680.301    5546.627      185879.924 

qu_1_df <- non_neg_potential_pay_diff %>%
  filter(potential_pay_diff <= 1822.717)


qu_2_df <- non_neg_potential_pay_diff %>%
  filter(potential_pay_diff <= 2680.301) %>% filter(potential_pay_diff > 1822.717)


qu_3_df <- non_neg_potential_pay_diff %>%
  filter(potential_pay_diff <= 5546.627) %>% filter(potential_pay_diff > 2680.301)


qu_4_df <- non_neg_potential_pay_diff %>%
  filter(potential_pay_diff > 5546.627)

# ---- Q1 ----
# women employee summary
women_employee_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_1_df$potential_pay_diff)
sd(women_employee_qu_1_df$potential_pay_diff)

summary(women_employee_qu_1_df$age_13)
sd(women_employee_qu_1_df$age_13)



# women manager summary
women_manager_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_1_df$potential_pay_diff)
sd(women_manager_qu_1_df$potential_pay_diff)

summary(women_manager_qu_1_df$age_13)
sd(women_manager_qu_1_df$age_13)




# women summary
women_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "0") 

summary(women_qu_1_df$potential_pay_diff)
sd(women_qu_1_df$potential_pay_diff)

summary(women_qu_1_df$age_13)
sd(women_qu_1_df$age_13)






# men employee summary
men_employee_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_1_df$potential_pay_diff)
sd(men_employee_qu_1_df$potential_pay_diff)

summary(men_employee_qu_1_df$age_13)
sd(men_employee_qu_1_df$age_13)



# men manager summary
men_manager_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_1_df$potential_pay_diff)
sd(men_manager_qu_1_df$potential_pay_diff)

summary(men_manager_qu_1_df$age_13)
sd(men_manager_qu_1_df$age_13)



# men summary
men_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "1") 

summary(men_qu_1_df$potential_pay_diff)
sd(men_qu_1_df$potential_pay_diff)

summary(men_qu_1_df$age_13)
sd(men_qu_1_df$age_13)


# overall summary


summary(qu_1_df$potential_pay_diff)
sd(qu_1_df$potential_pay_diff)




# ---- Q2 ----
# women employee summary
women_employee_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_2_df$potential_pay_diff)
sd(women_employee_qu_2_df$potential_pay_diff)

summary(women_employee_qu_2_df$age_13)
sd(women_employee_qu_2_df$age_13)



# women manager summary
women_manager_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_2_df$potential_pay_diff)
sd(women_manager_qu_2_df$potential_pay_diff)

summary(women_manager_qu_2_df$age_13)
sd(women_manager_qu_2_df$age_13)




# women summary
women_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "0") 

summary(women_qu_2_df$potential_pay_diff)
sd(women_qu_2_df$potential_pay_diff)

summary(women_qu_2_df$age_13)
sd(women_qu_2_df$age_13)






# men employee summary
men_employee_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_2_df$potential_pay_diff)
sd(men_employee_qu_2_df$potential_pay_diff)

summary(men_employee_qu_2_df$age_13)
sd(men_employee_qu_2_df$age_13)



# men manager summary
men_manager_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_2_df$potential_pay_diff)
sd(men_manager_qu_2_df$potential_pay_diff)

summary(men_manager_qu_2_df$age_13)
sd(men_manager_qu_2_df$age_13)



# men summary
men_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "1") 

summary(men_qu_2_df$potential_pay_diff)
sd(men_qu_2_df$potential_pay_diff)

summary(men_qu_2_df$age_13)
sd(men_qu_2_df$age_13)


# overall summary


summary(qu_2_df$potential_pay_diff)
sd(qu_2_df$potential_pay_diff)


# ---- Q3 ----
# women employee summary
women_employee_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_3_df$potential_pay_diff)
sd(women_employee_qu_3_df$potential_pay_diff)

summary(women_employee_qu_3_df$age_13)
sd(women_employee_qu_3_df$age_13)



# women manager summary
women_manager_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_3_df$potential_pay_diff)
sd(women_manager_qu_3_df$potential_pay_diff)

summary(women_manager_qu_3_df$age_13)
sd(women_manager_qu_3_df$age_13)




# women summary
women_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "0") 

summary(women_qu_3_df$potential_pay_diff)
sd(women_qu_3_df$potential_pay_diff)

summary(women_qu_3_df$age_13)
sd(women_qu_3_df$age_13)






# men employee summary
men_employee_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_3_df$potential_pay_diff)
sd(men_employee_qu_3_df$potential_pay_diff)

summary(men_employee_qu_3_df$age_13)
sd(men_employee_qu_3_df$age_13)



# men manager summary
men_manager_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_3_df$potential_pay_diff)
sd(men_manager_qu_3_df$potential_pay_diff)

summary(men_manager_qu_3_df$age_13)
sd(men_manager_qu_3_df$age_13)



# men summary
men_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "1") 

summary(men_qu_3_df$potential_pay_diff)
sd(men_qu_3_df$potential_pay_diff)

summary(men_qu_3_df$age_13)
sd(men_qu_3_df$age_13)


# overall summary


summary(qu_3_df$potential_pay_diff)
sd(qu_3_df$potential_pay_diff)



# ---- Q4 ----
# women employee summary
women_employee_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_4_df$potential_pay_diff)
sd(women_employee_qu_4_df$potential_pay_diff)

summary(women_employee_qu_4_df$age_13)
sd(women_employee_qu_4_df$age_13)



# women manager summary
women_manager_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_4_df$potential_pay_diff)
sd(women_manager_qu_4_df$potential_pay_diff)

summary(women_manager_qu_4_df$age_13)
sd(women_manager_qu_4_df$age_13)




# women summary
women_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "0") 

summary(women_qu_4_df$potential_pay_diff)
sd(women_qu_4_df$potential_pay_diff)

summary(women_qu_4_df$age_13)
sd(women_qu_4_df$age_13)






# men employee summary
men_employee_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_4_df$potential_pay_diff)
sd(men_employee_qu_4_df$potential_pay_diff)

summary(men_employee_qu_4_df$age_13)
sd(men_employee_qu_4_df$age_13)



# men manager summary
men_manager_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_4_df$potential_pay_diff)
sd(men_manager_qu_4_df$potential_pay_diff)

summary(men_manager_qu_4_df$age_13)
sd(men_manager_qu_4_df$age_13)



# men summary
men_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "1") 

summary(men_qu_4_df$potential_pay_diff)
sd(men_qu_4_df$potential_pay_diff)

summary(men_qu_4_df$age_13)
sd(men_qu_4_df$age_13)


# overall summary


summary(qu_4_df$potential_pay_diff)
sd(qu_4_df$potential_pay_diff)









# ======================================================================== Table S4 ===================================================================





# age summary
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

summary(individual_communication$age_13)


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", "num_weekday_total", "num_weekend_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)

summary(non_neg_potential_pay_diff$age_13)





# ======================================================================== Table S5 ===================================================================


rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 

total_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                "num_email_total", "num_targets_total", "num_words_total", "num_warmth_words_total", "num_competence_words_total",
                "per_warmth_total", "per_competence_total", "num_weekday_total", "num_weekend_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)

summary(non_neg_potential_pay_diff$age_13)
#  0%     25%       50%     75%     100% 
# 19       37       45      53      75





qu_1_df <- non_neg_potential_pay_diff %>%
  filter(age_13 <= 37)


qu_2_df <- non_neg_potential_pay_diff %>%
  filter(age_13 <= 45) %>% filter(age_13 > 37)


qu_3_df <- non_neg_potential_pay_diff %>%
  filter(age_13 <= 53) %>% filter(age_13 > 45)


qu_4_df <- non_neg_potential_pay_diff %>%
  filter(age_13 > 53)

# ---- Q1 ----
# women employee summary
women_employee_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_1_df$age_13)
sd(women_employee_qu_1_df$age_13)

summary(women_employee_qu_1_df$per_warmth_total)
sd(women_employee_qu_1_df$per_warmth_total)

summary(women_employee_qu_1_df$per_competence_total)
sd(women_employee_qu_1_df$per_competence_total)



# women manager summary
women_manager_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_1_df$age_13)
sd(women_manager_qu_1_df$age_13)

summary(women_manager_qu_1_df$per_warmth_total)
sd(women_manager_qu_1_df$per_warmth_total)

summary(women_manager_qu_1_df$per_competence_total)
sd(women_manager_qu_1_df$per_competence_total)




# women summary
women_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "0") 

summary(women_qu_1_df$age_13)
sd(women_qu_1_df$age_13)

summary(women_qu_1_df$per_warmth_total)
sd(women_qu_1_df$per_warmth_total)

summary(women_qu_1_df$per_competence_total)
sd(women_qu_1_df$per_competence_total)






# men employee summary
men_employee_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_1_df$age_13)
sd(men_employee_qu_1_df$age_13)

summary(men_employee_qu_1_df$per_warmth_total)
sd(men_employee_qu_1_df$per_warmth_total)

summary(men_employee_qu_1_df$per_competence_total)
sd(men_employee_qu_1_df$per_competence_total)



# men manager summary
men_manager_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_1_df$age_13)
sd(men_manager_qu_1_df$age_13)

summary(men_manager_qu_1_df$per_warmth_total)
sd(men_manager_qu_1_df$per_warmth_total)

summary(men_manager_qu_1_df$per_competence_total)
sd(men_manager_qu_1_df$per_competence_total)



# men summary
men_qu_1_df <- qu_1_df %>%
  filter(gender_13 == "1") 

summary(men_qu_1_df$age_13)
sd(men_qu_1_df$age_13)

summary(men_qu_1_df$per_warmth_total)
sd(men_qu_1_df$per_warmth_total)

summary(men_qu_1_df$per_competence_total)
sd(men_qu_1_df$per_competence_total)


# overall summary


summary(qu_1_df$age_13)
sd(qu_1_df$age_13)


# ---- Q2 ----
# women employee summary
women_employee_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_2_df$age_13)
sd(women_employee_qu_2_df$age_13)

summary(women_employee_qu_2_df$per_warmth_total)
sd(women_employee_qu_2_df$per_warmth_total)

summary(women_employee_qu_2_df$per_competence_total)
sd(women_employee_qu_2_df$per_competence_total)



# women manager summary
women_manager_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_2_df$age_13)
sd(women_manager_qu_2_df$age_13)

summary(women_manager_qu_2_df$per_warmth_total)
sd(women_manager_qu_2_df$per_warmth_total)

summary(women_manager_qu_2_df$per_competence_total)
sd(women_manager_qu_2_df$per_competence_total)




# women summary
women_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "0") 

summary(women_qu_2_df$age_13)
sd(women_qu_2_df$age_13)

summary(women_qu_2_df$per_warmth_total)
sd(women_qu_2_df$per_warmth_total)

summary(women_qu_2_df$per_competence_total)
sd(women_qu_2_df$per_competence_total)






# men employee summary
men_employee_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_2_df$age_13)
sd(men_employee_qu_2_df$age_13)

summary(men_employee_qu_2_df$per_warmth_total)
sd(men_employee_qu_2_df$per_warmth_total)

summary(men_employee_qu_2_df$per_competence_total)
sd(men_employee_qu_2_df$per_competence_total)



# men manager summary
men_manager_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_2_df$age_13)
sd(men_manager_qu_2_df$age_13)

summary(men_manager_qu_2_df$per_warmth_total)
sd(men_manager_qu_2_df$per_warmth_total)

summary(men_manager_qu_2_df$per_competence_total)
sd(men_manager_qu_2_df$per_competence_total)



# men summary
men_qu_2_df <- qu_2_df %>%
  filter(gender_13 == "1") 

summary(men_qu_2_df$age_13)
sd(men_qu_2_df$age_13)

summary(men_qu_2_df$per_warmth_total)
sd(men_qu_2_df$per_warmth_total)

summary(men_qu_2_df$per_competence_total)
sd(men_qu_2_df$per_competence_total)


# overall summary


summary(qu_2_df$age_13)
sd(qu_2_df$age_13)


# ---- Q3 ----
# women employee summary
women_employee_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_3_df$age_13)
sd(women_employee_qu_3_df$age_13)

summary(women_employee_qu_3_df$per_warmth_total)
sd(women_employee_qu_3_df$per_warmth_total)

summary(women_employee_qu_3_df$per_competence_total)
sd(women_employee_qu_3_df$per_competence_total)



# women manager summary
women_manager_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_3_df$age_13)
sd(women_manager_qu_3_df$age_13)

summary(women_manager_qu_3_df$per_warmth_total)
sd(women_manager_qu_3_df$per_warmth_total)

summary(women_manager_qu_3_df$per_competence_total)
sd(women_manager_qu_3_df$per_competence_total)




# women summary
women_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "0") 

summary(women_qu_3_df$age_13)
sd(women_qu_3_df$age_13)

summary(women_qu_3_df$per_warmth_total)
sd(women_qu_3_df$per_warmth_total)

summary(women_qu_3_df$per_competence_total)
sd(women_qu_3_df$per_competence_total)






# men employee summary
men_employee_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_3_df$age_13)
sd(men_employee_qu_3_df$age_13)

summary(men_employee_qu_3_df$per_warmth_total)
sd(men_employee_qu_3_df$per_warmth_total)

summary(men_employee_qu_3_df$per_competence_total)
sd(men_employee_qu_3_df$per_competence_total)



# men manager summary
men_manager_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_3_df$age_13)
sd(men_manager_qu_3_df$age_13)

summary(men_manager_qu_3_df$per_warmth_total)
sd(men_manager_qu_3_df$per_warmth_total)

summary(men_manager_qu_3_df$per_competence_total)
sd(men_manager_qu_3_df$per_competence_total)



# men summary
men_qu_3_df <- qu_3_df %>%
  filter(gender_13 == "1") 

summary(men_qu_3_df$age_13)
sd(men_qu_3_df$age_13)

summary(men_qu_3_df$per_warmth_total)
sd(men_qu_3_df$per_warmth_total)

summary(men_qu_3_df$per_competence_total)
sd(men_qu_3_df$per_competence_total)


# overall summary


summary(qu_3_df$age_13)
sd(qu_3_df$age_13)


# ---- Q4 ----
# women employee summary
women_employee_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "0")


summary(women_employee_qu_4_df$age_13)
sd(women_employee_qu_4_df$age_13)

summary(women_employee_qu_4_df$per_warmth_total)
sd(women_employee_qu_4_df$per_warmth_total)

summary(women_employee_qu_4_df$per_competence_total)
sd(women_employee_qu_4_df$per_competence_total)



# women manager summary
women_manager_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "0") %>%
  filter(manager_13 == "1")

summary(women_manager_qu_4_df$age_13)
sd(women_manager_qu_4_df$age_13)

summary(women_manager_qu_4_df$per_warmth_total)
sd(women_manager_qu_4_df$per_warmth_total)

summary(women_manager_qu_4_df$per_competence_total)
sd(women_manager_qu_4_df$per_competence_total)




# women summary
women_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "0") 

summary(women_qu_4_df$age_13)
sd(women_qu_4_df$age_13)

summary(women_qu_4_df$per_warmth_total)
sd(women_qu_4_df$per_warmth_total)

summary(women_qu_4_df$per_competence_total)
sd(women_qu_4_df$per_competence_total)






# men employee summary
men_employee_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "0")


summary(men_employee_qu_4_df$age_13)
sd(men_employee_qu_4_df$age_13)

summary(men_employee_qu_4_df$per_warmth_total)
sd(men_employee_qu_4_df$per_warmth_total)

summary(men_employee_qu_4_df$per_competence_total)
sd(men_employee_qu_4_df$per_competence_total)



# men manager summary
men_manager_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "1") %>%
  filter(manager_13 == "1")

summary(men_manager_qu_4_df$age_13)
sd(men_manager_qu_4_df$age_13)

summary(men_manager_qu_4_df$per_warmth_total)
sd(men_manager_qu_4_df$per_warmth_total)

summary(men_manager_qu_4_df$per_competence_total)
sd(men_manager_qu_4_df$per_competence_total)



# men summary
men_qu_4_df <- qu_4_df %>%
  filter(gender_13 == "1") 

summary(men_qu_4_df$age_13)
sd(men_qu_4_df$age_13)

summary(men_qu_4_df$per_warmth_total)
sd(men_qu_4_df$per_warmth_total)

summary(men_qu_4_df$per_competence_total)
sd(men_qu_4_df$per_competence_total)


# overall summary


summary(qu_4_df$age_13)
sd(qu_4_df$age_13)




# ======================================================================== Table S6 ===================================================================



# after excluding 38 employees without gender information
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))

individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)


cols_manager <- c("manager_13", "potential_pay_diff", "per_warmth_total", "per_competence_total", "num_email_total", "num_targets_total", "gender_13")
individual_manager_df <- na.omit(individual_communication[,cols_manager]) #494


non_neg_manager_w_gender_df <- individual_manager_df %>% filter(potential_pay_diff > -1) # 486


categorical_manager_w_gender <-c('manager_13')
non_neg_manager_w_gender_df[categorical_manager_w_gender] <- lapply(non_neg_manager_w_gender_df[categorical_manager_w_gender], as.factor)
non_neg_manager_w_gender_df$manager_13 <- relevel(non_neg_manager_w_gender_df$manager_13, ref = "0")

# stats 
table(non_neg_manager_w_gender_df$manager_13)
#  0      1 
# 397   89

non_neg_manager_w_gender_df %>%
  group_by(manager_13, gender_13) %>%
  summarise(num = n())
#  manager_13   gender_13   num
#     0            0        169
#     0            1        228
#     1            0        20
#     1            1        69


# potential pay - sig
# Welch Two Sample t-test
var.test(potential_pay_diff~manager_13, data = non_neg_manager_w_gender_df)$p.value
potential_pay_diff_t_test <- t.test(potential_pay_diff~manager_13, data = non_neg_manager_w_gender_df, alternative = "two.sided", var.equal = FALSE); potential_pay_diff_t_test
non_neg_manager_w_gender_df %>%
  group_by(manager_13) %>%
  summarise(sd_warmth_percent = sd(potential_pay_diff))



# warmth - sig
# Two Sample t-test
var.test(per_warmth_total~manager_13, data = non_neg_manager_w_gender_df)$p.value
warmth_percent_t_test <- t.test(per_warmth_total~manager_13, data = non_neg_manager_w_gender_df, alternative = "two.sided", var.equal = TRUE); warmth_percent_t_test
non_neg_manager_w_gender_df %>%
  group_by(manager_13) %>%
  summarise(sd_warmth_percent = sd(per_warmth_total))



# competence - not sig
# Two Sample t-test
var.test(per_competence_total~manager_13, data = non_neg_manager_w_gender_df)$p.value
competence_percent_t_test <- t.test(per_competence_total~manager_13, data = non_neg_manager_w_gender_df, alternative = "two.sided", var.equal = TRUE); competence_percent_t_test
non_neg_manager_w_gender_df %>%
  group_by(manager_13) %>%
  summarise(sd_competence_percent = sd(per_competence_total))




# num_email_total - sig
# Two Sample t-test
var.test(num_email_total~manager_13, data = non_neg_manager_w_gender_df)$p.value
num_email_total_t_test <- t.test(num_email_total~manager_13, data = non_neg_manager_w_gender_df, alternative = "two.sided", var.equal = TRUE); num_email_total_t_test
non_neg_manager_w_gender_df %>%
  group_by(manager_13) %>%
  summarise(sd_warmth_percent = sd(num_email_total))



# num_targets_total - sig
# Two Sample t-test
var.test(num_targets_total~manager_13, data = non_neg_manager_w_gender_df)$p.value
num_targets_total_t_test <- t.test(num_targets_total~manager_13, data = non_neg_manager_w_gender_df, alternative = "two.sided", var.equal = TRUE); num_targets_total_t_test
non_neg_manager_w_gender_df %>%
  group_by(manager_13) %>%
  summarise(sd_competence_percent = sd(num_targets_total))




# ======================================================================== Table S7 + Fig. S1 ===================================================================

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)


within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within")
individual_within <- na.omit(individual_communication[,within_cols]) #590

# potential pay < 0
non_neg_potential_pay_diff <- individual_within %>% filter(potential_pay_diff >-1) # 581


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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0') #396
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1')  #89

non_neg_potential_pay_diff <- employee_df # 396


lm_salary_demo_com <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                           log_num_email_within+ log_num_targets_within + log_per_warmth_within + log_per_competence_within, data = non_neg_potential_pay_diff); summary(lm_salary_demo_com)




quants <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_res <- map(quants, ~rq(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                            log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within  , 
                          tau = .x, 
                          data=non_neg_potential_pay_diff)
)


summary(qr_res[[3]])

stargazer::stargazer(lm_salary_demo_com, qr_res, 
                     rq.se = "boot",
                     column.labels = c("OLS", paste("tau = ", quants)),
                     covariate.labels = c("(Gender) Women",
                                          "Age",
                                          "(Company) Luxury",
                                          "Email Number",
                                          "Email Target",
                                          "Communion",
                                          "Agency"),
                     dep.var.labels = "Pay Change",
                     #omit = c("Constant"),
                     model.numbers = TRUE,
                     model.names =  FALSE,
                     keep.stat = c('n'),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     type ='text')


qr_tidy <- map(qr_res, ~tidy(.x, se = "boot", conf.int = TRUE)) %>%
  bind_rows()

ols_tidy <- tidy(lm_salary_demo_com, conf.int = TRUE)

term_labels <- c(`(Intercept)` = "(Intercept)", 
                 `gender_130` = "(Gender) Women",
                 `age_13` = "Age",
                 `company_13Luxury` = "(Company) Luxury",
                 `log_num_email_within` = "Email Number",
                 `log_num_targets_within` = "Email Target",
                 `log_per_warmth_within` = "Communion",
                 `log_per_competence_within` = "Agency")

ggplot(aes(x = tau,y = estimate), data = qr_tidy) +
  geom_point(color = "#27408b", size = 3) +
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.05, size  = 0.3, position = position_dodge(.9)) +
  geom_hline(data = ols_tidy, aes(yintercept = estimate), color = "red") + 
  facet_wrap(~term, scales="free", ncol=2, labeller = as_labeller(term_labels)) +
  theme(strip.text.x = element_text(size = 13))





# ======================================================================== Table S8 ===================================================================




# ----- regression 1 - overall: agency ~ gender -----

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


Table_S8_lm_1 <- lm(log_per_competence_total ~ gender_13 + age_13 + manager_13 + company_13 +func_13 +
                      log_num_email_total + log_num_targets_total, data = non_neg_potential_pay_diff); summary(Table_S8_lm_1) 

stargazer(Table_S8_lm_1, star.cutoffs = c(0.05, 0.01, 0.001), type='text')


# ----- regression 2 - overall: communion ~ gender -----
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


Table_S8_lm_2 <- lm(log_per_warmth_total ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                      log_num_email_total + log_num_targets_total, data = non_neg_potential_pay_diff); summary(Table_S8_lm_2)

stargazer(Table_S8_lm_2, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')





# ----- regression 3 - within: agency ~ gender -----
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3


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






Table_S8_lm_3 <- lm(log_per_competence_within ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                      log_num_email_within + log_num_targets_within, data = non_neg_potential_pay_diff); summary(Table_S8_lm_3)

stargazer(Table_S8_lm_3, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')









# ----- regression 4 - within: communion ~ gender -----
rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3


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






Table_S8_lm_4 <- lm(log_per_warmth_within ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                      log_num_email_within + log_num_targets_within, data = non_neg_potential_pay_diff); summary(Table_S8_lm_4)

stargazer(Table_S8_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')





# ---------- regression 5 - across: agency ~ gender -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



across_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_across", "num_targets_across", "num_words_across", "num_warmth_words_across", "num_competence_words_across",
                 "per_warmth_across", "per_competence_across", 'func_13')
individual_across <- na.omit(individual_communication[,across_cols]) #238

# potential pay < 0
non_neg_potential_pay_diff <- individual_across %>% filter(potential_pay_diff >-1) # 217



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


Table_S8_lm_5 <- lm(log_per_competence_across ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                      log_num_email_across + log_num_targets_across, data = non_neg_potential_pay_diff); summary(Table_S8_lm_5)

stargazer(Table_S8_lm_5, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')








# ---------- regression 6 - across: acommunion ~ gender -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



across_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_across", "num_targets_across", "num_words_across", "num_warmth_words_across", "num_competence_words_across",
                 "per_warmth_across", "per_competence_across", 'func_13')
individual_across <- na.omit(individual_communication[,across_cols]) #238

# potential pay < 0
non_neg_potential_pay_diff <- individual_across %>% filter(potential_pay_diff >-1) # 217



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


Table_S8_lm_6 <- lm(log_per_warmth_across ~ gender_13 + age_13 + manager_13 + company_13 + func_13 +
                      log_num_email_across + log_num_targets_across, data = non_neg_potential_pay_diff); summary(Table_S8_lm_6)

stargazer(Table_S8_lm_6, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')



stargazer(Table_S8_lm_1, Table_S8_lm_2, Table_S8_lm_3, Table_S8_lm_4, Table_S8_lm_5, Table_S8_lm_6,align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')







# ======================================================================== Table S9 ===================================================================








# ----- overall - Employee - Age <= 53: pay change ~ gender -----
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
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) #494


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)#486


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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0')
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1') 

non_neg_potential_pay_diff <- employee_df


Table_S9_lm_1 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_total + log_num_targets_total + log_per_warmth_total + log_per_competence_total, data = non_neg_potential_pay_diff); summary(Table_S9_lm_1)



Table_S9_lm_2 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_total + log_num_targets_total + log_per_warmth_total + log_per_competence_total + gender_13*log_per_competence_total, data = non_neg_potential_pay_diff); summary(Table_S9_lm_2)


stargazer(Table_S9_lm_1, Table_S9_lm_2, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')



# ----- within - Employee - Age <= 53: agency moderation -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X"))

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3



individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)



within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0') 
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1') 

non_neg_potential_pay_diff <- employee_df 



Table_S9_lm_3 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within, data = non_neg_potential_pay_diff); summary(Table_S9_lm_3)



Table_S9_lm_4 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within + gender_13*log_per_competence_within, data = non_neg_potential_pay_diff); summary(Table_S9_lm_4)


stargazer(Table_S9_lm_3, Table_S9_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

stargazer(Table_S9_lm_1, Table_S9_lm_2, Table_S9_lm_3, Table_S9_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

















# ======================================================================== Table S10 ===================================================================






# ----- overall - Manager - Age <= 53: pay change ~ gender -----
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
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) #494


# potential pay < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)#486


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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0')
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1') 

non_neg_potential_pay_diff <- manager_df


Table_S10_lm_1 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_total + log_num_targets_total + log_per_warmth_total + log_per_competence_total, data = non_neg_potential_pay_diff); summary(Table_S10_lm_1)



Table_S10_lm_2 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_total + log_num_targets_total + log_per_warmth_total + log_per_competence_total + gender_13*log_per_competence_total, data = non_neg_potential_pay_diff); summary(Table_S10_lm_2)


stargazer(Table_S10_lm_1, Table_S10_lm_2, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')



# ----- within - Manager - Age <= 53: agency moderation -----

rm(list = ls())
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 

age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3



individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13)



within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0') 
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1') 

non_neg_potential_pay_diff <- manager_df 



Table_S10_lm_3 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within, data = non_neg_potential_pay_diff); summary(Table_S10_lm_3)



Table_S10_lm_4 <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 +
                      log_num_email_within + log_num_targets_within + log_per_warmth_within + log_per_competence_within + gender_13*log_per_competence_within, data = non_neg_potential_pay_diff); summary(Table_S10_lm_4)


stargazer(Table_S10_lm_3, Table_S10_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

stargazer(Table_S10_lm_1, Table_S10_lm_2, Table_S10_lm_3, Table_S10_lm_4, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')



















# ======================================================================== Table S11 + Table S12 ===================================================================

rm(list = ls())
mediate <- mediation::mediate
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
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# pay change < 0
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

categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


# agency mediation
comp_percent_fit.totaleffect <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + log_num_email_total + log_num_targets_total + log_per_warmth_total, 
                                   data = non_neg_potential_pay_diff); summary(comp_percent_fit.totaleffect)

comp_percent_fit.mediator=lm(log_per_competence_total~gender_13 + age_13 + manager_13 + company_13 + log_num_email_total + log_num_targets_total + log_per_warmth_total, 
                             data = non_neg_potential_pay_diff);summary(comp_percent_fit.mediator)

comp_percent_fit.dv=lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + log_num_email_total + log_per_warmth_total + log_per_competence_total + log_num_targets_total, 
                       data = non_neg_potential_pay_diff);summary(comp_percent_fit.dv)
stargazer(comp_percent_fit.totaleffect, comp_percent_fit.mediator, comp_percent_fit.dv, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')


mediation_comp_percent_results = mediate(comp_percent_fit.mediator, comp_percent_fit.dv, 
                                         treat='gender_13', mediator='log_per_competence_total', boot=T,treat.value = '0', control.value = '1');summary(mediation_comp_percent_results)



# ======================================================================== Table S13 + Table S14 ===================================================================


rm(list = ls())
mediate <- mediation::mediate
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3



individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 



within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within")
individual_within <- na.omit(individual_communication[,within_cols]) 


# pay change < 0
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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


# agency mediation - within
comp_percent_fit.totaleffect <- lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + log_num_email_within + log_num_targets_within + log_per_warmth_within, 
                                   data = non_neg_potential_pay_diff);summary(comp_percent_fit.totaleffect)

comp_percent_fit.mediator=lm(log_per_competence_within~gender_13 + age_13 + manager_13 + company_13 + log_num_email_within + log_num_targets_within + log_per_warmth_within, 
                             data = non_neg_potential_pay_diff);summary(comp_percent_fit.mediator)

comp_percent_fit.dv=lm(log_potential_pay_diff ~ gender_13 + age_13 + manager_13 + company_13 + log_num_email_within + log_per_warmth_within + log_per_competence_within + log_num_targets_within, 
                       data = non_neg_potential_pay_diff);summary(comp_percent_fit.dv)

stargazer(comp_percent_fit.totaleffect, comp_percent_fit.mediator, comp_percent_fit.dv, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

mediation_comp_percent_results = mediate(comp_percent_fit.mediator, comp_percent_fit.dv, 
                                         treat='gender_13', mediator='log_per_competence_within', boot=T,treat.value = '0', control.value = '1');summary(mediation_comp_percent_results)









# ======================================================================== Table S15 + Table S16 ===================================================================


rm(list = ls())
mediate <- mediation::mediate
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
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


# pay change < 0
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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0') 
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1')  

non_neg_potential_pay_diff <- employee_df 



# agency mediation
comp_percent_fit.totaleffect <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 + log_num_email_total + log_num_targets_total + log_per_warmth_total, 
                                   data = non_neg_potential_pay_diff);summary(comp_percent_fit.totaleffect)


comp_percent_fit.mediator=lm(log_per_competence_total~gender_13 + age_13  + company_13 + log_num_email_total + log_num_targets_total + log_per_warmth_total, 
                             data = non_neg_potential_pay_diff);summary(comp_percent_fit.mediator)


comp_percent_fit.dv=lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 + log_num_email_total + log_per_warmth_total + log_per_competence_total + log_num_targets_total, 
                       data = non_neg_potential_pay_diff);summary(comp_percent_fit.dv)


stargazer(comp_percent_fit.totaleffect, comp_percent_fit.mediator, comp_percent_fit.dv, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

mediation_comp_percent_results = mediate(comp_percent_fit.mediator, comp_percent_fit.dv, 
                                         treat='gender_13', mediator='log_per_competence_total', boot=T,treat.value = '0', control.value = '1'); summary(mediation_comp_percent_results)








# ======================================================================== Table S17 + Table S18 ===================================================================


rm(list = ls())
mediate <- mediation::mediate
individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3




individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 


within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within")
individual_within <- na.omit(individual_communication[,within_cols]) 


# pay change < 0
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
categorical_col <-c('gender_13', 'manager_13', 'company_13')
non_neg_potential_pay_diff[categorical_col] <- lapply(non_neg_potential_pay_diff[categorical_col], as.factor)
non_neg_potential_pay_diff$gender_13 <- relevel(non_neg_potential_pay_diff$gender_13, ref = "1")
non_neg_potential_pay_diff$manager_13 <- relevel(non_neg_potential_pay_diff$manager_13, ref = '0')
non_neg_potential_pay_diff$company_13 <- relevel(non_neg_potential_pay_diff$company_13, ref = "Standard")


employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0')
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1') 
non_neg_potential_pay_diff <- employee_df 


# agency mediation
comp_percent_fit.totaleffect <- lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 + log_num_email_within + log_num_targets_within + log_per_warmth_within, 
                                   data = non_neg_potential_pay_diff);summary(comp_percent_fit.totaleffect)


comp_percent_fit.mediator=lm(log_per_competence_within~gender_13 + age_13  + company_13 + log_num_email_within + log_num_targets_within + log_per_warmth_within, 
                             data = non_neg_potential_pay_diff);summary(comp_percent_fit.mediator)


comp_percent_fit.dv=lm(log_potential_pay_diff ~ gender_13 + age_13  + company_13 + log_num_email_within + log_per_warmth_within + log_per_competence_within + log_num_targets_within, 
                       data = non_neg_potential_pay_diff);summary(comp_percent_fit.dv)


stargazer(comp_percent_fit.totaleffect, comp_percent_fit.mediator, comp_percent_fit.dv, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')

mediation_comp_percent_results = mediate(comp_percent_fit.mediator, comp_percent_fit.dv, 
                                         treat='gender_13', mediator='log_per_competence_within', boot=T,treat.value = '0', control.value = '1');summary(mediation_comp_percent_results)




# ======================================================================== Table S19 ===================================================================

rm(list = ls())
set.seed(1839)

model14 <- function(iv, dv, med, mod, data, samples = 2000) {
  data[, "medxmod"] <- data[, med] * data[, mod]
  dichot <- all(data[, mod] == 0 | data[, mod] == 1)
  
  if (dichot) {
    # The name of the moderator, as a character string. If the moderator is numeric with only 0s and 1s, 
    # it will return the simple indirect effects at the values of 0 and 1
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp*", iv, "\n",
      "imm := a * b3\nind_0 := a * b1\nind_1 := a * b1 + imm"
    )
  } else {
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp * ", iv, "\n",
      "imm := a * b3\n",
      
      mod, " ~ modmean * 1\n",
      mod, " ~~ modvar * ", mod, "\n",
      
      "ind_lo := a * b1 + imm * (modmean - sqrt(modvar))\n",
      "ind_mn := a * b1 + imm * modmean\n",
      "ind_hi := a * b1 + imm * (modmean + sqrt(modvar))"
    )
  }
  
  out <- summary(lavaan::sem(model = model, data = data, se = "bootstrap", bootstrap = samples), 
                 fit.measures = FALSE,
                 standardize = TRUE,
                 rsquare = TRUE)
  
  return(out)
}

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
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


individual_total['num_gender'] = individual_total['gender_13']
individual_total['num_manager'] = individual_total['manager_13']
individual_total['num_company'] = ifelse(individual_total['company_13'] == 'Luxury', 1, 0)


# pay change < 0
non_neg_potential_pay_diff <- individual_total %>% filter(potential_pay_diff >-1)#486


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

#define variables
iv_warmth_all = c('num_gender + age_13 + num_manager + num_company + log_num_email_total + log_per_competence_total + log_num_targets_total')
iv_competence_all = c('num_gender + age_13 + num_manager + num_company + log_num_email_total + log_per_warmth_total + log_num_targets_total')


dv_warmth = 'log_potential_pay_diff'
dv_competence = 'log_potential_pay_diff'


med_warmth = 'log_per_warmth_total'
mod_warmth = 'num_gender'


med_competence = 'log_per_competence_total'
mod_competence = 'num_gender'



agency_mod_med_results <- model14(iv_competence_all, dv_competence, med_competence, mod_competence, 
                                  non_neg_potential_pay_diff, samples = 2000); agency_mod_med_results


# ======================================================================== Table S20 ===================================================================

rm(list = ls())
set.seed(1839)


model14 <- function(iv, dv, med, mod, data, samples = 2000) {
  data[, "medxmod"] <- data[, med] * data[, mod]
  dichot <- all(data[, mod] == 0 | data[, mod] == 1)
  
  if (dichot) {
    # The name of the moderator, as a character string. If the moderator is numeric with only 0s and 1s, 
    # it will return the simple indirect effects at the values of 0 and 1
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp*", iv, "\n",
      "imm := a * b3\nind_0 := a * b1\nind_1 := a * b1 + imm"
    )
  } else {
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp * ", iv, "\n",
      "imm := a * b3\n",
      
      mod, " ~ modmean * 1\n",
      mod, " ~~ modvar * ", mod, "\n",
      
      "ind_lo := a * b1 + imm * (modmean - sqrt(modvar))\n",
      "ind_mn := a * b1 + imm * modmean\n",
      "ind_hi := a * b1 + imm * (modmean + sqrt(modvar))"
    )
  }
  
  out <- summary(lavaan::sem(model = model, data = data, se = "bootstrap", bootstrap = samples), 
                 fit.measures = FALSE,
                 standardize = TRUE,
                 rsquare = TRUE)
  
  return(out)
}



individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 


age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53)


individual_communication <- age_qu_3


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 


within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within")
individual_within <- na.omit(individual_communication[,within_cols]) 


individual_within['num_gender'] = individual_within['gender_13']
individual_within['num_manager'] = individual_within['manager_13']
individual_within['num_company'] = ifelse(individual_within['company_13'] == 'Luxury', 1, 0)


# pay change < 0
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



# define variables
iv_warmth_all = c('num_gender + age_13 + num_manager + num_company + log_num_email_within + log_per_competence_within + log_num_targets_within')
iv_competence_all = c('num_gender + age_13 + num_manager + num_company + log_num_email_within + log_per_warmth_within + log_num_targets_within')


dv_warmth = 'log_potential_pay_diff'
dv_competence = 'log_potential_pay_diff'


med_warmth = 'log_per_warmth_within'
med_competence = 'log_per_competence_within'


mod_warmth = 'num_gender'
mod_competence = 'num_gender'


# moderated mediation effect of agency
model14(iv_competence_all, dv_competence, med_competence, mod_competence, 
        non_neg_potential_pay_diff, samples = 2000)





# ======================================================================== Table S21 ===================================================================
rm(list = ls())
set.seed(1839)


model14 <- function(iv, dv, med, mod, data, samples = 2000) {
  data[, "medxmod"] <- data[, med] * data[, mod]
  dichot <- all(data[, mod] == 0 | data[, mod] == 1)
  
  if (dichot) {
    # The name of the moderator, as a character string. If the moderator is numeric with only 0s and 1s, 
    # it will return the simple indirect effects at the values of 0 and 1
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp*", iv, "\n",
      "imm := a * b3\nind_0 := a * b1\nind_1 := a * b1 + imm"
    )
  } else {
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp * ", iv, "\n",
      "imm := a * b3\n",
      
      mod, " ~ modmean * 1\n",
      mod, " ~~ modvar * ", mod, "\n",
      
      "ind_lo := a * b1 + imm * (modmean - sqrt(modvar))\n",
      "ind_mn := a * b1 + imm * modmean\n",
      "ind_hi := a * b1 + imm * (modmean + sqrt(modvar))"
    )
  }
  
  out <- summary(lavaan::sem(model = model, data = data, se = "bootstrap", bootstrap = samples), 
                 fit.measures = FALSE,
                 standardize = TRUE,
                 rsquare = TRUE)
  
  return(out)
}



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
                "per_warmth_total", "per_competence_total")
individual_total <- na.omit(individual_communication[,total_cols]) 


individual_total['num_gender'] = individual_total['gender_13']
individual_total['num_manager'] = individual_total['manager_13']
individual_total['num_company'] = ifelse(individual_total['company_13'] == 'Luxury', 1, 0)


# pay change < 0
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

employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0') 
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1')  

non_neg_potential_pay_diff <- employee_df

iv_warmth_all = c('num_gender + age_13  + num_company + log_num_email_total + log_per_competence_total + log_num_targets_total')
iv_competence_all = c('num_gender + age_13  + num_company + log_num_email_total + log_per_warmth_total + log_num_targets_total')


dv_warmth = 'log_potential_pay_diff'
dv_competence = 'log_potential_pay_diff'


med_warmth = 'log_per_warmth_total'
mod_warmth = 'num_gender'


med_competence = 'log_per_competence_total'
mod_competence = 'num_gender'



model14(iv_competence_all, dv_competence, med_competence, mod_competence, 
        non_neg_potential_pay_diff, samples = 2000)
























# ======================================================================== Table S22 ===================================================================

rm(list = ls())
set.seed(1839)


model14 <- function(iv, dv, med, mod, data, samples = 2000) {
  data[, "medxmod"] <- data[, med] * data[, mod]
  dichot <- all(data[, mod] == 0 | data[, mod] == 1)
  
  if (dichot) {
    # The name of the moderator, as a character string. If the moderator is numeric with only 0s and 1s, 
    # it will return the simple indirect effects at the values of 0 and 1
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp*", iv, "\n",
      "imm := a * b3\nind_0 := a * b1\nind_1 := a * b1 + imm"
    )
  } else {
    model <- paste0(
      med, " ~ a * ", iv, "\n",
      dv, " ~ b1 * ", med, " + b2 * ", mod, " + b3 * medxmod + cp * ", iv, "\n",
      "imm := a * b3\n",
      
      mod, " ~ modmean * 1\n",
      mod, " ~~ modvar * ", mod, "\n",
      
      "ind_lo := a * b1 + imm * (modmean - sqrt(modvar))\n",
      "ind_mn := a * b1 + imm * modmean\n",
      "ind_hi := a * b1 + imm * (modmean + sqrt(modvar))"
    )
  }
  
  out <- summary(lavaan::sem(model = model, data = data, se = "bootstrap", bootstrap = samples), 
                 fit.measures = FALSE,
                 standardize = TRUE,
                 rsquare = TRUE)
  return(out)
}


individual_communication <- read.csv("communication_share_df.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-c("X")) 



age_qu_3 <- individual_communication %>%
  filter(age_13 <= 53) 


individual_communication <- age_qu_3


individual_communication <- individual_communication %>%
  mutate(salary_bonus_13 = annual_salary_13 + bonus_13) %>%
  mutate(salary_bonus_14 = annual_salary_14 + bonus_14) %>%
  mutate(potential_pay_diff = salary_bonus_14 - salary_bonus_13) 


within_cols <- c("gender_13", "age_13", "manager_13", "company_13", "potential_pay_diff", 
                 "num_email_within", "num_targets_within", "num_words_within", "num_warmth_words_within", "num_competence_words_within",
                 "per_warmth_within", "per_competence_within")
individual_within <- na.omit(individual_communication[,within_cols]) 


individual_within['num_gender'] = individual_within['gender_13']
individual_within['num_manager'] = individual_within['manager_13']
individual_within['num_company'] = ifelse(individual_within['company_13'] == 'Luxury', 1, 0)


# pay change < 0
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

employee_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '0') 
manager_df <- non_neg_potential_pay_diff %>% filter(manager_13 == '1')  

non_neg_potential_pay_diff <- employee_df

# define variables
iv_warmth_all = c('num_gender + age_13  + num_company + log_num_email_within + log_per_competence_within + log_num_targets_within')
iv_competence_all = c('num_gender + age_13  + num_company + log_num_email_within + log_per_warmth_within + log_num_targets_within')


dv_warmth = 'log_potential_pay_diff'
dv_competence = 'log_potential_pay_diff'


med_warmth = 'log_per_warmth_within'
med_competence = 'log_per_competence_within'


mod_warmth = 'num_gender'
mod_competence = 'num_gender'


#moderated mediation effect of agency

model14(iv_competence_all, dv_competence, med_competence, mod_competence, 
        non_neg_potential_pay_diff, samples = 2000)







