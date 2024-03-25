
#Assignment
#Imagine that Freedom ran a recent marketing campaign to promote the value 
#proposition of how the debt relief program helps people achieve financial freedom. 
#Assume the cost of this campaign was $5 million. There are five months of data in the datasets provided. 
#Let’s say campaign took place over the course of the third month. 
#You now want to show the marketing, sales and operations teams just how successful this campaign was.


#Loading libraries ----

library(tidyverse)
library(lubridate)
library(stringr)
library(broom)
library(data.table)


# 1.0 Data & Experiment Setup ----

calendar_data <- read.csv('./Data/calendar_data.csv')
client_data <- read.csv('./Data/client_data.csv')
deposit_data <- read_csv('./Data/deposit_data.csv')

client_data <- client_data %>% mutate(client_id = str_extract(
    client_data$client_id, "\\d+")) #Convert client_id in the same format as deposit_dt table
                                                                                        
calendar_data <- calendar_data %>% mutate(
    gregorian_date = as.Date(gregorian_date)) #Making sure gregorian_date column in Date format

deposit_data <- deposit_data %>% left_join(
    calendar_data,by = join_by(deposit_date == gregorian_date)) #Joining two tables

deposit_bw_to_m <- deposit_data %>% filter(deposit_cadence == "Biweekly") %>% select(-deposit_date) %>% unique() %>% 
    mutate(deposit_cadence = str_replace_all(deposit_cadence, "Biweekly", "Monthly")) %>%
    mutate(deposit_amount = deposit_amount * 2) #Converting biweekly data into monthly

deposit_df_clean <- deposit_data %>% mutate(
    deposit_cadence = str_replace_all(deposit_cadence, "Extra", "Monthly")) %>% 
    select(-deposit_date) %>% filter(deposit_cadence == "Monthly") %>% union_all(deposit_bw_to_m) #Converting "extra" to monthly label
                                                                                                  #for ease of use


# 2.0 Data Exploration ----

deposit_by_month <- deposit_df_clean %>% 
    group_by(month_name, deposit_type) %>% 
    summarise(amount = sum(deposit_amount)) %>% ungroup() %>% 
    mutate(amount = amount/1000000) #Exploring the difference between scheduled deposits and actual over 5 months

deposit_by_month %>% ggplot(
    aes(x = month_name, y = amount, colour = deposit_type, group = deposit_type)) + 
    geom_line() + labs(y = 'Millions of $', x = 'Month #', title = 'Aggregate deposits: Actual vs. Scheduled') 

# 2.1 Client Analysis ----

actual_deposit_count <- deposit_df_clean %>% filter(
    deposit_type == 'Actual Deposit') %>% group_by(month_name) %>% 
    summarise(actual_deposit_count = n_distinct(client_id)) #Calculating a number of distinct clients by month who made actual deposits

scheduled_deposit_count <- deposit_df_clean %>% filter(
    deposit_type == 'Scheduled Deposit') %>% group_by(month_name) %>% 
    summarise(scheduled_deposit_count = n_distinct(client_id)) #Calculating a number of distinct clients by month who had scheduled deposits

combined_deposit_count <- merge(actual_deposit_count,scheduled_deposit_count, by.y = "month_name") %>% 
    mutate(difference = actual_deposit_count-scheduled_deposit_count) #Combining the two

combined_deposit_count %>% ggplot(aes(x = month_name)) + 
    geom_bar(aes(y = actual_deposit_count, fill = "Actual"),stat = "identity", just = 1, width = 0.2, alpha = 0.6) +
    geom_bar(aes(y = scheduled_deposit_count, fill = "Scheduled"),stat = "identity", just = 0.1, width = 0.2, alpha = 0.6) +
    labs(title = "Actual vs Scheduled Deposit Count", x = "Month", y = "Deposit Count") +
    theme_minimal() #Visualizing the results
    

# 3.0 Creating tidy table to prepare for A/B test ----

deposit_df_tidy <- deposit_df_clean %>% filter(deposit_type == "Actual Deposit") %>% 
    pivot_wider(values_from = deposit_amount, names_from = month_name) %>% select(-deposit_cadence, -deposit_type)

deposit_df_tidy <- deposit_df_tidy %>% mutate_if(is.list, function(x,y) sapply(x, function(y) sum(y)))

deposit_df_tidy <- deposit_df_tidy %>% rename(
    month_1 = 'Month 1', month_2 = 'Month 2', month_3 = 'Month 3', month_4 = 'Month 4', month_5 = 'Month 5')

deposit_df_tidy <- deposit_df_tidy %>% left_join(client_data, by = join_by(client_id))

# 3.1 A/B Test ----

#Purpose of this test is to see whether a customer group that was attracted using advertising is bringing more revenue compared
# to the other group

deposit_df_tidy <- deposit_df_tidy %>% mutate(
  assignment = case_when(month_3 > 0 & month_1 == 0 & month_2 == 0 ~ "treatment", .default = "control"))

deposit_df_tidy %>% group_by(assignment) %>% summarise(count = n()) #uneven data split

deposit_df_tidy <- deposit_df_tidy %>% mutate(
  fees_earned = 0.2 * (month_1 + month_2 + month_3 + month_4 + month_5)) # Assumed take rate of 20%

count_function <- function(x) {
  ifelse(x > 0, 1, 0)
}

n_months <- apply(deposit_df_tidy %>% select(month_5:month_3) %>% apply(2, count_function), 1, sum) %>% as.tibble()

deposit_df_tidy <- deposit_df_tidy %>% mutate(
  n_months = n_months$value) #Number of months a given client is making deposits

deposit_df_tidy <- deposit_df_tidy %>% mutate(
  avg_fees = fees_earned / n_months) #dividing total fees by number of months to see how much a given client brought on avg.


t_tests <- replicate(100, {
  deposit_control_sample <- deposit_df_tidy %>% filter(assignment == "control")
  deposit_treatment_sample <- deposit_df_tidy %>% filter(assignment == "treatment")
  sample_ind_control <- sample(1:nrow(deposit_control_sample), nrow(deposit_treatment_sample), replace = TRUE)
  sample_ind_treatment <- sample(1:nrow(deposit_treatment_sample), nrow(deposit_treatment_sample), replace = TRUE)
  deposit_control_sample <- deposit_control_sample[sample_ind_control,]
  deposit_treatment_sample <- deposit_treatment_sample[sample_ind_treatment,]
  sample <- union_all(deposit_control_sample, deposit_treatment_sample)
  result <- lm(data = sample, avg_fees ~ assignment)
  tidy(result,conf.int = TRUE)
}) #Since we have uneven data split between treatment group and no treatment group, I bootstrapping to compare two groups running the test 100 times to make sure I have robust results.

conf_intervals <- as.data.frame(t(t_tests[6:7,]))
conf_intervals <- lapply(conf_intervals, function(x) sapply(x, "[", 2)) %>% as.tibble() #Getting confidence intervals from conducted t tests

conf_intervals$interval <- seq_len(nrow(conf_intervals))
average_diff <- mean(c(mean(conf_intervals$conf.low), mean(conf_intervals$conf.high))) #Averaging confidence intervals

p_values <- as.data.frame((t(t_tests[5,])))
p_values <- sapply(p_values, function(x) sapply(x, "[", 2)) %>% as.tibble() #Extracting p values to see if results were significant


# Plot

ggplot(p_values, aes(x = value)) + geom_density() 

ggplot(conf_intervals, aes()) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = interval)) +
  geom_vline(xintercept = average_diff)+
  annotate("text", x = average_diff, y = Inf, label = "Average difference in two groups", vjust = 0.9, hjust = -0.1, size = 3) +
  labs(title = "Confidence Intervals", x = "Range", y = "Intervals #") +
  theme_minimal() #This graph shows average $ difference between two groups across 100 simulations. On average marketing campaign
                  #brought clients who bring 2.5$ worth of monthly revenue more than no treatment group.


#total_fees_sim <- replicate(100, {
#  deposit_control_sample <- deposit_df_tidy %>% filter(assignment == "control")
#  deposit_treatment_sample <- deposit_df_tidy %>% filter(assignment == "treatment")
# sample_ind_control <- sample(1:nrow(deposit_control_sample), nrow(deposit_treatment_sample), replace = TRUE)
#  sample_ind_treatment <- sample(1:nrow(deposit_treatment_sample), nrow(deposit_treatment_sample), replace = TRUE)
#  deposit_control_sample <- deposit_control_sample[sample_ind_control,]
#  deposit_treatment_sample <- deposit_treatment_sample[sample_ind_treatment,]
#  sample <- union_all(deposit_control_sample, deposit_treatment_sample)
#  sample <- sample %>% group_by(assignment) %>% summarise(fees = sum(fees_earned))
#})
#total_fees_sim <- t(total_fees_sim)
#estimated_fees <- sapply(total_fees_sim[,2], function(x) sapply(x, "[", 1)) %>% as.data.frame() %>% unname() %>% t() %>% as.tibble()
#colnames(estimated_fees) <- c("control","treatment")






