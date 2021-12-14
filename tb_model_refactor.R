## Author: Corinne Bintz
## Date: 12/7/21
## Purpose: Run linear regression models using TB funding data

## clear memory
rm(list=ls())

## Install packages and load libraries
pacman::p_load(tidyverse, data.table, ggplot2, lmerTest, segmented, multcomp, broom, broom.mixed)

## Define directories
dir <- 'Desktop/tb_data/output_data'

## Read in prepped data file
tb_df <- fread(file.path(dir, "prepped_data.csv"))

## Function to save results
save_model_results <- function(model, modelname) {
  tidy_table <- broom.mixed::tidy(model)
  tidy_df <-  as.data.frame(t(fixef(model)))
  write.csv(tidy_df, file.path(dir, paste0(modelname, 'csv')))
}

# Find median of countries with both exp lab and conf rr mdr data
tb_df_no_na <- tb_df %>% 
  filter(!is.na(conf_rrmdr) & !is.na(exp_lab) )  %>% 
  filter(!is.na(total_wealth_per_capita))
length(unique(tb_df_no_na))
current_median <- median(tb_df_no_na$total_wealth_per_capita, na.rm = T)

## Model mdr per capita ~ lab expenditure per capita, grouped effects on time and country
mdr_exp_lab_cap <- lmer(conf_rrmdr_cap ~ exp_lab_cap + (time|location_name), data = tb_df)
save_model_results(mdr_exp_lab_cap, 'mdr_exp_lab_cap')

# Model changes in MDR predicted by changes in lab expenditure
mdr_exp_lab_time <- lmer(conf_rrmdr ~ exp_lab*time + (1|location_name), data=tb_df_no_na)
save_model_results(mdr_exp_lab_time, 'mdr_exp_lab_time')

#Model changes in MDR predicted by changes in lab expenditure, without lab_exp*time interaction
mdr_exp_lab_time2 <- lmer(conf_rrmdr ~ exp_lab + (time|location_name), data=tb_df_no_na)
save_model_results(mdr_exp_lab_time2, 'mdr_exp_lab_time2')

#Model changes in MDR predicted by changes in lab expenditure, without lab_exp*time interaction, countries above median wealth per capita
mdr_exp_lab_time_above <- lmer(conf_rrmdr ~ exp_lab + (time|location_name), data=tb_df_no_na[total_wealth_per_capita >current_median])
save_model_results(mdr_exp_lab_time_above, 'mdr_exp_lab_time_above')

#Model changes in MDR predicted by changes in lab expenditure, without lab_exp*time interaction, countries below median wealth per capita
mdr_exp_lab_time_below <- lmer(conf_rrmdr ~ exp_lab + (time|location_name), data=tb_df_no_na[total_wealth_per_capita <current_median])
save_model_results(mdr_exp_lab_time_below, 'mdr_exp_lab_time_below')

