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

## Model mdr per capita ~ lab expenditure per capita, grouped effects on time and country
mdr_exp_lab_cap <- lmer(conf_rrmdr_cap ~ exp_lab_cap + (time|location_name), data = tb_df)
summary(mdr_exp_lab_cap)

# Model changes in MDR predicted by changes in lab expenditure
mdr_exp_lab_time <- lmer(conf_rrmdr ~ exp_lab*time + (1|location_name), data=tb_df)
summary(mdr_exp_lab_time)

#Model changes in MDR predicted by changes in lab expenditure, without lab_exp*time interaction
mdr_exp_lab_time2 <- lmer(conf_rrmdr ~ exp_lab + (time|location_name), data=tb_df)
summary(mdr_exp_lab_time2)
broom.mixed::tidy(mdr_exp_lab_time2)
results_mdr_exp_lab_time2 <-  as.data.frame(t(fixef(mdr_exp_lab_time2)))
write.csv(results_mdr_exp_lab_time2, file.path(dir, 'conf_lab_exp.csv')) # save results for graphing

# Find median of countries with both exp lab and conf rr mdr data
tb_df_no_na <- tb_df %>% 
  filter(!is.na(conf_rrmdr) & !is.na(exp_lab) ) 

nrow(tb_df_no_na)
length(unique(tb_df_no_na$ihme_loc_id))
current_median <- median(tb_df_no_na$total_wealth_per_capita)

#Model changes in MDR predicted by changes in lab expenditure, without lab_exp*time interaction, countries above median wealth per capita
mdr_exp_lab_time_above <- lmer(conf_rrmdr ~ exp_lab + (time|location_name), data=tb_df_no_na[total_wealth_per_capita >current_median])
summary(mdr_exp_lab_time_above)
broom.mixed::tidy(mdr_exp_lab_time_above)
results_mdr_exp_lab_time_above <-  as.data.frame(t(fixef(mdr_exp_lab_time_above)))
write.csv(results_mdr_exp_lab_time_above, file.path(dir, 'conf_lab_exp_above.csv')) # save results for graphing

#Model changes in MDR predicted by changes in lab expenditure, without lab_exp*time interaction, countries below median wealth per capita
mdr_exp_lab_time_below <- lmer(conf_rrmdr ~ exp_lab + (time|location_name), data=tb_df_no_na[total_wealth_per_capita <current_median])
summary(mdr_exp_lab_time_below)
broom.mixed::tidy(mdr_exp_lab_time_below)
results_mdr_exp_lab_time_below <-  as.data.frame(t(fixef(mdr_exp_lab_time_below)))
write.csv(results_mdr_exp_lab_time_below, file.path(dir, 'conf_lab_exp_below.csv')) # save results for graphing

