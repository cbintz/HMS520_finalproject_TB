## Author: Corinne Bintz
## Date: 12/7/21
## Purpose: Prepare data for TB funding analysis

## clear memory
rm(list=ls())

## Install packages and load libraries
pacman::p_load(tidyverse, data.table)

## Define directories
indir <- 'Desktop/tb_data/input_data'
outdir <- 'Desktop/tb_data/output_data'

## Read in all data files with columns of interest

## From WHO
mdr_burden <- fread(file.path(indir, "WHO_MDR_RR_TB_burden_estimates_2020-07-23.csv"))[,c('iso3','e_rr_pct_new', 'year')]

tb_burden <- fread(file.path(indir, "WHO_TB_burden_countries_2020-07-23.csv"))[,c('iso3','e_inc_num', 'year')]

tb_notifications <- fread(file.path(indir, "TB_notifications_2020-11-21.csv"))[,c('iso3','conf_rrmdr', 'year')]

tb_lab_expenditure <- fread(file.path(indir, "TB_expenditure_utilisation_2020-10-15.csv"))[,c('iso3','exp_lab', 'year')]

## From World Bank
pop_dens <- fread(file.path(indir, "wb_popdens.csv"))[,-c(1:3)]
pop_dens <- pop_dens %>% 
  filter(`Country Code` != "") # filter out blank rows before reshaping
# reshape wide to long
pop_dens <- pop_dens [,-c("1960 [YR1960]")] # no data for this year
pop_dens_long <- melt(setDT(pop_dens), id.vars = c("Country Code"), variable.name = "year", value.name = "population_density")
pop_dens_long[, year := as.numeric(substr(year, 1, 4))] # clean year names
colnames(pop_dens_long)[1] <- "iso3" # adjust column name for merging

wealth <- fread(file.path(indir, 'world_bank_Total_wealth_Data.csv'))[-c(142:163),] # remove non country level rows from data
wealth <- wealth[,-c(1,3:4)] # don't want these columns
wealth_long <- melt(setDT(wealth), id.vars = c("Country Code"), variable.name = "year", value.name = "total_wealth_per_capita")
wealth_long[, year := as.numeric(substr(year, 1, 4))] # clean year names
colnames(wealth_long)[1] <- "iso3" # adjust column name for merging
wealth_long <- wealth_long %>% 
  filter(year == 2014) # only year relevant for years of our case data
wealth_long <- wealth_long[,-c("year")] # we want to merge this value onto all years

health_funding_data <- fread(file.path(indir, "world_bank_health_spending_per_capita.csv"))[-c(218:269),] # remove non country level rows from data
health_funding_data <- health_funding_data[,-c(1:3)] # don't want these columns
health_funding_data_long <- melt(setDT(health_funding_data), id.vars = c("Country Code"), variable.name = "year", value.name = "Current_health_expenditure_per_capita")
health_funding_data_long[, year := as.numeric(substr(year, 1, 4))] # clean year names
colnames(health_funding_data_long)[1] <- "iso3" # adjust column name for merging

private_funding_data <- fread(file.path(indir, "private_percent.csv"))[-c(216:264),] # remove non country level rows from data
private_funding_data <- private_funding_data[,-c(1:3)] # don't want these columns
private_funding_data_long <- melt(setDT(private_funding_data), id.vars = c("Country Code"), variable.name = "year", value.name = "Domestic_private_funding_share")
private_funding_data_long[, year := as.numeric(substr(year, 1, 4))] # clean year names
colnames(private_funding_data_long)[1] <- "iso3" # adjust column name for merging

## From IHME
ihme_tb_funding <- fread(file.path(indir, "ihme_tb.csv"))

# merge all data together
tb_funding_data <- Reduce(function(x, y) merge(x, y, by = c("iso3", "year"), all=TRUE), list(mdr_burden, tb_burden, tb_notifications, tb_lab_expenditure, pop_dens_long,health_funding_data_long,  private_funding_data_long  ))
tb_funding_data <- merge(tb_funding_data, wealth_long, by = "iso3")

# filter to just be level 3 locations
locs <- fread(file.path(indir, 'locs.csv'))[,c("location_name", "ihme_loc_id", "location_id")]
setdiff(locs$ihme_loc_id, tb_funding_data$iso3) # missing taiwan
tb_funding_data <- merge(locs, tb_funding_data, by.x = "ihme_loc_id", by.y = "iso3")

# add on population estimates for calculating lab expenditure per capita and confirmed mdr per capita 
pop <- fread(file.path(indir, 'pop.csv'))
colnames(pop)[4] <- "year"
tb_funding_data <- merge(tb_funding_data, pop, by = c("location_id", "year"), )

# replace .. with NA
tb_funding_data[tb_funding_data == ".."] <- NA

## calculate per capita values
tb_funding_data[,exp_lab_cap := exp_lab/population]
tb_funding_data[,conf_rrmdr_cap := conf_rrmdr/population]

## clean up columns 
tb_funding_data <- tb_funding_data[,-c("V1","run_id", "sex_id", "location_id")]

# add time variable
tb_funding_data[, time := year - 2014]

# compute median wealth per capita
median_wealth <- median(tb_funding_data$total_wealth_per_capita)
tb_funding_data[, compare_median := as.factor(ifelse(total_wealth_per_capita >= median_wealth, "Above median total wealth per capita", "Below median total wealth per capita"))]

# save prepared dataset
write.csv(tb_funding_data, file.path(outdir, 'prepped_data.csv'), row.names = FALSE)
