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

#  Function to clean world bank datasets
clean_wb <- function(dataset, varname){
  data_s <- fread(file.path(indir, dataset))[,-c("Series Name", "Series Code", "Country Name")]
  data_s <- data_s %>% 
    filter(`Country Code` != "") # filter out blank rows before reshaping
  data_long <- melt(setDT(data_s), id.vars = c("Country Code"), variable.name = "year", value.name = varname)
  data_long[, year := as.numeric(substr(year, 1, 4))] # clean year names
  colnames(data_long)[1] <- "iso3" # adjust column name for merging
  return(data_long)
}

## Read in all data files with columns of interest
## From WHO
mdr_burden <- fread(file.path(indir, "WHO_MDR_RR_TB_burden_estimates_2020-07-23.csv"))[,c('iso3','e_rr_pct_new', 'year')]

tb_burden <- fread(file.path(indir, "WHO_TB_burden_countries_2020-07-23.csv"))[,c('iso3','e_inc_num', 'year')]

tb_notifications <- fread(file.path(indir, "TB_notifications_2020-11-21.csv"))[,c('iso3','conf_rrmdr', 'year')]

tb_lab_expenditure <- fread(file.path(indir, "TB_expenditure_utilisation_2020-10-15.csv"))[,c('iso3','exp_lab', 'year')]

## From World Bank
pop_dens_long <- clean_wb("wb_popdens.csv", "population_density")[year != 1960] # no data for 1960
wealth_long <- clean_wb("world_bank_Total_wealth_Data.csv", "total_wealth_per_capita")[year == 2014] # Only interested in 2014
wealth_long <- wealth_long[,-c("year")]
health_funding_data_long <- clean_wb("world_bank_health_spending_per_capita.csv", "Current_health_expenditure_per_capita")
private_funding_data_long <- clean_wb("private_percent.csv", "domestic_private_funding_share")

## From IHME
ihme_tb_funding <- fread(file.path(indir, "ihme_tb.csv"))[,-c("location_id", "location_name")]

# merge all data together
tb_funding_data <- Reduce(function(x, y) merge(x, y, by = c("iso3", "year"), all=TRUE), list(tb_notifications, mdr_burden, tb_burden, tb_lab_expenditure, pop_dens_long,health_funding_data_long,  private_funding_data_long,ihme_tb_funding  ))
tb_funding_data <- merge(tb_funding_data, wealth_long, by = "iso3", all = TRUE)

# filter to just be level 3 locations
locs <- fread(file.path(indir, 'locs.csv'))[,c("location_name", "ihme_loc_id", "location_id")]
length(unique(tb_funding_data$iso3))
setdiff(locs$ihme_loc_id, tb_funding_data$iso3) # missing taiwan 
tb_funding_data <- merge(tb_funding_data,locs, by.x= "iso3", by.y = "ihme_loc_id")

# add on population estimates for calculating lab expenditure per capita and confirmed mdr per capita 
pop <- fread(file.path(indir, 'pop.csv'))
colnames(pop)[4] <- "year"
#pop_locs <- merge(pop, locs[, c("location_id", "ihme_loc_id")], by = "location_id")
tb_funding_data <- merge(tb_funding_data, pop, by.x = c("year", "location_id"), by.y = c( "year", "location_id") )

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
median_wealth <- median(tb_funding_data$total_wealth_per_capita, na.rm=T)
tb_funding_data[, compare_median := as.factor(ifelse(total_wealth_per_capita >= median_wealth, "Above median total wealth per capita", "Below median total wealth per capita"))]
# save prepared dataset
write.csv(tb_funding_data, file.path(outdir, 'prepped_data.csv'), row.names = FALSE)
