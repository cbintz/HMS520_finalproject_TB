## Author: Corinne Bintz
## Date: 12/7/21
## Purpose: Create exploratory visualizations for TB funding analysis

## clear memory
rm(list=ls())

## Install packages and load libraries
pacman::p_load(tidyverse, data.table, ggplot2, gridExtra,scales)

## Define directories
indir <- 'Desktop/tb_data/output_data'
outdir <- 'Desktop/tb_data/visualizations'

## Read in prepped data file
tb_df <- fread(file.path(indir, "prepped_data.csv"))

## write function to save figure
save_figure <- function(figure, figurename) {
 
  ggsave(file.path(outdir, paste0(figurename, ".pdf")), width = 8, height = 8)
  
}

## Read in model results
results_mdr_exp_lab_time2 <- fread(file.path(indir, 'conf_lab_exp.csv'))
results_mdr_exp_lab_time_above <- fread(file.path(indir, 'conf_lab_exp_above.csv'))
results_mdr_exp_lab_time_below <- fread(file.path(indir, 'conf_lab_exp_below.csv'))

median_wealth <- median(tb_df$total_wealth_per_capita)

# Let's graph estimated % of new TB cases with rifampicin resistant TB as a function of total wealth per capita
conf_rrmdr_wealth <- ggplot(data = tb_df, aes(x = total_wealth_per_capita, y = e_rr_pct_new))+ 
  geom_point(stat = "identity", aes(color = compare_median))+
  geom_vline(xintercept = median_wealth, size=0.5) + 
  geom_smooth(method=lm, color = "black") + 
  xlab("Total wealth per capita") + 
  ylab("Estimated % of new TB cases with rifampicin resistant TB") +
  ggtitle("Number of confirmed rr tb cases by total wealth per capita") +
  theme_bw() +
  labs(color = "Relation to global median total wealth per capita")+
  theme(panel.grid = element_blank())
save_figure(conf_rrmdr_wealth, 'conf_rrmdr_wealth')

# Let's look closer at countries below the median income
below <- ggplot(data = tb_df[compare_median == "Below median total wealth per capita"], aes(x = total_wealth_per_capita, y = e_rr_pct_new))+ 
  geom_point(stat = "identity")  +
  geom_vline(xintercept = median_wealth, 
             color = "blue", size=0.5) + 
  geom_smooth(aes(x = total_wealth_per_capita, y = e_rr_pct_new), method=lm) + 
  xlab("Total wealth per capita") + 
  ylab("Estimated % of new TB cases with rifampicin resistant TB") +
  ggtitle("Countries below the global median total wealth per capita") +
  theme_bw() +
  scale_x_continuous(label = comma)+
  labs(color = "Relation to global median total wealth per capita")+
  theme(panel.grid = element_blank(), 
        text  = element_text(size = 10))
save_figure(below, 'below')

# Let's look closer at countries above the median income
above <- ggplot(data = tb_df[compare_median == "Above median total wealth per capita"], aes(x = total_wealth_per_capita, y = e_rr_pct_new))+ 
  geom_point(stat = "identity")  +
  geom_vline(xintercept = median_wealth, 
             color = "blue", size=0.5) + 
  geom_smooth(aes(x = total_wealth_per_capita, y = e_rr_pct_new), method=lm) + 
  xlab("Total wealth per capita") + 
  ylab("Estimated % of new TB cases with rifampicin resistant TB") +
  scale_x_continuous(label = comma)+
  ggtitle("Countries above the global median total wealth per capita") +
  theme_bw() +
  labs(color = "Relation to global median total wealth per capita")+
  theme(panel.grid = element_blank(),
        text  = element_text(size = 10))
save_figure(above, 'above')

#Let's graph these together
above_below <- arrangeGrob(below, above, ncol=2)
ggsave(file.path(outdir, "above_below.png"), above_below, height = 5.2, width = 10, dpi = 600)

# Graph confirmed cases by laboratory expenditure 
conf_lab_exp  <- ggplot(data = tb_df, aes(x = exp_lab, y = conf_rrmdr, color=location_name)) +
  labs(x=" Actual expenditure on laboratory infrastructure, equipment and supplies (US Dollars)",y="Number of Laboratory-Confirmed RR-TB or MDR-TB cases")+
  geom_point(shape = 16, size=1.8) +
  geom_abline( data =results_mdr_exp_lab_time2, aes(intercept=`(Intercept)`, slope=exp_lab)) +
  theme(legend.position = "none") +
  ggtitle("Laboratory expenditure and cases of RR-TB or MDR-TB")+
  scale_x_continuous(label = comma)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")
save_figure(conf_lab_exp, 'conf_lab_exp')

# Graph confirmed cases by laboratory expenditure: above median 
conf_lab_exp_above  <- ggplot(data = tb_df[compare_median == "Above median total wealth per capita"], aes(x = exp_lab, y = conf_rrmdr, color=location_name)) +
  labs(x=" Actual expenditure on laboratory infrastructure, equipment and supplies \n (US Dollars)",y="Number of Laboratory-Confirmed RR-TB or MDR-TB cases")+
  geom_point(shape = 16, size=1.8) +
  geom_abline( data =results_mdr_exp_lab_time_above, aes(intercept=`(Intercept)`, slope=exp_lab)) +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma)+
  ggtitle("Cases of RR-TB or MDR-TB by laboratory expenditure,\n locations above median total wealth per capita")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none", 
        text  = element_text(size = 10))
save_figure(conf_lab_exp_above, 'conf_lab_exp_above')

# Graph confirmed cases by laboratory expenditure: below median 
conf_lab_exp_below  <- ggplot(data = tb_df[compare_median == "Below median total wealth per capita"], aes(x = exp_lab, y = conf_rrmdr, color=location_name)) +
  labs(x=" Actual expenditure on laboratory infrastructure, equipment and supplies \n (US Dollars)",y="Number of Laboratory-Confirmed RR-TB or MDR-TB cases")+
  geom_point(shape = 16, size=1.8) +
  geom_abline( data =results_mdr_exp_lab_time_below, aes(intercept=`(Intercept)`, slope=exp_lab)) +
  theme(legend.position = "none") +
  ggtitle("Cases of RR-TB or MDR-TB by laboratory expenditure,\n locations below median total wealth per capita")+
  theme_bw() + 
  scale_x_continuous(label = comma)+
  theme(panel.grid = element_blank(),
        legend.position = "none", 
        text  = element_text(size = 10))
save_figure(conf_lab_exp_below, 'conf_lab_exp_below')

#Let's graph these together
above_below_conf_rrmdr <- arrangeGrob(conf_lab_exp_below, conf_lab_exp_above, ncol=2)
ggsave(file.path(outdir, "above_below_conf_rrmdr.png"), above_below_conf_rrmdr, height = 5.2, width = 10, dpi = 600)




