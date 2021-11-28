# Author: Corinne Bintz
library(lmerTest)
library(tidyverse)
library(dplyr)
library(segmented)
library(multcomp)
library(data.table)
lab_mdr_pop_no_na_cap <- fread("mdr_labexp.csv")
lab_mdr_pop_no_na <- fread("mdr_labexp_before_cap.csv")
e_inc_rr_num_18_19_pop_cap <- fread("e_inc_rr_num_18_19_pop_cap.csv")
e_inc_rr_num_18_pop_cap <- e_inc_rr_num_18_19_pop_cap[year == 2018]

# with WHO pop: using 2018 population
who_pop_csv <- merge(e_inc_rr_num_18_pop_cap[, c('e_pop_num', 'country')], lab_mdr_pop_no_na_cap[, -c("mdr_cap", "exp_lab_cap", "pop")], by = c("country"))
who_pop_csv_cap <- who_pop_csv %>%
  mutate(mdr_cap = as.numeric(conf_rrmdr) / as.numeric(e_pop_num)) %>%
  mutate(exp_lab_cap = as.numeric(exp_lab) / as.numeric(e_pop_num))

#without lab_exp*time interaction using 2018 WHO population estimates
revised_who_pop <- lmer(mdr_cap ~ exp_lab_cap + (time|country), data=who_pop_csv_cap)
summary(revised_who_pop)

#Number of countries
length(unique(lab_mdr_pop_no_na_cap$country))

#Years
unique(lab_mdr_pop_no_na_cap$year)

#Changes in MDR predicted by changes in lab expenditure
revised <- lmer(conf_rrmdr ~ exp_lab*time + (1|country), data=lab_mdr_pop_no_na_cap)
summary(revised)

#without lab_exp*time interaction
revised2 <- lmer(conf_rrmdr ~ exp_lab + (time|country), data=lab_mdr_pop_no_na_cap)
summary(revised2)

lm.1 <- lm(conf_rrmdr ~ exp_lab, data = lab_mdr_pop_no_na_cap)
summary(lm.1)

#segmented did not work with lmer
#Step-2: run segmented model with two potential breakpoints (npsi)
seg.1 <- segmented(lm.1, npsi=1, control = seg.control(n.boot = 100))
seg.2 <- segmented(lm.1, npsi=2, control = seg.control(n.boot = 100))

#the first psi is significant
pscore.test(seg.1, seg.Z = ~exp_lab, more.break = FALSE)
#the second psi is significant
pscore.test(seg.1, seg.Z = ~exp_lab, more.break = TRUE)

#the third is significant 
pscore.test(seg.2, seg.Z = ~exp_lab, more.break = TRUE)
#Step-4: Calculate overall AAPC 2007 - 2019

#export table
library("tidyr")
x <- tidy(revised2)
write.csv(x,"~/Desktop/tb/table2.csv", row.names = FALSE)


e_inc_csv <- left_join(e_inc_rr_num_18_19_pop_cap, lab_mdr_pop_no_na_cap, by = c("country", "year"))
# estimated incidence per capita according to who population  
no_na_e_inc_csv <- na.omit(e_inc_csv)
revised3 <- lmer(e_inc_rr_cap ~ exp_lab*time + (1|country), data=no_na_e_inc_csv)
summary(revised3)

revised4 <- lmer(e_inc_rr_cap ~ exp_lab + (time|country), data=no_na_e_inc_csv)

tb_burden <- fread("~/Desktop/tb/TB_burden_countries_2020-12-15.csv")

#estimated total population number
who_pop <- tb_burden[,c("e_pop_num", "country", "year")] 
lab_mdr_who_pop_no_na_cap <- left_join(lab_mdr_pop_no_na_cap, who_pop, by = c("country", "year"))

lab_mdr_who_pop_no_na_cap <- lab_mdr_who_pop_no_na_cap %>%
  mutate(mdr_who_cap = conf_rrmdr / e_pop_num)  %>%
  mutate(exp_lab_who_cap = exp_lab / e_pop_num)

revised_whopop <- lmer(mdr_who_cap ~ exp_lab_who_cap + (time|country), data=lab_mdr_who_pop_no_na_cap)
summary(revised_whopop)

#world bank wealth
wealth <-fread('~/Desktop/tb/world_bank_Total_wealth_Data.csv')
wealth_2014 <- wealth[,c('Country Name','2014 [YR2014]')]
colnames(wealth_2014)[1] <- "country"
colnames(wealth_2014)[2] <- "total_wealth_per_capita_2014"

lab_mdr_who_pop_no_na_cap_wealth <- na.omit(left_join(lab_mdr_who_pop_no_na_cap, wealth_2014, by='country'))

#median wealth per capita
med <- median(lab_mdr_who_pop_no_na_cap_wealth$total_wealth_per_capita_2014)

# countries below median wealth (105)
countries_below <- lab_mdr_who_pop_no_na_cap_wealth %>% #countries below the median 
  filter(total_wealth_per_capita_2014 <= med)

countries <- countries_below$country
length(countries)

revised_below <- lmer(conf_rrmdr ~ exp_lab + (time|country), data=countries_below)
summary(revised_below) #issinglar error?

# countries above median wealth (101)
countries_above <- lab_mdr_who_pop_no_na_cap_wealth %>% #countries above the median 
  filter(total_wealth_per_capita_2014 > med)

countries <- countries_above$country
length(countries)

revised_above <- lmer(conf_rrmdr ~ exp_lab + (time|country), data=countries_above)
summary(revised_above) 

#export tables for above and below median wealth 
library("tidyr")
below <- tidy(revised_below)
above <- tidy(revised_above)
write.csv(below,"~/Desktop/tb/tablebelow.csv", row.names = FALSE)
write.csv(above,"~/Desktop/tb/tableabove.csv", row.names = FALSE)

#max wealth
max_wealth <- max(lab_mdr_who_pop_no_na_cap_wealth$total_wealth_per_capita_2014)
third <- max_wealth/3

# bottom third of wealth

countries_bottom_third <- lab_mdr_who_pop_no_na_cap_wealth %>% #countries above the median 
  filter(total_wealth_per_capita_2014 < third)

revised_bottom_third <- lmer(conf_rrmdr ~ exp_lab + (time|country), data=countries_bottom_third)
summary(revised_bottom_third) #issingular error

# middle third of wealth 
countries_middle_third <- lab_mdr_who_pop_no_na_cap_wealth %>% #countries above the median 
  filter(total_wealth_per_capita_2014 < third*2 & total_wealth_per_capita_2014 > third)

revised_middle_third <- lmer(conf_rrmdr ~ exp_lab + (time|country), data=countries_middle_third)
summary(revised_middle_third) #issingular error

# upper third of wealth 
countries_upper_third <- lab_mdr_who_pop_no_na_cap_wealth %>% #countries above the median 
  filter(total_wealth_per_capita_2014 >= third*2)

revised_upper_third <- lmer(conf_rrmdr ~ exp_lab + (time|country), data=countries_upper_third)
summary(revised_upper_third) #issingular error

total_wealth_reg <- lmer(conf_rrmdr ~ total_wealth_per_capita_2014 + (time|country), data=lab_mdr_who_pop_no_na_cap_wealth)
summary(total_wealth_reg)
#export table
z <- tidy(total_wealth_reg)
write.csv(z,"~/Desktop/tb/tablewealth.csv", row.names = FALSE)

total_wealth_reg_below <- lmer(conf_rrmdr ~ total_wealth_per_capita_2014 + (time|country), data=countries_below)
summary(total_wealth_reg_below)

total_wealth_reg_above <- lmer(conf_rrmdr ~ total_wealth_per_capita_2014 + (time|country), data=countries_above)
summary(total_wealth_reg_above)


library(ggplot2)
#Graph lab expenditure vs. confirmed cases
ggplot(lab_mdr_pop_no_na_cap, aes(x = exp_lab, y = conf_rrmdr, colour=country)) +
  labs(x=" Actual expenditure on laboratory infrastructure, equipment and supplies (US Dollars)
",y="Number of Laboratory-Confirmed RR-TB or MDR-TB cases")+
  geom_point(shape = 16, size=1.8) +w
  geom_abline(aes(intercept=`(Intercept)`, slope=exp_lab), as.data.frame(t(fixef(revised2)))) +
  theme(legend.position = "none") +
  ggtitle("Laboratory expenditure and cases of RR-TB or MDR-TB")

  #check correlation between MDR and sales
  
#sales data 
cart_sales <-  fread("mtb_delivery_2016_2019.csv")
cart_sales <- cart_sales[,-c("V5")]
colnames(cart_sales) <- c("country", "2017", "2018", "2019")
cart_sales <- cart_sales[-1,]
countries_mdr <- unique(lab_mdr_pop_no_na_cap$country)

cart_sales$`2017`= as.numeric(gsub("[\\%,]", "", cart_sales$`2017`))
cart_sales$`2018`= as.numeric(gsub("[\\%,]", "", cart_sales$`2018`))
cart_sales$`2019`= as.numeric(gsub("[\\%,]", "", cart_sales$`2019`))
cart_sales_no_na <- na.omit(cart_sales)
cart_sales_long <- gather(cart_sales_no_na, year, pct_change, `2017`:`2019`, factor_key=TRUE)
class(cart_sales_long$year)
lab_mdr_pop_no_na_cap$year <- as.factor(lab_mdr_pop_no_na_cap$year)
mdr_cart_sales <- left_join(lab_mdr_pop_no_na_cap, cart_sales_long, by = c("country", "year"))
mdr_cart_sales <- na.omit(mdr_cart_sales)
cor.test(mdr_cart_sales$mdr_cap, mdr_cart_sales$pct_change, method= "pearson")

e_inc_rr_num_18_19_pop_cap$year <- as.numeric(e_inc_rr_num_18_19_pop_cap$year)
cart_sales_long$year <- as.numeric(cart_sales_long$year)
e_inc_sales <- left_join(e_inc_rr_num_18_19_pop_cap, cart_sales_long, by = c("country", "year"))
e_inc_sales <- na.omit(e_inc_sales)
e_inc_sales <- e_inc_sales %>%
  mutate(time = year-2018)
cor.test(e_inc_sales$e_inc_rr_cap, e_inc_sales$pct_change, method= "pearson")
cor.test(e_inc_sales$e_inc_rr_num, e_inc_sales$pct_change, method= "pearson")


#model with cart sales
#mdr cart sales aren't per capita, so maybe mdr_cap shouldn't be?

lmercart <-  lmer(conf_rrmdr ~ pct_change + (time|country), data=mdr_cart_sales)
summary(lmercart)

lmercart_einc_rr_num <-  lmer(e_inc_rr_num ~ pct_change + (1|country), data=e_inc_sales)
summary(lmercart_einc_rr_num)

