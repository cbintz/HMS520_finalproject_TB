#Author: Corinne Bintz

#Install packages and load libraries
install.packages("tidyverse")
install.packages('data.table')
install.packages("janitor")
install.packages("lmerTest")
library(lmerTest)
library(tidyverse)
library(data.table)
library(janitor)

#load TB burden estimate csv's from the WHO
burden <- fread("WHO_MDR_RR_TB_burden_estimates_2020-07-23.csv")
tb_who <- fread("WHO_TB_burden_countries_2020-07-23.csv")
WHO_mdr <- burden[, c('country', 'e_rr_pct_new')]
mdr_sort<- WHO_mdr[order(-WHO_mdr$e_rr_pct_new),]
pop_dens<- fread("world_bank_pop_dens.csv")

pop_dens_cut <- pop_dens[-(1:4), , drop=FALSE]
colnames(pop_dens_cut)[1] <- "Country Name"
colnames(pop_dens_cut)[2] <- "Country Code"


pop_2018 <- pop_dens_cut[-(1), ,]
pop_sort<- pop_2018[order(-pop_2018$V63),]

just_2018_pop <- pop_sort[,c("Country Name", "V63")]
colnames(just_2018_pop)[2] <- "pop_dens"



tb_who_2014 <- tb_who %>% #let's just look at 2014
  filter(year==2014)

tb_inc <- tb_who_2014[, c('country', 'e_inc_num')] # just income

#load wealth data from world bank
wealth <-fread('world_bank_Total_wealth_Data.csv')

wealth_with_change <- wealth %>%
  mutate(diff = `2014 [YR2014]`-`2005 [YR2005]`) #add column for changes in wealth
wealth <- wealth$diff = wealth$`2014 [YR2014]`-wealth$`2005 [YR2005]`
wealth_2014 <- wealth[,c('Country Name','2014 [YR2014]')]
wealth_2010 <- wealth[,c('Country Name','2010 [YR2010]')]

wealth_mdr_14 <- left_join(wealth_2014, WHO_mdr,  by = c("Country Name" = "country")) #wealth and mdr data for 2014
wealth_mdr_10 <- left_join(wealth_2010, WHO_mdr,  by = c("Country Name" = "country")) #wealth and mdr data for 2010
colnames(wealth_mdr_14)[2] <- "total_wealth_per_capita" #let's rename our columns to make more sense
colnames(wealth_mdr_10)[2] <- "total_wealth_per_capita"

sortedwealth <- sort(wealth_mdr_14$total_wealth_per_capita)

wealth_tb_inc <- left_join(wealth_mdr_14, tb_inc,  by = c("Country Name" = "country"))

wealth_mdr_inc_popdens <- left_join(wealth_tb_inc, just_2018_pop,  by = c("Country Name"))

# model mdr as a function of total wealth per capita and total tb incidence
mdr_wealth_inc <- lm(e_rr_pct_new~pop_dens + total_wealth_per_capita + e_inc_num,
             data= wealth_mdr_inc_popdens)
summary(mdr_wealth_inc)

# model tb incidence as a function of total wealth per capita and mdr rates
tbinc_wealth_mdr <- lm(e_inc_num~total_wealth_per_capita + e_rr_pct_new,
                       data= wealth_tb_inc)
summary(tbinc_wealth_mdr)

colnames(wealth_tb_2014)[2] <- "total_wealth_per_capita"
colnames(wealth_tb_2014)[1] <- "Country"
sorteddf<- wealth_mdr_14[order(wealth_mdr_14$total_wealth_per_capita),]
complete_sorteddf <- na.omit(sorteddf)

# Let's graph mdr as a function of wealth per capita noting countries above and below the median wealth per capita
complete_sorteddf %>%
  ggplot(aes(x = total_wealth_per_capita, y = e_rr_pct_new))+ 
  geom_point(stat = "identity")+
  geom_vline(xintercept = med, 
             color = "blue", size=0.5) + 
  geom_smooth(data = countries_below, aes(total_wealth_per_capita, e_rr_pct_new, color = "green"), method=lm) + 
  geom_smooth(data = countries_above, aes(total_wealth_per_capita, e_rr_pct_new, color = "red"), method=lm)  + 
  xlab("Total wealth per capita") + 
  ylab("Estimated % of new TB cases with rifampicin resistant TB") 

# Let's look closer at countries below the median income
below <- countries_below %>%
  ggplot(aes(x = total_wealth_per_capita, y = e_rr_pct_new))+ 
  geom_point(stat = "identity")  +
  geom_vline(xintercept = med, 
             color = "blue", size=0.5) + 
  geom_smooth(data = countries_below, aes(total_wealth_per_capita, e_rr_pct_new, color = "red"), method=lm) + 
  xlab("Total wealth per capita") + 
  ylab("Estimated % of new TB cases with rifampicin resistant TB")

# Let's look closer at countries above the median income
above <- countries_above %>%
  ggplot(aes(x = total_wealth_per_capita, y = e_rr_pct_new))+ 
  geom_point(stat = "identity") +
  geom_vline(xintercept = med, 
             color = "blue", size=0.5) + 
  geom_smooth(data = countries_above, aes(total_wealth_per_capita, e_rr_pct_new, color = "green"), method=lm)  + 
  xlab("Total wealth per capita") + 
  ylab("Estimated % of new TB cases with rifampicin resistant TB") 

library(gridExtra)

#Let's graph these together
above_below <- grid.arrange(below, above, ncol=2)

bottom <-sorteddf[1:53,] # countries in the bottom third of wealth per capita 
complete_bottom <- na.omit(bottom)

middle <- sorteddf[54:106,] # countries in the middle third of wealth per capita 

top<- sorteddf[107:158,] # countries in the top third of wealth per capita 

# let's model mdr rates as a function of total wealth per capita for countries in the bottom third of wealth per capita 
mdr_bottom <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= bottom)
summary(mdr_bottom)

# let's model mdr rates as a function of total wealth per capita for countries in the middle third of wealth per capita 
mdr_middle <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= middle)
summary(mdr_middle)

# let's model mdr rates as a function of total wealth per capita for countries in the top third of wealth per capita 
mdr_top <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= top)
summary(mdr_top)


med <- median(wealth_mdr_2014$total_wealth_per_capita, na.rm = TRUE) # find the median total wealth per capita

countries_above <- wealth_mdr_2014 %>%
  filter(total_wealth_per_capita > med) #countries above the median 
view(countries_above) #79
countries_below <- wealth_mdr_2014 %>% #countries below the median 
  filter(total_wealth_per_capita <= med)
view(countries_below) #79

# let's model mdr rates as a function of total wealth per capita for countries above the median wealth per capita 
mdr_above_median <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= countries_above)
summary(mdr_above_median)

# let's model mdr rates as a function of total wealth per capita for countries below the median wealth per capita 
mdr_below_median <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= countries_below)
summary(mdr_below_median)

# mdr as a function of total wealth per capita for 2014
mdr_14 <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= wealth_mdr_14)
summary(mdr_14)

# mdr as a function of total wealth per capita for 2010
mdr_10 <- lm(e_rr_pct_new~total_wealth_per_capita,
             data= wealth_mdr_10)
summary(mdr_10)

# total tb incidence as a function of wealth per capita for 2014
inc_14 <- lm(e_inc_num~total_wealth_per_capita,
             data= wealth_tb_inc)
summary(inc_14)

#load most recent TB notifications
tb_notifications <- fread("TB_notifications_2020-11-21.csv")
just_confrrmdr <- tb_notifications[ , c("country", "year", "conf_rrmdr")]
just_confrrmdr_nona <- na.omit(just_confrrmdr)

# read in health funding data from the world bank
health_funding_data <- fread("world_bank_health_spending_per_capita.csv")

#convert from wide to long format
health_funding_clean <- health_funding_data[,-c(1:2, 4)]

health_funding_data_gathered <- health_funding_clean %>%
  gather(year, spending, `1960 [YR1960]` :`2019 [YR2019]`, factor_key=TRUE)

health_funding_data_gathered[health_funding_data_gathered == ".."] <- NA
health_funding_data_gathered[health_funding_data_gathered == ""] <- NA
colnames(health_funding_data_gathered)[1] <- "country"
health_funding_data_gathered$year <- str_replace_all(health_funding_data_gathered$year,
                                                     "(\\[)([:alpha:]{2})([:digit:]{4})(\\])$",
                                                     "")

health_funding_data_gathered$year <- as.numeric(health_funding_data_gathered$year)
health_funding_data_gathered$spending <- as.numeric(health_funding_data_gathered$spending)
health_funding_data_gathered_nona <- na.omit(health_funding_data_gathered)

private_funding_data <- fread("private_percent.csv")
private_funding_clean <- private_funding_data[,-c(1:2, 4)]

# wide to long format
private_funding_data_gathered <- private_funding_clean %>%
  gather(year, funding, `1960 [YR1960]` :`2019 [YR2019]`, factor_key=TRUE)

private_funding_data_gathered[private_funding_data_gathered == ".."] <- NA
private_funding_data_gathered[private_funding_data_gathered == ""] <- NA
colnames(private_funding_data_gathered)[1] <- "country"
private_funding_data_gathered$year <- str_replace_all(private_funding_data_gathered$year,
                                                     "(\\[)([:alpha:]{2})([:digit:]{4})(\\])$",
                                                     "")
private_funding_data_gathered$year <- as.numeric(private_funding_data_gathered$year)
private_funding_data_gathered$funding <- as.numeric(private_funding_data_gathered$funding)

conf_rrmdr_health_spending_per_capita <- left_join(health_funding_data_gathered, just_confrrmdr,  by = c("country" = "country", "year" = "year" ))

conf_rrmdr_health_spending_per_capita_nona <- na.omit(conf_rrmdr_health_spending_per_capita)


conf_rrmdr_private_funding_percent <- left_join(private_funding_data_gathered, conf_rrmdr_health_spending_per_capita,  by = c("country" = "country", "year" = "year" ))
conf_rrmdr_private_funding_percent_nona <- na.omit(conf_rrmdr_private_funding_percent)

conf_rrmdr_public_private <- left_join(conf_rrmdr_private_funding_percent, conf_rrmdr_health_spending_per_capita,  by = c("country" = "country", "year" = "year", "conf_rrmdr" = "conf_rrmdr", "spending" = "spending"))
conf_rrmdr_public_private_nona <- na.omit(conf_rrmdr_public_private)

conf_rrmdr_health_spending_per_capita_nona <- conf_rrmdr_health_spending_per_capita_nona %>%
  mutate(time = year-2014)

conf_rrmdr_private_funding_percent_nona <- conf_rrmdr_private_funding_percent_nona %>%
  mutate(time = year-2014)


#conditional two level growth model 
#public funding
conditional <- lmer(conf_rrmdr~ spending + (time | country), data=conf_rrmdr_health_spending_per_capita_nona)
summary(conditional)

conf_rrmdr_public_private_nona <- conf_rrmdr_public_private_nona  %>%
  mutate(time = year-2014)

#public and private funding
conditional1 <- lmer(conf_rrmdr~ spending * funding + (time | country), data=conf_rrmdr_public_private_nona)
summary(conditional1)

#private health funding as a percent of total health funding
conditional2 <- lmer(conf_rrmdr~ funding + (time | country), data=conf_rrmdr_private_funding_percent_nona)
summary(conditional2)

#load in IHME health financing data
ihme_tb_funding <- fread("ihme_tb.csv")

conf_rrmdr_ihme <- left_join(just_confrrmdr_nona, ihme_tb_funding,  by = c("country" = "location_name", "year" = "year"))
conf_rrmdr_ihme  <- conf_rrmdr_ihme   %>%
  mutate(time = year-2014)

#Multivariate linear mixed model

multi1 <- lmer(conf_rrmdr~ the_total_mean + ghes_non_notified_per_the_mean + (time | country), data=conf_rrmdr_ihme)
summary(multi1)

#neg
#total tb spending
conditional3 <- lmer(conf_rrmdr~ the_total_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional3)

#neg
#Fraction of Total TB Spending on Non-Notified TB Cases from Government Sources
conditional4 <- lmer(conf_rrmdr~ ghes_non_notified_per_the_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional4)

#pos and not significant
#Fraction of Total TB Spending on Notified TB Cases from Government Sources
conditional30 <- lmer(conf_rrmdr~ ghes_notified_per_the_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional30)

#neg
#Government TB Spending on Notified TB Cases (thousands of 2019 USD)
conditional5 <- lmer(conf_rrmdr~ ghes_notified_total_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional5)

#neg
#Government TB Spending on Non-Notified TB Cases (thousands of 2019 USD)
conditional6 <- lmer(conf_rrmdr~ ghes_non_notified_total_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional6)

#pos
#Out-of-Pocket TB Spending (thousands of 2019 USD)
conditional7 <- lmer(conf_rrmdr~ oop_total_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional7)

#pos
#Prepaid Private TB Spending (thousands of 2019 USD)
conditional8 <- lmer(conf_rrmdr~ ppp_total_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional8)

#pos
#Development Assistance for Health for TB (thousands of 2019 USD)
conditional9 <- lmer(conf_rrmdr~ dah_total + (time | country), data=conf_rrmdr_ihme)
summary(conditional9)


#neg
#Fraction of Total TB Spending from Out-of-Pocket Sources
conditional11 <- lmer(conf_rrmdr~ oop_per_the_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional11)

#neg
#Fraction of Total TB Spending from Prepaid Private Sources
conditional12 <- lmer(conf_rrmdr~ ppp_per_the_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional12)

#neg
#Fraction of Total TB Spending from Development Assistance for Health
conditional13 <- lmer(conf_rrmdr~ dah_per_the + (time | country), data=conf_rrmdr_ihme)
summary(conditional13)


#neg
#Total TB Spending per Incident Case (2019 USD)
conditional15 <- lmer(conf_rrmdr~ the_per_case_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional15)

#neg
#Government TB Spending on Notified TB Cases per Incident Case (2019 USD)
conditional16 <- lmer(conf_rrmdr~ ghes_notified_per_case_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional16)

#neg
#Government TB Spending on Non-Notified TB Cases per Incident Case (2019 USD)
conditional17 <- lmer(conf_rrmdr~ ghes_non_notified_per_case_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional17)

#neg
#Out-of-Pocket TB Spending per Incident Case (2019 USD)
conditional19 <- lmer(conf_rrmdr~ oop_per_case_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional19)

#neg
#Prepaid Private TB Spending per Incident Case (2019 USD)
conditional20 <- lmer(conf_rrmdr~ ppp_per_case_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional20)

#neg
#Development Assistance for Health for TB Spending per Incident Case (2019 USD)
conditional21 <- lmer(conf_rrmdr~ dah_per_case + (time | country), data=conf_rrmdr_ihme)
summary(conditional21)

#pos
#Total TB Spending per Person (2019 USD)
conditional22 <- lmer(conf_rrmdr~ the_per_cap_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional22)

#pos
#Government TB Spending on Notified TB Cases per Person (2019 USD)
conditional23 <- lmer(conf_rrmdr~ ghes_notified_per_cap_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional23)

#neg
#Government TB Spending on Non-Notified TB Cases per Person (2019 USD)
conditional24 <- lmer(conf_rrmdr~ ghes_non_notified_per_cap_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional24)

#neg
#Out-of-Pocket TB Spending per Person (2019 USD)
conditional25 <- lmer(conf_rrmdr~ oop_per_cap_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional25)

#neg
#Prepaid Private TB Spending per Person (2019 USD)
conditional26 <- lmer(conf_rrmdr~ ppp_per_cap_mean + (time | country), data=conf_rrmdr_ihme)
summary(conditional26)

#neg
#Development Assistance for Health for TB Spending per Person (2019 USD)
conditional27 <- lmer(conf_rrmdr~ dah_per_cap + (time | country), data=conf_rrmdr_ihme)
summary(conditional27)

# Let's look more closely at India
#with resistance data
india <- conf_rrmdr_ihme %>%
  filter(country == "India")

#without resistance data
india_norr <- ihme_tb_funding %>%
  filter(location_name == "India")

india  <- india   %>%
  mutate(time = year-2014)

notified_spending <- india_norr$ghes_notified_total_mean 
notified_spending[18] - notified_spending[1] #increased

nonnotified_spending <- india_norr$ghes_non_notified_total_mean 
nonnotified_spending[18] - nonnotified_spending[1] #increased

conditional28 <- lm(conf_rrmdr~ dah_per_cap, data=india)
summary(conditional28)

conditional29 <- lm(conf_rrmdr~ ghes_notified_total_mean , data=india)
summary(conditional29)

conditional30 <- lm(conf_rrmdr~ ghes_non_notified_total_mean , data=india)
summary(conditional30)

conditional31 <- lm(conf_rrmdr~ the_total_mean , data=india)
summary(conditional31)

conditional32 <- lm(conf_rrmdr~ oop_total_mean , data=india)
summary(conditional32)

conditional33 <- lm(conf_rrmdr~ ppp_per_cap_mean , data=india)
summary(conditional33)


# Looking at completeness of data on the number of sites providing various diagnostics
laboratories <- fread("TB_laboratories_2020-10-14.csv")
lab_brazil <- laboratories %>%
  filter(country == "Brazil")

lab_china <- laboratories %>%
  filter(country == "China")

lab_US <- laboratories %>%
  filter(country == "United States")

lab_Russia <- laboratories %>%
  filter(country == "Russian Federation")

lab_Ukraine <- laboratories %>%
  filter(country == "Ukraine")

lab_India <- laboratories %>%
  filter(country == "India")

lab_SouthAfrica <- laboratories %>%
  filter(country == "South Africa")

colnames(lab_SouthAfrica)

wealth_lab <- left_join(wealth, laboratories, by = c("Country Name" = "country"))

#Investigating relationship between wealth and testing

#Scatter plot with wealth on x-axis and # smear lab on y axis
wealth_lab  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_sm))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs providing TB diagnostic services using smear microscopy") 

wealth_lab_mdr <- left_join(wealth_lab, just_confrrmdr_nona, by = c("Country Name" = "country", "year" = "year"))

#Scatter plot with # lab on x-axis and mdr on y axis
wealth_lab_mdr  %>%
  ggplot(aes(x = lab_sm, y = conf_rrmdr ))+ 
  geom_point(stat = "identity")+
  xlab("# of labs providing TB diagnostic services using smear microscopy") + 
  ylab("MDR") 

#Scatter plot with wealth on x-axis and # dst lab on y axis
wealth_lab  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_dst))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs providing TB diagnostic services using drug susceptibility testing") 

#Scatter plot with wealth on x-axis and # culture lab on y axis
wealth_lab  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_cul))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs providing TB diagnostic services using culture") 

mdr_lab <- lm(conf_rrmdr~ lab_cul * lab_dst *lab_sm, data=wealth_lab_mdr)
summary(mdr_lab)

mdr_wealth <- lm(conf_rrmdr~ `2014 [YR2014]`, data=wealth_lab_mdr)
summary(mdr_wealth)


expenditure <- fread("TB_expenditure_utilisation_2020-10-15.csv")
wealth_lab_mdr_exp <- left_join(wealth_lab_mdr, expenditure, by = c("Country Name" = "country", "year" = "year", "iso2" = "iso2", "iso3" = "iso3", "iso_numeric" = "iso_numeric", "g_whoregion" = "g_whoregion"))


lab <- expenditure[,c("exp_lab", "country", "year")]
#Let's look at wealth and lab infrastructure spending
wealth_lab_mdr_exp  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = exp_lab))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("Actual expenditure on lab infrastructure, equipment and supplies (USD)") +
  xlim(0, 300000)

#Let's look at lab infrastructure spending and mdr
wealth_lab_mdr_exp  %>%
  ggplot(aes(x = exp_lab, y = conf_rrmdr))+ 
  geom_point(stat = "identity")+
  xlab("Actual expenditure on lab infrastructure, equipment and supplies (USD)") + 
  ylab("MDR") 
colnames(wealth_lab_mdr_exp)

mdr_2014 <- wealth_lab_mdr_exp %>%
  filter(year == 2014)

mdr_2014 <- mdr_2014[,c("Country Name", "conf_rrmdr")]
colnames(mdr_2014) <- c("Country Name", "conf_rrmdr14")


mdr_2018 <- wealth_lab_mdr_exp %>%
  filter(year == 2018)

mdr_2018 <- mdr_2018[,c("Country Name", "conf_rrmdr")]
colnames(mdr_2018) <- c("Country Name", "conf_rrmdr18")

mdr_14_18 <- left_join(mdr_2014, mdr_2018, by = ("Country Name"))

mdr_14_18 <- mdr_14_18 %>%
  mutate(delta_mdr = conf_rrmdr18 - conf_rrmdr14)
mdr_14_18_sort <- mdr_14_18[order(mdr_14_18$delta_mdr),]

# lab_sm: for 2009-2014
# exp_lab: 2017-2019: don't have world bank population data for these years 

world_bank_pop <- fread("world_bank_total_pop.csv")

world_bank_pop <- world_bank_pop%>%
  filter(`Series Code` == "SP.POP.TOTL")

world_bank_pop_gathered <- world_bank_pop %>%
  gather(year, pop, `2000 [YR2000]` :`2015 [YR2015]`, factor_key=TRUE)

world_bank_pop_gathered$year <- str_replace_all(world_bank_pop_gathered$year,
                                                     "(\\[)([:alpha:]{2})([:digit:]{4})(\\])$",
                                                     "")

world_bank_pop_gathered[world_bank_pop_gathered == ".."] <- NA
world_bank_pop_gathered[world_bank_pop_gathered == ""] <- NA

world_bank_pop_gathered$year <- as.numeric(world_bank_pop_gathered$year)
world_bank_pop_gathered <- world_bank_pop_gathered[,-c(1:2)]



wealth_lab_mdr_exp_pop <- left_join(wealth_lab_mdr_exp, world_bank_pop_gathered, by = c("Country Name" = "Country Name", "year" = "year", "Country Code" = "Country Code"))
wealth_lab_mdr_exp_pop  <- wealth_lab_mdr_exp_pop  %>%
  mutate(lab_sm_100 = (lab_sm/ as.numeric(wealth_lab_mdr_exp_pop$pop)) * 100000) %>%
  mutate(lab_dst_100 = (lab_dst/ as.numeric(wealth_lab_mdr_exp_pop$pop)) * 100000) %>%
  mutate(lab_cul_100 = (lab_cul/ as.numeric(wealth_lab_mdr_exp_pop$pop)) * 100000) %>%
  mutate(lab_sm_cap = lab_sm/ as.numeric(wealth_lab_mdr_exp_pop$pop)) %>%
  mutate(lab_dst_cap = lab_dst/ as.numeric(wealth_lab_mdr_exp_pop$pop)) %>%
  mutate(lab_cul_cap = lab_cul/ as.numeric(wealth_lab_mdr_exp_pop$pop)) %>%
  mutate(conf_rrmdr_100 = (conf_rrmdr/ as.numeric(wealth_lab_mdr_exp_pop$pop)) * 100000)



#Investigating relationship between wealth and testing

wealth_lab_mdr <- left_join(wealth_lab, just_confrrmdr_nona, by = c("Country Name" = "country", "year" = "year"))


#Scatter plot with wealth on x-axis and # culture lab per 100k on y axis
wealth_lab_mdr_exp_pop   %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_cul_100))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs per 100k providing TB diagnostic services using culture") 


#Scatter plot with wealth on x-axis and # culture lab per capita on y axis
wealth_lab_mdr_exp_pop   %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_cul_cap))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs per capita providing TB diagnostic services using culture") 


#Scatter plot with wealth on x-axis and # smear lab per 100k on y axis
wealth_lab_mdr_exp_pop   %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_sm_100))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs per 100k providing lab smear services using culture") 


#Scatter plot with wealth on x-axis and # smear lab per capita on y axis
wealth_lab_mdr_exp_pop   %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_sm_cap))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs per capita providing lab smear services using culture") 


#Scatter plot with wealth on x-axis and # dst lab per cap on y axis
wealth_lab_mdr_exp_pop  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_dst_cap))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs per capita providing TB diagnostic services using drug susceptibility testing") 

#Scatter plot with wealth on x-axis and # dst lab per 100k on y axis
wealth_lab_mdr_exp_pop  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = lab_dst_100))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("# of labs per 100k providing TB diagnostic services using drug susceptibility testing") 


#Scatter plot with # lab sm on x-axis and mdr on y axis (both per 100k)
wealth_lab_mdr_exp_pop  %>%
  ggplot(aes(x = lab_sm_100, y = conf_rrmdr_100))+ 
  geom_point(stat = "identity")+
  xlab("# of labs providing TB diagnostic services using smear microscopy") + 
  ylab("MDR") 

#Scatter plot with # labdst on x-axis and mdr on y axis (both per 100k)
wealth_lab_mdr_exp_pop  %>%
  ggplot(aes(x = lab_dst_100, y = conf_rrmdr_100))+ 
  geom_point(stat = "identity")+
  xlab("# of labs providing TB diagnostic services using drug susceptibility testing") + 
  ylab("MDR") 

#Scatter plot with # lab cul on x-axis and mdr on y axis (both per 100k)
wealth_lab_mdr_exp_pop  %>%
  ggplot(aes(x = lab_cul_100, y = conf_rrmdr_100))+ 
  geom_point(stat = "identity")+
  xlab("# of labs providing TB diagnostic services using lab culture testing") + 
  ylab("MDR") 





# let's just get data for 2015
x <- wealth_lab_mdr_exp_pop[c('year', 'pop', 'exp_lab', 'Country Name')]
pop_2015 <- x %>%
  filter(year == 2015)

colnames(pop_2015)[2] <- "pop_2015"
pop_2015_only<- pop_2015[c('Country Name', 'pop_2015')]

cfor_cap <- left_join(x, pop_2015_only, by = "Country Name")
wealth_lab_mdr_exp_pop_2015 <- left_join(wealth_lab_mdr_exp_pop, pop_2015_only, by = "Country Name")

#scale mdr and lab expenditure for population
#world bank population data from 2015
wealth_lab_mdr_exp_pop_2015  <- wealth_lab_mdr_exp_pop_2015  %>%
  mutate(confrr_mdr_cap = (conf_rrmdr/ as.numeric(wealth_lab_mdr_exp_pop_2015$pop_2015)))  %>%
  mutate(exp_lab_cap = (exp_lab/ as.numeric(wealth_lab_mdr_exp_pop_2015$pop_2015)))

#Let's look at lab infrastructure spending and mdr (per capita)
colnames(wealth_lab_mdr_exp_pop_2015)
wealth_lab_mdr_exp_pop_2015  %>%
  ggplot(aes(x = exp_lab_cap, y = confrr_mdr_cap, color = `2014 [YR2014]`))+ 
  geom_point(stat = "identity")+
  xlab("Actual expenditure on lab infrastructure/supplies (USD) per capita") + 
  ylab("MDR per capita")  +    scale_colour_gradientn(colours = terrain.colors(3))

wealth_lab_mdr_exp_pop  %>%
  ggplot(aes(x = `2014 [YR2014]`, y = confrr_mdr_cap))+ 
  geom_point(stat = "identity")+
  xlab("Total wealth per capita (2014)") + 
  ylab("Actual expenditure on lab infrastructure/supplies (USD) per capita") 

wealth_lab_mdr_exp_pop$`2014 [YR2014]`

lookmdr <- wealth_lab_mdr_exp_pop[c('Country Name', 'year', 'conf_rrmdr')]

# earliest year for mdr is 2014
wealth_lab_mdr_exp_pop_2014 <- wealth_lab_mdr_exp_pop %>%
  filter(year==2014)

colnames(wealth_lab_mdr_exp_pop_2014)
conf_rrmdr_base14 <- wealth_lab_mdr_exp_pop_2014[c('conf_rrmdr', 'Country Name', 'iso3')]


colnames(conf_rrmdr_base14)[1] <-'conf_rrmdr_base14'
conf_rrmdr_base14 <- na.omit(conf_rrmdr_base14)
#ghes_non_notified_per_the_mean
#ghes_notified_per_the_mean
#wealth_lab_mdr_exp_pop_2014$confrr_mdr should this be per capita?
#TB testing capacity over time: lab expenditure? 
#look at for clumped countries: increasing/decreasing mdr

wealth_lab_mdr_exp_pop_ihme <- left_join(wealth_lab_mdr_exp_pop, conf_rrmdr_ihme, by = c("Country Name" = "country", "year" = "year", "iso3" = "iso3", "conf_rrmdr" = "conf_rrmdr"))
wealth_lab_mdr_exp_pop_ihme2 <- left_join(conf_rrmdr_base14, wealth_lab_mdr_exp_pop_ihme, by = c("Country Name" = "Country Name", "iso3" = "iso3"))

colnames(wealth_lab_mdr_exp_pop_ihme2)[2] <- "country"

wealth_lab_mdr_exp_pop_ihme2 <- wealth_lab_mdr_exp_pop_ihme2[-c(1369, 1370, 1371), ]
colnames(wealth_lab_mdr_exp_pop_ihme)

to_join <- wealth_lab_mdr_exp_pop_ihme2[c('conf_rrmdr_base14', 'country', 'year')]




wealth_lab_mdr_exp_pop_ihme3$exp_lab

funding <- wealth_lab_mdr_exp_pop_ihme[c("conf_rrmdr", "ghes_non_notified_per_the_mean","ghes_notified_per_the_mean", "Country Name", "time", "year")]
funding_no_na <- na.omit(y)

lab_exp <- wealth_lab_mdr_exp_pop_ihme2[c("country","exp_lab", "year")]
lab_exp_no_na <- na.omit(x)


#create a data table with just the lab expenditure for 2017
lab_exp_no_na_17_og <- lab_exp_no_na %>%
  filter(year == 2017)

lab_exp_no_na_17 <- lab_exp_no_na_17_og[c("country", "exp_lab")]
colnames(lab_exp_no_na_17)[2] <- "exp_lab_17"

#create a data table with just the lab expenditure for 2018
lab_exp_no_na_18 <- lab_exp_no_na %>%
  filter(year == 2018)

lab_exp_no_na_18 <- lab_exp_no_na_18[c("country", "exp_lab")]
colnames(lab_exp_no_na_18)[2] <- "exp_lab_18"

#create a data table with just the lab expenditure for 2019
lab_exp_no_na_19 <- lab_exp_no_na %>%
  filter(year == 2019)

lab_exp_no_na_19 <- lab_exp_no_na_19[c("country", "exp_lab")]
colnames(lab_exp_no_na_19)[2] <- "exp_lab_19"

x_mdr14 <- left_join(x_no_na, conf_rrmdr_base14, by = c("country" = "Country Name"))
x_mdr14_no_na <- na.omit(x_mdr14)

mdr14_labexp_17 <- left_join(lab_exp_no_na_17, conf_rrmdr_base14, by = c("country" = "Country Name"))
mdr14_labexp_17_no_na <- na.omit(mdr14_labexp_17)

colnames(x_mdr14_no_na)[2] <- "exp_lab_no_na"

# combine lab exp for 2017, 2018, and 2019
lab_exp_no_na_17_18 <- left_join(lab_exp_no_na_17, lab_exp_no_na_18, by = "country")

lab_exp_no_na_17_18_19 <- left_join(lab_exp_no_na_17_18, lab_exp_no_na_19, by = "country")

wealth_lab_mdr_exp_pop_ihme3 <- left_join(x_mdr14_no_na, y_no_na, by = c("country" = "Country Name"))
wealth_lab_mdr_exp_pop_ihme4 <- left_join(x_mdr14_no_na, y_no_na, by = c("country" = "Country Name", "year" = "year"))

# use with assumption that lab funding remains constant from 2017 onwards
wealth_lab_mdr_exp_pop_ihme5 <- left_join(lab_exp_no_na_17_18_19, funding_no_na, by = c("country" = "Country Name"))
wealth_lab_mdr_exp_pop_ihme5 <- left_join(wealth_lab_mdr_exp_pop_ihme5, to_join, by = c("country", "year"))


conditional_new1 <-  lmer(conf_rrmdr~ ghes_non_notified_per_the_mean + ghes_notified_per_the_mean + exp_lab_no_na + conf_rrmdr_base14 + (time | country), data=wealth_lab_mdr_exp_pop_ihme3)
# try to not make it time variable
# or restrict to countries that have data 
# per capita 
summary(conditional_new1)

conditional_new <-  lmer(conf_rrmdr~ ghes_non_notified_per_the_mean + exp_lab_17+ ghes_notified_per_the_mean + conf_rrmdr_base14 + (time | country), data=wealth_lab_mdr_exp_pop_ihme5)
summary(conditional_new)

#let's just look at MDR and lab expenditure over time
mdr_lab_exp <-  lmer(conf_rrmdr ~ exp_lab_17 + exp_lab_18 + exp_lab_19 + (time | country), data=wealth_lab_mdr_exp_pop_ihme5)
summary(mdr_lab_exp)

# let's look at variation in lab expenditure across countries 
lab_exp_no_na_17 %>%
  ggplot(aes(x = exp_lab_17))+ 
  geom_histogram(bins = 10) + 
  ylab("Number of countries") +
  xlab("Lab Expenditure in 2017") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) 

#let's look closer at 0 to 300000
lab_exp_no_na_17 %>%
  ggplot(aes(x = exp_lab_17))+ 
  geom_histogram(bins = 10) + 
  ylab("Number of countries") +
  xlab("Lab Expenditure in 2017") +
  xlim(0, 300000)

mean(wealth_lab_mdr_exp_pop_ihme5$exp_lab_17)
median(wealth_lab_mdr_exp_pop_ihme5$exp_lab_17)
sd(wealth_lab_mdr_exp_pop_ihme5$exp_lab_17)

#bar chart
lab_exp_17_sort<-lab_exp_no_na_17[order(lab_exp_no_na_17$exp_lab_17),]
lab_exp_17_sort %>%
  ggplot(aes(x = reorder(country,
                     exp_lab_17), y = exp_lab_17))+ 
  geom_bar(stat = "identity")+
  xlab("Country") + 
  ylab("Lab expenditure 2017") 

d <- density(lab_exp_no_na_17$exp_lab_17) # returns the density data
plot(d)

no_na_18<- na.omit(lab_exp_no_na_17_18_19$exp_lab_18)
d <- density(no_na_18) # returns the density data
plot(d)

mean(no_na_18)
median(no_na_18)
lab_exp_no_na_17_18_19 %>%
  filter(country == "India")
#regression lines for change in mdr???

lab_exp_no_na_17_18_19 <- lab_exp_no_na_17_18_19 %>%
  mutate(delta = exp_lab_19 - exp_lab_17)

lab_exp_no_na_17_18_19 %>%
  ggplot(aes(x = reorder(country,
                         delta), y = delta))+ 
  geom_bar(stat = "identity")+
  xlab("Country") + 
  ylab("Change in lab expenditure 2017-2019") 

country_delta <- lab_exp_no_na_17_18_19[c("country", "delta")]
wealth_lab_mdr_exp_pop_ihme6 <- left_join(wealth_lab_mdr_exp_pop_ihme5, country_delta, by = c("country"))

delta_mdr_lab_exp <- left_join(mdr_14_18, country_delta, by = c("Country Name" = "country"))
delta_mdr_lab_exp_no_na <- na.omit(delta_mdr_lab_exp)


colnames(delta_mdr_lab_exp_no_na)[1] <- "country"
# change in MDR predicted by changes in lab expenditure
delta_mdr_delta_lab_exp <-  lm(delta_mdr ~ delta , data=delta_mdr_lab_exp_no_na)
summary(mdr_delta_lab_exp)

#look at lab expenditure without india (outliar and large spending)
lab_exp_no_india <- lab_exp_no_na_17_18_19 %>%
  filter(country != "India")

median(na.omit(lab_exp_no_india$exp_lab_19))

world_bank_pop_gathered_15 <- world_bank_pop_gathered %>% # most recent year is 2015
  filter(year == 2015)
updated_notifications <- fread("TB_notifications_2020-11-21.csv")
just_conf_rrmdr <- updated_notifications[,c("conf_rrmdr", "country", "year")] 
y <- na.omit(just_conf_rrmdr)
# ukraine decreasing, but IHME and paper (https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(19)30568-7/fulltext#section-7c530872-6235-4433-899c-b3f276970189) says it's increasing???
lab_mdr_no_na <- na.omit(left_join(just_conf_rrmdr, lab, by = c("country", "year")))
world_bank_pop_gathered_15 <- world_bank_pop_gathered_15[c("Country Name", "pop")]
lab_mdr_pop_no_na <- left_join(lab_mdr_no_na, world_bank_pop_gathered_15, by = c("country" = "Country Name"))
lab_mdr_pop_no_na_cap <- lab_mdr_pop_no_na %>%
  mutate(mdr_cap = as.numeric(conf_rrmdr) / as.numeric(pop)) %>%
  mutate(exp_lab_cap = as.numeric(exp_lab) / as.numeric(pop))
lab_mdr_pop_no_na_cap <- na.omit(lab_mdr_pop_no_na_cap)

lab_mdr_pop_no_na_cap <- lab_mdr_pop_no_na_cap %>%
  mutate(time = year - 2017)

lab_model <- lmer(mdr_cap ~ exp_lab + time + (1 | country), data=lab_mdr_pop_no_na_cap)
summary(lab_model)


lab_mdr_no_na_slopes <- lab_mdr_pop_no_na_cap %>% 
  group_by(country)%>% 
  mutate(lab_slopes = lm(exp_lab_cap ~ year)$coefficients[2]) %>%
  mutate(mdr_slopes = lm(mdr_cap ~ year)$coefficients[2]) 

mdr_trend <- lm(mdr_cap ~ year, data=lab_mdr_no_na_slopes)
summary(mdr_trend)

#MDR predicted by changes in lab expenditure
mdr_slope_lab_exp <-  lmer(mdr_cap ~ lab_slopes + (1|country), data=lab_mdr_no_na_slopes)
summary(mdr_slope_lab_exp)

#Changes in MDR predicted by changes in lab expenditure
slope_mdr_slope_lab_exp <-  lmer(mdr_slopes ~ lab_slopes + (1 | country), data=lab_mdr_no_na_slopes)
summary(slope_mdr_slope_lab_exp)

# USING LM INSTEAD OF LMER

#MDR predicted by changes in lab expenditure
mdr_slope_lab_exp <-  lm(mdr_cap ~ lab_slopes, data=lab_mdr_no_na_slopes)
summary(mdr_slope_lab_exp)

#Changes in MDR predicted by changes in lab expenditure
slope_mdr_slope_lab_exp <-  lm(mdr_slopes ~ lab_slopes, data=lab_mdr_no_na_slopes)
summary(slope_mdr_slope_lab_exp)

countries <- lab_mdr_pop_no_na_cap$country
length(unique(countries))
revised <- lmer(conf_rrmdr ~ exp_lab*time + (1|country), data=lab_mdr_pop_no_na_cap)
install.packages("broom.mixed")
library(broom.mixed)
library("tidyr")
x <- tidy(revised)
write.csv(x,"~/Desktop/tb/table.csv", row.names = FALSE)


# FINAL CSVS USED FOR LMER MODEL IN LMER_LABEXP_MODEL.R
write.csv(lab_mdr_pop_no_na_cap,"~/Desktop/tb/mdr_labexp.csv", row.names = FALSE)
write.csv(lab_mdr_pop_no_na,"~/Desktop/tb/mdr_labexp_before_cap.csv", row.names = FALSE)

#Investgating estimated incidence, but it is only available for 2019
#e_inc_rr_num from MDR_RR_TB_burden_estimates_2020-11-17.csv
mdr_estimates_19 <- fread("~/Desktop/tb/MDR_RR_TB_burden_estimates_2020-12-15.csv")
#e_pop_num from tb burden countries
tb_burden <- fread("~/Desktop/tb/TB_burden_countries_2020-12-15.csv")

#estimated total population number
who_pop <- tb_burden[,c("e_pop_num", "country", "year")] 

#Estimated incidence of rifampicin resistant TB (absolute number)	only available for 2019			
e_inc_rr_num_19 <- mdr_estimates_19[,c("e_inc_rr_num", "country", "year")] 

mdr_estimates_18 <- fread("~/Desktop/tb/MDR_RR_TB_burden_estimates_2020-07-23.csv")
e_inc_rr_num_18 <- mdr_estimates_18[,c("e_inc_rr_num", "country", "year")] 

e_inc_rr_num_18_19 <- rbind(e_inc_rr_num_19, e_inc_rr_num_18)

e_inc_rr_num_18_19_pop <- left_join(e_inc_rr_num_18_19, who_pop, by = c("country", "year"))
e_inc_rr_num_18_19_pop_cap <- e_inc_rr_num_18_19_pop %>%
  mutate(e_inc_rr_cap = e_inc_rr_num/e_pop_num)
write.csv(e_inc_rr_num_18_19_pop_cap,"~/Desktop/tb/e_inc_rr_num_18_19_pop_cap.csv", row.names = FALSE)
