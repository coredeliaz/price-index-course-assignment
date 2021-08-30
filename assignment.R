#### Building and analyzing a price index with R ####
# Christina(Chi) Zhang
# August 2021
install.packages(c("dplyr", "tidyr", "gpindex", "devtools", "ggplot2"))
devtools::install_github("ppd-dpp/calpr")
install.packages("goeveg")
install.packages("zoo")
install.packages("formattable")

#---- Bring in libraries ----
library(dplyr)
library(tidyr)
library(gpindex)
library(calpr)
library(ggplot2)
library(zoo)
library(goeveg)
library(formattable)

setwd("D:\\2-statistics projects\\ppicourse")

Sys.setlocale("LC_TIME","English")

#---- Bring in data ----
source('https://raw.githubusercontent.com/ppd-dpp/price-index-course/master/scripts/get_data.R')

#---- Step 1: Make the weights ----
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Make provincial weights
weights_prov <- weights %>%
  group_by(year, province) %>% # group all sales values by reference year and province
  summarize(weight = sum(weight)) %>% # value of all sales in each province in each year
  mutate(share_prov = scale_weights(weight)) %>% # scale to sum to 1
  select(-weight)# drop the weight column as it won't be needed again

# Make product weights
weights_prod <- weights %>%
  group_by(year, province) %>%
  mutate(share_prod = scale_weights(weight)) %>% # scale to sum to 1
  select(-weight) 

#---- Step 2: Calculate the geomean for product K ----
dat_micro_K <- dat_micro %>%
  mutate(period = year_month(period)) %>% # turn transaction dates into month-by-year dates
  group_by(province, product, year, period) %>%
  summarize(geomean = geometric_mean(price)) # calculate the geometric average

#---- Step 3: Calculate the geomean for products A to J ----
dat_index_AK <- dat_gps %>%
  mutate(period = year_month(period)) %>% # turn transaction dates into month-by-year dates
  gather(key = "quote", value = "price", price1:price3) %>% # melt into a long dataset
  group_by(province, product, year, period) %>%
  drop_na() %>% #remove missing quotes
  summarize(geomean = geometric_mean(price)) %>% # calculate the geometric average
  bind_rows(dat_micro_K) # append to dat_micro

#---- Step 4: Calculate the period-over-period elemental indices ----
dat_index <- dat_index_AK %>%  
  group_by(province, product) %>%
  mutate(geomean_previous = lag(geomean,default = geomean[1])) %>% #match the last period geomean value
  mutate(rel = geomean/geomean_previous) #calculate the period-over-period elemental indices

#---- Step 5: Price update the product weights ----
dat_index_update_weight <- dat_index %>%  
  merge(weights_prod, by = c("province", "product", "year"), all.x = TRUE ) %>% 
  group_by(province, product, year) %>% 
  mutate(share_prod_u = update_weights(cumprod(lag(rel, default = 1)), share_prod)) %>% # price update the weights
  group_by(year,period,province) %>%
  mutate(share_prod_u = scale_weights(share_prod_u)) #rescale adjusted weights to sum to unity

#---- Step 6: Calculate the province-level index ----
index_prov <- dat_index_update_weight %>%
  group_by(period, year, province) %>%
  summarize(index_provincial_overmonth = arithmetic_mean(rel,share_prod_u)) %>% #month to month provincial-level index
  group_by(province,year) %>%
  mutate(index_provincial = cumprod(index_provincial_overmonth)) #provincial-level index at price 2018 and 2019 respectively

#---- Step 7: Calculate the Canada-level index ----
index <- index_prov %>%
  merge(weights_prov, by = c("year", "province"), all.x = TRUE ) %>%
  group_by(period, year) %>%
  summarize(index_provincial = arithmetic_mean(index_provincial, share_prov)) %>% #national-level index at price 2018 and 2019 respectively
  mutate(province = 0) %>%  #province 0 is the Canada-level
  bind_rows(index_prov) #append provincial indices

#---- Step 8: Chain the 2018 and 2019 indices ----
index_chain <- index %>%
  group_by(province) %>%
  mutate(chain_ratio = ifelse(year == "2018-01-01", 1 , index_provincial[period =="2019-01-01" & year =="2018-01-01"])) %>% #prepare the 20190101 ratio for all 2019 reference indices
  mutate(index = index_provincial * chain_ratio * 100) %>% #chain indices
  group_by(period, province) %>%
  summarize(index = unique(index)) %>% # remove the duplicate rows for the link month
  ungroup()

#---- Step 9: Quarter the monthly indices ----
my_index <- index_chain %>%
  filter(province == '0') %>% # keep only the Canada-level index
  mutate(period = year_quarter(period)) %>%
  group_by(period) %>%
  summarize(index = mean(index)) %>%
  mutate(index = round(index /index [period == '2019-01-01'] * 100, digits = 1), source = "Estimation") # rebase to Jan 2019

index_crea_quaterly <- index_crea %>%
  filter(period >= '2018-01-01') %>%
  mutate(period = year_quarter(period)) %>%
  group_by(period) %>%
  summarize(index = mean(index)) %>%
  mutate(index = round(index/index[period == '2019-01-01'] * 100, digits = 1), source = "CREA") # rebase to Jan 2019

#---- Step 10: Append all three indices together ----  
index_mw_mixed <- index_mw %>%
  mutate(period= as.Date(period, format= "%Y-%m-%d"),source = "Monsterweb") %>%
  bind_rows(index_crea_quaterly, my_index)

#---- Question 1&2: Line graph for provincial indices ----
province <- data.frame("province_name" = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC"),
                       "province" = c(10,11,12,13,24,35,46,47,48,59))

province_line <- index_chain %>%
  merge(province, by = c("province"), all.x = TRUE) %>%
  drop_na(province_name) #remove national indices

ggplot(province_line, #plot of provincial line graph over years
  aes(x = period, y = index, group = province_name, color = province_name, linetype = province_name)) +
  geom_line(size = 1.5) +
  scale_colour_manual(values = rainbow(10)) +
  scale_y_continuous(breaks = seq(100, 200, 10)) +
  scale_x_date(date_labels = "%b %Y", expand = c(0,0)) +  
  theme_bw() +
  theme(text = element_text(size=25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.title=element_blank())+
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("index (January 2018=100)") 

ggsave("Provincial indices.tiff", width = 12, height = 9, device='tiff', dpi=700)

province_line_last <- province_line %>%
  filter(period == max(period))

cat("Question 1:",province_line_last$province_name[which.max(province_line_last$index)], #question 1,name of the largest increased province
    "increased largest to",
    round(max(province_line_last$index),1), #question 1,value of the largest increased province
    "comparing to January 2018.")

cat("Question 2:",province_line_last$province_name[which.min(province_line_last$index)], #question 1,name of the smallest increased province
    "increased smallest to",
    round(min(province_line_last$index),1), #question 2,value of the smallest increased province
    "comparing to January 2018.")

#---- Question 3: Line graph for provincial indices ----
percent(max(abs(index_prov$index_provincial_overmonth - 1))) #question 3,the largest month-over-month movement in prices
index_prov[which.max(abs(index_prov$index_provincial_overmonth-1)),] #question 3,province and month of largest movement

#---- Qustion 4: Line graph for three national indices ----
ggplot(index_mw_mixed,
   aes(x = as.yearqtr(period), y = index, group = source, color = source)) +
   geom_line(size = 1.5) +
   scale_colour_brewer(palette = "Set1") +
   scale_y_continuous(breaks = seq(60, 140, 10)) +
   scale_x_yearqtr(format ="%YQ%q") +
   theme_bw() +
   theme(text = element_text(size=25),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position="bottom",
         legend.title=element_blank())+
   xlab(NULL) +
   ylab(NULL) +
   ggtitle("index (2019Q1 = 100)") 

ggsave("National indices.tiff", width = 12, height = 9, device='tiff', dpi=700)

index_mw_mixed_last <- index_mw_mixed %>%
  filter(period == "2019-10-01")
  
cat("Quesiton 4: The",index_mw_mixed_last$source[which.max(index_mw_mixed_last$index)], #question 4,name of the largest increased index
    "index shows the largest increase to",
    round(max(index_mw_mixed_last$index),1), #question 4,value of the largest increased index
    "since Q1 2019.")

#---- Question 5: CV for provincial indices  ----
CV_provincial <- index_chain %>%
  group_by(province) %>%
  summarise(cv = cv(index) * 100) #calculate coefficient of variation for provincial indices

ifelse(CV_provincial$cv > 33.3, CV_provincial$province, "Pass")  #test the valid based on coefficients of variation>33.3
