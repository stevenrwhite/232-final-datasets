# [PREAMBLE]
#
# [FILENAME] --> 232RMSW_finalproject.R
#
# [NAMES] --> Rachel Mroz, Steven White
#
# [DATE] --> 03/16/2022
# 
# [EXTRA PACKAGEES TO INSTALL NOT DISCUSSED IN CLASS] --> leaflet
# 
# [DESCRIPTION] --> This code accomplishes the following tasks...
# 
# GRAPH 1: mil_spending_total (continuous) (ORDERED BAR GRAPH)
# GRAPH 2: mil_spend_pct_gdp  (continuous) (BUBBLE MAP, BIGGEST BUBBLE==MOST SPENDING)
# GRAPH 3: ruggedness, mil_spend_pct_gdp (both continuous) (SCATTER)
# 
# OTHER VARIABLES: country (list of countries)
#
#
#
# Load packages
library(tidyverse)
library(leaflet)

# Read data
cntry_stats <- read_csv('https://raw.githubusercontent.com/ameliahg/PSCI232/main/country_stats_recent.csv')


# Creating regression m1
m1 <- lm(ruggedness~mil_spend_pct_gdp, data=cntry_stats)
print(summary(m1))
# Creating scatter plot s1
s1 <- ggplot(data=cntry_stats, aes(y=sqrt(mil_spend_pct_gdp),
                           x=ruggedness)) +
  geom_point(alpha=0.4, color="red") + geom_smooth(method="lm")
print(s1, na.rm=TRUE)
ggsave('s1.pdf',plot=s1,width=6,height=6)








