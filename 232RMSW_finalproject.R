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
m1 <- lm(total_sch_adult~gdppc_ppp, data=cntry_stats)
print(summary(m1))
# Creating scatter plot s1
s1 <- ggplot(data=cntry_stats, aes(x=total_sch_adult,
                           y=gdppc_ppp)) +
  geom_point(alpha=0.4, color="red") + geom_smooth(method="lm")
print(s1, na.rm=TRUE)
ggsave('s1.pdf',plot=s1,width=6,height=6)


#Creating bar grapg b1
b1 <- ggplot(data=cntry_stats, aes(x=reorder(country, -mil_spend_total),
                                   y=sqrt(mil_spend_total)))+
  geom_col()
print(b1 +labs(x="country", y="Spending (USD)",
                               title="Military Spending Total By Country",
                               subtitle="From highest to lowest"), na.rm=TRUE)
ggsave('b1.pdf', plot=b1,width=10,height=10)








