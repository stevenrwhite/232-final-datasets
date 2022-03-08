# [START HERE]
# [FILENAME] --> 232RMSW_finalproject.R
#
# [NAMES] --> Rachel Mroz, Steven White
# [DATE] --> 03/16/2022
# [EXTRA PACKAGES TO INSTALL NOT DISCUSSED IN CLASS] --> ggrepel, plotly
# 
# [DESCRIPTION] --> This code accomplishes the following tasks...
# 
# GRAPH 1: [SCATTER PLOT VIEW IN VIEWER] variables: total_sch_adult, gdppc_ppp
        # Creates an interactive scatter plot using plotly package
        # comparing adult education rates to a country's GDP per capita.
        # Runs regression line through data points.
        # IN CODE: Prints correlation between the two variables.
        # INTERACTIVE: Use mouse to hover over points and regression line
        # to reveal information about each point, including exact point coordinates
        # and country name.
#
# GRAPH 2: [HORIZONTAL BAR GRAPH VIEW IN PLOTS] variables: country, mil_spend+total
        # Creates bar graph of military spending, ordered from highest to lowest.
        # Filters results to show only top 20 highest spending countries.
        # These countries have a GDP per capita of over 10 Billion.
        # Rotates bar graph from initial position to for ease of viewing,
        # in addition to other aesthetic choices.
        # 
        # 
#
# GRAPH 3: [INTERACTIVE BUBBLE PLOT VIEW IN VIEWER] variables: basic_water_access, childmort_
        # Creates an interactive bubble plot (based on country population) 
        # comparing a country's access to water and its child mortality rate.
        # Rruns regression line through data points.
        # IN CODE: Prints correlation between the two variables.
        # INTERACTIVE: Use mouse to hover over points and regression line
        # to reveal information about each specific point, including exact point coordinates
        # and country name.
#
#
#
# Load packages
library(tidyverse)
library(ggrepel)
library(plotly)

# Read data
cntry_stats <- read_csv('https://raw.githubusercontent.com/ameliahg/PSCI232/main/country_stats_recent.csv')



# Creating regression m1
m1 <- lm(total_sch_adult~gdppc_ppp, data=cntry_stats)
print(summary(m1))
# Creating scatter plot s1
s1 <- ggplot(data=cntry_stats, aes(x=total_sch_adult, y=gdppc_ppp, 
                                   group=1,
                                   text= paste("Country: ", country))) +
  theme_bw()+
  geom_point(alpha=0.6, color="darkgreen") + geom_smooth(method="lm")
print(ggplotly(s1 +  labs(x="Education Level", y="GDP per capita",
                          title="Education Rates Compared to Country GDP")),
      na.rm=TRUE) # Running ggplotly in this line opens the graph in viewer tab.
                  # Does not make .pdf file interactive.
# Prints Correlation of adult schooling and GDP per capita --> 0.6654355
print(cor(cntry_stats$total_sch_adult,cntry_stats$gdppc_ppp,use='complete.obs'))
ggsave('s1.pdf',plot=s1,width=6,height=6)



# Creating bar graph b1
b1 <- ggplot(data=filter(cntry_stats,mil_spend_total>10000000000),
             #Filters graph to only show countries with a GDP per capita of over 10 BILLION
             aes(x=reorder(country, +mil_spend_total),
                                   y=sqrt(mil_spend_total)))+ 
  coord_flip() + labs(x="Country", y="Spending",
       title="Military Spending Total By Country",
       subtitle="From highest to lowest")+
  geom_col(color="white", fill="darkred")
print(b1, na.rm=TRUE)
ggsave('b1.pdf', plot=b1,width=10,height=10)



# Regression for bub1
m2 <- lm(basic_water_access~childmort_, data=cntry_stats)
print(summary(m2))
# Creating bubble plot
bub1 <- ggplot(data=cntry_stats, aes(x=basic_water_access, y=childmort_, size=pop_total,
                                     group=1,
                                     text= paste("Country: ", country))) +
  theme_bw()+
  geom_point(alpha=0.2, color="blue") + geom_smooth(method="lm")
print(ggplotly(bub1 +  labs(x="Basic Water Access", y="Child Mortality Rate",
                          title="Correlation of Access to Water and Child Mortality By Country")),
      na.rm=TRUE)
# Print Correlation --> cor==-0.8348726
print(cor(cntry_stats$basic_water_access,cntry_stats$childmort_,use='complete.obs'))
ggsave('bub1.pdf',plot=bub1,width=6,height=6)




