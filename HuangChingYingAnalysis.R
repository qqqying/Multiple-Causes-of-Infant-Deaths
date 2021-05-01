# project analysis
# Ching-Ying Huang

setwd("~/Documents/GitHub/Multiple-Causes-of-Infant-Deaths/data")

load("infmort.rda")

library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
library(table1)

newborn <- data.frame(Year=2010:2019,
                      Births=c())

ex_infmort <- infmort %>% 
  mutate(Cause=replace(Cause, Cause %in% c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15"), "04")) %>%
  mutate(Cause=replace(Cause, Cause %in% c("19", "20", "21", "22", "23", "24", "25", "26"), "18")) %>%
  uncount(Deaths)






table1(~ Sex + Age + Month + Place + Race | Cause, 
       data=ex_infmort, topclass="Rtable1-grid Rtable1-shade Rtable1-times")
table1(~ Sex + Cause + Month + Place + Race | Age, 
       data=ex_infmort, topclass="Rtable1-grid Rtable1-shade Rtable1-times")

table1(~ Sex + Cause + Month + Place + Race + Age | Year, 
       data=ex_infmort, topclass="Rtable1-grid Rtable1-shade Rtable1-times")


birth19 <- 3757582


# Number of infant deaths by year
ex_infmort %>% 
  count(Year) %>% 
  ggplot(aes(x=Year, y=n)) +
  geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) + 
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths, 2010-2019") +
  theme_minimal() 


# Number of infant deaths by Sex
ex_infmort %>% 
  count(Year, Sex) %>% 
  ggplot(aes(fill=Sex, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Sex, 2010-2019") +
  theme_minimal() 



# Number of infant deaths by Month
ex_infmort %>% 
  count(Year, Month) %>% 
  ggplot(aes(fill=Month, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Month, 2010-2019") +
  theme_minimal() 



# Number of infant deaths by Place
ex_infmort %>% 
  count(Year, Place) %>% 
  ggplot(aes(fill=Place, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Place, 2010-2019") +
  theme_minimal() 

fig <- ex_infmort %>% 
  count(Year, Place) %>% 
  ggplot(aes(x=Year, y=n, group=Place)) +
  geom_line(aes(color=Place)) + 
  geom_point(aes(color=Place)) +
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Place, 2010-2019") +
  theme_minimal() 

fig %>% 
  ggplotly

# Number of infant deaths by Race
ex_infmort %>% 
  count(Year, Race) %>% 
  ggplot(aes(fill=Race, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Race, 2010-2019") +
  theme_minimal() 

fig <- ex_infmort %>% 
  count(Year, Race) %>% 
  ggplot(aes(x=Year, y=n, group=Race)) +
  geom_line(aes(color=Race)) + 
  geom_point(aes(color=Race)) +
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Race, 2010-2019") +
  theme_minimal() 

fig %>% 
  ggplotly


# Number of infant deaths by Cause
ex_infmort %>% 
  count(Year, Cause) %>% 
  ggplot(aes(fill=Cause, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Cause, 2010-2019") +
  theme_minimal() 

fig <- ex_infmort %>% 
  count(Year, Cause) %>% 
  ggplot(aes(x=Year, y=n, group=Cause)) +
  geom_line(aes(color=Cause)) + 
  geom_point(aes(color=Cause)) +
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Cause, 2010-2019") +
  theme_minimal() 

fig %>% 
  ggplotly



  
#--------------------------------------------
#try

fig <- ex_infmort %>% 
  count(Year, Month) %>% 
  spread(Month, n) %>% 
  plot_ly(x= ~Year, y= ~n, type="bar") %>%  
  layout(yaxis=list(title="Count"), barmode="stack")



# Proportion of Places of deaths by Year
ggplot(ex_infmort, aes(x=factor(Year), fill=factor(Place))) +
  geom_bar(position="fill", width=0.7) +
  scale_x_discrete("Year", expand=c(0,0)) +
  scale_y_continuous("Proportion of Places", expand=c(0,0)) + 
  theme_bw()

# Proportion of Places of deaths by Race
ggplot(ex_infmort, aes(x=factor(Year), fill=factor(Race))) +
  geom_bar(position="fill", width=0.7) +
  scale_x_discrete("Year", expand=c(0,0)) +
  scale_y_continuous("Proportion of Races", expand=c(0,0)) + 
  theme_bw()

