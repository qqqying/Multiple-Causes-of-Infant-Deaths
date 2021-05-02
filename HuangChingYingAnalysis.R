# project analysis
# Ching-Ying Huang

setwd("~/Documents/GitHub/Multiple-Causes-of-Infant-Deaths/data")

load("infmort.rda")

library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(table1)
library(latticeExtra) # For Dual Y axis line chart

## Extend data by number of deaths
## Categorized some causes into one 
ex_infmort <- infmort %>% 
  mutate(Cause=replace(Cause, Cause %in% c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15"), "04")) %>%
  mutate(Cause=replace(Cause, Cause %in% c("19", "20", "21", "22", "23", "24", "25", "26"), "18")) %>%
  uncount(Deaths)


## Number of infant deaths by year

# Newborn data, 2010-2019
newborn <- data.frame(Year=2010:2019,
                      Births=c(4007105,3961220,3960796,3940764,3998175,
                               3988733,3956112,3864754,3801534,3757582))

# Calculate infant mortality 
mort <- ex_infmort %>% 
  count(Year, Sex) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>%
  rename(Deaths=n)

obj1 <- xyplot(Deaths ~ Year | Sex, data=mort, type="l", lwd=2)
obj2 <- xyplot(Rate ~ Year | Sex, data=mort, type="l", lwd=2)
# Dual Y axis line chart
doubleYScale(obj1, obj2, text=c("Number of Infant Deaths", "Infant Mortality Rate per 1,000 live births"), add.ylab2=TRUE, use.style=TRUE)

## Table
ex_infmort$Cause <- factor(ex_infmort$Cause, levels=as.integer(names(sort(table(ex_infmort$Cause), decreasing=TRUE))))
table1(~ Cause | factor(Year), 
       data=ex_infmort, topclass="Rtable1-grid Rtable1-shade Rtable1-times")

## New dataset with only top 8 causes
t8_infmort <- ex_infmort %>% 
  mutate(Cause=replace(Cause, Cause %in% c("29", "16", "30", "28", "38", "31", "42", "27"), NA)) %>%
  mutate(Cause=if_else(is.na(Cause), "Others", as.character(Cause))) 

## Causes of infant deaths
t8_infmort %>% 
  count(Year, Cause) %>% 
  ggplot(aes(x=Year, y=n, group=Cause)) +
  geom_line(aes(color=Cause)) + 
  geom_point(aes(color=Cause)) +
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by cause, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_minimal() 


## Causes of infant deaths by Sex
t8_infmort %>% 
  count(Cause, Sex, Year) %>% 
  ggplot(aes(x=factor(Year), y=n, group=Cause)) +
  geom_line(aes(color=Cause)) + 
  geom_point(aes(color=Cause)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Causes of infant deaths by sex, 2010-2019") +
  facet_grid(Cause ~ Sex, scales="free_y")




# Causes of infant deaths by Month
t8_infmort %>% 
  count(Cause, Month, Year) %>% 
  ggplot(aes(x=factor(Month), y=n, fill=Cause)) +
  geom_boxplot(position=position_dodge(1)) +
  xlab("Month") +
  ylab("Number of infant deaths") +
  ggtitle("Causes of infant deaths by month, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_bw()



# Number of infant deaths by Place
t8_infmort %>% 
  count(Cause, Place, Year) %>% 
  ggplot(aes(x=factor(Place), y=n, fill=Cause)) +
  geom_boxplot(position=position_dodge(1)) +
  xlab("Place") +
  ylab("Number of infant deaths") +
  ggtitle("Causes of infant deaths by place, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_bw()


# Number of infant deaths by Race
t8_infmort %>% 
  count(Cause, Race, Year) %>% 
  ggplot(aes(x=factor(Race), y=n, fill=Cause)) +
  geom_boxplot(position=position_dodge(1)) +
  xlab("Race") +
  ylab("Number of infant deaths") +
  ggtitle("Causes of infant deaths by Race, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_bw()


t8_infmort %>% 
  count(Cause, Race, Year) %>% 
  ggplot(aes(x=factor(Cause), y=n, fill=Race)) +
  geom_boxplot(position=position_dodge(1)) +
  xlab("Cause") +
  ylab("Number of infant deaths") +
  ggtitle("Race by causes of infant deaths, 2010-2019") +
  facet_wrap( ~ Race, scales="free_y") +
  theme_bw()






##






  
#--------------------------------------------
#try

fig <- t8_infmort %>% 
  count(Year, Month) %>% 
  spread(Month, n) %>% 
  plot_ly(x= ~Year, y= ~n, type="bar") %>%  
  layout(yaxis=list(title="Count"), barmode="stack")



# Proportion of Places of deaths by Year
ggplot(t8_infmort, aes(x=factor(Year), fill=factor(Place))) +
  geom_bar(position="fill", width=0.7) +
  scale_x_discrete("Year", expand=c(0,0)) +
  scale_y_continuous("Proportion of Places", expand=c(0,0)) + 
  theme_bw()

# Proportion of Places of deaths by Race
ggplot(t8_infmort, aes(x=factor(Year), fill=factor(Race))) +
  geom_bar(position="fill", width=0.7) +
  scale_x_discrete("Year", expand=c(0,0)) +
  scale_y_continuous("Proportion of Races", expand=c(0,0)) + 
  theme_bw()

# Number of infant deaths by year
test <- t8_infmort %>% 
  count(Year) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>% 
  rename(Death=n) %>% 
  select(-Births) %>% 
  gather(Data, value, 2:3) %>% 
  ggplot(aes(x=Year, y=value, fill=Data)) +
  geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) + 
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Rate per 1,000 live births") +
  ggtitle("INFANT MORTALITY RATE, 2010-2019") +
  theme_minimal() 

# Number of infant deaths by Cause
t8_infmort %>% 
  count(Year, Cause) %>% 
  ggplot(aes(fill=Cause, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Cause, 2010-2019") +
  theme_minimal() 


# Number of infant deaths by Race
t8_infmort %>% 
  count(Year, Race) %>% 
  ggplot(aes(fill=Race, y=n, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Race, 2010-2019") +
  theme_minimal() 

t8_infmort %>% 
  count(Year, Race) %>% 
  ggplot(aes(x=Year, y=n, group=Race)) +
  geom_line(aes(color=Race)) + 
  geom_point(aes(color=Race)) +
  scale_x_continuous(breaks=seq(2010,2019,1)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by Race, 2010-2019") +
  theme_minimal() 



