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
  scale_x_continuous(breaks=seq(2010,2019,1), guide=guide_axis(angle=90)) +
  xlab("Year") +
  ylab("Number of infant deaths") +
  ggtitle("Number of infant deaths by cause, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_minimal() 

## Rate
t8_infmort %>% 
  count(Year, Cause) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>%
  select(-Births, -n) %>% 
  ggplot(aes(x=Year, y=Rate, group=Cause)) +
  geom_line(aes(color=Cause)) + 
  geom_point(aes(color=Cause)) +
  scale_x_continuous(breaks=seq(2010,2019,1), guide=guide_axis(angle=90)) +
  xlab("Year") +
  ylab("Infant Mortality Rate per 1,000 live births") +
  ggtitle("Rate by cause, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_minimal() 



## Causes of infant deaths by Sex
t8_infmort %>% 
  count(Cause, Sex, Year) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>%
  select(-Births, -n) %>% 
  ggplot(aes(x=factor(Year), y=Rate, group=Cause)) +
  geom_line(aes(color=Cause)) + 
  geom_point(aes(color=Cause)) +
  ggpubr::rotate_x_text() +
  xlab("Year") +
  ylab("Infant Mortality Rate per 1,000 live births") +
  ggtitle("Causes of infant deaths by sex, 2010-2019") +
  facet_grid(Cause ~ Sex, scales="free_y")

# Number of infant deaths by Age
t8_infmort %>% 
  count(Cause, Age, Year) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>%
  select(-Births, -n) %>% 
  ggplot(aes(x=factor(Age), y=Rate, fill=Cause)) +
  geom_boxplot(position=position_dodge(1), notch=TRUE) +
  scale_x_discrete(guide=guide_axis(angle=90)) +
  xlab("Age") +
  ylab("Infant Mortality Rate per 1,000 live births") +
  ggtitle("Cause of infant deaths by age, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_bw()


# pie chart (Number of infant deaths by age)

t8_infmort %>% 
  mutate(Age=replace(Age, Age %in% c("01"), "Under 1 hour")) %>%
  mutate(Age=replace(Age, Age %in% c("02"), "1 - 23 hours")) %>%
  mutate(Age=replace(Age, Age %in% c("03", "04", "05", "06", "07", "08"), "1 - 6 days")) %>%
  mutate(Age=replace(Age, Age %in% c("09", "10", "11"), "7 - 27 days")) %>%
  mutate(Age=replace(Age, Age %in% c("12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22"), "28 - 364 days")) %>%
  group_by(Age, Year) %>%
  tally() %>% 
  ggplot(aes(x=2, y=n, fill=Age)) +
  geom_bar(stat="identity", position=position_fill()) +
  coord_polar(theta="y") +
  xlim(0.5, 2.5) +
  facet_wrap( ~ Year, ncol=5) +
  theme_void() +
  ggtitle("Total infant deaths by age at death from 2010 to 2019\n") 



# Causes of infant deaths by Month
t8_infmort %>% 
  count(Cause, Month, Year) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>%
  select(-Births, -n) %>% 
  ggplot(aes(x=factor(Month), y=Rate, fill=Cause)) +
  geom_boxplot(position=position_dodge(1), notch=TRUE) +
  xlab("Month") +
  ylab("Infant Mortality Rate per 1,000 live births") +
  ggtitle("Causes of infant deaths by month, 2010-2019") +
  facet_wrap( ~ Cause, scales="free_y") +
  theme_bw()



# Number of infant deaths by Place
t8_infmort %>% 
  count(Cause, Place, Year) %>% 
  left_join(newborn, by="Year") %>%
  mutate(Rate=(n/Births)*1000) %>%
  select(-Births, -n) %>% 
  ggplot(aes(x=factor(Cause), y=Rate, fill=Place)) +
  geom_boxplot(position=position_dodge(1), notch=TRUE) +
  xlab("Cause") +
  ylab("Infant Mortality Rate per 1,000 live births") +
  ggtitle("Place by causes of infant deaths, 2010-2019") +
  facet_wrap( ~ Place, scales="free_y") +
  theme_bw()


