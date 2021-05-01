# reference
# https://sites.google.com/site/timriffepersonal/DemogBlog/newformetrickforworkingwithbigishdatainr

setwd("~/Documents/GitHub/Multiple-Causes-of-Infant-Deaths/data")
library(tidyverse)
library(sqldf)

for(i in 10:19){
  if(i==18){
    filex <- file("~/Desktop/Programming/Project/data/raw/VS18MORT.txt")
  } else {
    filex <- file(paste0("~/Desktop/Programming/Project/data/raw/VS",i,"MORT.DUSMCPUB"))
  }
  
  attr(filex, "file.format") <- list(sep=",", header=FALSE)
  assign(paste0("mort",i), 
           sqldf("select 
                  substr(V1, 160, 2) as Cause, 
                  substr(V1, 69, 1) as Sex, 
                  substr(V1, 81, 2) as Age,
                  substr(V1, 65, 2) as Month,
                  substr(V1, 83, 1) as Place,
                  substr(V1, 445, 2) as Race,
                  count(*) Deaths 
                 from filex
                 where Age <> '  '
                 group by Cause, Sex, Age, Month, Place, Race"))
  
  close(filex)
  
}


# save(mort19, file="mort19.rda")

## Add "Year" column and combine all datasets

infmort <- NULL

for(i in 10:19){
  object <- get(paste0("mort",i))
  object <- data.frame(object, 2000+i)
  colnames(object)[8] <- "Year"
  infmort <- rbind(infmort, object)
}


infmort <- infmort %>% 
  mutate(Year=as.integer(Year))


# save final file
save(infmort, file="infmort.rda")

