library(tidyverse)
library(cansim)
library(stringr)
library(sunburstR)
df <- get_cansim(1410006401)

#2000-2018
#Median hourly wage rate for male/female

#clean data
df1 <- (df
        %>% rename(Industry = `North American Industry Classification System (NAICS)`)
        %>% select(REF_DATE, GEO, Wages, `Type of work`, Industry,Sex, VALUE)
        %>% filter(REF_DATE == 2000:2018, GEO == "Canada",str_starts(`Type of work`,"Both", negate = TRUE),
                   str_starts(Wages,"Median hourly"),
                   Sex != "Both sexes")
        %>% mutate(`Type of work`= 
                     recode(`Type of work`,
                            `Full-time employees`="FullTime", 
                            `Part-time employees` = "PartTime")))
  
df2 <- data.frame((df1 
         %>% unite(new, REF_DATE:Sex,sep = "-")))

#not displaying wages but percentages?
sunburst(df2)




