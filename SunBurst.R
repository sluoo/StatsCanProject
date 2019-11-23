library(tidyverse)
library(cansim)
library(stringr)
library(sunburstR)
df <- get_cansim(1410006401)

#2000-2018 and all of Canada
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
         %>% select(Industry,REF_DATE,`Type of work`,Sex,VALUE)
         %>% unite(new, Industry:Sex, sep = "-"))
         %>% drop_na()
         %>% rename(value = VALUE))

#not displaying wages but percentages?
sunburst(df2,percent = FALSE)
sund2b(df2,
       rootLabel = "Industry",
       tooltip =  sund2bTooltip(
         html = htmlwidgets::JS("
function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
  ) 
)
         

# change the tooltip
sund2b(
  sequences,
  tooltip = sund2bTooltip(
    html = htmlwidgets::JS("
function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
  )
)      
         
         
         
#things to work on if we go forward
# which years to focus on
# reorder the columns in the data so it displays sex, year, etc. 
# what are the main things to focus on
# results may look better if i used the industry vs. # of ppl in industry
# this is the industry vs. wage data... (my bad)








