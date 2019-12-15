library(tidyverse)
library(ggplot2)
theme_classic()
theme_update(panel.spacing = grid::unit(0, "lines"))
library(scales)
library(viridis)
library(broom)
library(gganimate)
library(RColorBrewer)
library(plotly)
library(forcats)
library(shiny)

##Facet plot for gender breakdown by Industry
## Additional tooltips for Workforce size, male and female median annual wages

####################################################

##Download Original Zip File for Industry by Gender (Reminder only)
##download.file("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/edu-sco/Tables/Files/98-402-X2016010-T4-csv-eng.zip", destfile="Utility/statscan.zip")

##Wage Data File by Industry
##Custom extraction from original 6GB census data (see script used elsewhere
## If no Income data is available, replace value with "No Data"
merged <- (read_csv("Utility/wageData_national.csv") 
           %>% filter(`Median Income`!= 0))
merged$Education <- factor(merged$Education, levels = c("College, CEGEP or other non-university certificate or diploma",
                                                        "University certificate, diploma or degree at bachelor level or above",
                                                        "Bachelor's degree",
                                                        "Master's degree",
                                                        "Earned doctorate"), ordered = TRUE)    
merged$`Median Income` <- unlist(merged$`Median Income`)

#############################################

Facet_names <- c(
  `College, CEGEP or other non-university certificate or diploma` = "College and Other",
  `University certificate, diploma or degree at bachelor level or above` = "University Certificate",
  `Bachelor's degree` = "Bachelor's degree",
  `Master's degree` = "Master's degree",
  `Earned doctorate` = "Earned doctorate"
)

#Simple Facet Plot for Industry by Age by Education Level (National Level)

(ggplot(merged, aes(x=merged$`Median Income`, y=Industry, color=Sex, group=Sex))
  + geom_point()
  + theme_dark()
+ scale_x_log10(labels= comma)
+ scale_color_manual(values=c("pink", "blue"))
+ facet_grid(Education ~ Age, labeller = labeller(Education = Facet_names))
+ xlab("National Median Income (CAD)")
+ ylab("Field")
)


