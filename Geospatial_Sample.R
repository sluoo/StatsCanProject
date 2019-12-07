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

#Geospatial data related packages
library(ggmap)
library(sf)
library(htmlwidgets)
library(rgdal) #This seems to work better for me than sf for reading shape files when they need to be re-projected

##Facet plot for gender breakdown by Industry
## Additional tooltips for Workforce size, male and female median annual wages

####################################################

##Download Original Zip File for Industry by Gender (Reminder only)
##download.file("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/edu-sco/Tables/Files/98-402-X2016010-T4-csv-eng.zip", destfile="Utility/statscan.zip")

##Wage Data File by Industry
##Custom extraction from original 6GB census data
wages <- read_csv("Utility/wageData.csv")

## Unzip and read CSV for Industry by Gender (Note: Parsing failures are blank rows in the data)
## Many, many columns to rename...
## Drop useless columns and NA rows
source <- read_csv("Utility/98-402-X2016010-T4-CANPR-eng.csv") 
source_renamed <- (source %>% rename("GeoName"="Geographic name", 
                                     "AreaTotal"="Total – STEM and BHASE (non-STEM) groupings - Classification of Instructional Programs (CIP) 2016 (2016 counts)",
                                     "Science and science technology"="STEM fields of study - Science and science technology (2016 counts)",
                                     "Engineering and engineering technology"="STEM fields of study - Engineering and engineering technology (2016 counts)",
                                     "Mathematics and computer and information science"="STEM fields of study - Mathematics and computer and information science (2016 counts)",
                                     "Business and administration"="BHASE (non-STEM) fields of study - Business and administration (2016 counts)",
                                     "Arts and humanities"="BHASE (non-STEM) fields of study - Arts and humanities (2016 counts)",
                                     "Social and behavioural sciences"="BHASE (non-STEM) fields of study - Social and behavioural sciences (2016 counts)",
                                     "Legal professions and studies"="BHASE (non-STEM) fields of study - Legal professions and studies (2016 counts)",
                                     "Health care"="BHASE (non-STEM) fields of study - Health care (2016 counts)",
                                     "Education and teaching"="BHASE (non-STEM) fields of study - Education and teaching (2016 counts)",
                                     "Trades, services, natural resources and conservation"="BHASE (non-STEM) fields of study - Trades, services, natural resources and conservation (2016 counts)",
                                     "TotalPercent"="Total – STEM and BHASE (non-STEM) groupings - Classification of Instructional Programs (CIP) 2016 (% distribution 2016)"
)
%>% drop_na()
%>% filter(GeoName!="Canada")
%>% filter(Age=="All ages, 15-plus")
%>% dplyr::select(-c("Geographic code", "Age", "Global non-response rate", "Data quality flag", 19:29))
)

## Join wage data with demographic data
merged <- (source_renamed %>% gather(c(5:14), key="Industry", value="Cohort Size") 
%>% left_join(wages))

##Numbers for both genders
bothsexes <- (merged
              %>% filter(Sex== "Both sexes")
              %>% rename("Median Income (Both Genders)"="Median Income")
              %>% dplyr::select(-c("Sex"))
)

##Numbers for females only
females <- (merged
            %>% filter(Sex== "Female")
            %>% rename("Median Income (Female)"="Median Income",
                       "FemaleGenderCount"="Cohort Size")
            %>% dplyr::select(-c("Sex", "AreaTotal"))
)

##Numbers for males only
males <- (merged
            %>% filter(Sex== "Male")
            %>% rename("Median Income (Male)"="Median Income")
          %>% dplyr::select(-c("Cohort Size", "Sex", "AreaTotal"))
)


##Recombine Tables and Calculate Female Proportions
##Filter out Most Frequent Industries per Jurastiction
femaleprop <- (left_join(bothsexes,females) %>% left_join(males)
               %>% mutate(FemalePercent = round(100*(FemaleGenderCount/`Cohort Size`), digits=0))
)

##Extract most common industry by education level, based on femaleprop table
##Create Five Tables For Different Education Levels
##Within each table, extract top value for each province, then rejoin all at the end
##Not ideal but this level of striation requires some drastic measures to clean

College <- (femaleprop %>% filter(Education=="College, CEGEP or other non-university certificate or diploma") 
            %>% group_by(GeoName) %>% slice(which.max(`Cohort Size`)))
UniCert <- (femaleprop %>% filter(Education=="University certificate, diploma or degree at bachelor level or above")
            %>% group_by(GeoName) %>% slice(which.max(`Cohort Size`)))
Bach <- (femaleprop %>% filter(Education=="Bachelor's degree")
         %>% group_by(GeoName) %>% slice(which.max(`Cohort Size`)))
Master <- (femaleprop %>% filter(Education=="Master's degree")
           %>% group_by(GeoName) %>% slice(which.max(`Cohort Size`)))
PhD <- (femaleprop %>% filter(Education=="Earned doctorate")
        %>% group_by(GeoName) %>% slice(which.max(`Cohort Size`)))

#Final Cleaned Data Table
dtafin <- (bind_rows(College, UniCert, Bach, Master, PhD, id=NULL))
##Factor Education Column (essentially for facet reordering)
dtafin$Education <- factor(dtafin$Education, levels = c("College, CEGEP or other non-university certificate or diploma",
                                                        "University certificate, diploma or degree at bachelor level or above",
                                                        "Bachelor's degree",
                                                        "Master's degree",
                                                        "Earned doctorate"), ordered = TRUE)             
## If no Income data is available, replace value with "No Data"
dtafin$`Median Income (Female)`[which(dtafin$`Median Income (Female)`==0)] <- "No Data"
dtafin$`Median Income (Male)`[which(dtafin$`Median Income (Male)`==0)] <- "No Data"

####################################################

## Load Shape File, Join Shape File with Data 
## Use digital boundary file for performance
canada_shape <- st_read("Shape Files/Digital Boundary/lpr_000a16a_e.shp")
joined_dta <- left_join(canada_shape, dtafin, by=c("PRENAME"="GeoName")) %>% dplyr::rename("Jurisdiction"="PRENAME")

##Change strip text names without renaming data
Facet_names <- c(
  `College, CEGEP or other non-university certificate or diploma` = "College and Other",
  `University certificate, diploma or degree at bachelor level or above` = "University Certificate",
  `Bachelor's degree` = "Bachelor's degree",
  `Master's degree` = "Master's degree",
  `Earned doctorate` = "Earned doctorate"
)

#Create ggplot with dummy aesthetics for later use
plot <- (ggplot(joined_dta)
         + geom_sf(aes(common=Jurisdiction, 
                       fill=Industry, label1=`Cohort Size`, 
                       label2=FemalePercent, 
                       label3=`Median Income (Female)`,
                       label4=`Median Income (Male)`,
                       geometry=geometry))
         + scale_fill_viridis(discrete=TRUE, option="plasma")
         + xlab("Longitude")
         + ylab("Latitude")
         + facet_wrap(~Education, labeller = as_labeller(Facet_names))
         + theme_classic()
         + guides(alpha=FALSE)
         + theme(legend.position=c(0.83, 0.25), 
                 panel.spacing = unit(0.5, "lines"),
                 axis.text = element_blank(),
                 axis.ticks=element_blank(),
                 legend.title = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 )
)

##Map with multiple tooltips
##Trick is to use previously established dummy aesthetics to display info in joined table

htmlplot <- (ggplotly(plot, tooltip=c("common", "label1", "label2", "label3", "label4"), width=800, height=450) 
  %>% add_annotations(text="Predominant Field for Workforce Cohort",
                      xref="paper", yref="paper",
                      x=0.7, xanchor="left",
                      y=0.47, yanchor="top",
                      legendtitle=TRUE, showarrow=FALSE)
  %>% layout(autosize=F, legend=list(x=0.7, y=0.025, yanchor="bottom"), hovermode = "closest") 
)

####################################################

#Separate Plots by Education Level


edl1 <- (merged %>% dplyr::filter(Education=="College, CEGEP or other non-university certificate or diploma") 
         %>% plot_ly(x = ~`Median Income`, y = ~GeoName, 
                   text = ~`Median Income`, 
                   hoverinfo = text) 
  %>% add_markers(frame = ~Industry, color = ~Sex)
  %>% layout()
)

edl2 <- (merged %>% dplyr::filter(Education=="University certificate, diploma or degree at bachelor level or above") 
         %>% plot_ly(x = ~`Median Income`, y = ~GeoName, 
                     text = ~`Median Income`, 
                     hoverinfo = text) 
         %>% add_markers(frame = ~Industry, color = ~Sex)
         %>% layout()
)

edl3 <- (merged %>% dplyr::filter(Education=="Bachelor's degree") 
         %>% plot_ly(x = ~`Median Income`, y = ~GeoName, 
                     text = ~`Median Income`, 
                     hoverinfo = text) 
         %>% add_markers(frame = ~Industry, color = ~Sex)
         %>% layout()
)

edl4 <- (merged %>% dplyr::filter(Education=="Master's degree") 
         %>% plot_ly(x = ~`Median Income`, y = ~GeoName, 
                     text = ~`Median Income`, 
                     hoverinfo = text) 
         %>% add_markers(frame = ~Industry, color = ~Sex)
         %>% layout()
)

edl5 <- (merged %>% dplyr::filter(Education=="Earned doctorate") 
         %>% plot_ly(x = ~`Median Income`, y = ~GeoName, 
                     text = ~`Median Income`, 
                     hoverinfo = text) 
         %>% add_markers(frame = ~Industry, color = ~Sex)
         %>% layout()
)

