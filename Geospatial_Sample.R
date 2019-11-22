library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
theme_classic()
theme_update(panel.spacing = grid::unit(0, "lines"))
library(directlabels)
library(cowplot)
library(scales)
library(MASS)
library(splines)
library(viridis)
library(broom)
library(gganimate)
library(RColorBrewer)
library(plotly)

#Geospatial data related packages
library(maps)
library(raster)
library(leaflet)
library(ggmap)
library(sf)
library(geogrid)
library(htmlwidgets)
library(rgdal) #This seems to work better for me than sf for reading shape files when they need to be re-projected

#Current figure is gender breakdown by region and industry
#Create multiple facet plots in question
#Interactive element omitted - too difficult.

##Download and Read zip file into local director
## Next step is to redirect it into a neater folder... sorry about that
download.file("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/edu-sco/Tables/Files/98-402-X2016010-T4-csv-eng.zip", destfile="statscan.zip")

## Unzip and read CSV (Note: Parsing failures are blank rows in the data)
## Many, many columns to rename...
## Drop useless columns and NA rows
source <- read_csv(unzip("statscan.zip", "98-402-X2016010-T4-CANPR-eng.csv")) 
source_renamed <- (source %>% rename("GeoName"="Geographic name", 
                                   "AreaTotal"="Total – STEM and BHASE (non-STEM) groupings - Classification of Instructional Programs (CIP) 2016 (2016 counts)",
                                   "SciTech"="STEM fields of study - Science and science technology (2016 counts)",
                                   "EngTech"="STEM fields of study - Engineering and engineering technology (2016 counts)",
                                   "MathCS"="STEM fields of study - Mathematics and computer and information science (2016 counts)",
                                   "BizAdmin"="BHASE (non-STEM) fields of study - Business and administration (2016 counts)",
                                   "Artsy"="BHASE (non-STEM) fields of study - Arts and humanities (2016 counts)",
                                   "SocSci"="BHASE (non-STEM) fields of study - Social and behavioural sciences (2016 counts)",
                                   "Legal"="BHASE (non-STEM) fields of study - Legal professions and studies (2016 counts)",
                                   "Health"="BHASE (non-STEM) fields of study - Health care (2016 counts)",
                                   "EdTeach"="BHASE (non-STEM) fields of study - Education and teaching (2016 counts)",
                                   "Trade"="BHASE (non-STEM) fields of study - Trades, services, natural resources and conservation (2016 counts)",
                                   "TotalPercent"="Total – STEM and BHASE (non-STEM) groupings - Classification of Instructional Programs (CIP) 2016 (% distribution 2016)"
)
%>% drop_na()
%>% dplyr::select(-c(19:29)))

###############################################

#There may be a way to simplify this...

##Numbers for both genders
bothsexes <- (source_renamed %>% gather(c(9:18), key="Industry", value="TotalGenderCount") 
%>% filter(GeoName!="Canada")
%>% filter(Age=="All ages, 15-plus")
%>% filter(Sex== "Both sexes")
%>% dplyr::select(-c("Geographic code", "Sex", "Global non-response rate", "Data quality flag"))
)

##Numbers for females only
females <- (source_renamed %>% gather(c(9:18), key="Industry", value="FemaleGenderCount") 
%>% filter(GeoName!="Canada")
%>% filter(Age=="All ages, 15-plus")
%>% filter(Sex== "Female")
%>% dplyr::select(-c("Geographic code", "Sex", "AreaTotal", "Global non-response rate", "Data quality flag"))
)

##Recombine Tables and Calculate Female Proportions
##Filter out Most Frequent Industries per Jurastiction
femaleprop <- (left_join(bothsexes,females) 
               %>% mutate(FemalePercent = 100*(FemaleGenderCount/TotalGenderCount))
)

####################################################

##Create Five Tables For Different Education Levels
##Within each table, extract top value for each province
##Not ideal but this level of striation requires some drastic measures to clean

College <- (femaleprop %>% filter(Education=="College, CEGEP or other non-university certificate or diploma") 
            %>% group_by(GeoName) %>% slice(which.max(TotalGenderCount)))
UniCert <- (femaleprop %>% filter(Education=="University certificate, diploma or degree at bachelor level or above")
            %>% group_by(GeoName) %>% slice(which.max(TotalGenderCount)))
Bach <- (femaleprop %>% filter(Education=="Bachelor's degree")
         %>% group_by(GeoName) %>% slice(which.max(TotalGenderCount)))
Master <- (femaleprop %>% filter(Education=="Master's degree")
           %>% group_by(GeoName) %>% slice(which.max(TotalGenderCount)))
PhD <- (femaleprop %>% filter(Education=="Earned doctorate")
        %>% group_by(GeoName) %>% slice(which.max(TotalGenderCount)))
 
dtafin <- bind_rows(College, UniCert, Bach, Master, PhD, id=NULL)
 
## Load Shape File, Join Shape File with Data (it works reasonably now even on my laptop)
## Major change is the shape file used - less precise but reduces render time to seconds from minutes
canada_shape <- st_read("Shape Files/Digital Boundary/lpr_000a16a_e.shp")
joined_dta <- left_join(canada_shape, dtafin, by=c("PRENAME"="GeoName"))


##SciTech_Male

#Changed plot, forgot to update this part
#Will fill in sometime tomorrow

#p3 <- spTransform(canada_shape, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
#tidydta <- tidy(p3, group=group)

(ggplot(joined_dta)
  + geom_sf(aes(fill=Industry, geometry=geometry, alpha=FemalePercent))
  + scale_fill_viridis(name = "Industry", discrete=TRUE, option="plasma")
  + scale_alpha_continuous(name = "Percent Female", range=c(0.5, 1))
  + xlab("Longitude")
  + ylab("Latitude")
  + facet_wrap(~Education)
  + theme_classic()
  + theme(legend.position=c(0.83, 0.25), 
          panel.spacing = unit(0.5, "lines"),
          axis.text = element_text(size=12),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10),
          strip.text.x = element_text(size=10),)
)

## Next steps:

## Possible breakdown by major metropolitan areas