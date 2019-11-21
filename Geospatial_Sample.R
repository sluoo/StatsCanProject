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
library(tidyr)
library(dplyr)

#Geospatial data related packages
library(maps)
library(raster)
library(leaflet)
library(ggmap)
library(sf)
library(geogrid)
library(htmlwidgets)
library(rgdal) #This seems to work better for me than sf for reading shape files when they need to be re-projected

#Current figure is demographic breakdown by region and industry


##Download and Read zip file
## Next step is to redirect it into a neater folder... sorry about that
## Many, many columns to rename...


## Zip File into Local Directory
download.file("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/edu-sco/Tables/Files/98-402-X2016010-T4-csv-eng.zip", destfile="statscan.zip")

## Read CSV (Note: Parsing failures are blank rows in the data)
## Put Fields of Study as Identifiers next to education level in a single column
## Collect Industry counts and Industry Percentages into one column each with labels
source <- (read_csv(unzip("statscan.zip", "98-402-X2016010-T4-CANPR-eng.csv")) 
           %>% rename("Freq"="Total – STEM and BHASE (non-STEM) groupings - Classification of Instructional Programs (CIP) 2016 (2016 counts)",
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
           %>% gather(c(9:18), key="Industry", value="Counts") 
           %>% gather(c(10:19), key="IndustryPercent", value="Percent")
           %>% dplyr::select(-c("Geographic code", "Global non-response rate", "Data quality flag", "TotalPercent", "IndustryPercent"))
)


## Load Shape File

canada_shape <- readOGR("Shape Files/lpr_000b16a_e.shp")

#Project shape file to latitude/longitude (scaling for map overlay if needed)
p3 <- spTransform(canada_shape, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

#Force transformed shape file into data frame so it can be put over the map as geom_map
#Will generate some warning messages if warnings are not suppressed, which don't seem to be relevant, 
#Don't think there is a better way to handle this
tidydta <- tidy(p3, group=group)

buffer <- 0.01        ## Add a small lat/long buffer to the edges of the map layer to make sure shape files aren't touching the edge
darkness_value <- 0.25 ## Transparency setting for scatter points
tint <- 0.4           ## Tinting for watercolor map and shape files (make things a little less vibrant so the data shows up better)

#Changed plot, forgot to update this part
#Will fill in sometime tomorrow
(ggplot(source)
  + geom_sf(aes(fill=),alpha=tint, map=tidydta, color="#404040")
+ scale_fill_viridis(name = "Industry", discrete=TRUE, option="plasma")
+ xlab("Longitude")
+ ylab("Latitude")
+ facet_wrap(~room_type, nrow=2, ncol=2)
+ guides(alpha=FALSE)
+ theme(legend.position=c(0.75, 0.25), 
        panel.spacing = unit(0.5, "lines"),
        axis.text = element_text(size=12),
        legend.key.size = unit(0.5, "in"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        strip.text.x = element_text(size=20),)
)

## Next steps:

## Possible breakdown by major metropolitan areas