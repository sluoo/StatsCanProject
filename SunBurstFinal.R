library(tidyverse)
library(cansim)
library(stringr)
library(gganimate)
library(sunburstR)

#Function to round values and add prefix 
f2si_round<-function (number,rounding=F) 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  if (lut[ix]!=1) {
    if (rounding==T) {
      sistring <- paste(round(number/lut[ix],1), pre[ix])
    }
    else {
      sistring <- paste(number/lut[ix], pre[ix])
    } 
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}



dta <- get_cansim(1410005101) %>% normalize_cansim_values()

#CleanData 
dta1 <- (dta
         %>% select(REF_DATE,GEO,`Job tenure`,
                    `Type of work`,Sex,`Age group`,VALUE)
         %>% filter(GEO=="Canada",str_starts(`Job tenure`,"Total",negate = TRUE),
                    str_starts(`Job tenure`,"Average",negate = TRUE),
                    str_starts(`Type of work`,"Full"),
                    Sex != "Both sexes",
                    str_detect(`Age group`, "to|65"),
                    str_detect(`Age group`,"44",negate = TRUE))
         %>% mutate(`Job tenure`=factor(`Job tenure`),Sex=factor(Sex),
                    REF_DATE=as.integer(REF_DATE),`Age group`=factor(`Age group`),
                    VALUE=as.numeric(VALUE))
         %>% drop_na())


#Reorder the job tenure
dta2 <- (mutate(dta1, `Job tenure` = fct_relevel(dta1$`Job tenure`,
                                                 c("1 to 3 months",
                                                   "4 to 6 months",
                                                   "7 to 12 months",
                                                   "13 to 60 months",
                                                   "61 to 120 months",   
                                                   "121 to 240 months",
                                                  "241 months or more"))))

#Recoding labels for clarity
dta3 <- mutate(dta2, `Job tenure` = (dta2$`Job tenure`  
                                     %>% fct_recode(`Job Tenure: 1 to 3 months`="1 to 3 months",
                                                    `Job Tenure: 4 to 6 months` ="4 to 6 months",
                                                    `Job Tenure: 7 to 12 months`="7 to 12 months",
                                                    `Job Tenure: 1 to 5 years`="13 to 60 months",
                                                    `Job Tenure: 5 to 10 years`="61 to 120 months",
                                                    `Job Tenure: 10 to 20 years`="121 to 240 months",
                                                    `Job Tenure: 20 years plus`= "241 months or more")))

dta31 <- mutate(dta3, `Age group`= (dta3$`Age group` 
                            %>% fct_recode(`Age Group: 15 to 24 years`="15 to 24 years",
                            `Age Group: 25 to 54 years` = "25 to 54 years",
                            `Age Group: 55 to 64 years`="55 to 64 years",
                            `Age Group: 65 years and over`="65 years and over")))


dta4 <- data.frame((dta31 
                    %>% mutate(REF_DATE=factor(REF_DATE))
                    %>% select(`Sex`,`Job tenure`,REF_DATE,`Age group`,VALUE)
                    %>% unite(seq,`Sex`:`Age group`,sep="-")
                    %>% select(seq,VALUE)))



dta5 <- data.frame((dta31
                    %>% mutate(REF_DATE=factor(REF_DATE))
                    %>% select(`Sex`,`Job tenure`,REF_DATE,`Age group`,VALUE)
                    %>% mutate(VALUE1 = f2si_round(VALUE,rounding = TRUE))
                    %>% unite(seq,`Sex`:`Age group`,sep="-")
                    %>% select(seq,VALUE1)))


sunplot1 <- print(sund2b(dta4,
                  rootLabel = "Sex",
                  colors = htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory20b)"),
                  tooltip =  sund2bTooltip(followMouse = TRUE,
                                           html = htmlwidgets::JS("function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
                  ) 
))







