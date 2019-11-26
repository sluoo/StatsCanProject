library(tidyverse)
library(cansim)
library(stringr)
library(gganimate)
library(sunburstR)

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

#Recoding labels for clarity
dta2 <- mutate(dta1, `Job tenure` = (dta1$`Job tenure`  
                                     %>% fct_recode(`1 to 5 years`="13 to 60 months",
                 `5 to 10 years`="61 to 120 months",
                 `10 to 20 years`="121 to 240 months",
                 `20 years plus`= "241 months or more")))

#Reorder the job tenure
dta3 <- (mutate(dta2, `Job tenure` = fct_relevel(dta2$`Job tenure`,
                                                c("1 to 3 months",
                                                  "4 to 6 months",
                                                  "7 to 12 months",
                                                  "1 to 5 years",
                                                  "5 to 10 years",
                                                  "10 to 20 years",
                                                  "20 years plus"))))
         #%>% mutate(PERCENT = (VALUE/sum(VALUE)*100)))




#plot job tenure
p <- (ggplot(dta3, aes(x=fct_inorder(`Job tenure`),y=VALUE,fill=Sex)) 
      + geom_col(data= subset(dta2,Sex== "Males")) 
      + geom_col(data=subset(dta2, Sex== "Females"), aes(y=(VALUE)*-1))
      + scale_fill_brewer(palette = "Set1")
      + coord_flip()
      + theme_classic())

print(p)

#use geom_path and interpolation 
           

g <- (ggplot(dta3, aes(x=fct_inorder(`Age group`),y=VALUE,fill=Sex)) 
      + geom_col(data= subset(dta2,Sex== "Males")) 
      + geom_col(data=subset(dta2, Sex== "Females"), aes(y=(VALUE)*-1))
      + scale_fill_brewer(palette = "Set2")
      + coord_flip()
      + theme_classic())

p1 <- (p
       + labs(title = 'Year:{frame_time}')
       + transition_time(REF_DATE) 
       + ease_aes('linear'))

animate(p1, fps = 2) #slower

g1 <- (g
       + labs(title = 'Year:{frame_time}')
       + transition_time(REF_DATE) 
       + ease_aes('linear'))

animate(g1, fps = 2) #slower

##I'm not sure if the plots above are very informative??
## It's more interesting to see job tenure and age group together
## rather than seperately 



dta4 <- data.frame((dta3 
                    %>% mutate(REF_DATE=factor(REF_DATE))
                    %>% select(REF_DATE,`Sex`,`Job tenure`,`Age group`,VALUE)
                    %>% unite(seq,REF_DATE:`Age group`,sep="-")
                    %>% select(seq,VALUE)))



#This plot is better for this dataset I think
sunplot <- sund2b(dta4,
       rootLabel = "Year",
       colors = htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory20b)"),
       tooltip =  sund2bTooltip(followMouse = TRUE,
          html = htmlwidgets::JS("
function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
       ) 
)





