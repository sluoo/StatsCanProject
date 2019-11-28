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

#plot job tenure
p <- print(ggplot(dta3, aes(x=fct_inorder(`Job tenure`),y=VALUE,fill=Sex)) 
      + geom_col(data= subset(dta2,Sex== "Males")) 
      + geom_col(data=subset(dta2, Sex== "Females"), aes(y=(VALUE)*-1))
      + scale_fill_brewer(palette = "Set1")
      + scale_y_continuous(breaks = c(-3e+06,-2e+06,-1e+06,0,1e+06,2e+06,3e+06),
                             label=c("3M","2M","1M","0","1M","2M","3M"))
      + geom_text(aes(x= label=paste(VALUE," ")),hjust=3)
      + annotate("text", x=1, y=1.5e+06, label="Males", size=4.5, color="#377eb8") 
      + annotate("text", x=1, y=-1.5e+06, label="Females", size=4.5, color="#e41a1c")
      + coord_flip()
      + labs(y="Population in Millions",x="Length of Employment",
             subtitle = "Length of Employment in Canada (1971-2018)")
      + theme_classic()
      + theme(legend.position = "none"))


pyrPlot <- (p
       + labs(title = 'Year:{frame_time}')
       + transition_time(REF_DATE) 
       + ease_aes('linear'))

animate(pyrPlot,fps = 2, height= 500, width=600) #slower


##I'm not sure if the plot above is very informative??
## It's more interesting to see job tenure and age group together
## rather than seperately 








