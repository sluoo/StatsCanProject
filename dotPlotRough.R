library(tidyverse)
library(cansim)
library(stringr)
library(gganimate)
library(sunburstR)
library(av)

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

h <- print(dta3 
      %>% ggplot(aes(x=`Job tenure`,y=VALUE, size=VALUE, color=`Age group`))
      + geom_jitter()
      + facet_grid(.~Sex) 
      + theme(axis.text.x = element_text(angle = 60, hjust = 1)))

dotPlot <- (h
            + labs(title = 'Year:{frame_time}')
            + transition_time(REF_DATE) 
            + ease_aes('linear')
            + shadow_trail(alpha=0.3))

dotPlot1 <- animate(dotPlot,fps = 3, height= 500, 
                    width=600) #renderer = av_renderer()) #slower

#slow down the movement of the dots 
#change alpha parameter of shadow_trail? 
#keep shadow_trail? I kinda like it,you can see the overall trend better
#Now that I look at it more, this plot is a lot more informative 
#than the other two. You can see more clearly the differences 
#between the gender and the majority job tenure bracket/group