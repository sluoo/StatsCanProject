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
  
  h2 <- print(dta3 
             %>% ggplot(aes(x=`Job tenure`,y=VALUE, size=0.1, color=`Age group`)) ## Fix size to a constant value
             + geom_point()
             + facet_grid(.~Sex) 
             + theme_minimal()
             + theme(axis.text.x = element_text(angle = 60, hjust = 1))
             + labs(y="Number of People with Logarithmic Scale", 
                    subtitle="Job Tenure in Canada (1976-2018) based Age Group")
             + scale_color_brewer(palette="Dark2")
             + scale_y_continuous(trans="log10", labels=scales::comma)
             #Add log scale for resolution near zero, force everything into US Standard notation 
             + guides(size=FALSE)) #hide size legend (seems a bit redundant)
  
  
  dotPlot_log <- (h2
              + labs(title = 'Year:{frame_time}')
              + transition_time(REF_DATE) 
              + ease_aes('linear')
              + shadow_trail(alpha=0.1)
              )
  
  dotPlot1_log <- animate(dotPlot_log,fps = 3, height= 600, 
                      width=800,renderer = av_renderer()) 
  
  anim_save("dotOutput_log.mp4",dotPlot1_log)
  
  #add a bit of width (4:3 aspect ratio is pretty standard for older stuff/projectors)
