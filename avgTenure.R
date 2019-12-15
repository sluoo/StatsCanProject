library(tidyverse)
library(cansim)
library(stringr)
library(plotly)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

dta <- get_cansim(1410005101) %>% normalize_cansim_values()

#CleanData 
dta1 <- (dta
         %>% select(REF_DATE,GEO,`Job tenure`,
                    `Type of work`,Sex,`Age group`,VALUE)
         %>% filter(GEO=="Canada",
                    str_starts(`Job tenure`,"Average"),
                    str_starts(`Type of work`,"Full"),
                    str_detect(`Age group`, "to|65"),
                    str_detect(`Age group`,"44",negate = TRUE))
         %>% mutate(`Job tenure`=factor(`Job tenure`),Sex=factor(Sex),
                    REF_DATE=as.numeric(REF_DATE),`Age group`=factor(`Age group`),
                    VALUE=as.numeric(VALUE))
         %>% drop_na())

#Round to years 
dta2 <- data.frame(dta1 
         %>% group_by(REF_DATE,Sex)
         %>% summarise(mean_tenure = round((mean(VALUE))/12,1))
         %>% mutate(Sex = fct_relevel(Sex, c("Males","Both sexes","Females")))
         %>% rename(Year=REF_DATE,AvgTenure=mean_tenure))

#changed all to purple to better specify the half-way point 
plot <- print(dta2 %>% ggplot(aes(x=Year,y=AvgTenure,color=Sex))
              +geom_point()
              +geom_line()
              + labs(y="Average job tenure (Years)",x="Years",title = "Average job tenure in Canada by sex")
              + theme_minimal()
              + theme(title = element_text(size=9,face="bold"),
                      axis.title.x = element_text(size = 10,
                                                  face = "bold",vjust=0.2),
                      axis.title.y = element_text(size=9,
                                                  face="bold"),
                      axis.text.x = element_text(face="bold"),
                      axis.text.y = element_text(face="bold"))
              + scale_color_manual(values = c("#377eb8","#7e03a8","#cc4678")))
ggplotly(plot) 



###Problem with this is that age groups are mixed in...seperate this

dta3 <- data.frame(dta1 
         %>% group_by(REF_DATE,Sex,`Age group`)
         %>% summarise(mean_tenure = round((mean(VALUE))/12,1))
         %>% rename(Year=REF_DATE,AvgTenure=mean_tenure))



#display age and facet gender 
dta4 <- (dta3
         %>% filter(Sex !="Both sexes")
         %>% mutate(Age.group=fct_relevel(Age.group,
                                            c("65 years and over",
                                              "55 to 64 years",
                                              "25 to 54 years",
                                              "15 to 24 years"))))
#remove points to avoid crowding 
p <- print(dta4 %>% 
             ggplot(aes(x=Year,y=AvgTenure,color=Age.group))
           #+ geom_point()
           + geom_line(size=0.7)
           + labs(y="Average job tenure (Years)",x="Years")
           + theme_minimal()
           + theme(
                   axis.title.x = element_text(size =9,
                                               face = "bold"),
                   axis.title.y = element_text(size=9,
                                               face="bold"),
                   axis.text.x = element_text(face="bold"),
                   axis.text.y = element_text(face="bold"))
           + scale_color_manual(values = rev(c("#f89441","#cc4678","#7e03a8","#0d0887"))))


p1 <- print(p 
            + facet_grid(.~Sex)
            + theme(strip.text.x = element_text(size=9,face="bold"),
                    strip.background = element_rect(fill="white")))

ggplotly(p1, width=850, height=500)




          
         
         
         
    