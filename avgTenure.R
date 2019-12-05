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

dta2 <- data.frame(dta1 
         %>% group_by(REF_DATE,Sex)
         %>% summarise(mean_tenure = mean(VALUE))
         %>% mutate(Sex = fct_relevel(Sex, c("Males","Both sexes","Females"))))

# 
# (dta2 %>% plot_ly(x = ~REF_DATE,
#                  y = ~mean_tenure,
#                  split = ~Sex,
#                  #frame = ~REF_DATE,
#                  type = "scatter",
#                  mode = "lines+markers"))

plot <- print(dta2 %>% ggplot(aes(x=REF_DATE,y=mean_tenure,color=Sex))
              +geom_point()
              +geom_line()
              + labs(y="Average job tenure",x="Years",title = "Average job tenure in Canada by sex")
              + theme_minimal()
              + theme(title = element_text(size=10,face="bold"),
                      axis.title.x = element_text(size = 10,
                                                  face = "bold",vjust=0.2),
                      axis.title.y = element_text(size=10,
                                                  face="bold"),
                      axis.text.x = element_text(face="bold"),
                      axis.text.y = element_text(face="bold"))
              + scale_color_manual(values = c("#377eb8","#f89441","#cc4678")))
ggplotly(plot) 

###Problem with this is that age groups are mixed in...seperate this

dta3 <- data.frame(dta1 
         %>% group_by(REF_DATE,Sex,`Age group`)
         %>% summarise(mean_tenure = mean(VALUE)))

# #not good
# (dta3 %>% plot_ly(x = ~REF_DATE,
#                   y = ~mean_tenure,
#                   split = ~Age.group,
#                   #frame = ~REF_DATE,
#                   type = "scatter",
#                   mode = "lines+markers"))

#display age and facet gender 
dta4 <- (dta3 %>% 
           filter(Sex !="Both sexes"))

p <- print(dta4 %>% 
             ggplot(aes(x=REF_DATE,y=mean_tenure,color=Age.group))
           + geom_point()
           + geom_line()
           + labs(y="Average job tenure",x="Years")
           + theme_minimal()
           + theme(
                   axis.title.x = element_text(size = 10,
                                               face = "bold",vjust=0.2),
                   axis.title.y = element_text(size=10,
                                               face="bold"),
                   axis.text.x = element_text(face="bold"),
                   axis.text.y = element_text(face="bold"))
           + scale_color_manual(values = c("#f89441","#cc4678","#7e03a8","#0d0887")))


p1 <- print(p 
            + facet_grid(.~Sex)
            + theme(strip.text.x = element_text(size=10,face="bold"),
                    strip.background = element_rect(fill="white")))

ggplotly(p1)




          
         
         
         
    