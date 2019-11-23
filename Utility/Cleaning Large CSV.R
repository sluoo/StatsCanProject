#Sole purpose of script is to clean 2016 Census Sample Data (6GB CSV file)
#Extract small subset that is usable

big_source<- read_csv("E:/Documents/000 - Personal Files/1I - University/03 - MSc Statistics (McMaster)/02 Courses/Stats 744 - Data Visualization/Large Dataset for Final Project/98-400-X2016280_English_CSV_data.csv") 
smaller_dta <- (big_source 
                %>% filter(GEO_NAME %in% c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
                                           "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
                                           "Alberta", "British Columbia", "Yukon", "Northwest Territories", "Nunavut"))
                %>% filter(`Member ID: Highest certificate, diploma or degree (12)` %in% c(5, 7, 8, 11, 12))
                %>% filter(`Member ID: Location of study (5)`==1)
                %>% filter(`Member ID: Immigrant status (4A)`==1)
                %>% filter(`Member ID: Class of worker (3)`==1)
                %>% filter(`Member ID: Sex and age (18)` %in%  c(1, 7, 13))
                %>% filter(`Member ID: STEM and BHASE (non-STEM) groupings, Major field of study - Classification of Instructional Programs (CIP) 2016 (36)` 
                           %in% c(4, 8, 11, 15, 18, 21, 22, 25, 30, 31))
                %>% dplyr::select(c(4, 8, 20, 23, 28))
                %>% rename("GeoName"="GEO_NAME", 
                           "Education"="DIM: Highest certificate, diploma or degree (12)",
                           "Sex"="DIM: Sex and age (18)",
                           "Industry"="DIM: STEM and BHASE (non-STEM) groupings, Major field of study - Classification of Instructional Programs (CIP) 2016 (36)",
                           "Median Income"="Dim: Employment income statistics (7): Member ID: [3]: Median employment income ($)")
)

 #Rename factor
smaller_dta$Sex[which(smaller_dta$Sex=="Total - Sex and age")] <- "Both sexes"

#Write to smaller .csv file

write_csv(smaller_dta, path="E:/Documents/GitHub/StatsCan/wageData.csv")





