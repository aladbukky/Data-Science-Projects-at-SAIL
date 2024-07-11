#Install and load the required packages
install.packages("janitor")
install.packages("tibble")
library(tibble)
library(tidyr)
library(ggplot2)
library(janitor)
library(dplyr)
library(skimr)
library(readr)
library('tabyl')
library(purrr)
if(!require(pacman)) install.packages("pacman")
pacman:: p_load(
  tidyverse, #meta package
  inspectdf,
  plotly,
  janitor,
  visdat,
  esquisse
)

#load the data set
ebola_sierra <- read.csv("D:/DDrive - Document/R_Tutoria_365/ebola_sierra_leone.csv")
ebola_sierra <- as.data.frame(ebola_sierra)
View(ebola_sierra)# view in tabular form

#Explore the data set to understand the structure, dimension and summary statistics
dim(ebola_sierra)
str(ebola_sierra)
# OR
glimpse(ebola_sierra) 
summary(ebola_sierra)
names(ebola_sierra)# get the names of the columns of the data set

#Cleaning the data set
#Viewing the summary shows that the age column has 4 missing values. It has reveals that the
#date column is not in appropriate format. 

#One way to clean the age column is to replace the missing value with the median age of the column, 
#wich is usually unaffected by outlier and to preserve the dataset observations, given the NAs is small. 
median_age <- median(ebola_sierra$age, na.rm = TRUE)
median_age

ebola_sierra$age[is.na(ebola_sierra$age)] <- median_age
#confirming if na's have been removed
any(is.na(ebola_sierra$age))
any(is.na(ebola_sierra))

#convert the date column from character format to date format

ebola_sierra$date_of_onset <- as.Date(ebola_sierra$date_of_onset, format = "%Y-%m-%d")
ebola_sierra$date_of_sample <- as.Date(ebola_sierra$date_of_sample, format = "%Y-%m-%d")
str(ebola_sierra)

#get the minimum and maximum age in the data set from the
ebola_sierra[which.min(ebola_sierra$age),]
ebola_sierra[which.max(ebola_sierra$age),]

#get the mean age of the female and male in the data set
mean(ebola_sierra$age[ebola_sierra$sex == "F"])
mean(ebola_sierra$age[ebola_sierra$sex == "M"])

#get the earliest time date of onset, the earliest date of discovery of the disease symptom and the latest of this data set

#min(ebola_sierra$date_of_onset)
#min(ebola_sierra$date_of_sample)
ebola_sierra[which.min(ebola_sierra$date_of_onset),]
ebola_sierra[which.max(ebola_sierra$date_of_sample),]

#plot the age distribution of the data set
age_freq <- ggplot(ebola_sierra, aes(x = age)) 
age_freq + geom_histogram(binwidth = 5,
                          fill ="red4",
                          colour ="darkgrey")+
  ggtitle("Age Frequency Distribution")+
  theme_classic()+
  labs(x = "Age", y ="Frequency")


#get the frequency of status by their sex

sex_status_count <- ebola_sierra %>%
  tabyl(sex, status) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")
  sex_status_count
  
#export the table for report
write.csv(sex_status_count, "Reports/sex_status_count.csv",
          row.names = F)
  
  #plot graph of the sex by status count
  ggplot(ebola_sierra, aes(x= sex, fill = status)) +
    geom_bar(position = "dodge")+
    labs(title = "Count of Status by Sex", x = "Sex", y = "Count", fill = "Status") +
    theme_minimal()
  

#get the district count 
district_count <- ebola_sierra %>% 
  tabyl(district) %>% 
  adorn_pct_formatting(digits = 0) %>% 
  arrange(desc(n))
district_count

#export the district_count table for report
write.csv(district_count, "Reports/district_count.csv",
          row.names = F)
#plot district count
ggplot(district_count, aes(x = reorder(district, n), y= n)) +
         geom_bar(stat ="identity", fill ="steelblue")+
         labs(title = "Frequency of Districts", x= "District" , y = "Frequency")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#get the district count by status
status_district_count <- ebola_sierra %>%
  tabyl(district, status) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting(digit = 0) %>% 
  adorn_ns(position = "front")
  
  status_district_count
  
  #export the district count by status table for report
  write.csv(status_district_count, "Reports/status_district_count.csv",
            row.names = F)
  
  # Plot showing the cases by district by status
  ggplot(data = ebola_sierra, aes(x = district, fill = factor(status))) +
    geom_bar() +
    theme_classic() +
    labs(title = "Ebola Cases by District", fill = "Status")+
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  #get the district by status and sex count
  distr.status.sex_count <- ebola_sierra %>%
    tabyl(sex, status, district)
  distr.status.sex_count
  
  #convert the list(distr.status.sex_count) to a data frame to combine into one dataframe
  combined_df <- bind_rows(
    lapply(names(distr.status.sex_count), function(district) {
      df <- distr.status.sex_count[[district]]
      df$district <- district
      df
    })
  )
  
  combined_df
  
# reshape the data into long format
  
  combined_long_df <- pivot_longer(combined_df, cols = -c(sex, district),
                                   names_to = "status", values_to = "count")
  combined_long_df
  
#plot the graph of district by sex and status
  
   ggplot(combined_long_df, aes(x = district, y = count, fill = status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ sex) +
    labs(title = "Distribution of Status by District and Sex",
         x = "District",
         y = "Count",
         fill = "Status") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  