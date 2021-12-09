#install packages

install.packages("readxl")
install.packages("janitor")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")


#Reading source file having data in two separate sheets
library(readxl)
library(tidyverse)

mental_health_1 <- read_excel("survey.xlsx", sheet = "Sheet1", 
                              col_types = c("date", "numeric", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text"))
View(mental_health_1)


library(readxl)
mental_health_2 <- read_excel("survey.xlsx", sheet = "Sheet2", 
                              col_types = c("date", "numeric", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text"))
View(mental_health_2)

#Merging data from both the sheets
mental_health <- merge(mental_health_1, mental_health_2, all.x= TRUE, all.y = TRUE) 

view(mental_health)

#Looking for details like variable name, type etc
str(mental_health)

#Identifying missing values in the dataset
colSums(is.na(mental_health))

#Column Timestamp has incorrect values as well as missing values
#replacing those values with NA

library(dplyr)
mental_health$Timestamp <- as.character(mental_health$Timestamp) 
mental_health$Timestamp[is.na(mental_health$Timestamp)] <- " "
mental_health$Timestamp <- recode(mental_health$Timestamp, " " = "NA", 
                                  "1905-07-06 00:00:00" = "NA", 
                                  "1905-07-07 00:00:00" = "NA")

#drop rows with missing values 
library(tidyr)
mental_health_final <- mental_health %>% drop_na()

#rechecking missing values
colSums(is.na(mental_health_final))


#making the columns names format consistent with lowercase for columns
#Timestamp, Age, Gender, Country

names(mental_health_final)[1]<-"timestamp"
names(mental_health_final)[2]<-"age"
names(mental_health_final)[3]<-"gender"
names(mental_health_final)[4]<-"country"

#Check the Age distribution using Histogram
hist(mental_health_final$age)

#histogram shows the existence of extreme values in the variable "age"
#Since it is a study of working people, need to remove the age values 
#those are less than 15 and more than 80
library(tidyverse)
mental_health_final <- 
  mental_health_final[-c(which(mental_health_final$age > 80 | mental_health_final$age < 15)), ]

summary(mental_health_final$age)
hist(mental_health_final$age)

#Identifying unique values in the variable "gender"
unique(mental_health_final$gender)
#as we saw, there are many input values, will assign all male related gender as Male and female related gender as Female
#all other will be assigned as Others

library(stringr)
mental_health_final$gender <- tolower(mental_health_final$gender)
gender1 <- as.vector(mental_health_final$gender)

Female <- 
  c('female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail')

Male <-  
  c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle') 

Others <- 
  c('queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer', 'androgyne', 'agender', 'guy (-ish) ^_^', 'male leaning androgynous', 'neuter', 'queer', 'ostensibly male, unsure what that really means','a little about you','p','all', 'trans-female', 'trans woman', 'female (trans)','something kinda male?', '1','2')

gender1 <- sapply(as.vector(gender1), function(x) if(x %in% Male) "Male" else x)
gender1 <- sapply(as.vector(gender1), function(x) if(x %in% Female) "Female" else x)
gender1 <- sapply(as.vector(gender1), function(x) if(x %in% Others) "Others" else x)

#Update the gender variable 
mental_health_final$gender <- gender1

#create the frequency table of gender
table(mental_health_final$gender)


#dropping timestamp variable as there is no analysis required around date and time of inputs
mental_health_final <- mental_health_final[,-1]

#there are 5 entries as country "US", replacing it by "United States"
table(mental_health_final$country)
mental_health_final$country[mental_health_final$country=="US"]<-"United States"

#rechecking
table(mental_health_final$country)


#variable "no_employee" have 450 rows with incorrect values, 
#these values cannot be corrected as it should be one from below:
#"6-25", "26-100", "100-500", "500-1000", and "More than 1000".
unique(mental_health_final$no_employees)
table(mental_health_final$no_employees)

#in variable "treatment", replacing "N" with "No", "-" with "NA" and "Y" with "Yes" 
#there are only 2 values with "-", assuming that respondent wanted to replay NA

table(mental_health_final$treatment)

mental_health_final$treatment[mental_health_final$treatment=="N"]<-"No"
mental_health_final$treatment[mental_health_final$treatment=="-"]<-"NA"
mental_health_final$treatment[mental_health_final$treatment=="Y"]<-"Yes"

#rechecking
table(mental_health_final$treatment)


#variable "work_interfere", replacing "0" with "Never" as 0 implies negative response 
#there are only 2 observations with response as "0"
table(mental_health_final$work_interfere)

mental_health_final$work_interfere[mental_health_final$work_interfere=="0"]<-"Never"

#check if change reflects
table(mental_health_final$work_interfere)

#variable "remote_work", replacing "-" with "No" as "-" implies negative response 

table(mental_health_final$remote_work)

mental_health_final$remote_work[mental_health_final$remote_work=="-"]<-"No"

#checking if change reflects
table(mental_health_final$remote_work)


#in variable "phys_health_consequence", replacing "N" with "No" and "2" with "No" 
#in general, 1 is associated with Yes, and 0 with No, here since its not 1, treating 2 as No

table(mental_health_final$phys_health_consequence)

mental_health_final$phys_health_consequence[mental_health_final$phys_health_consequence=="N"]<-"No"
mental_health_final$phys_health_consequence[mental_health_final$phys_health_consequence=="2"]<-"No"

#checking if change reflects
table(mental_health_final$phys_health_consequence)


#now data is free from incorrect, missing and irrelevant values
#let's plot the bar chart for the participants from each country
library(ggplot2)
graph_country <- ggplot(mental_health_final, aes(country))
graph_country + geom_bar(width = 0.5) + 
  coord_flip() +
  theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5)) +
  labs(title="Countrywise distribution of the suvey participants")


#let's plot a Histogram for variable age`
qplot(mental_health_final$age,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Age", 
      xlab = "Age",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(10,100))

#Pie chart to see response if mental health interferes with work
table(mental_health_final$work_interfere)

library(dplyr)
library(ggplot2)
data <- data.frame(a=mental_health_final$work_interfere,b=1:1236)
data <- data %>% 
  group_by(a) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(a))
data$label <- scales::percent(data$per)

names(data)[1]<-"Work_interfere"

ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=Work_interfere), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))+
  labs (title="Do you feel that your mental health condition interferes with your work?")

#To export clean dataset "mental_health_final" to csv
write.csv(mental_health_final, "mental_health_clean_final.csv")

