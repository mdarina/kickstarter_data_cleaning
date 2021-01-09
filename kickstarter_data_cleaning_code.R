#Setting the working directory
setwd("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter")

#Installing packages
#install.packages("splitstackshape")

library(plyr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)

#DATA TO COMPLETE THE KICKSTARTER FILES TAKEN FROM:
#currency exchange: https://www.ofx.com/en-au/forex-news/historical-exchange-rates/yearly-average-rates/
#list of USA states: https://worldpopulationreview.com/states/state-abbreviations
#list of Canadian provinces: https://www.fintrac-canafe.gc.ca/reporting-declaration/docs/CountryCAN_e.xls

#WORKING WITH JSON DATA
#UNZIPPING JSON FILES
for(x in 2014:2015){
  mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",x,sep="/")
  zip_file <- list.files(path=mydir, pattern="*.zip", full.names=T)
  for(i in 1:length(zip_file)){
    dir.create(file.path(mydir, i))
    setwd(file.path(mydir, i))
    ldply(.data=zip_file[i], .fun=unzip, 
          exdir=paste(mydir,i,sep="/"))
  }
}

#READING JSON FILES WASN'T REALLY STRAIGHTFORWARD AS THEY HAD TO BE TREATED DIFFERENTLY
#READING 2014 WAS PRETTY STRAIGHTFORWARD. SO, I CREATED A LOOP TO READ THEM
year <- 2014
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",year,sep="/")
zip_file <- list.files(path=mydir, pattern="*.zip", full.names=T)
for(x in 1:length(zip_file)){
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",year, x,sep="/")
setwd(mydir)
  
#creating a variable that retrieves a month value from a file name
date_month <- substr(sub(".*/","",zip_file[x]), 18,19)

#getting the file name
file <- sub(".*/","",list.files(path = mydir, pattern="*.json", full.names=T))
  
#reading the JSON file
one <- fromJSON('Kickstarter_Kickstarter.json', simplifyVector = FALSE)
  
#unnesting lists (tidyr library)
kick <- tibble(data=one) %>% unnest_wider(data) %>% unnest_longer(projects) %>% unnest_wider(projects)
  
#extracting and renaming creator id
creator <- tibble(creator = kick$creator) %>% unnest_wider(creator)
creatorId = as.data.frame(creator$id)
kick$creatorId <-  creatorId$`creator$id`
  
#renaming variable state to condition (because state is a location)
names(kick)[names(kick) == "state"] <- "condition"
  
#extracting location data
localization <- tibble(localization = kick$location) %>% unnest_wider(localization)
city = as.data.frame(localization$name)
kick$city <- city$`localization$name`
region <- as.data.frame(localization$state)
kick$region = region$`localization$state`
  
#extracting categories and subcategories
cat <- tibble(category=kick$category) %>% unnest_wider(category)
subcat <- as.data.frame(cat$name)
kick$subcategoryName <- subcat$`cat$name`
  
kick$categories <- str_extract(cat$slug, "[[:alnum:]\\s&]+")
  
#binding data to create a complete set
kickstart <- as.data.frame(cbind(id = kick$id,
                                   creator_id = kick$creatorId,
                                   name = as.character(kick$name),
                                   blurb = as.character(kick$blurb),
                                   backers_count = kick$backers_count,
                                   goal = kick$goal,
                                   pledged = kick$pledged,
                                   currency = as.character(kick$currency),
                                   country = as.character(kick$country),
                                   city = as.character(kick$city),
                                   state = as.character(kick$region),
                                   created_at = kick$created_at,
                                   launched_at = kick$launched_at,
                                   deadline = kick$deadline,
                                   state_changed_at = kick$state_changed_at,
                                   category = str_to_sentence(kick$categories),
                                   subcategory = str_to_sentence(kick$subcategoryName),
                                   project_state = as.character(kick$condition),
                                   status = as.character(ifelse (kick$pledged>=kick$goal, "successful", "failed"))),
                             stringsAsFactors=F)
  
#adjusting, changing data types    
kickstart$created_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$created_at)), origin="1970-01-01"))
kickstart$deadline <- as.Date(as.POSIXct(as.numeric(as.character(kick$deadline)), origin="1970-01-01"))
kickstart$launched_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$launched_at)), origin="1970-01-01"))
kickstart$state_changed_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$state_changed_at)), origin="1970-01-01"))
kickstart$backers_count <- as.numeric(as.character(kickstart$backers_count))
kickstart$pledged <- as.numeric(as.character(kickstart$pledged))
kickstart$projectDays <- difftime(kickstart$deadline,kickstart$launched_at, units="days")
kickstart$goal <- as.numeric(as.character(kickstart$goal))
kickstart$dateScraped <- as.Date(paste(year, date_month, 01, sep="-"))
  
#saving the file
generated_date <- paste(year, date_month, 01, sep="-")
dates <- format(as.Date(generated_date), "%B")
a <- paste(dates, year, ".csv", sep="")
write.csv(kickstart, paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter",
                             a,sep="/"), row.names = FALSE)
}


#YEAR 2015 HAD JSON FILES SOME OF WHICH NEEDED ADDITIONAL TREATMENT
#APRIL 2015 WAS THE SAME AS 2014 FILES, I READ SEPARATELY
#THE JUNE AND AUGUST 2015 JSON FILES 
#HAD TO BE TREATED DIFFERENTLY BEFORE THEY COULD BE
#SAVED AS A CSV FILE

#I HAD TO OPEN THOSE JSON FILES WITH WINDOWS NOTEPAD AND
#DELETE [   ]   BRACKETS THAT WERE PRESENT IN SOME PROJECT
#DESCRIPTIONS AS THEY WERE CONFUSING R AND CAUSED THE ERROR BELOW
# Error: parse error: premature EOF
# [
#   (right here) ------^
    
#AND COMMAS HAD TO BE ADDED BETWEEN ALL OCCURENCES OF }{  "projects": [
#EXCEPT FOR THE FIRST LINE WHERE {  "projects": [ WAS USED
#TO AVOID THE ERROR BELOW
# Error: parse error: after array element, I expect ',' or ']'
# 7&ref=category&seed=2390189"}{ "projects": [  {   "id": 1699
#                      (right here) ------^
#I WENT INTO EACH JSON FILE AND GOT A COMMA INSERTED
#IN THE ABOVE MENTIONED LOCATIONS

#IN ADDITION, I WENT TO THE END OF THE FILE AND PRESSED ENTER
#TO ADD AN EMPTY ROW TO INDICATE THE END OF THE FILE

#A LOOP TO READ THE APRIL, JUNE & AUGUST 2015 FILES
year <- 2015
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",year,sep="/")
zip_file <- list.files(path=mydir, pattern="*.zip", full.names=T)
for(x in 1:3){
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",year, x,sep="/")
setwd(mydir)
  
#creating a variable that retrieves a month value from a file name
date_month <- substr(sub(".*/","",zip_file[x]), 18,19)
  
#getting the file name
file <- sub(".*/","",list.files(path = mydir, pattern="*.json", full.names=T))
  
#reading the JSON file
one <- fromJSON(file, simplifyVector = FALSE)
  
#unnesting lists (tidyr library)
kick <- tibble(data=one) %>% unnest_wider(data) %>% unnest_longer(projects) %>% unnest_wider(projects)
  
#extracting and renaming creator id
creator <- tibble(creator = kick$creator) %>% unnest_wider(creator)
creatorId = as.data.frame(creator$id)
kick$creatorId <-  creatorId$`creator$id`
  
#renaming variable state to condition (because state is a location)
names(kick)[names(kick) == "state"] <- "condition"
  
#extracting location data
localization <- tibble(localization = kick$location) %>% unnest_wider(localization)
city = as.data.frame(localization$name)
kick$city <- city$`localization$name`
region <- as.data.frame(localization$state)
kick$region = region$`localization$state`
  
#extracting categories and subcategories
cat <- tibble(category=kick$category) %>% unnest_wider(category)
subcat <- as.data.frame(cat$name)
kick$subcategoryName <- subcat$`cat$name`
  
kick$categories <- str_extract(cat$slug, "[[:alnum:]\\s&]+")
  
#binding data to create a complete set
kickstart <- as.data.frame(cbind(id = kick$id,
                                   creator_id = kick$creatorId,
                                   name = as.character(kick$name),
                                   blurb = as.character(kick$blurb),
                                   backers_count = kick$backers_count,
                                   goal = kick$goal,
                                   pledged = kick$pledged,
                                   currency = as.character(kick$currency),
                                   country = as.character(kick$country),
                                   city = as.character(kick$city),
                                   state = as.character(kick$region),
                                   created_at = kick$created_at,
                                   launched_at = kick$launched_at,
                                   deadline = kick$deadline,
                                   state_changed_at = kick$state_changed_at,
                                   category = str_to_sentence(kick$categories),
                                   subcategory = str_to_sentence(kick$subcategoryName),
                                   project_state = as.character(kick$condition),
                                   status = as.character(ifelse (kick$pledged>=kick$goal, "successful", "failed"))),
                             stringsAsFactors=F)
  
#adjusting, changing data types    
kickstart$created_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$created_at)), origin="1970-01-01"))
kickstart$deadline <- as.Date(as.POSIXct(as.numeric(as.character(kick$deadline)), origin="1970-01-01"))
kickstart$launched_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$launched_at)), origin="1970-01-01"))
kickstart$state_changed_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$state_changed_at)), origin="1970-01-01"))
kickstart$backers_count <- as.numeric(as.character(kickstart$backers_count))
kickstart$pledged <- as.numeric(as.character(kickstart$pledged))
kickstart$projectDays <- difftime(kickstart$deadline,kickstart$launched_at, units="days")
kickstart$goal <- as.numeric(as.character(kickstart$goal))
kickstart$dateScraped <- as.Date(paste(year, date_month, 01, sep="-"))
  
#saving the file
generated_date <- paste(year, date_month, 01, sep="-")
dates <- format(as.Date(generated_date), "%B")
a <- paste(dates, year, ".csv", sep="")
write.csv(kickstart, paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",
                             a,sep="/"), row.names = FALSE)
}


#IMPORTING THE LAST JSON 2015 FILE (OCTOBER)
#THIS FILE WAS DIFFERENT FROM ALL OTHER JSON FILES
#IT CONTAINED ONLY ROWS OF CODE, SO HAD TO BE READ VIA
#READLINES FUNCTION AT FIRST
year <- 2015
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",year,sep="/")
zip_file <- list.files(path=mydir, pattern="*.zip", full.names=T)
x<-4
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/json_files14_15",year, x,sep="/")
setwd(mydir)
  
#creating a variable that retrieves a month value from a file name
date_month <- substr(sub(".*/","",zip_file[x]), 18,19)
  
file <- sub(".*/","",list.files(path = mydir, pattern="*.json", full.names=T))
  
out <- lapply(readLines(file), fromJSON)
one <- tibble(data=out) %>% unnest_wider(data) %>% unnest_wider(data)%>% unnest_longer(projects) 
kick <- one$projects 

#extracting and renaming creator id
kick$creatorId <- kick$creator$id

#renaming variable state to condition (because state is a location)
names(kick)[names(kick) == "state"] <- "condition"
  
#extracting location data
kick$city <- kick$location$name
kick$region <- kick$location$state

#extracting categories and subcategories
kick$subcategoryName <- kick$category$name
kick$categories <- str_extract(kick$category$slug, "[[:alnum:]\\s&]+")
  
#binding data to create a complete set
kickstart <- as.data.frame(cbind(id = kick$id,
                                   creator_id = kick$creatorId,
                                   name = as.character(kick$name),
                                   blurb = as.character(kick$blurb),
                                   backers_count = kick$backers_count,
                                   goal = kick$goal,
                                   pledged = kick$pledged,
                                   currency = as.character(kick$currency),
                                   country = as.character(kick$country),
                                   city = as.character(kick$city),
                                   state = as.character(kick$region),
                                   created_at = kick$created_at,
                                   launched_at = kick$launched_at,
                                   deadline = kick$deadline,
                                   state_changed_at = kick$state_changed_at,
                                   category = str_to_sentence(kick$categories),
                                   subcategory = str_to_sentence(kick$subcategoryName),
                                   project_state = as.character(kick$condition),
                                   status = as.character(ifelse (kick$pledged>=kick$goal, "successful", "failed"))),
                             stringsAsFactors=F)
  
  #adjusting, changing data types    
  kickstart$created_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$created_at)), origin="1970-01-01"))
  kickstart$deadline <- as.Date(as.POSIXct(as.numeric(as.character(kick$deadline)), origin="1970-01-01"))
  kickstart$launched_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$launched_at)), origin="1970-01-01"))
  kickstart$state_changed_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$state_changed_at)), origin="1970-01-01"))
  kickstart$backers_count <- as.numeric(as.character(kickstart$backers_count))
  kickstart$pledged <- as.numeric(as.character(kickstart$pledged))
  kickstart$projectDays <- difftime(kickstart$deadline,kickstart$launched_at, units="days")
  kickstart$goal <- as.numeric(as.character(kickstart$goal))
  kickstart$dateScraped <- as.Date(paste(year, date_month, 01, sep="-"))
  
  #saving the file
  generated_date <- paste(year, date_month, 01, sep="-")
  dates <- format(as.Date(generated_date), "%B")
  a <- paste(dates, year, ".csv", sep="")
  write.csv(kickstart, paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter",
                             a,sep="/"), row.names = FALSE)
  
  
#JSON FILES TOOK REALLY LONG TO READ, SO CSV FILES WERE IMPORTED INSTEAD
#THE OCTOBER 2015 CSV WAS IN THE JSON FORMAT MOSTLY, SO
#I DOWNLOADED AND IMPORTED ITS JSON COUNTERPART INSTEAD.
#BEGINNING FROM NOVEMBER 2015, I IMPORTED CSV FILES
#ONE FILE TOOK ONLY UP TO 3 MINUTES TO READ AND WRITE

#WORKING WITH CSV DATA
#UNZIPPING ALL DOWNLOADED FILES USING A FOR LOOP
for(x in 2015:2020){
  mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter",x,sep="/")
  zip_file <- list.files(path=mydir, pattern="*.zip", full.names=T)
  for(i in 1:length(zip_file)){
    dir.create(file.path(mydir, i))
    setwd(file.path(mydir, i))
    ldply(.data=zip_file[i], .fun=unzip, 
          exdir=paste(mydir,i,sep="/"))
  }
}


#READING, CLEANING AND MERGING SETS FOR YEARS: 2015-2020
#USING A FOR LOOP
for(year in 2015:2020){
  mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter",year,sep="/")
  zip_file <- list.files(path=mydir, pattern="*.zip", full.names=T)
for(x in 1:length(zip_file)){
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter",year, x,sep="/")
setwd(mydir)

#creating a variable that retrieves a month value from a file name
date_month <- substr(sub(".*/","",zip_file[x]), 18,19)

#reading csv files in batch
kick <- do.call(rbind, lapply(list.files(path = mydir, pattern="*.csv", full.names=T), read.csv))

#retrieving needed values, cleaning the data
kick$categories <- str_extract(sub(".*\"slug\"","",kick$category), "[[:alnum:]\\s&]+")
kick$subcategoryName <- str_extract(sub(".*\"name\"","", kick$category), "[[:alnum:]\\s&]+")

kick$creatorId <- str_extract(sub(".*\"id\"","", kick$creator), "\\d+")
kick$condition <- str_extract(sub(".*\"state\"","", kick$profile),"[[:alnum:]\\s&]+")

kick$city <- str_extract(sub(".*\"name\"","", kick$location),"[[:alnum:]\\s&]+")
kick$region <- str_extract(sub(".*\"state\"","", kick$location),"[[:alnum:]\\s&]+")

#binding the data and creating a set
kickstart <- as.data.frame(cbind(id = kick$id,
                                 creator_id = kick$creatorId,
                                 name = as.character(kick$name),
                                 blurb = as.character(kick$blurb),
                                 backers_count = kick$backers_count,
                                 goal = kick$goal,
                                 pledged = kick$pledged,
                                 currency = as.character(kick$currency),
                                 country = as.character(kick$country),
                                 city = as.character(kick$city),
                                 state = as.character(kick$region),
                                 created_at = kick$created_at,
                                 launched_at = kick$launched_at,
                                 deadline = kick$deadline,
                                 state_changed_at = kick$state_changed_at,
                                 category = str_to_sentence(kick$categories),
                                 subcategory = str_to_sentence(kick$subcategoryName),
                                 project_state = as.character(kick$condition),
                                 status = as.character(ifelse (kick$pledged>=kick$goal, "successful", "failed"))),
                           stringsAsFactors=F)

#adjusting, changing data types    
kickstart$created_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$created_at)), origin="1970-01-01"))
kickstart$deadline <- as.Date(as.POSIXct(as.numeric(as.character(kick$deadline)), origin="1970-01-01"))
kickstart$launched_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$launched_at)), origin="1970-01-01"))
kickstart$state_changed_at <- as.Date(as.POSIXct(as.numeric(as.character(kick$state_changed_at)), origin="1970-01-01"))
kickstart$backers_count <- as.numeric(as.character(kickstart$backers_count))
kickstart$pledged <- as.numeric(as.character(kickstart$pledged))
kickstart$projectDays <- difftime(kickstart$deadline,kickstart$launched_at, units="days")
kickstart$goal <- as.numeric(as.character(kickstart$goal))
kickstart$dateScraped <- as.Date(paste(year, date_month, 01, sep="-"))

#saving the file
generated_date <- paste(year, date_month, 01, sep="-")
dates <- format(as.Date(generated_date), "%B")
a <- paste(dates, year, ".csv", sep="")
write.csv(kickstart, paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter",
                           a,sep="/"), row.names = FALSE)
}
}


#FILES CREATED IN TOTAL
#setting a new directory
mydir <- paste("D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter")

#creating a list
list <- list.files(path=mydir, pattern="*.csv", full.names=T)

#getting the number of files
length(list)

#MERGING ALL THE FILES TO CREATE A MASTER FILE
kickstarter2014_2020 <- do.call(rbind, lapply(list.files(path = mydir, pattern="*.csv", full.names=T), read.csv))
# glimpse(kickstarter2014_2020)

#Changing the date variable to class date
kickstarter2014_2020$dateScraped <- as.Date(kickstarter2014_2020$dateScraped)

#Sorting the set by projectId and date
newSet  <- kickstarter2014_2020[order(kickstarter2014_2020$id, 
                                  kickstarter2014_2020$dateScraped, decreasing = TRUE),]

head(newSet$id)

#Keeping rows that are not duplicated
kickAll<- newSet[!duplicated(newSet$id), ]

head(kickAll)

#Saving the file for further work!!! 
write.csv(kickAll, 
          "D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/kickAll.csv", 
          row.names = FALSE)

# #Full file
dim(kickstarter2014_2020)
# #File without duplicates
dim(kickAll)
# #Difference in the number of observations
sum(table(kickstarter2014_2020$state)) - sum(table(kickAll$state))

#READING THE KICKSTARTER DATA (MASTER FILE)
kickstarter_data <- read.csv("kickAll.csv", stringsAsFactors = F)

#ADDING CURRENCY EXCHANGE VALUES TO THE KICKSTARTER DATA
#IMPORTING A CSV FILE
currency <- read.csv("D:\\dasha\\Documents\\Concordia\\SASUniversityEdition\\sas_code\\portfolio\\kickstarter\\extra_files\\currency.csv",
                     stringsAsFactors = F)

#Renaming the date variable
names(currency)[1] <- "date"
head(currency)
glimpse(currency)

#Pivoting the currency table to get the column names into the column levels
currency_pivot <- currency %>% pivot_longer(-date, names_to="currency", values_to="rate", values_drop_na=TRUE)

#Changing the date format
currency_pivot$date <- dmy(currency_pivot$date)

#Creating a key - currency date
year <- year(currency_pivot$date)
date_month <- month(currency_pivot$date)
generated_date <- paste(year, date_month, 01, sep="-")
currency_pivot$currency_date <- as.Date(generated_date)
currency_pivot <- currency_pivot[,-1]

#create a date that starts with the first day of the month
#will be used as a part of the composite key
#together with the currency exchnage
kickstarter_data$state_changed_at <- as.Date(kickstarter_data$state_changed_at)
kickstarter_data$created_at <- as.Date(kickstarter_data$created_at)
kickstarter_data$launched_at <- as.Date(kickstarter_data$launched_at)
kickstarter_data$deadline <- as.Date(kickstarter_data$deadline)

#Generate the date based on the launched at date
year <- year(kickstarter_data$launched_at)
date_month <- month(kickstarter_data$launched_at)
generated_date <- paste(year, date_month, 01, sep="-")
kickstarter_data$currency_date <- as.Date(generated_date)

#Looking at the data structure
glimpse(kickstarter_data)


#MERGING TWO SETS
kickstarter_all <- kickstarter_data %>% left_join(currency_pivot, by = c("currency_date", "currency"))
glimpse(kickstarter_all)

kickstarter_all$goalUSD <- round(kickstarter_all$goal*kickstarter_all$rate,2)
kickstarter_all$pledgedUSD <- round(kickstarter_all$pledged*kickstarter_all$rate,2)
kickstarter_all$pledgedOverGoal <- round(kickstarter_all$pledgedUSD/kickstarter_all$goalUSD,4)
kickstarter_all$pledgedPerBacker <- round(kickstarter_all$pledgedUSD/kickstarter_all$backers_count)


#SAVING THE FINAL SET - WITH THE USD DATA
write.csv(kickstarter_all, 
          "D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/kickFull.csv", 
          row.names = FALSE)


kickFull <- read.csv("kickFull.csv", stringsAsFactors = F)

glimpse(kickFull)


#CREATE A SET FOR ANALYSIS
#SUBSETTING THE FOLLOWING WAY:
#- CUT OFF DEADLINE DATE - BEFORE THE FINAL SET WAS SCRAPED
#- NOT CANCELED AND NOT SUSPENDED
#Subsetting the data to only contain
#failed and successful projects and that are currently inactive for model building

kickSuccess <- subset(kickFull, deadline < as.Date(paste(2020, 12, 17, sep="-")) &
                        project_state != "canceled" & project_state != "suspended")

#Checking if subsetting worked
table(kickSuccess$project_state)

#Saving the file for further reference - all the work will be done on this file
write.csv(kickSuccess, 
          "D:/dasha/Documents/Concordia/SASUniversityEdition/sas_code/portfolio/kickstarter/kickSuccess.csv", 
          row.names = FALSE)
