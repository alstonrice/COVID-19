#
# read JHU covid-19 data
#
# 2020-04-17 lgreski 
# 2020-11-10 updated for 2 corrections of column names after 5/29/2020

# edit this line to reflect where data is stored on local machine 
directory <- "./csse_covid_19_data/csse_covid_19_daily_reports/"

# read all files in directory
theFiles <- list.files(path=directory,pattern="*.csv",full.names = TRUE)
filenames <- list.files(path=directory,pattern="*.csv")

# read and clean column information, add missing columns for earliest data
# for details, see ./R/analyzeColumnNamesByDay.R in github repository 
library(lubridate)

dataList <- lapply(1:length(theFiles),function(x){
  y <-read.csv(theFiles[x],stringsAsFactors=FALSE)
  # clean column names and add missing columns 
  fileDate <- mdy(substr(filenames[x],1,10))

    # common processing across all files
  colnames(y) <- gsub("\\.","_",colnames(y))
  colnames(y) <- sub("ï__","",colnames(y)) 

  # processing specific to first batch of files
  if(fileDate < mdy("02/01/2020")) {
    y$Province_State <- NA
    y$Lat <- NA
    y$Long_ <- NA
    y$Active <- NA
    y$Admin2 <- NA
    y$FIPS <- NA 
    y$Combined_Key <- NA 
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  } else if (fileDate < mdy("03/01/2020")){
    # cleaning specific to first wave of files
    y$Lat <- NA
    y$Long_ <- NA
    y$Active <- NA
    y$Admin2 <- NA
    y$FIPS <- NA 
    y$Combined_Key <- NA     
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  } else if(fileDate < mdy("03/22/2020")) {
       colnames(y) <- sub("Latitude","Lat",colnames(y))
       colnames(y) <- sub("Longitude","Long_",colnames(y))
       
       colnames(y) <- sub("Last.Update","Last_Update",colnames(y))
       colnames(y) <- sub("Province.State","Province_State",colnames(y))
       colnames(y) <- sub("Longitude","Long_",colnames(y))
       y$Active <- NA
       y$Admin2 <- NA 
       y$FIPS <- NA
       y$Combined_Key <- NA 
       y$Case_Fatality_Ratio <- NA
       y$Incident_Rate <- NA 
  } else if(fileDate < mdy("05/29/2020")){
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  }
  else if (fileDate <= mdy("11/09/2020")){
    colnames(y) <- sub("Case.Fatality_Ratio","Case_Fatality_Ratio",colnames(y))
    colnames(y) <- sub("Incidence_Rate","Incident_Rate",colnames(y))
    
  }
  # extract date from file name, assign to date column because
  # Last_Update field format varies day by day and a good programmer is 
  # a lazy programmer
  y$date <- substr(filenames[x],1,10)
  y
  })

# check number of columns, should be 15 for all files 
table(unlist(lapply(1:length(theFiles),function(x) length(names(dataList[[x]])))))

data <- do.call(rbind,dataList)
###Classified as a character
data$date <- as.Date(data$date)
###Change to a data
class(data$date)


# make directory & save the RDS
if(!dir.exists("./rdsData")) dir.create("./rdsData")
library(lubridate)
data$date <- mdy(data$date)
saveRDS(data,"./rdsData/covidData.rds")

#subset by date
system.time(data <- readRDS("./rdsData/covidData.rds"))

############################################################################################


#####    Start Running the CODE Here    ######

directory <- "./csse_covid_19_data/csse_covid_19_daily_reports/"

# read all files in directory
theFiles <- list.files(path=directory,pattern="*.csv",full.names = TRUE)
filenames <- list.files(path=directory,pattern="*.csv")

# read and clean column information, add missing columns for earliest data
# for details, see ./R/analyzeColumnNamesByDay.R in github repository 
library(lubridate)

dataList <- lapply(1:length(theFiles),function(x){
  y <-read.csv(theFiles[x],stringsAsFactors=FALSE)
  # clean column names and add missing columns 
  fileDate <- mdy(substr(filenames[x],1,10))
  
  # common processing across all files
  colnames(y) <- gsub("\\.","_",colnames(y))
  colnames(y) <- sub("ï__","",colnames(y)) 
  
  # processing specific to first batch of files
  if(fileDate < mdy("02/01/2020")) {
    y$Province_State <- NA
    y$Lat <- NA
    y$Long_ <- NA
    y$Active <- NA
    y$Admin2 <- NA
    y$FIPS <- NA 
    y$Combined_Key <- NA 
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  } else if (fileDate < mdy("03/01/2020")){
    # cleaning specific to first wave of files
    y$Lat <- NA
    y$Long_ <- NA
    y$Active <- NA
    y$Admin2 <- NA
    y$FIPS <- NA 
    y$Combined_Key <- NA     
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  } else if(fileDate < mdy("03/22/2020")) {
    colnames(y) <- sub("Latitude","Lat",colnames(y))
    colnames(y) <- sub("Longitude","Long_",colnames(y))
    
    colnames(y) <- sub("Last.Update","Last_Update",colnames(y))
    colnames(y) <- sub("Province.State","Province_State",colnames(y))
    colnames(y) <- sub("Longitude","Long_",colnames(y))
    y$Active <- NA
    y$Admin2 <- NA 
    y$FIPS <- NA
    y$Combined_Key <- NA 
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  } else if(fileDate < mdy("05/29/2020")){
    y$Case_Fatality_Ratio <- NA
    y$Incident_Rate <- NA 
  }
  else if (fileDate <= mdy("11/09/2020")){
    colnames(y) <- sub("Case.Fatality_Ratio","Case_Fatality_Ratio",colnames(y))
    colnames(y) <- sub("Incidence_Rate","Incident_Rate",colnames(y))
    
  }
  # extract date from file name, assign to date column because
  # Last_Update field format varies day by day and a good programmer is 
  # a lazy programmer
  y$date <- substr(filenames[x],1,10)
  y
})

# check number of columns, should be 15 for all files 
table(unlist(lapply(1:length(theFiles),function(x) length(names(dataList[[x]])))))

data <- do.call(rbind,dataList)
library(lubridate)
data$date <- mdy(data$date)

############################# Countries being checked#####
# clean the country names
data$Country_Region <- sub(" Azer","Azer",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub(", The","",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Dominica$","Dominican Republic",data$Country_Region)
data$Country_Region <- sub("g SAR","g",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub(" (Islamic Republic of)","",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Macao SAR","Macau",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Mainland ","",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("occupied Palestinian territory","Israel",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("West Bank and Gaza","Israel",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Palestine","Israel",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Republic of the Congo","Congo (Kinshasa)",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Republic of Korea","Korea, South",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Northern Ireland","Ireland",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Republic of ","",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Diamond Princess","Cruise Ship",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Russian Federation","Russia",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("South Korea","Korea, South",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("South Sudan","Sudan",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Summer Olympics 2020","Japan",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Taiwan*","Taiwan",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("The Gambia","Gambia",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("The Bahamas","Bahamas",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Taipei and environs","Taiwan",data$Country_Region,fixed = TRUE)
data$Country_Region <- sub("Winter Olympics 2022","China",data$Country_Region,fixed = TRUE)


# make directory & save the RDS
if(!dir.exists("./rdsData")) dir.create("./rdsData")
saveRDS(data,"./rdsData/covidData.rds")
