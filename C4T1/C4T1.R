G.JOHNSON
C4.T1
FEB-14-2022

#LOAD LIBRARIES
library(tidyverse)
library(RMariaDB) #Connect to SQL db
library(dplyr) # Used to create primary Data Frame; "bind_rows"
library(lubridate) # Used in preprocessing for Date & times

## Create a database connection
con = dbConnect(MariaDB(), user='deepAnalytics', password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the db
dbListTables(con)
#[1] "iris"    "yr_2006" "yr_2007" "yr_2008" "yr_2009" "yr_2010"

## List attributes contained in a table
dbListFields(con, 'iris')
#[1] "id"            "SepalLengthCm" "SepalWidthCm"  "PetalLengthCm"
#[5] "PetalWidthCm"  "Species" 

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

## List attributes associated with the yr_2006 table
dbListFields(con, 'yr_2006')
#[1] "id"                    "Date"                  "Time"                  "Global_active_power"   "Global_reactive_power"
#[6] "Global_intensity"      "Voltage"               "Sub_metering_1"        "Sub_metering_2"        "Sub_metering_3"   

## Use dbGetQuery function to download tables 2006-2010 w/ the specified attributes
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                        Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                        Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                        Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                        Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                        Sub_metering_3 FROM yr_2010")

## Investigate each dataframe
str(yr_2006)
#'data.frame':	21992 obs. of  5 variables:
#$ Date          : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
#$ Time          : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  1 1 2 1 1 2 1 1 1 2 ...
#$ Sub_metering_3: num  17 16 17 17 17 17 17 17 17 16 ...
summary(yr_2006)
#Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3 
#Length:21992       Length:21992       Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
#Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
#Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 0.00  
#Mean   : 1.249   Mean   : 2.215   Mean   : 7.41  
#3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.00  
#Max.   :77.000   Max.   :74.000   Max.   :20.00  
head(yr_2006)
#Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2006-12-16 17:24:00              0              1             17
#2 2006-12-16 17:25:00              0              1             16
#3 2006-12-16 17:26:00              0              2             17
#4 2006-12-16 17:27:00              0              1             17
#5 2006-12-16 17:28:00              0              1             17
#6 2006-12-16 17:29:00              0              2             17
tail(yr_2006)
#Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#21987 2006-12-31 23:54:00              0              0              0
#21988 2006-12-31 23:55:00              0              0              0
#21989 2006-12-31 23:56:00              0              0              0
#21990 2006-12-31 23:57:00              0              0              0
#21991 2006-12-31 23:58:00              0              0              0
#21992 2006-12-31 23:59:00              0              0              0

str(yr_2007)

summary(yr_2007)

head(yr_2007)

tail(yr_2007)

str(yr_2008)

summary(yr_2008)

head(yr_2008)

tail(yr_2008)

str(yr_2009)

summary(yr_2009)

head(yr_2009)

tail(yr_2009)

str(yr_2010)

summary(yr_2010)

head(yr_2010)

tail(yr_2010)
#Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#457389 2010-11-26 20:57:00              0              0              0
#457390 2010-11-26 20:58:00              0              0              0
#457391 2010-11-26 20:59:00              0              0              0
#457392 2010-11-26 21:00:00              0              0              0
#457393 2010-11-26 21:01:00              0              0              0
#457394 2010-11-26 21:02:00              0              0              0

## 2010 does not cover the full year

# Create Primary Data Frame (Multi-Year)

## Combine tables into one dataframe using dplyr
newDF <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)

#investigrate new Data Frame
str(newDF)
#data.frame':	2049280 obs. of  5 variables:
# $ Date          : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
# $ Time          : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  1 1 2 1 1 2 1 1 1 2 ...
# $ Sub_metering_3: num  17 16 17 17 17 17 17 17 17 16 ...
summary(newDF)
#Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Length:2049280     Length:2049280     Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
#Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 1.000  
#Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
#3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
#Max.   :88.000   Max.   :80.000   Max.   :31.000  
head(newDF) 
#Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#1 2006-12-16 17:24:00              0              1             17
#2 2006-12-16 17:25:00              0              1             16
#3 2006-12-16 17:26:00              0              2             17
#4 2006-12-16 17:27:00              0              1             17
#5 2006-12-16 17:28:00              0              1             17
#6 2006-12-16 17:29:00              0              2             17
tail(newDF)
#Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
#2049275 2010-11-26 20:57:00              0              0              0
#2049276 2010-11-26 20:58:00              0              0              0
#2049277 2010-11-26 20:59:00              0              0              0
#2049278 2010-11-26 21:00:00              0              0              0
#2049279 2010-11-26 21:01:00              0              0              0
#2049280 2010-11-26 21:02:00              0              0              0

##Preprocessing DateTime-Lubridate

# Combine Date and Time attribute values in a new attribute column
newDF <- cbind(newDF, paste(newDF$Date, newDF$Time), stringsAsFactors=FALSE)

# Give the new attribute in the 6th column a header name
### NOTE: if you downloaded more than 5 attributes you will need to change the column number
colnames(newDF) [6] <- "DateTime"

## Move the DateTime attribute within the dataset
DTdf <- newDF[,c(ncol(newDF), 1:(ncol(newDF) -1))]
head(DTdf)

##Convert DateTime from character to POSIXct
DTdf$DateTime <- as.POSIXct(DTdf$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(DTdf$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(DTdf)
#'data.frame':	2049280 obs. of  6 variables:
#$ DateTime      : POSIXct, format: "2006-12-16 18:24:00" "2006-12-16 18:25:00" "2006-12-16 18:26:00" "2006-12-16 18:27:00" ...
#$ Date          : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
#$ Time          : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
#$ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2: num  1 1 2 1 1 2 1 1 1 2 ...
#$ Sub_metering_3: num  17 16 17 17 17 17 17 17 17 16 ...

##Lubridate time
#Create "year" attribute with lubridate
DTdf$year <- year(DTdf$DateTime)
DTdf$quarter <- quarter(DTdf$DateTime)
DTdf$month <- month(DTdf$DateTime)
DTdf$week <- week(DTdf$DateTime)
#DTdf$weekday <- weekday(DTdf$DateTime)  #does not exist
DTdf$day <- day(DTdf$DateTime)
DTdf$hour <- hour(DTdf$DateTime)
DTdf$minute <- minute(DTdf$DateTime)

### Initial Exploration of the data
summary(DTdf)
#paste(newDF$Date, newDF$Time)     Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3      DateTime                        year         quarter         month             week            day             hour          minute    
#Length:2049280                Length:2049280     Length:2049280     Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :2006-12-16 18:24:00   Min.   :2006   Min.   :1.00   Min.   : 1.000   Min.   : 1.00   Min.   : 1.00   Min.   : 0.0   Min.   : 0.0  
#Class :character              Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:2007-12-10 06:37:45   1st Qu.:2007   1st Qu.:1.00   1st Qu.: 3.000   1st Qu.:13.00   1st Qu.: 8.00   1st Qu.: 5.0   1st Qu.:15.0  
#Mode  :character              Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 1.000   Median :2008-11-30 02:22:30   Median :2008   Median :2.00   Median : 6.000   Median :26.00   Median :16.00   Median :12.0   Median :30.0  
#Mean   : 1.122   Mean   : 1.299   Mean   : 6.458   Mean   :2008-12-02 01:59:44   Mean   :2008   Mean   :2.49   Mean   : 6.455   Mean   :26.29   Mean   :15.71   Mean   :11.5   Mean   :29.5  
#3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000   3rd Qu.:2009-11-23 21:31:15   3rd Qu.:2009   3rd Qu.:3.00   3rd Qu.: 9.000   3rd Qu.:39.00   3rd Qu.:23.00   3rd Qu.:18.0   3rd Qu.:45.0  
#Max.   :88.000   Max.   :80.000   Max.   :31.000   Max.   :2010-11-26 22:02:00   Max.   :2010   Max.   :4.00   Max.   :12.000   Max.   :53.00   Max.   :31.00   Max.   :23.0   Max.   :59.0  

str(DTdf)
#'data.frame':	2049280 obs. of  14 variables:
#$ paste(newDF$Date, newDF$Time): chr  "2006-12-16 17:24:00" "2006-12-16 17:25:00" "2006-12-16 17:26:00" "2006-12-16 17:27:00" ...
#$ Date                         : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
#$ Time                         : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
#$ Sub_metering_1               : num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2               : num  1 1 2 1 1 2 1 1 1 2 ...
#$ Sub_metering_3               : num  17 16 17 17 17 17 17 17 17 16 ...
#$ DateTime                     : POSIXct, format: "2006-12-16 18:24:00" "2006-12-16 18:25:00" "2006-12-16 18:26:00" "2006-12-16 18:27:00" ...
#$ year                         : num  2006 2006 2006 2006 2006 ...
#$ quarter                      : int  4 4 4 4 4 4 4 4 4 4 ...
#$ month                        : num  12 12 12 12 12 12 12 12 12 12 ...
#$ week                         : num  50 50 50 50 50 50 50 50 50 50 ...
#$ day                          : int  16 16 16 16 16 16 16 16 16 16 ...
#$ hour                         : int  18 18 18 18 18 18 18 18 18 18 ...
#$ minute                       : int  24 25 26 27 28 29 30 31 32 33 ...

# Change datatypes
DTdf$Date <- as.Date.numeric(DTdf$Date)
DTdf$Time <- as.Date.numeric(DTdf$Time)
DTdf$quarter <- as.Date.numeric(DTdf$quarter, origin = "2006-12-16")
DTdf$month <- as.Date.numeric(DTdf$month, origin = "2006-12-16")
DTdf$week <- as.Date.numeric(DTdf$week, origin = "2006-12-16")
DTdf$day <- as.Date.numeric(DTdf$day, origin = "2006-12-16")
DTdf$hour <- as.Date.numeric(DTdf$hour, origin = "2006-12-16")
DTdf$minute <- as.Date.numeric(DTdf$minute, origin = "2006-12-16")

summary(DTdf)
#DateTime                       Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3        year         quarter         month             week            day             hour          minute    
#Min.   :2006-12-16 18:24:00   Length:2049280     Length:2049280     Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :2006   Min.   :1.00   Min.   : 1.000   Min.   : 1.00   Min.   : 1.00   Min.   : 0.0   Min.   : 0.0  
#1st Qu.:2007-12-10 06:37:45   Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:2007   1st Qu.:1.00   1st Qu.: 3.000   1st Qu.:13.00   1st Qu.: 8.00   1st Qu.: 5.0   1st Qu.:15.0  
#Median :2008-11-30 02:22:30   Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 1.000   Median :2008   Median :2.00   Median : 6.000   Median :26.00   Median :16.00   Median :12.0   Median :30.0  
#Mean   :2008-12-02 01:59:44                                         Mean   : 1.122   Mean   : 1.299   Mean   : 6.458   Mean   :2008   Mean   :2.49   Mean   : 6.455   Mean   :26.29   Mean   :15.71   Mean   :11.5   Mean   :29.5  
#3rd Qu.:2009-11-23 21:31:15                                         3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000   3rd Qu.:2009   3rd Qu.:3.00   3rd Qu.: 9.000   3rd Qu.:39.00   3rd Qu.:23.00   3rd Qu.:18.0   3rd Qu.:45.0  
#Max.   :2010-11-26 22:02:00                                         Max.   :88.000   Max.   :80.000   Max.   :31.000   Max.   :2010   Max.   :4.00   Max.   :12.000   Max.   :53.00   Max.   :31.00   Max.   :23.0   Max.   :59.0  

str(DTdf)
#'data.frame':	2049280 obs. of  14 variables:
#$ paste(newDF$Date, newDF$Time): chr  "2006-12-16 17:24:00" "2006-12-16 17:25:00" "2006-12-16 17:26:00" "2006-12-16 17:27:00" ...
#$ Date                         : Date, format: NA NA NA NA ...
#$ Time                         : Date, format: NA NA NA NA ...
#$ Sub_metering_1               : num  0 0 0 0 0 0 0 0 0 0 ...
#$ Sub_metering_2               : num  1 1 2 1 1 2 1 1 1 2 ...
#$ Sub_metering_3               : num  17 16 17 17 17 17 17 17 17 16 ...
#$ DateTime                     : POSIXct, format: "2006-12-16 18:24:00" "2006-12-16 18:25:00" "2006-12-16 18:26:00" "2006-12-16 18:27:00" ...
#$ year                         : num  2006 2006 2006 2006 2006 ...
#$ quarter                      : Date, format: "2006-12-20" "2006-12-20" "2006-12-20" "2006-12-20" ...
#$ month                        : Date, format: "2006-12-28" "2006-12-28" "2006-12-28" "2006-12-28" ...
#$ week                         : Date, format: "2007-02-04" "2007-02-04" "2007-02-04" "2007-02-04" ...
#$ day                          : Date, format: "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
#$ hour                         : Date, format: "2007-01-03" "2007-01-03" "2007-01-03" "2007-01-03" ...
#$ minute                       : Date, format: "2007-01-09" "2007-01-10" "2007-01-11" "2007-01-12" ...

### Questions

#Which sub-meter is using the most power? Sub_metering_3 (electric water-heater and a/c)
# The least? Sub_metering_1 (kitchen, containing mainly a dishwasher, an oven and a microwave)
# Min - all have 0 for min which means no electricity was used
# Max - Sub_metering_1 and Sub_metering_2 are both 80+ and Sub_metering_3 is only 31; 1 and 2 use a lot more electricity but are more on demand, where as 3 is constant... think thermostat

##Propose three high-level recommendations you can suggest based on your initial exploration of the power consumption data
# 1. Adding household size would provide more insight into energy consumption (how many rooms)
# 2. Separating electric water heater and a/c would help customers conserve power better; the same can be said about appliances in other sub_metering groups
# 3. Adding smart sub_metering would enable customers to monitor their energy usage in real-time and lower their energy consumption





