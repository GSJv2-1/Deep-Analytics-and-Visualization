G.JOHNSON
C4.T2
FEB-18-2022

#LOAD LIBRARIES
library(RMariaDB) #Connect to SQL db
library(dplyr) # Used to create primary Data Frame; "bind_rows"
library(lubridate) # Used in preprocessing for Date & times
library(tidyverse)
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)

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
DTdf$weekDay <- weekdays(DTdf$DateTime)
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
#DTdf$Date <- as.Date.numeric(DTdf$Date)
#DTdf$Time <- as.Date.numeric(DTdf$Time)
DTdf$quarter <- as.numeric(DTdf$quarter)
#DTdf$month <- as.numeric(DTdf$month)
#DTdf$week <- as.numeric(DTdf$week)
DTdf$day <- as.numeric(DTdf$day)
DTdf$hour <- as.numeric(DTdf$hour)
DTdf$minute <- as.numeric(DTdf$minute)
DTdf$weekDay <- weekdays(DTdf$DateTime)
#DTdf$weekDay <- as.numeric(DTdf$weekDay)

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

#Remove Date and Time

DTdf2 <- select(DTdf, -c(2, 3))
str(DTdf2)

################################################################################
#                  Visualize the data
################################################################################

##Granularity
# Plot all of sub-meter 1
plot(DTdf2$Sub_metering_1)

##Subsetting and Meaningful Time Periods
# Subset the second week of 2008 - All observations
houseWeek <- filter(DTdf2, year == 2008 & week == 2)
# Plot subset houseWeek
plot(houseWeek$Sub_metering_1)
plot(houseWeek$Sub_metering_2)
plot(houseWeek$Sub_metering_3)


#Do Plotly for week 2 year 2008
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Visualize a Single Day with Plotly
## Subset the 9th day of January 2008 - All observations
houseDay <- filter(DTdf2, year == 2008 & month == 1 & day ==9)
# Plot sub meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of January 2008 - 10 Minute Frequency
houseDay10 <- filter(DTdf2, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 15th  week of 2008 - All observations
houseWeek142008 <- filter(DTdf2, year == 2008 & week == 15)
# Plot subset houseWeek
plot(houseWeek142008$Sub_metering_1)
plot(houseWeek142008$Sub_metering_2)
plot(houseWeek142008$Sub_metering_3)

#Do Plotly for week 15 year 2008
plot_ly(houseWeek142008, x = ~houseWeek142008$DateTime, y = ~houseWeek142008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek142008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek142008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Fifteenth Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of April 2008 - All observations
houseDay3 <- filter(DTdf2, year == 2008 & month == 4 & day ==9)
# Plot sub meter 1
plot_ly(houseDay3, x = ~houseDay3$DateTime, y = ~houseDay3$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay3, x = ~houseDay3$DateTime, y = ~houseDay3$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay3$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay3$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption April  9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of April  2008 - 10 Minute Frequency
houseDay10_3 <- filter(DTdf2, year == 2008 & month == 4 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10_3, x = ~houseDay10_3$DateTime, y = ~houseDay10_3$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10_3$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10_3$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption April 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 28th week of 2008 - All observations
houseWeek282008 <- filter(DTdf2, year == 2008 & week == 28)
# Plot subset houseWeek
plot(houseWeek282008$Sub_metering_1)
plot(houseWeek282008$Sub_metering_2)
plot(houseWeek282008$Sub_metering_3)

#Do Plotly for week 28 year 2008
plot_ly(houseWeek282008, x = ~houseWeek282008$DateTime, y = ~houseWeek282008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek282008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek282008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 28th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of July 2008 - All observations
houseDay2 <- filter(DTdf2, year == 2008 & month == 7 & day ==9)
# Plot sub meter 1
plot_ly(houseDay2, x = ~houseDay2$DateTime, y = ~houseDay2$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay2, x = ~houseDay2$DateTime, y = ~houseDay2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of July 2008 - 10 Minute Frequency
houseDay10_2 <- filter(DTdf2, year == 2008 & month == 7 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10_2, x = ~houseDay10_2$DateTime, y = ~houseDay10_2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10_2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10_2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 41st week of 2008 - All observations
houseWeek412008 <- filter(DTdf2, year == 2008 & week == 41)
# Plot subset houseWeek
plot(houseWeek412008$Sub_metering_1)
plot(houseWeek412008$Sub_metering_2)
plot(houseWeek412008$Sub_metering_3)


#Do Plotly for week 41 year 2008
plot_ly(houseWeek412008, x = ~houseWeek412008$DateTime, y = ~houseWeek412008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek412008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek412008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 41st Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of July 2008 - All observations
houseDay4 <- filter(DTdf2, year == 2008 & month == 10 & day ==9)
# Plot sub meter 1
plot_ly(houseDay4, x = ~houseDay4$DateTime, y = ~houseDay4$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay4, x = ~houseDay4$DateTime, y = ~houseDay4$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay4$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay4$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption October 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of October 2008 - 10 Minute Frequency
houseDay10_4 <- filter(DTdf2, year == 2008 & month == 10 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10_4, x = ~houseDay10_4$DateTime, y = ~houseDay10_4$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10_4$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10_4$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption October 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###############################################################################
#               2009
###############################################################################


##Subsetting and Meaningful Time Periods
# Subset the second week of 2009 - All observations
houseWeek2009 <- filter(DTdf2, year == 2009 & week == 2)
# Plot subset houseWeek
plot(houseWeek2009$Sub_metering_1)
plot(houseWeek2009$Sub_metering_2)
plot(houseWeek2009$Sub_metering_3)

#Do Plotly for week 2 year 2009
plot_ly(houseWeek2009, x = ~houseWeek2009$DateTime, y = ~houseWeek2009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Week of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#Visualize a Single Day with Plotly
## Subset the 9th day of January 2008 - All observations
houseDay_1_09 <- filter(DTdf2, year == 2009 & month == 1 & day ==9)
# Plot sub meter 1
plot_ly(houseDay_1_09, x = ~houseDay_1_09$DateTime, y = ~houseDay_1_09$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay_1_09, x = ~houseDay_1_09$DateTime, y = ~houseDay_1_09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay_1_09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay_1_09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of January 2009 - 10 Minute Frequency
houseDay1_09_10 <- filter(DTdf2, year == 2009 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay1_09_10, x = ~houseDay1_09_10$DateTime, y = ~houseDay1_09_10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay1_09_10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay1_09_10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 15th  week of 2008 - All observations
houseWeek152009 <- filter(DTdf2, year == 2009 & week == 15)
# Plot subset houseWeek
plot(houseWeek152009$Sub_metering_1)
plot(houseWeek152009$Sub_metering_2)
plot(houseWeek152009$Sub_metering_3)


#Do Plotly for week 15 year 2009
plot_ly(houseWeek152009, x = ~houseWeek152009$DateTime, y = ~houseWeek152009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek152009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek152009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Fifteenth Week of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of April 2009 - All observations
houseDay2_09 <- filter(DTdf2, year == 2009 & month == 4 & day ==9)
# Plot sub meter 1
plot_ly(houseDay2_09, x = ~houseDay2_09$DateTime, y = ~houseDay2_09$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay2_09, x = ~houseDay2_09$DateTime, y = ~houseDay2_09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay2_09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay2_09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption April  9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of April  2009 - 10 Minute Frequency
houseDay4_09_10 <- filter(DTdf2, year == 2009 & month == 4 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay4_09_10, x = ~houseDay4_09_10$DateTime, y = ~houseDay4_09_10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay4_09_10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay4_09_10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption April 9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 28th week of 2009 - All observations
houseWeek282009 <- filter(DTdf2, year == 2009 & week == 28)
# Plot subset houseWeek
plot(houseWeek282009$Sub_metering_1)
plot(houseWeek282009$Sub_metering_2)
plot(houseWeek282009$Sub_metering_3)


#Do Plotly for week 28 year 2009
plot_ly(houseWeek282009, x = ~houseWeek282009$DateTime, y = ~houseWeek282009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek282009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek282009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 28th Week of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of July 2009 - All observations
houseDay3_09 <- filter(DTdf2, year == 2009 & month == 7 & day ==10)
# Plot sub meter 1
plot_ly(houseDay3_09, x = ~houseDay3_09$DateTime, y = ~houseDay3_09$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay3_09, x = ~houseDay3_09$DateTime, y = ~houseDay3_09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay3_09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay3_09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 10th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of July 2009 - 10 Minute Frequency
houseDay7_09_10  <- filter(DTdf2, year == 2009 & month == 7 & day == 10 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay7_09_10, x = ~houseDay7_09_10$DateTime, y = ~houseDay7_09_10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay7_09_10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay7_09_10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption July 10th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 41st week of 2009 - All observations
houseWeek412009 <- filter(DTdf2, year == 2009 & week == 41)
# Plot subset houseWeek
plot(houseWeek412009$Sub_metering_1)
plot(houseWeek412009$Sub_metering_2)
plot(houseWeek412009$Sub_metering_3)


#Do Plotly for week 41 year 2009
plot_ly(houseWeek412009, x = ~houseWeek412009$DateTime, y = ~houseWeek412009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek412009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek412009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 41st Week of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of October 2009 - All observations
houseDay4_09 <- filter(DTdf2, year == 2009 & month == 10 & day ==9)
# Plot sub meter 1
plot_ly(houseDay4_09, x = ~houseDay4_09$DateTime, y = ~houseDay4_09$Sub_metering_1,
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2, and 3 with title, legend and labels - All observations
plot_ly(houseDay4_09, x = ~houseDay4_09$DateTime, y = ~houseDay4_09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay4_09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay4_09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption October 9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity

# Subset the 9th day of October 2008 - 10 Minute Frequency
houseDay10_09_10 <- filter(DTdf2, year == 2009 & month == 10 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10_09_10, x = ~houseDay10_09_10$DateTime, y = ~houseDay10_09_10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10_09_10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10_09_10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption October 9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Vizualize quarters
# Subset the second week of 2008 - All observations
houseQuarter12008 <- filter(DTdf2, year == 2008 & quarter == 1)
# Plot subset houseWeek
plot(houseQuarter12008$Sub_metering_1)
plot(houseQuarter12008$Sub_metering_2)
plot(houseQuarter12008$Sub_metering_3)


#Do Plotly for quarter 1 year 2008
plot_ly(houseQuarter12008, x = ~houseQuarter12008$DateTime, y = ~houseQuarter12008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter12008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter12008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption First Quarter of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the second quarter of 2008 - All observations
houseQuarter22008 <- filter(DTdf2, year == 2008 & quarter == 2)
# Plot subset houseQuarter22008
plot(houseQuarter22008$Sub_metering_1)
plot(houseQuarter22008$Sub_metering_2)
plot(houseQuarter22008$Sub_metering_3)

#Do Plotly for quarter 2 year 2008
plot_ly(houseQuarter22008, x = ~houseQuarter22008$DateTime, y = ~houseQuarter22008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter22008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter22008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Quarter of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the third quarter of 2008 - All observations
houseQuarter32008 <- filter(DTdf2, year == 2008 & quarter == 3)
# Plot subset houseQuarter
plot(houseQuarter32008$Sub_metering_1)
plot(houseQuarter32008$Sub_metering_2)
plot(houseQuarter32008$Sub_metering_3)


#Do Plotly for Q3 year 2008
plot_ly(houseQuarter32008, x = ~houseQuarter32008$DateTime, y = ~houseQuarter32008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter32008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter32008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Third Quarter of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the fourth quarter of 2008 - All observations
houseQuarter42008 <- filter(DTdf2, year == 2008 & quarter == 4)
# Plot subset houseQuarter
plot(houseQuarter42008$Sub_metering_1)
plot(houseQuarter42008$Sub_metering_2)
plot(houseQuarter42008$Sub_metering_3)


#Do Plotly for Q4 year 2008
plot_ly(houseQuarter42008, x = ~houseQuarter42008$DateTime, y = ~houseQuarter42008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter42008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter42008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Fourth Quarter of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

####2009 quarter

# Subset the first quarter of 2009 - All observations
houseQuarter12009 <- filter(DTdf2, year == 2009 & quarter == 1)
# Plot subset houseQuarter12009
plot(houseQuarter12009$Sub_metering_1)
plot(houseQuarter12009$Sub_metering_2)
plot(houseQuarter12009$Sub_metering_3)


#Do Plotly for Q1 year 2009
plot_ly(houseQuarter12009, x = ~houseQuarter12009$DateTime, y = ~houseQuarter12009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter12009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter12009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption First Quarter of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the second quarter of 2009 - All observations
houseQuarter22009 <- filter(DTdf2, year == 2009 & quarter == 2)
# Plot subset houseQuarter22009
plot(houseQuarter22009$Sub_metering_1)
plot(houseQuarter22009$Sub_metering_2)
plot(houseQuarter22009$Sub_metering_3)


#Do Plotly for Q2 year 2009
plot_ly(houseQuarter22009, x = ~houseQuarter22009$DateTime, y = ~houseQuarter22009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter22009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter22009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second Quarter of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the third quarter of 2009 - All observations
houseQuarter32009 <- filter(DTdf2, year == 2009 & quarter == 3)
# Plot subset houseQuarter
plot(houseQuarter32009$Sub_metering_1)
plot(houseQuarter32009$Sub_metering_2)
plot(houseQuarter32009$Sub_metering_3)


#Do Plotly for Q3 year 2009
plot_ly(houseQuarter32009, x = ~houseQuarter32009$DateTime, y = ~houseQuarter32009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter32009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter32009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Third Quarter of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the fourth quarter of 2009 - All observations
houseQuarter42009 <- filter(DTdf2, year == 2009 & quarter == 4)
# Plot subset houseQuarter
plot(houseQuarter42009$Sub_metering_1)
plot(houseQuarter42009$Sub_metering_2)
plot(houseQuarter42009$Sub_metering_3)


#Do Plotly for Q4 year 2009
plot_ly(houseQuarter42009, x = ~houseQuarter42009$DateTime, y = ~houseQuarter42009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter42009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter42009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Fourth Quarter of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset all of  2008 - All observations
houseYear08 <- filter(DTdf2, year == 2008)
# Plot subset houseQuarter
plot(houseYear08$Sub_metering_1)
plot(houseYear08$Sub_metering_2)
plot(houseYear08$Sub_metering_3)


#Do Plotly for year 2008
plot_ly(houseYear08, x = ~houseYear08$DateTime, y = ~houseYear08$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear08$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYear08$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption All of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset all of 2009 - All observations
houseYear09 <- filter(DTdf2, year == 2009)
# Plot subset houseQuarter
plot(houseYear09$Sub_metering_1)
plot(houseYear09$Sub_metering_2)
plot(houseYear09$Sub_metering_3)


#Do Plotly for year 2009
plot_ly(houseYear09, x = ~houseYear09$DateTime, y = ~houseYear09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYear09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption All of 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

################################################################################
#
#                Prepare to Analyize the Data
#
################################################################################

## Subset ton one observation per week on Mondays at 8:00pm for 2007, 2008, & 2009
house070809weekly <- filter(DTdf2, weekDay == "Monday" & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start = c(2007,1))

## Produce time series plots

## Plot sub-meter 3 with autoplot
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

######
# 2 more visualizations for Time Series
#####

#Sub-meter 1
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008, & 2009
#house070809weekly <- filter(DTdf2, weekDay == "Monday" & hour == 20 & minute == 1)

## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency = 52, start = c(2007,1))
## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency = 52, start = c(2007,1))

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809weekly)


################################################################################
#
#               Forecasting a time series
#
################################################################################

## Apply time series linear regression to the sub-meter 3(1 & 2) ts object and use
# summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)
#Residual standard error: 7.36 on 152 degrees of freedom
#Multiple R-squared:  0.2569,	Adjusted R-squared:  0.002673 
#F-statistic: 1.011 on 52 and 152 DF,  p-value: 0.4669
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season)
summary(fitSM2)
#Residual standard error: 6.943 on 152 degrees of freedom
#Multiple R-squared:  0.2359,	Adjusted R-squared:  -0.0255 
#F-statistic: 0.9024 on 52 and 152 DF,  p-value: 0.6589
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season)
summary(fitSM1)
#Residual standard error: 4.602 on 152 degrees of freedom
#Multiple R-squared:  0.2356,	Adjusted R-squared:  -0.02596 
#F-statistic: 0.9007 on 52 and 152 DF,  p-value: 0.6619

## Create the forecast for sub-meter 3(1 & 2). Forecast ahead 20 time periods
forecastfitSM3 <- forecast(fitSM3, h=20)
forecastfitSM2 <- forecast(fitSM2, h=20)
forecastfitSM1 <- forecast(fitSM1, h=20)
## Plot the forecast for sub-meter 3(1 & 2)
plot(forecastfitSM3)
plot(forecastfitSM2)
plot(forecastfitSM1)
## Create sub-meter 3(1 & 2) forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level = c(80,90))
forecastfitSM2c <- forecast(fitSM2, h=20, level = c(80,90))
forecastfitSM1c <- forecast(fitSM1, h=20, level = c(80,90))
## Plot sub-meter 3(1 & 2) forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0,20), ylab = "Watt-Hours", xlab = "Time", main = "Sub-meter 3 Forecast")
plot(forecastfitSM2c, ylim = c(0,20), ylab = "Watt-Hours", xlab = "Time", main = "Sub-meter 2 Forecast")
plot(forecastfitSM1c, ylim = c(0,20), ylab = "Watt-Hours", xlab = "Time", main = "Sub-meter 1 Forecast")

################################################################################
#
#               Decomposing a Seasonal Time Series
#
################################################################################

##Decomposition 

## Decompose Sub-meter 3(1 & 2) into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
components070809SM2weekly <- decompose(tsSM2_070809weekly)
components070809SM1weekly <- decompose(tsSM1_070809weekly)

## Plot decomposed sub-meter 3(1 & 2)
plot(components070809SM3weekly)
title(sub = "Sub-meter 3")
plot(components070809SM2weekly)
title(sub = "Sub-meter 2")
plot(components070809SM1weekly)
title(sub = "Sub-meter 1")

## Check summary statistics for decomposed sub-meter 3(1 & 2)
summary(components070809SM3weekly)
#         Length Class  Mode     
#x        205    ts     numeric  
#seasonal 205    ts     numeric  
#trend    205    ts     numeric  
#random   205    ts     numeric  
#figure    52    -none- numeric  
#type       1    -none- character
summary(components070809SM2weekly)
#         Length Class  Mode     
#x        205    ts     numeric  
#seasonal 205    ts     numeric  
#trend    205    ts     numeric  
#random   205    ts     numeric  
#figure    52    -none- numeric  
#type       1    -none- character
summary(components070809SM1weekly)
#         Length Class  Mode     
#x        205    ts     numeric  
#seasonal 205    ts     numeric  
#trend    205    ts     numeric  
#random   205    ts     numeric  
#figure    52    -none- numeric  
#type       1    -none- character

################################################################################
#
#                 Holt-Winters Forecasting
#
################################################################################

## Remove Seasonal Components

## Seasonal adjusting sub-meter 3(1 & 2) by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
plot(decompose(tsSM2_070809Adjusted))
plot(decompose(tsSM1_070809Adjusted))
##look at the scale for the seasonal section Sub-meter 3. -1e-15 through 5e-16. That's a decimal with 15 zeros before 1
##A very very small number indeed. For all practical purposes the seasonality has been removed. 

## Holt-Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta  = FALSE, gamma = FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta  = FALSE, gamma = FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta  = FALSE, gamma = FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))
## HoltWinters Forecast without seasonality and plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time - Sub-meter 3")
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time - Sub-meter 2")
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time - Sub-meter 1")

## Now change confidence levels and plot ONLY the forecasted area

# Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level = c(10,25))
## Plot ONLY the forcasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time - Sub-meter 3", start(2010))
# Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level = c(10,25))
## Plot ONLY the forcasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time - Sub-meter 2", start(2010))
# Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level = c(10,25))
## Plot ONLY the forcasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time - Sub-meter 1", start(2010))

