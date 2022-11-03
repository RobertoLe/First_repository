# Descripción: 
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-07
##########################D#####################################################

library(tidyr)

#urlPackage <- "https://cran.r-project.org/bin/macosx/big-sur-arm64/contrib/4.2/RMySQL_0.10.23.tgz"
#install.packages(urlPackage, repos=NULL, type="source") 

library(RMySQL)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

dbListFields(con, 'yr_2006')

yr_2006 <- dbGetQuery(con, "SELECT Date,Time,Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date,Time,Global_active_power, Global_reactive_power, Global_intensity, Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date,Time,Global_active_power, Global_reactive_power, Global_intensity, Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date,Time,Global_active_power, Global_reactive_power, Global_intensity, Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date,Time,Global_active_power, Global_reactive_power, Global_intensity, Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2010")


summary(yr_2006)
str(yr_2006)
head(yr_2006)
tail(yr_2006)

summary(yr_2007)
str(yr_2007)
head(yr_2007)
tail(yr_2007)

summary(yr_2008)
str(yr_2008)
head(yr_2008)
tail(yr_2008)

summary(yr_2009)
str(yr_2009)
head(yr_2009)
tail(yr_2009)

summary(yr_2010)
str(yr_2010)
head(yr_2010)
tail(yr_2010)

# rbind can help binding data frames of equal number of columns
# bind_rows can help bind data frames with unequal number of columns

df1 <- rbind(yr_2006,yr_2007,yr_2008,yr_2009,yr_2010)

summary(df1)
str(df1)
head(df1)
tail(df1)

library(dplyr)

df2 <- bind_rows(yr_2006,yr_2007,yr_2008,yr_2009,yr_2010)

summary(df1)
str(df1)
head(df1)
tail(df1)

## Combine Date and Time attribute values in a new attribute column
df1 <-cbind(df1,paste(df1$Date,df1$Time), stringsAsFactors=FALSE)


## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(df1)[10] <-"DateTime"

names(df1)

## Move the DateTime attribute within the dataset
df1 <- df1[,c(ncol(df1), 1:(ncol(df1)-1))]
head(df1)

library(tidyr)
## Convert DateTime from character to POSIXct 
df1$DateTime <- as.POSIXct(df1$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(df1$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(df1)

#install.packages("lubridate")

library(lubridate)

## Create "year" attribute with lubridate

df1$year <- year(df1$DateTime)

df1$quarter <- quarter(df1$DateTime)

df1$month <- month(df1$DateTime)

df1$week <- week(df1$DateTime)

df1$weekday <- weekdays.Date(df1$DateTime)

df1$day <- day(df1$DateTime)

df1$hour <- hour(df1$DateTime)

df1$minute <- minute(df1$DateTime)

# Description ######################################################################

#1.date: Date in format dd/mm/yyyy
#2.time: time in format hh:mm:ss
#3.global_active_power: household global minute-averaged active power (in kilowatt)
#4.global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#5.voltage: minute-averaged voltage (in volt)
#6.global_intensity: household global minute-averaged current intensity (in ampere)
#7.sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
# It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#8.sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
# It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#9.sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
# It corresponds to an electric water-heater and an air-conditioner.

#(global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3) 
# represents the active energy consumed every minute (in watt hour) in the 
# household by electrical equipment not measured in sub-meterings 1, 2 and 3.

# End Description ######################################################################

summary(df1)

hist(df1$Sub_metering_1)
hist(df1$Sub_metering_2)
hist(df1$Sub_metering_3)

# Sub_metering 1 spends on average (1.122) the least amount of power in watt-hour,
# but has the highest record (88) in watt-hour of active energy.
# Sub_metering 2 spends on average 1.299 in watt-hour of active energy and 
# has a maximum of 80.
# Sub_metering 3 spends on average 6.458 in watt-hour of active energy, 
# the highest average of active energy of all. However, it has the lowest 
# maximum of 31 in watt-hour of active energy. 

cor1 <- df1[,4:18]

cor1$weekday <- NULL

library(corrplot)
correlation <- cor(cor1)

corrplot(correlation, method = "circle")

# Standard deviation
sd(df1$Sub_metering_1)
sd(df1$Sub_metering_2)
sd(df1$Sub_metering_3)

# Coeficient of Variation
sd(df1$Sub_metering_1)/mean(df1$Sub_metering_1)
sd(df1$Sub_metering_2)/mean(df1$Sub_metering_2)
sd(df1$Sub_metering_3)/mean(df1$Sub_metering_3)
# The higher the coefficient of variation, 
# the greater the level of dispersion around the mean.

df1$activeEnergy <- with(df1,Global_active_power*1000/60 - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

names(df1)
summary(df1)




