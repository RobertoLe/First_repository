# Descripción: Visualization of and prediction of time series in C4T2
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-10
##########################D#####################################################
library(tidyr)
library(dplyr)
library(lubridate)

time_series <- read.csv("R Data Sets/Time_series.csv")

summary(time_series)
str(time_series)

time_series$DateTime <- as.POSIXct(time_series$DateTime, "%Y/%m/%d %H:%M:%S")

attr(time_series$DateTime, "tzone") <- "Europe/Paris"

str(time_series)

time_series$X <- NULL
#plot(time_series$Sub_metering_1)

# Visualizatins ######################################################################

## Subset the second week of 2008 - All Observations
houseWeek <- filter(time_series, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

houseWeek2 <- filter(time_series, year == 2008 & week == 2 & 
            (hour == 4 | hour == 8 | hour == 12 | hour == 16 | 
              hour == 20 | hour == 24))
plot_ly(houseWeek2, x = ~houseWeek2$DateTime, y = ~houseWeek2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2 of January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


library(plotly)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(time_series, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(time_series, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset the 30th of August 2010 in 10 min frequency
houseDay30 <- filter(time_series, year == 2010 & month == 8 & day == 30 & 
                     (minute == 0 | minute == 10 | minute == 20 | minute == 30 | 
                       minute == 40 | minute == 50))
plot_ly(houseDay30, x = ~houseDay30$DateTime, y = ~houseDay30$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay30$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay30$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 30th, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset week 35 of August 2010 in 4 hour frequency
houseWeek35 <- filter (time_series, year == 2010 & month== 8 & week == 35 &
                         (hour == 4 | hour == 8 | hour == 12 | hour == 16 | 
                            hour == 20 | hour == 24))
plot_ly(houseWeek35, x = ~houseWeek35$DateTime, y = ~houseWeek35$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek35$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek35$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 35 of August, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Subset data to year 2008
year2008 <- filter(time_series, year == 2008)

# Make data frame so it is easier to make the pie chart
data2008 <- data.frame("Energy_spent" = c(sum(year2008$Sub_metering_1),
                                          sum(year2008$Sub_metering_2),
                                          sum(year2008$Sub_metering_3),
                                          sum(year2008$activeEnergy)), 
                       "Categories" = c("Kitchen","Laundry","Water Heater & AC",
                                        "Rest of the House"))

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

fig <- plot_ly(data2008, labels = ~Categories, values = ~Energy_spent, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(Energy_spent, ' watt-hours'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)

fig <- fig %>% layout(title = 'Energy spent in the house on 2008',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig

dataDay10 <- data.frame("Energy_spent" = c(sum(houseDay10$Sub_metering_1),
                                          sum(houseDay10$Sub_metering_2),
                                          sum(houseDay10$Sub_metering_3),
                                          sum(houseDay10$activeEnergy)), 
                       "Categories" = c("Kitchen","Laundry","Water Heater & AC",
                                        "Rest of the House"))
fig2 <- plot_ly(dataDay10, labels = ~Categories, values = ~Energy_spent, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(Energy_spent, ' watt-hours'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)

fig2 <- fig2 %>% layout(title = 'Energy spent in the house on January 9, 2008',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig2

# hour 1 of January 9th 2008
hour_1 <- filter(time_series, year == 2008, month == 1, day == 9, hour == 7)
dataHour1 <- data.frame("Energy_spent" = c(sum(hour_1$Sub_metering_1),
                                           sum(hour_1$Sub_metering_2),
                                           sum(hour_1$Sub_metering_3),
                                           sum(hour_1$activeEnergy)), 
                        "Categories" = c("Kitchen","Laundry","Water Heater & AC",
                                         "Rest of the House"))

fig3 <- plot_ly(dataHour1, labels = ~Categories, values = ~Energy_spent, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste(Energy_spent, ' watt-hours'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE)

fig3 <- fig3 %>% layout(title = 'Energy spent in the house on 7 am January 9, 2008',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig3



hour_2 <- filter(time_series, year == 2008, month == 1, day == 9, hour == 14)
dataHour2 <- data.frame("Energy_spent" = c(sum(hour_2$Sub_metering_1),
                                           sum(hour_2$Sub_metering_2),
                                           sum(hour_2$Sub_metering_3),
                                           sum(hour_2$activeEnergy)), 
                        "Categories" = c("Kitchen","Laundry","Water Heater & AC",
                                         "Rest of the House"))

fig4 <- plot_ly(dataHour2, labels = ~Categories, values = ~Energy_spent, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste(Energy_spent, ' watt-hours'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE)

fig4 <- fig4 %>% layout(title = 'Energy spent in the house on 2 pm January 9, 2008',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig4


hour_3 <- filter(time_series, year == 2008, month == 1, day == 9, hour == 18)
dataHour3 <- data.frame("Energy_spent" = c(sum(hour_3$Sub_metering_1),
                                           sum(hour_3$Sub_metering_2),
                                           sum(hour_3$Sub_metering_3),
                                           sum(hour_3$activeEnergy)), 
                        "Categories" = c("Kitchen","Laundry","Water Heater & AC",
                                         "Rest of the House"))

fig5 <- plot_ly(dataHour3, labels = ~Categories, values = ~Energy_spent, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste(Energy_spent, ' watt-hours'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE)

fig5 <- fig5 %>% layout(title = 'Energy spent in the house on 6 pm January 9, 2008',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig5

# Create time series ######################################################################

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008, 2009,
# and 2010
house070809weekly <- filter(time_series, weekday == 'Monday' & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=60, start=c(2007,1))

tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))

tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 3, xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 3") +
  theme_classic()

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809weekly, ts.colour = 2, xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 2") +
  theme_classic()

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809weekly, ts.colour = 4, xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 1") +
  theme_classic()


# Forecasting ######################################################################

#urlPackage <-"https://cran.r-project.org/src/contrib/Archive/forecast/forecast_8.8.tar.gz"
#install.packages(urlPackage, repos=NULL, type="source") 

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
library(forecast)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


# sub 2 model
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)

forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

# sub 1 model
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)

forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")
  
summary(fitSM1)$r.squared
summary(fitSM2)$r.squared
summary(fitSM3)$r.squared

summary(fitSM1)
round(sqrt(deviance(fitSM1)/df.residual(fitSM1)),3)
round(sqrt(deviance(fitSM2)/df.residual(fitSM2)),3)
round(sqrt(deviance(fitSM3)/df.residual(fitSM3)),3)


r2data <- data.frame("Model"= c("Sub_meter_1","Sub_meter_2","Sub_meter_3"),
                     "R2" = c(summary(fitSM1)$r.squared,
                              summary(fitSM2)$r.squared,
                              summary(fitSM3)$r.squared),
                     "RSE" = c(round(sqrt(deviance(fitSM1)/df.residual(fitSM1)),3),
                               round(sqrt(deviance(fitSM2)/df.residual(fitSM2)),3),
                               round(sqrt(deviance(fitSM3)/df.residual(fitSM3)),3)))

df2 <- melt(r2data, id.vars='Model')

library(ggplot2)
ggplot(df2, aes(x=Model, y= value, fill=variable )) + 
  geom_bar(stat='identity', position='dodge')+
  coord_flip()+
  theme_classic()+
  geom_text(aes(label = round(value,2)),position = position_dodge(0.70))+
  ggtitle("RSE and R2 of the Models")
  
library(reshape2)
ggplot(r2data, aes(x=Model, y=RSE)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
  theme_classic()+
  ggtitle("RSE of the Models")

r2data


# Decomposition ######################################################################

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

# Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)
## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM2weekly)

# Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 1
summary(components070809SM1weekly)
summary(components070809SM1weekly$seasonal)
summary(components070809SM1weekly$trend)
summary(components070809SM1weekly$random)

summary(components070809SM2weekly$seasonal)
summary(components070809SM2weekly$trend)
summary(components070809SM2weekly$random)

summary(components070809SM3weekly$seasonal)
summary(components070809SM3weekly$trend)
summary(components070809SM3weekly$random)

# Removing seasonal components ######################################################################

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")


## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))



## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")


## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))



## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")


## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))


summary(tsSM1_070809Adjusted)
summary(tsSM2_070809Adjusted)
summary(tsSM3_070809Adjusted)


