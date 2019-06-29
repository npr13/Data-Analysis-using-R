install.packages("xlsx") 
library("xlsx")
install.packages("ggplot2")
#install.packages("ggplot")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("reshape2")
library(reshape2)

#data_collection
water2011 <- read.xlsx('water_consumption_2011.xls', sheetIndex = 1)
water2012 <- read.xlsx('water_consumption_2012.xls', sheetIndex = 1)
water2013 <- read.xlsx('water_consumption_2013.xls', sheetIndex = 1)
water2014 <- read.xlsx('water_consumption_2014.xls', sheetIndex = 1)
water2015 <- read.xlsx('water_consumption_2015.xls', sheetIndex = 1)
water <- rbind(water2011, water2012, water2013, water2014, water2015)
water$year <- as.numeric(as.character(water$year))
water <- water[complete.cases(water[,-1]),]

#data_modification
water$residential.accounts <- NULL
water$total.consumption <- NULL
water$average.consumption <- NULL
water$commercial.accounts <- NULL
water$total.count <- NULL

#data_visualisation
options("scipen"=100, "digits"=4)
p1 <- ggplot(aes(x = city.ward, y = annual.residential.usage), data = water) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(1, 45, 1)) +
  ylab('Water Usage(in cubic meters)') +
  ggtitle('Total Residential Usage by Ward 2011 - 2015') 

p2 <- ggplot(aes(x = city.ward, y = annual.commercial.usage), data=water) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(1, 45, 1)) +
  ylab('Water Usage(in cubic meters)') +
  ggtitle('Total Commercial Usage by Ward 2011 - 2015') 
#It is clear that ward 42, - Scarborough-Rouge River had the most residential usage and Ward 27, - Toronto Centre Rosedale had the most commercial usage in total from 2011 to 2015. 
grid.arrange(p1, p2, ncol=1)

p3 <- ggplot(data=water,
             aes(x=city.ward, y=average.residential.usage)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(1, 45, 1)) +
  ylab('Water Usage(in cubic meters)') +
  ggtitle('Average Residential Usage by Ward 2011 - 2015')

p4 <- ggplot(data=water,
             aes(x=city.ward, y=average.commercial.usage)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(1, 45, 1)) +
  ylab('Water Usage(in cubic meters)') +
  ggtitle('Average Commercial Usage by Ward 2011 - 2015') 
#again,ward 27, - Toronto Centre Rosedale had the most residential usage on average from 2011 to 2015, and ward 11, - York South West consumed the most water commercially on average from 2011 to 2015. 
grid.arrange(p3, p4, ncol=1)

water1 <- water
water1$average.residential.usage <- NULL
water1$average.commercial.usage <- NULL
water_long <- melt(water1, id = c('city.ward', 'year'))
ggplot(data=water_long,
       aes(x=city.ward, y=value, color = variable)) +
  geom_line() +
  ylab('Total Water Usage(in cubic meters)') +
  ggtitle('Total Water Usage') + facet_wrap(~year) 
#It has been consistent that ward 27 consumed the most water commercially every year from 2011 to 2015. 

ggplot(data=water,
       aes(x=city.ward, y=average.commercial.usage)) +
  geom_line() +
  ylab('Water Usage(in cubic meters)') +
  ggtitle('Average Commercial Usage') + facet_wrap(~year) 

ggplot(data=water,
       aes(x=city.ward, y=average.residential.usage)) +
  geom_line() +
  ylab('Water Usage(in cubic meters)') +
  ggtitle('Average Residential Usage') + facet_wrap(~year) 

water2011 <- water[water$year==2011 & water$average.residential.usage, ]
water2015 <- water[water$year==2015 & water$average.residential.usage, ]
summary(water2011$average.residential.usage)
summary(water2015$average.residential.usage)

install.packages("rgdal")
library(rgdal)
install.packages("sp")
library(sp)
ogrInfo("C:/Users/Admin/Documents", "icitw_wgs84")
toronto.rg <- readOGR("C:/Users/Admin/Documents", "icitw_wgs84")
print(proj4string(toronto.rg))

plot(toronto.rg, axes=TRUE, border="gray")

install.packages("rgeos")
library(rgeos)
install.packages("maptools")
library(maptools)
toronto.rg.df <- fortify(toronto.rg,region = "scode_name")

toronto.rg.df$id <- as.integer(toronto.rg.df$id)
ggplot(data=toronto.rg.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = 'wheat', color = 'wheat4', size = 1) + theme()
head(toronto.rg.df)

water_2015_df <- merge(toronto.rg.df, water2015, by.x = 'id', by.y = 'city.ward')
water_2015_df <- water_2015_df[order(water_2015_df$order), ]
# get the centroids and then convert them to a SpatialPointsDataFrame for ward labelling
centroids <- as.data.frame(gCentroid(toronto.rg, byid = T))
centroids[['id']] <- toronto.rg@data$scode_name

total_commercial_2015 <- water_2015_df[c(1:8,11)]
ggplot(data = total_commercial_2015, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = annual.commercial.usage), color = 'maroon4', alpha = 0.5) +
  scale_fill_gradient(name='Water Usage in Cubic Meters',low = 'lightyellow', high = "magenta4") +
  labs(title = "Toronto Total Commercial Water Usage by Ward 2015") +
  geom_text(aes(x=x,y=y, group=NULL, label=id), data = centroids, size=2)

total_residential_2015 <- water_2015_df[c(1:9)]
ggplot(data = total_residential_2015, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = annual.residential.usage), color = 'maroon4', alpha = 0.8) +
  scale_fill_gradient(name='Water Usage in Cubic Meters',low = 'lightyellow', high = "magenta4") +
  labs(title = "Toronto Total Residential Water Usage by Ward 2015") +
  geom_text(aes(x=x,y=y, group=NULL, label=id), data = centroids, size=2)

average_commercial_2015 <- water_2015_df[c(1:8, 12)]
ggplot(data = average_commercial_2015, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = average.commercial.usage), color = 'maroon4', alpha = 0.8) +
  scale_fill_gradient(name='Water Usage in Cubic Meters', low = 'lightyellow', high = "magenta4") +
  labs(title = "Toronto Average Commercial Water Usage by Ward 2015") +
  geom_text(aes(x=x,y=y, group=NULL, label=id), data = centroids, size=2)

average_residential_2015 <- water_2015_df[c(1:8, 10)]
ggplot(data = average_residential_2015, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = average.residential.usage), color = 'maroon4', alpha = 0.8) +
  scale_fill_gradient(name='Water Usage in Cubic Meters', low = 'lightyellow', high = "magenta4") +
  labs(title = "Toronto Average Residential Water Usage by Ward 2015") +
  geom_text(aes(x=x,y=y, group=NULL, label=id), data = centroids, size=2)


#wardwise_prediction_neuralnet
install.packages("neuralnet")
library(neuralnet)
w1 <- read.xlsx('w1.xlsx', sheetIndex = 1)
plot(w1,main="Total Consumption in Ward1 with Total Count")
#min max noramlisation
w1$year <- (w1$year - min(w1$year))/(max(w1$year) - min(w1$year))
w1$total.consumption.w1 <- (w1$total.consumption.w1 - min(w1$total.consumption.w1))/(max(w1$total.consumption.w1) - min(w1$total.consumption.w1))
w1$total.count <- (w1$total.count - min(w1$total.count))/(max(w1$total.count) - min(w1$total.count))
#partitioning
set.seed(222)
ind <- sample(2,nrow(w1),replace = TRUE,prob = c(0.7,0.3))
train<-w1[ind==1,]
test<-w1[ind==2,]
#neuralnet
n <- neuralnet( total.consumption.w1 ~ year + total.count , data = train ,hidden = 1,err.fct = "sse", linear.output = FALSE,learningrate = 0.001)
n$result.matrix
plot(n)
output <- compute(n,train[,-1])
head(output$net.result)
head(train)
#node output calculation with sigmoid activation function
in3<-2.99766+(-4.38778*0.06666667)+(0.77481* 0.4347826)
out3<-1/(1+exp(-in3))
in4<- -3.51005+(5.30445*out3)
out4<-1/(1+exp(-in4))
out4

#wardwise_forecasting
install.packages("forecast")
library(forecast)
w27 <- read.xlsx('w27.xlsx', sheetIndex = 1)
plot(w27)
lines(w27)
class(w27)
w27.ts<-ts(w27$total.consumption.w27,start = 2005,deltat = 2)
class(w27.ts)
w27.ts
plot(w27.ts,main="Water consumption in Ward27",xlab="Years",ylab="Water Consumption")
acf(w27.ts)
pacf(w27.ts)
auto.arima(w27.ts)
m<-arima(w27.ts,order = c(3,0,5))
m.prediction<-forecast(m,h=5)
m.prediction
plot(m.prediction,main="Predicted Water consumption in Ward27",xlab="Years",ylab="Water Consumption")
accuracy(m)

w42 <- read.xlsx('w42.xlsx', sheetIndex = 1)
plot(w42)
lines(w42)
class(w42)
w42.ts<-ts(w42$total.consumption.w42,start = 2005,deltat = 2)
class(w42.ts)
w42.ts
plot(w42.ts,main="Water consumption in Ward42",xlab="Years",ylab="Water Consumption")
acf(w42.ts)
pacf(w42.ts)
auto.arima(w42.ts)
m<-arima(w42.ts,order = c(1,3,2))
m.prediction<-forecast(m,h=5)
m.prediction
plot(m.prediction,main="Predicted Water consumption in Ward42",xlab="Years",ylab="Water Consumption")
accuracy(m)

#population_forecasting
population <- read.xlsx('population_updated.xlsx', sheetIndex = 1)
plot(population)
class(population)
population.ts<-ts(population$population,start = 2001,deltat = 2)
class(population.ts)
population.ts
plot(population.ts,main="Population trend of Toranto",xlab="Years",ylab="Population")
acf(population.ts)
pacf(population.ts)
auto.arima(population.ts)
m<-arima(population.ts,order = c(1,2,0))
m.prediction<-forecast(m,h=5)
m.prediction
plot(m.prediction,main="Expected Population of Toranto",xlab="Years",ylab="Population")

######################################################################
x <- c(1,2,3,4)
p1<-4607142
p2<-5035232
p3<-p2-p1
for (val in x) {
  n=12*x
  p4<-n/60
  P<-p1+p4*p3
}
print(P)

x <- c(1,2,3,4)
p1<-5035232
p2<-5499233
p3<-p2-p1
for (val in x) {
  n=12*x
  p4<-n/60
  P<-p1+p4*p3
}
print(P)

x <- c(1,2,3,4)
p1<-5499233
p2<-5867292
p3<-p2-p1
for (val in x) {
  n=12*x
  p4<-n/60
  P<-p1+p4*p3
}
print(P)
######################################################################

#water_consumption_forecasting
y=-4581081.66*2015+9535168992.29
a=-4581081.66*2017+9535168992.29
b=-4581081.66*2019+9535168992.29
c=-4581081.66*2021+9535168992.29
d=-4581081.66*2023+9535168992.29
e=-4581081.66*2025+9535168992.29
total_consumption <- read.xlsx('total_consumption.xlsx', sheetIndex = 1)
plot(total_consumption)
r<-lm(total.consumption ~ year,data=total_consumption)
abline(r)
summary(r)
predict(r,data.frame(year=c(2017,2019,2021,2023,2025)),interval = "confidence",level=.9)
x<-c(2017,2019,2021,2023,2025)
y<- c(295127275,285965112,276802949,267640785,258478622)
sp<-qplot(x,y,main="Predicted Water consumption",xlab="Years",ylab="Consumption")
sp + expand_limits(x=c(2017,2025), y=c(200000000, 300000000))


#liters_per_capita_per_day
lcd<-read.xlsx('lcd.xlsx',sheetIndex = 1)
plot(lcd)
lines(lcd)
class(lcd)
lcd.ts<-ts(lcd$l.c.d,start = 2001)
class(lcd.ts)
population.ts
plot(lcd.ts,main="l/c/d in Toranto",xlab="Years",ylab="L/c/d")
acf(lcd.ts)
pacf(lcd.ts)
auto.arima(lcd.ts)
m<-arima(lcd.ts,order = c(5,2,2))
m.prediction<-forecast(m,h=8)
m.prediction
plot(m.prediction,main="Expected l/c/d of Toranto",xlab="Years",ylab="L/C/D")
accuracy(m)
##################################################################################
x<- c(154656612.23,155226480.28,147917235.67,145938228.55,150512687.74,142286268.64,144844336.60,135200653.59,130533557.88,128868126.73,135104453.72,139915343,142284458.85,140431543.20,134939913.65)
y<-c(2481494,2485851,2490209,2494566,2498924,2503281,2525637,2547993,2570348,2592704,2615060,2638362,2661664,2684967,2708269)
x1<- x*1000
y1<-y*365
a<-c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
model<-for (val in a) {
p<-x1/y1
}
print(p)
##################################################################################

#Forecasted relationship between predicted water consumption and predicted population
x <- c(2017,2019,2021,2023,2025)
y1 <- c(295127275,285965112,276802949,267640785,258478622)
y2 <- c(2754874,2801480,2848086,2894691,2941297)
par(mar=c(4,7,3,7) + 0.1)
plot(x,y1, pch=15, axes=FALSE,ylim=c(50000000,350000000), xlab="", ylab="", 
     type="b",col="black", main="Forecasted relationship between predicted water consumption and predicted population")
axis(2, ylim=c(50000000,350000000),col="black",las=1) 
mtext("Predicted Water Consumption",side=2,line=5.5)
box()
par(new=TRUE)
plot(x,y2, pch=15,  xlab="", ylab="", ylim=c(2650000,3000000), 
     axes=FALSE, type="b", col="red")
mtext("Predicted Population",side=4,col="red",line=4.5) 
axis(4, ylim=c(2650000,3000000), col="red",col.axis="red",las=1)
axis(1,pretty(range(x),10))
mtext("years",side=1,col="black",line=2.5)  
legend("bottomright",legend=c("Predicted Water Consumption","Predicted Population"),text.col=c("black","red"),pch=c(15,15),col=c("black","red"),text.width = 2)
