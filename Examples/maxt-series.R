library(ggfortify)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

#I want to model the model wide average maximum temperature as a time series. 
#I've used hyper_tibble() to create a csv for it:

maxtfile <- "ERA-Interim_averages/average-Max-Temperature.csv"
maxt <- read_csv(maxtfile)

clean this up really quickly

dvec <- numeric(length=length(maxt$avg.max.temp))
for (i in 1:length(maxt$avg.max.temp)){
    dvec[i] <- paste(maxt$year[i],maxt$month[i],maxt$day[i],sep="-")
}
maxt <- maxt %>%
    mutate(date=lubridate::ymd(dvec),maxtemp=avg.max.temp)
maxt <- select(.data=maxt,date,maxtemp)
head(maxt)
remove(dvec)

#plot

ggplot(data=maxt, aes(x=date,y=maxtemp))+
    geom_line()+
    ylim(c(250,330))+
    ggtitle("Daily Maximum Temperature")+
    ylab("Maximum Temperature (degrees Kelvin)")+
    xlab("Date")

#This appears to be a sine wave, maybe with a slight trend to it. 
#Since the oscillations have to have a year long period, 
#I'll be fitting the model 
#Y = beta_0 + beta_1*X + alpha*sin(2*pi*X/365.25 + omega)
#This is not linear in terms of the parameters, 
#so we must use non linear least squares estimation.

t <- 1:length(maxt$date)
temp <- maxt$maxtemp
fit <- nls(temp ~ beta0 + beta1*t + alpha*sin((2*pi/365.25)*t + omega),
           start = list(beta0=290, alpha=10, omega=4.9, beta1=.000000000001))
summary(fit)

#We see a very small, but positive slope term for the trend on the data. 
#Let's plot this fitted curve:
parameters <- summary(fit)$coef[,1]
b0    <- parameters[1]
b1    <- parameters[4]
alpha <- parameters[2]
omega <- parameters[3]
pred <- function(t){
    return(b0 + b1*t + alpha*sin((2*pi*t/365.25)+omega))
}
yhat <- pred(t)
ggplot(data=NULL, aes(x=t,y=yhat))+geom_line(col="green")+
    geom_line(data=NULL, aes(x=t,y=temp))+
    xlim(1,1000)

#Now we can extract the residuals from this model and examine them 
#as a time series, plotting the series, the acf, and the pacf:

rdf <- tibble(
    t = (1:length(resid(fit))),
    residuals = resid(fit)
)
p0 <- ggplot(data=rdf)+geom_line(aes(x=t,y=residuals))
p1 <- autoplot(acf(rdf$residuals,plot=F))
p2 <- autoplot(pacf(rdf$residuals,plot=F))
grid.arrange(p0,p1,p2, nrow = 3) 



