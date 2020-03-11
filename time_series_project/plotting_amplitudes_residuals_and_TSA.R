library(tidyverse)
library(gridExtra)

#import data
amplitudes <- read.csv("Output/NyOrNeFl_amplitudes.csv")

#plot data
nyplot <- amplitudes %>% ggplot(aes(x=X,y=ny_amplitudes))+
    geom_point()+
    geom_smooth(method='lm',se=FALSE)
orplot <- amplitudes %>% ggplot(aes(x=X,y=or_amplitudes))+
    geom_point()+
    geom_smooth(method='lm',se=FALSE)
neplot <- amplitudes %>% ggplot(aes(x=X,y=ne_amplitudes))+
    geom_point()+
    geom_smooth(method='lm',se=FALSE)
flplot <- amplitudes %>% ggplot(aes(x=X,y=fl_amplitudes))+
    geom_point()+
    geom_smooth(method='lm',se=FALSE)
grid.arrange(nyplot,orplot,neplot,flplot,nrow=2)

#create linear models for residuals for TSA
nymod <- lm(ny_amplitudes~X,data=amplitudes)
ormod <- lm(or_amplitudes~X,data=amplitudes)
nemod <- lm(ne_amplitudes~X,data=amplitudes)
flmod <- lm(fl_amplitudes~X,data=amplitudes)
residfr <- data.frame(
    X=amplitudes$X,
    ny=nymod$residuals,
    or=ormod$residuals,
    ne=nemod$residuals,
    fl=flmod$residuals
)

#Plot residuals
rpny <- residfr %>% ggplot(aes(x=X,y=ny))+
    geom_line()
rpor <- residfr %>% ggplot(aes(x=X,y=or))+
    geom_line()
rpne <- residfr %>% ggplot(aes(x=X,y=ne))+
    geom_line()
rpfl <- residfr %>% ggplot(aes(x=X,y=fl))+
    geom_line()
grid.arrange(rpny,rpor,rpne,rpfl,nrow=2)

#ACF, PACF, and DFT for each state
acf(residfr$ny)
pacf(residfr$ny)
plot(abs(fft(residfr$ny)),type='l',xlim=c(0,20),ylab="DFT(NY Residuals)")

acf(residfr$or)
pacf(residfr$or)
plot(abs(fft(residfr$or)),type='l',xlim=c(0,20),ylab="DFT(OR Residuals)")

acf(residfr$ne)
pacf(residfr$ne)
plot(abs(fft(residfr$ne)),type='l',xlim=c(0,20),ylab="DFT(NE Residuals)")

acf(residfr$fl)
pacf(residfr$fl)
plot(abs(fft(residfr$fl)),type='l',xlim=c(0,20),ylab="DFT(FL Residuals)")