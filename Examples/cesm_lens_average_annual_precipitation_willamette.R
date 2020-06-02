library(tidyverse)
library(tidync)
library(gridExtra)

#this script is to calculate the yearly average precipitation for the willamette 
#valley, going back to 1920, using the CESM-LENS data.

#filename
precfile <- "/home/ST505/CESM-LENS/historical/PREC.nc"

#Define boundaries of willamette river valley (i just eyeballed these)
willamette <- list(
    max_lat = 46,
    min_lat = 43,
    min_lon = -123.5,
    max_lon = -122
)

#create a tibble for the precipitation data
prec <- tidync(precfile) %>% 
    hyper_filter(
        lat = dplyr::between(lat, willamette$min_lat,willamette$max_lat),
        lon = dplyr::between(lon-360, willamette$min_lon,willamette$max_lon)) %>% 
    hyper_tibble() %>%
    mutate(time=time-min(time)) #index time starting at t=0

#there are 86 years, each 365 days long in the CESM-LENS historical data.
#it does not apppear that data occurs for leap days, which is kind of odd. 
#nonetheless, since time t=0 is presumably January 1, 1920, this means that 
#the year of the observation is given by:
#           year = floor(time/365) + 1920
#so the year of a data point can be calculated by 
#           year = time%/%365 + 1920
#and the day of the year can be calculated by 
#           calendar_date = time%%365
#that all being said, we add "year" and "calendar_date" columns to the tibble:

prec <- prec%>%
    mutate(year=time%/%365 + 1920) %>%
    mutate(calendar_date=time%%365 + 1)

#create a tibble to hold yearly average temperatures across all pixels
avgprec <- tibble(
    year=unique(prec$year)
)

#add average YEARLY prec to tibble
avgprec <- avgprec %>%
    mutate(meanprec=map_dbl(avgprec$year,function(x) mean(filter(prec,year==x)$PREC)))

#plot average precipitation over years
ggplot(avgprec)+
    geom_point(mapping=aes(x=year,y=meanprec))+
    ggtitle("CESM-LENS average precipitation in Willamette valley region")+
    ylab("annual average precipitation")

#remove trend by fitting linear model, acf plots of residuals
lmod <- lm(meanprec~year, data=avgprec)
resids <- lmod$residuals
resid_acf <- acf(resids,plot=F)
resid_pacf <- acf(resids,plot=F,type="partial")

p1<- ggplot()+
    geom_line(aes(x=avgprec$year, y=resids))+
    geom_hline(yintercept = 0)+
    ggtitle("Residuals for CESM-LENS precipitation linear model")

p2 <- ggplot()+
    geom_col(
        mapping = aes(x=as.numeric(resid_acf$lag), 
                      y=as.numeric(resid_acf$acf))
    )+
    geom_abline(intercept=qnorm(.975)/sqrt(length(resids)),slope=0,color="blue")+
    geom_abline(intercept=-qnorm(.975)/sqrt(length(resids)),slope=0,color="blue")+
    ylim(-1,1)+
    ylab("correlation")+
    xlab("lag")+
    ggtitle("ACF plot")

p3 <- ggplot()+
    geom_col(
        mapping = aes(x=as.numeric(resid_pacf$lag), 
                      y=as.numeric(resid_pacf$acf))
    )+
    geom_abline(intercept=qnorm(.975)/sqrt(length(resids)),slope=0,color="blue")+
    geom_abline(intercept=-qnorm(.975)/sqrt(length(resids)),slope=0,color="blue")+
    ylim(-1,1)+
    ylab("correlation")+
    xlab("lag")+
    ggtitle("Partial ACF plot")

grid.arrange(p1,p2,p3,nrow=3)


#let's investigate decades
ggplot(prec)+
    stat_summary(
        mapping=aes(x=calendar_date,y=PREC),
        fun = mean,
        geom="line"
    )+
    facet_wrap(~(year%/%10))

# #Now we want to be looking at cumulative precipitation
# #let's make a new tibble for the average daily precipitation
# avgprec1 <- prec%>%
#     filter(mem==1)%>%
#     select(time,year,calendar_date)%>%
#     unique()
# 
# #add precipitation averaged across all pixels for each time index to this tibble
# avgprec1 <- avgprec1 %>%
#     mutate(meanprec=map_dbl(time,function(x) mean(filter(prec,time==x)$PREC)))
# 
# gbg <- map_dbl(avgprec1$time,function(x) mean(filter(prec,time==x&mem==1)$PREC))







