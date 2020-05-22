library(tidyverse)
library(tidync)
library(gridExtra)

#filenames
precfile <- "/home/ST505/ERA-Interim/historical/PREC.nc"

#dataframe for time index-ymd conversions
leapyears <- read.csv("examples/era_leapyears.csv")

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
    hyper_tibble()

#function converts time index to year
get_year <- function(x){
    for(i in 1:length(leapyears$year)){
        if((x>=leapyears$year_start_index[i])&(x<leapyears$year_start_index[i+1])){
            return(leapyears$year[i])
        }
    }
}

#function converts time index to day of year
get_calendar_day <- function(x){
    for(i in 1:length(leapyears$year)){
        if((x>=leapyears$year_start_index[i])&(x<leapyears$year_start_index[i+1])){
            return(x-leapyears$year_start_index[i]+1)
        }
    }    
}

#add year column to prec
prec <- prec %>%
    mutate(year=map_dbl(time,get_year)) %>%
    mutate(calendar_day=map_dbl(time,get_calendar_day))

#create a tibble to hold yearly average temperatures across all pixels
avgprec <- tibble(
    year=unique(prec$year)
)

#add average temp to tibble
avgprec <- avgprec %>%
    mutate(meanprec=map_dbl(avgprec$year,function(x) mean(filter(prec,year==x)$PREC)))

#plot yearly precipitation 
ggplot(avgprec)+
    geom_point(mapping=aes(x=year,y=meanprec))+
    ggtitle("ERA-Interim average precipitation in Willamette valley region")+
    ylab("annual average precipitation")

#remove trend by fitting linear model, acf plots of residuals
lmod <- lm(meanprec~year, data=avgprec)
resids <- lmod$residuals
resid_acf <- acf(resids,plot=F)
resid_pacf <- acf(resids,plot=F,type="partial")

p1<- ggplot()+
    geom_line(aes(x=avgprec$year, y=resids))+
    geom_hline(yintercept = 0)+
    ggtitle("Residuals for ERA-interim precipitation linear model")

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

