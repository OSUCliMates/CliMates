#cumulative precipitation
library(tidyverse)
library(tidync)
library(colorspace)
library(gridExtra)

#filename
precfile <- "/home/ST505/CESM-LENS/historical/PREC.nc"

#Define boundaries of window being analyzed
window <- list(
    max_lat = 46,
    min_lat = 43,
    min_lon = -123.5,
    max_lon = -122
)

#create a tibble for the precipitation data
prec <- tidync(precfile) %>% 
    hyper_filter(
        lat = dplyr::between(lat, window$min_lat,window$max_lat),
        lon = dplyr::between(lon-360, window$min_lon,window$max_lon)) %>% 
    hyper_tibble() %>%
    mutate(time=time-min(time))

#dear GOD this is data for 42 different models. we better nest that shit
nestprec <- prec%>%
    group_by(time)%>%
    nest()
    
#ok let's make a tibble for this and the time index, plus year and date columns
#using map_dbl on the data frame nested for each time index to find each time
#index's mean, giving each value of mem equal weight.
prec_tibble <- tibble(
    time=unique(prec$time),
    prec = map_dbl(nestprec$data,function(x) mean(x$PREC))
    )%>%
    mutate(year=time%/%365 + 1920) %>%
    mutate(calendar_date=time%%365 + 1)

#now we can do some EDA on prec_tibble
ggplot(prec_tibble)+
    geom_point(aes(x=calendar_date,y=prec))+
    geom_smooth(aes(x=calendar_date,y=prec),
                se=FALSE,
                method="loess",
                span=0.3)+
    geom_vline(xintercept=274,color="green")+
    annotate("text",x=270,y=1.5e-07,label="October 1st",hjust=1)

#function converts year and day of year into water year and day of water year
conv_to_water_year <- function(year,day){
    if(day<274){
        wateryear <- year
    }
    if(day>=274){
        wateryear <- year+1
    }
    return(wateryear)
} 
conv_to_water_day <- function(day){
    waterday <- (day-274)%%365
    return(waterday)
} #MAKE THESE NICER IF YOU'RE GOING TO PUT IT IN THE R FOLDER

prec_tibble <- prec_tibble%>%
    mutate(water_year = map2_dbl(.x=year,
                                 .y=calendar_date,
                                 .f=conv_to_water_year
                                 )
           )%>%
    mutate(water_day = map_dbl(.x=calendar_date,
                               .f=conv_to_water_day
                               )
           )

#At this point, prec_tibble is a tibble with the columns:
#<time, prec, year, calendar_date, water_year, water_day>
#What we want to do is to create a function that creates a cumulative rainfall
#by water year. I'm going to do this by subsetting prec_tibble, nesting him, then 
#applying a function to each of the nested data frames that adds a column for the
#cumsum(), then unnests and ungroups. 
add_cumsum <- function(x){
    return(x%>%mutate(cumulative_precip = cumsum(prec)))
}

prec_tibble2 <- prec_tibble %>%
    select(prec,water_year,water_day) %>%
    group_by(water_year) %>%
    nest()
prec_tibble3 <- prec_tibble2$data%>%
    map(.f=add_cumsum)%>%
    enframe()%>%
    unnest()%>%
    select(-name)%>%
    mutate(water_year = prec_tibble$water_year)%>%
    mutate(time=prec_tibble$time)
    
#Did that really work??
ggplot(prec_tibble3)+
    geom_line(aes(x=time,y=cumulative_precip))
ggplot(prec_tibble3)+
    geom_line(aes(x=time,y=cumulative_precip))+
    xlim(c(10000,12000))
#We can subset water_day==364 to see the cumulative rainfall per year
ggplot(filter(prec_tibble3,water_day==364&water_year>1920))+
    geom_line(aes(x=water_year,y=cumulative_precip))+
    geom_point(aes(x=water_year,y=cumulative_precip))

#what if time series?
mod1 <- lm(cumulative_precip~water_year,
             data=filter(prec_tibble3,water_day==364&water_year>1920))
ggplot()+
    geom_point(aes(x=filter(prec_tibble3,water_day==364&water_year>1920)$water_year,
                   y=mod1$residuals
                   )
              )+
    geom_hline(yintercept = 0)
acf(mod1$residuals)
acf(mod1$residuals,type="partial")
#####################################################
#now we want to average the waveforms across decades#
#####################################################
#function that takes a data frame with the following columns:
# *water_day
# *decade (ONLY ONE VALUE IN THIS COLUMN)
# *cumulative precip
#and then takes the average cumulative precip for each level of water_day
wave_agg <- function(x){
        x%>%
        group_by(water_day)%>%
        summarise(as.character(mode(x$decade)) , mean(cumulative_precip))
    }

#Create data frame with the averaged waveforms, in tidy form
avg_waveforms <- prec_tibble3 %>%
    mutate(decade=(water_year%/%10)*10)%>%
    mutate(cumulative_precip = cumulative_precip*8640000)%>%
    filter(between(water_year,1921,2005))%>%
    group_by(decade)%>%
    nest() %>%
    mutate(post_avg_cols = map(data,wave_agg))%>%
    select(decade, post_avg_cols)%>%
    unnest(cols=post_avg_cols) %>%
    rename(cumulative_precip_agg = `mean(cumulative_precip)`)

#plot!
p1 <- ggplot(avg_waveforms)+
    geom_line(aes(x=water_day,
                  y=cumulative_precip_agg,
                  color=decade,
                  group=decade),na.rm=TRUE
              )+
    xlab("Water Day")+
    ylab("Avg. Cum. Precip.")+
    annotate(geom = "rect", xmin=300,xmax = 365, ymin=125, ymax=150,
             alpha = 0, color="black", linetype=2)+
    ggtitle("Willamette")
p1

p2<-p1+
    xlim(300,365)+
    ylim(125,160)

grid.arrange(p1,p2)

###########################################################
#Do it again with the coast for Ericka
###########################################################

#filename
precfile <- "/home/ST505/CESM-LENS/historical/PREC.nc"

#Define boundaries of window being analyzed
window <- list(
    max_lat = 46.2585,
    min_lat = 42.0016,
    min_lon = -124.7,
    max_lon = -123.6
)

#create a tibble for the precipitation data
prec <- tidync(precfile) %>% 
    hyper_filter(
        lat = dplyr::between(lat, window$min_lat,window$max_lat),
        lon = dplyr::between(lon-360, window$min_lon,window$max_lon)) %>% 
    hyper_tibble() %>%
    mutate(time=time-min(time))

#dear GOD this is data for 42 different models. we better nest that shit
nestprec <- prec%>%
    group_by(time)%>%
    nest()

#ok let's make a tibble for this and the time index, plus year and date columns
#using map_dbl on the data frame nested for each time index to find each time
#index's mean, giving each value of mem equal weight.
prec_tibble <- tibble(
    time=unique(prec$time),
    prec = map_dbl(nestprec$data,function(x) mean(x$PREC)))%>%
    mutate(year=time%/%365 + 1920) %>%
    mutate(calendar_date=time%%365 + 1)

#function converts year and day of year into water year and day of water year
conv_to_water_year <- function(year,day){
    if(day<274){
        wateryear <- year
    }
    if(day>=274){
        wateryear <- year+1
    }
    return(wateryear)
} 
conv_to_water_day <- function(day){
    waterday <- (day-274)%%365
    return(waterday)
} #MAKE THESE NICER IF YOU'RE GOING TO PUT IT IN THE R FOLDER

prec_tibble <- prec_tibble%>%
    mutate(water_year = map2_dbl(.x=year,
                                 .y=calendar_date,
                                 .f=conv_to_water_year))%>%
    mutate(water_day = map_dbl(.x=calendar_date, 
                               .f=conv_to_water_day))

#At this point, prec_tibble is a tibble with the columns:
#<time, prec, year, calendar_date, water_year, water_day>
#What we want to do is to create a function that creates a cumulative rainfall
#by water year. I'm going to do this by subsetting prec_tibble, nesting him, then 
#applying a function to each of the nested data frames that adds a column for the
#cumsum(), then unnests and ungroups. 
add_cumsum <- function(x){
    return(x%>%mutate(cumulative_precip = cumsum(prec)))
}

prec_tibble2 <- prec_tibble %>%
    select(prec,water_year,water_day) %>%
    group_by(water_year) %>%
    nest()
prec_tibble3 <- prec_tibble2$data%>%
    map(.f=add_cumsum)%>%
    enframe()%>%
    unnest()%>%
    select(-name)%>%
    mutate(water_year = prec_tibble$water_year)%>%
    mutate(time=prec_tibble$time)

#Did that really work??
ggplot(prec_tibble3)+
    geom_line(aes(x=time,y=cumulative_precip))
ggplot(prec_tibble3)+
    geom_line(aes(x=time,y=cumulative_precip))+
    xlim(c(10000,12000))
#We can subset water_day==364 to see the cumulative rainfall per year
ggplot(filter(prec_tibble3,water_day==364&water_year>1920))+
    geom_line(aes(x=water_year,y=cumulative_precip))+
    geom_point(aes(x=water_year,y=cumulative_precip))

#what if time series?
mod1 <- lm(cumulative_precip~water_year,
           data=filter(prec_tibble3,water_day==364&water_year>1920))
ggplot()+
    geom_point(aes(x=filter(prec_tibble3,water_day==364&water_year>1920)$water_year,
                   y=mod1$residuals
    )
    )+
    geom_hline(yintercept = 0)
acf(mod1$residuals)
acf(mod1$residuals,type="partial")
#####################################################
#now we want to average the waveforms across decades#
#####################################################
#function that takes a data frame with the following columns:
# *water_day
# *decade (ONLY ONE VALUE IN THIS COLUMN)
# *cumulative precip
#and then takes the average cumulative precip for each level of water_day
wave_agg <- function(x){
    x%>%
        group_by(water_day)%>%
        summarise(as.character(mode(x$decade)) , mean(cumulative_precip))
}

#Create data frame with the averaged waveforms, in tidy form
avg_waveforms <- prec_tibble3 %>%
    mutate(decade=(water_year%/%10)*10)%>%
    mutate(cumulative_precip = cumulative_precip*8640000)%>%
    filter(between(water_year,1921,2005))%>%
    group_by(decade)%>%
    nest() %>%
    mutate(post_avg_cols = map(data,wave_agg))%>%
    select(decade, post_avg_cols)%>%
    unnest(cols=post_avg_cols) %>%
    rename(cumulative_precip_agg = `mean(cumulative_precip)`)

#plot!
p3 <- ggplot(avg_waveforms)+
    geom_line(aes(x=water_day,
                  y=cumulative_precip_agg,
                  color=decade,
                  group=decade),na.rm=TRUE
    )+
    xlab("Water Day")+
    ylab("Avg. Cum. Precip.")+
    annotate(geom = "rect", xmin=300,xmax = 365, ymin=125, ymax=150,
             alpha = 0, color="black", linetype=2)+
    ggtitle("Coastal")
p3

p4<-p3+
    xlim(300,365)+
    ylim(125,150)
p4

grid.arrange(p3,p4,nrow=1)


grid.arrange(p1,p2,p3,p4)