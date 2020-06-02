library(tidyverse)
library(printr)
library(ggplot2)
library(colorspace)
library(tidync)
library(maps)
library(ncdf4)
library(fields)
library(pracma)
library(raster)
library(RColorBrewer)
library(ggmap)
library(lubridate)
options(scipen=50)

filename <- "~/../ST505/ERA-Interim/historical/PREC.nc"


# Expanding to more points in SE OR
#############

SeTopLon <- -117
SeBotLon <- -120.5
SeLLat <- 41.5
SeRLat <- 44

Precip2 <- tidync(filename) %>% 
    hyper_filter(lat = dplyr::between(lat, abs(SeLLat),abs(SeRLat)),
                 lon = dplyr::between(lon-360, SeBotLon, SeTopLon)) %>% 
    hyper_tibble()

Precip2 <- Precip2 %>%
    mutate(lon2 = ifelse(lon>=180,lon-360,lon),
           time2 = as.Date(time,origin="1979-1-1 00:00:0.0"),
           locat = interaction(lat, lon))

# Where the points are in the state
ggplot(data=Precip2)+
    geom_point(mapping=aes(x=lon2, y=lat)) +
    borders("state")+
    coord_cartesian(xlim=c(-125, -116), ylim = c(41, 46))


# Average yearly precipitation for SE locations
#####################

averagePrecip2 <- Precip2 %>%
    mutate(year=year(time2)) %>%
    group_by(year, locat) %>%
    summarise(averageYrPrec=mean(PREC))

ggplot(data=averagePrecip2)+
    geom_line(mapping=aes(x=year, y=averageYrPrec, color=locat))+
    ggtitle("Average yearly precip. for all pts in SE OR (ERA)")+
    ylab("Average precipitation")



# Maximum yearly precipitation
########################
maxPrecip <- Precip2 %>%
    mutate(year=year(time2)) %>%
    group_by(year, locat) %>%
    summarise(maxPrec=max(PREC))

ggplot(data=maxPrecip)+
    geom_line(mapping=aes(x=year, y=maxPrec, color=locat))+
    ggtitle("Max yearly precip. for all pts in SE OR (ERA)")+
    ylab("Max precipitation")

# Proportion rain days
#############################

# For one specific year
precipDays <- Precip2 %>%
    filter(year(time2)==2005) %>%
    mutate(moYr=format(as.Date(time2), "%Y-%m")) %>%
    group_by(moYr, locat) %>%
    summarise(propRainDays=sum(PREC>0)/n())

ggplot(data=precipDays) +
    geom_point(mapping=aes(x=moYr, y=propRainDays, color=locat, group=1)) +
    geom_line(mapping=aes(x=moYr, y=propRainDays, color=locat, group=1))

# Average proportion of rain days over all points for specific year (With OMI)
OMI <- read.csv("~/CliMates/External Data/El Nino Metrics.csv")

colnames(OMI) <- c("Year", "01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12")

OMIyr <- OMI %>% filter(Year==2016) %>%
    pivot_longer(cols = colnames(OMI), names_to = "Month", values_to="OMI") %>%
    filter(Month != "Year")

avePrecipDays <- precipDays %>%
    summarise(avePropRainDays = mean(propRainDays)) %>%
    mutate(month = substr(moYr, 6, 7)) %>%
    bind_cols(OMIyr)

ggplot(avePrecipDays)+
    geom_bar(aes(x=month, fill=OMI), width=1)+
    geom_line(mapping=aes(x=month, y=avePropRainDays, group=1))+
    scale_fill_distiller(palette="Spectral")+
    scale_y_continuous(name="Average proportion of rain days", limits=c(0,1))+
    xlab("Month")+
    labs(fill="Oceanic Niño Index")+
    ggtitle("Average proportion of rain days in 2005 SE OR with Oceanic Niño Index",
            "Rain days at lowest when ONI is neutral")


# Average prop rain days extended to multiple years (with OMI)
precipDaysYears <- Precip2 %>%
    filter(year(time2)>=2000 & year(time2)<=2017) %>%
    mutate(month=format(as.Date(time2), "%m"),
           year=format(as.Date(time2), "%Y")) %>%
    group_by(year, month, locat) %>%
    summarise(propRainDays=sum(PREC>(6*10^-8))/n())

OMIYrs <- OMI %>% filter(Year>=2000 & Year <=2017) %>%
    pivot_longer(cols = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), names_to = "Month", values_to="OMI")

avePrecipDaysYears <- precipDaysYears %>%
    summarise(avePropRainDays = mean(propRainDays)) %>%
    bind_cols(OMIYrs)

avePrecipDaysYears <- subset(avePrecipDaysYears, select = -c(year, month))

ggplot(avePrecipDaysYears) +
    geom_bar(aes(x=Month, fill=OMI), width=1)+
    scale_fill_distiller(palette="Spectral")+
    geom_line(mapping=aes(x=Month, y=avePropRainDays, group=1))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_y_continuous(name="Average proportion of rain days", limits=c(0,1))+
    facet_wrap(~Year)+
    xlab("Month")+
    labs(fill="Oceanic Niño Index")+
    ggtitle("Average proportion of rain days with Oceanic Niño Index",
            "ERA Data, SE Oregon stations, faceted by 2000-2017")

# Removing seasonality to see the trends a bit easier
removeseason <- avePrecipDaysYears %>%
    group_by(Month) %>%
    mutate(DevFromAve=avePropRainDays-mean(avePropRainDays, na.rm=TRUE),
           devFromAveOMI=OMI-mean(OMI, na.rm=TRUE))

removeseason <- subset(removeseason, select=-c(month, year))

# Heat map but deviation line is tiny on y-axis
# This is the money plot for looking at all years OMI and deseasonalized rain day proportion
ggplot(seasonRemoved) +
    geom_col(aes(x=month, fill=devFromAveOMI, y=0.5), width=1)+
    geom_col(aes(x=month, fill=devFromAveOMI, y=-0.5), width=1)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    scale_fill_distiller(palette="Purples", direction =1)+
    geom_line(mapping=aes(x=month, y=DevFromAve, group=1))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_y_continuous(name="Average proportion of rain days", limits=c(-0.5, 0.5))+
    facet_wrap(~year)+
    xlab("Month")+
    labs(fill="Oceanic Niño Index")+
    ggtitle("Deseasonalized average proportion of rain days with Oceanic Niño Index",
            "ERA Data, SE Oregon stations, faceted by 2000-2017")

# Try standardizing both OMI and deviation so that their on same scale
standSeasonRem <- left_join(avePrecipDaysYears, removeseason) %>%
    mutate(DevFromAve=scale(avePropRainDays-monthAveProp),
           devFromAveOMI=scale(OMI-monthAveOMI))

# Plot with standardization
# Not good...
ggplot(standSeasonRem) +
    geom_col(aes(x=month, fill=devFromAveOMI, y=0.5), width=1)+
    geom_col(aes(x=month, fill=devFromAveOMI, y=-0.5), width=1)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    scale_fill_distiller(palette="Purples")+
    geom_line(mapping=aes(x=month, y=DevFromAve, group=1))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_y_continuous(name="Average proportion of rain days", limits=c(-2,2))+
    facet_wrap(~year)+
    xlab("Month")+
    labs(fill="Oceanic Niño Index")+
    ggtitle("Standardized average proportion of rain days with Oceanic Niño Index",
            "ERA Data, SE Oregon stations, faceted by 2000-2017")


# Instead of facet, make one long plot?
precipDaysYearsLong <- Precip2 %>%
    filter(year(time2)>=2000 & year(time2)<=2017) %>%
    mutate(moYr=format(as.Date(time2), "%Y/%m")) %>%
    group_by(moYr, locat) %>%
    summarise(propRainDays=sum(PREC>(6*10^-8))/n()) %>%
    summarise(avePropRainDays = mean(propRainDays)) %>%
    bind_cols(OMIYrs)

# Not cute
# X-axis is... wrong and its too zig-zaggy
ggplot(precipDaysYearsLong) +
    geom_bar(aes(x=moYr, fill=OMI), width=1)+
    scale_fill_distiller(palette="Purples")+
    geom_line(aes(x=moYr, y=avePropRainDays), group=1)+
    scale_y_continuous(name="Average proportion of rain days", limits=c(0,1))+
    scale_x_discrete(labels=c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008"))+
    labs(fill="Oceanic Niño Index")+
    ggtitle("Average proportion of rain days with Oceanic Niño Index",
            "ERA Data, SE Oregon stations, 2000-2017")
