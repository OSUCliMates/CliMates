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

# Investigating single point in SE OR
TopLon <- -118
BotLon <- -119
LLat <- 42.5
RLat <- 43

Precip1 <- tidync(filename) %>% 
    hyper_filter(lat = dplyr::between(lat, abs(LLat),abs(RLat)),
                 lon = dplyr::between(lon-360, BotLon,TopLon)) %>% 
    hyper_tibble() %>%
    mutate(lon2 = ifelse(lon>=180,lon-360,lon),
           time2 = as.Date(time,origin="1979-1-1 00:00:0.0"),
           locat = interaction(lat, lon2))

# Where the point is in the state
ggplot(data=Precip1)+
    geom_point(mapping=aes(x=lon2, y=lat)) +
    borders("state")+
    coord_cartesian(xlim=c(-125, -116), ylim = c(42, 46))

# Precip over all time points at this location
ggplot(data=Precip1)+
    geom_line(mapping=aes(x=time2, y=PREC))+
    ggtitle("Precipitation @ random pt. in SE OR over all time available")+
    xlab("Time")+
    ylab("Precipitation")

# Average yearly precipitation for one location
# Need Precip1 defined above
###################

averagePrecip <- Precip1 %>% 
    mutate(year=year(time2)) %>%
    group_by(year) %>%
    summarise(averageYrPrec=mean(PREC))

ggplot(data=averagePrecip)+
    geom_line(mapping=aes(x=year, y=averageYrPrec))+
    ggtitle("Average yearly precip. for central point in SE OR")+
    ylab("Average precipitation")