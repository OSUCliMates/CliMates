library(tidyverse)
library(printr)
library(ggplot2)
library(tidync)
library(maps)
library(ncdf4)
library(fields)
library(pracma)
library(raster)
library(ggmap)
library(lubridate)

filename <- "../ST505/ERA-Interim/historical/PREC.nc"

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

# Expanding to more points in SE OR
#############

SeTopLon <- -117
SeBotLon <- -120.5
SeLLat <- 41
SeRLat <- 44

Precip2 <- tidync(filename) %>% 
  hyper_filter(time<30,
               lat = dplyr::between(lat, abs(SeLLat),abs(SeRLat)),
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
  coord_cartesian(xlim=c(-125, -116), ylim = c(42, 46))



# Average yearly precipitation for one location
# Run code for single point above
###################

averagePrecip <- Precip1 %>% 
  mutate(year=year(time2)) %>%
  group_by(year) %>%
  summarise(averageYrPrec=mean(PREC))

ggplot(data=averagePrecip)+
  geom_line(mapping=aes(x=year, y=averageYrPrec))+
  ggtitle("Average yearly precip. for central point in SE OR")+
  ylab("Average precipitation")

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

# Average yearly precip averaged across all locations

  
