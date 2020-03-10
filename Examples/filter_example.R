library(tidyverse)
library(printr)
library(gganimate)
library(gifski)

library(tidync)
library(maps)

library(ncdf4)
library(fields)
library(pracma)
library(raster)
library(ggmap)


OR <- map_data("county", region = "oregon")
maxORlon <- max(OR$long)
minORlon <- min(OR$long)
maxORlat <- max(OR$lat)
minORlat <- min(OR$lat)


filename <- "MAXT.nc"

maxt <- tidync(filename) %>% 
    hyper_filter(time = time < 30,
                 lat = dplyr::between(lat, abs(minORlat),abs(maxORlat)),
                 lon = dplyr::between(lon-360, minORlon,maxORlon)) %>% 
    hyper_tibble()



maxt <- maxt %>%
    mutate(lon2 = ifelse(lon>=180,lon-360,lon))

maxt0 <- maxt %>% 
    filter(time == 0) %>% 
    mutate(lon2 = ifelse(lon>=180,lon-360,lon), 
           time2 = as.Date(time,origin="1979-1-1 00:00:0.0")) %>% 
    filter()

ggplot(maxt0, aes(y=lat, x=lon2, fill=MAXT), alpha  = .05) + 
    borders("state")+
    geom_tile() + 
    theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())+ 
    scale_fill_distiller(palette='Spectral') + 
    coord_cartesian(xlim=c(-125, -116), ylim = c(42, 46)) +
    scale_size_area() 


ggplot(maxt, aes(y=lat, x=lon2, fill=MAXT)) + 
    geom_tile() + 
    theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())+ 
    scale_fill_distiller(palette='Spectral') + 
    borders("state",xlim = c(minORlon, maxORlon), ylim = c(minORlat, maxORlat))+
    transition_time(time)