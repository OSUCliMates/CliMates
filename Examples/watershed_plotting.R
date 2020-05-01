library(sf)
library(tidyverse)
library(tidync)
library(maps)

#I downloaded a shapefile for the watersheds in Oregon from 
#https://datagateway.nrcs.usda.gov/GDGOrder.aspx

#read in the shapefile (be sure and pick a .shp file type)
sheds<- st_read('Examples/USGS_hydrologic_units/wbdhu8_a_or.shp')

#sheds is now a dataframe. the variable HUC8 is the 8-digit hydrologic unit code
#for more info on this, see https://water.usgs.gov/GIS/huc.html
#or https://water.usgs.gov/GIS/wbd_huc8.pdf

#subset the HU's based on if they're in the willamette river basin
willamette <- filter(sheds, between(as.numeric(as.character(sheds$HUC8)),17090001,17090012))
othersheds <- filter(sheds, !between(as.numeric(as.character(sheds$HUC8)),17090001,17090012))

#plot the watersheds!
shedplot <- ggplot() + 
  geom_sf(data = willamette, size = .75, color = "black", fill = "cyan1") +
  geom_sf(data = othersheds, size = .75, color = "black", fill = "darkorchid")+
  ggtitle("Oregon Watersheds") + 
  coord_sf()

shedplot

