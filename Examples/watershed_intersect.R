library(rgeos)
library(devtools)

library(ncdf4)
precfile <- "/home/ST505/CESM-LENS/historical/PREC.nc"
prec <- nc_open(precfile)
try(print(prec))


# Load in our functions - not working since 
devtools::load_all(path = here("CliMates"))

or_watershed_intersect <- function(data){
    # load in shapefile of Oregon Watersheds
    shp_filename <- "CliMates/Examples/USGS_hydrologic_units/wbdhu8_a_or.shp"
    hydrologic_units <- st_read(shp_filename)
    simp <- st_simplify(hydrologic_units, dTolerance = .01)
    #st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees
}


#Just plot outlines of all oregon watersheds
ggplot(simp) + geom_sf()

# Get CESM RCP Oregon data 
cesm_rcp <- get_data(data = "CESM",specific="rcp",type="PREC") %>%
  get_state(timemax = 3)






