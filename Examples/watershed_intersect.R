library(rgeos)
library(devtools)
library(here)
library(ncdf4)

# Load in our functions 
devtools::load_all(path = here::here("CliMates"))
devtools::document()
# get file metadata 
cesmlens <- get_data(data = "CESM",specific="rcp",type="PREC")
cesmlens_data <- nc_open(cesmlens)
try(print(prec))

# Get CESM RCP Oregon data 
cesm_rcp <- cesmlens %>%
    get_state(stat = "oregon",timemax = 3)

# Get ERA Precip data 
era <- get_data(data = "ERA",type ="PREC")%>%
    get_state(stat = "oregon",timemax = 3)


# load in shapefile of Oregon Watersheds
shp_filename <- here::here("Examples/USGS_hydrologic_units/wbdhu8_a_or.shp")
hydrologic_units <- st_read(shp_filename)
simplified_watersheds <- st_simplify(hydrologic_units, dTolerance = .01)
#st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees
# change CRS of watershed to match data 
transformed_watershed <- st_transform(simplified_watersheds,crs=st_crs(data))
joins <- st_join(data, transformed_watershed , join = st_intersects)
# add some filtering to get rid of not useful data 


# plot watersheds with OR points in CESM and ERA
state1 <- maps::map("county", region = "oregon", plot = F, fill = F)
or_map <- st_as_sf(state1)
ggplot() + 
    geom_sf(data =transformed_watershed,
            size = .75, color = "black", fill = NA) +
    geom_sf(data = or_map, color = "green", fill = NA) +
    geom_sf(data = cesm_rcp, aes(fill = PREC), color = "red") +
    geom_sf(data = era, aes(fill = PREC), color = "blue") 

#Just plot outlines of all oregon watersheds
ggplot(simp) + geom_sf()





