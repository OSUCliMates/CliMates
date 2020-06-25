library(tidync)
library(sf)
# ERA filepath
era_file <- get_filename()

# get us state boundaries
us_map <- maps::map("state", plot = F, fill = T)

# Save era data as tibble and add date information 
precip_tibble <- tidync(era_file) %>% 
  hyper_tibble() %>% 
  dplyr::mutate(lon = ifelse(lon>180,lon-360,lon))%>%
  mutate(calendar_year = as.Date(time, origin = "1979-1-1")) 

# convert us map to sf object
us_polygon <- sf::st_as_sf(us_map)

# converts points to a geometry column
era_points_as_sf <- sf::st_as_sf(precip_tibble,
                                 coords = c("lon", "lat"),
                                 crs = sf::st_crs(us_polygon))
# Takes around 30 min to run
# joins with state information
joined_us_states <- sf::st_join(era_points_as_sf, us_polygon) %>% 
  filter(!is.na(ID))

# save as .rds
rds_filename <- "~/CliMates/Output/era_states/full_era.rds"
readr::write_rds(joined_us_states, rds_filename)



