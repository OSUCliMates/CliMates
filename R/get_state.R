require(sf) 
require(tidyverse)
require(maps)
require(tidync)

#' Get the state data points 
#'
#' @param state String for the state
#' @param timemin The start time
#' @param timemax The end time
#' @return A data frame of the state data points within the time 


get_state <- function(state="oregon", timemin=0, timemax=14244){
    print(state)
    state1 <- maps::map("county", region = state, plot = F, fill = T)
    maxlon <- max(state1$x, na.rm = T)
    minlon <- min(state1$x, na.rm = T)
    maxlat <- max(state1$y, na.rm = T)
    minlat <- min(state1$y, na.rm = T)
    #filename <- "../st505/ERA-Interim/historical/MAXT.nc"
    filename <- "/Users/emilypalmer/Desktop/MAXT.nc"
    maxt <- tidync(filename) %>% 
        hyper_filter(time = time <= timemax & time >= timemin,
                     lat = dplyr::between(lat, abs(minlat),abs(maxlat)),
                     lon = dplyr::between(lon-360, minlon,maxlon)) %>% 
        hyper_tibble() %>% 
        mutate(lon2 = ifelse(lon>180,lon-360,lon))
    
    state_polygon <- st_as_sf(state1)
    points <- st_as_sf(maxt, coords = c("lon2", "lat"), crs = st_crs(state_polygon))
    joins <- st_join(points, state_polygon) %>% filter(!is.na(ID))
    return(joins)
}

