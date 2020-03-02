library(sf) 
library(tidyverse)


get_state <- function(state="oregon", timemax=1){
    state1 <- map("county", region = state, plot = F, fill = T)
    maxlon <- max(state1$x, na.rm = T)
    minlon <- min(state1$x, na.rm = T)
    maxlat <- max(state1$y, na.rm = T)
    minlat <- min(state1$y, na.rm = T)
    filename <- "/Users/emilypalmer/Desktop/ClimateProject/MAXT.nc"
    maxt <- tidync(filename) %>% 
        hyper_filter(time = time < timemax,
                     lat = dplyr::between(lat, abs(minlat),abs(maxlat)),
                     lon = dplyr::between(lon-360, minlon,maxlon)) %>% 
        hyper_tibble() %>% 
        mutate(lon2 = ifelse(lon>180,lon-360,lon))
    
    state_polygon <- st_as_sf(state1)
    points <- st_as_sf(maxt, coords = c("lon2", "lat"), crs = st_crs(US))
    joins <- st_join(points, state_polygon) %>% filter(!is.na(ID))
    return(joins)
}


test <- get_state(state = "oregon")
state_geom <- st_as_sf(map("county", region = "oregon",
                           plot = F, fill = T,boundary = T,myboarder = 1))

library(stars)
plot(test)
plot(test["MAXT"])

test 
state_df <- as.data.frame(state_geom)
all <- left_join(test,state_df)


ggplot() +
    geom_sf(data =state_geom, aes(geometry = geom)) 
    
ggplot() +
    geom_sf(data = test, aes(fill = MAXT, geometry = )) 
