library(tidync)
library(sf)
library(tidyverse)
library(maps)

create_windows <- function(yrs){
    timemin <- seq(0,14244,by = yrs*365.25 + 1)
    timemax <- seq(yrs*365.25,14244 + yrs*365.25,by = yrs*365.25 + 1)
    list <- list(timemins = timemin,timemaxs = timemax)
}

create_moving_window <- function(years,lag){
    return("hi")
}

get_state <- function(state="oregon", timemin=0, timemax=1){
    state1 <- map("county", region = state, plot = F, fill = T)
    maxlon <- max(state1$x, na.rm = T)
    minlon <- min(state1$x, na.rm = T)
    maxlat <- max(state1$y, na.rm = T)
    minlat <- min(state1$y, na.rm = T)
    filename <- "../st505/ERA-Interim/historical/MAXT.nc"
    #filename <- "MAXT.nc"
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


averaging <- function(df){
    adf <- df1 %>% 
        group_by(time) %>% 
        summarize(avg = mean(MAXT))
}

## Run the functions, and get a list of data frames of avereaged temps for each window
windows <- create_windows(4)
dfs <- purrr::pmap(windows,~get_state(timemin=.x,timemax=.y))
adfs <- purrr::map(dfs,averaging)


