#' Get data for a given state at a given timeframe
#'
#' @param filename path for where data is located
#' @param state String for the state
#' @param timemin The start time
#' @param timemax The end time
#' @return A data frame of the state data points within the time or NA if state doesnt exist in data


get_state_data <- function(filename, state="Oregon", timemin=0, timemax=14244){
    print(state)
    state1 <- NA
    try(state1 <- maps::map("county", region = state, plot = F, fill = T),
        silent = T)
    
    #state_us <- usmaps::us_map(regions = "counties", include = state)
    
    if(!is.na(state1[1])){
        maxlon <- max(state1$x, na.rm = T)
        minlon <- min(state1$x, na.rm = T)
        maxlat <- max(state1$y, na.rm = T)
        minlat <- min(state1$y, na.rm = T)
        # First filter out a square of the state 
        maxt <- tidync::tidync(filename) %>% 
            tidync::hyper_filter(time = time <= timemax & time >= timemin,
                     lat = dplyr::between(lat, abs(minlat),abs(maxlat)),
                     lon = dplyr::between(lon-360, minlon,maxlon)) %>% 
            tidync::hyper_tibble() %>% 
            dplyr::mutate(lon2 = ifelse(lon>180,lon-360,lon))
        state_polygon <- sf::st_as_sf(state1)
        points <- sf::st_as_sf(maxt, coords = c("lon2", "lat"), crs = sf::st_crs(state_polygon))
        # now filter out points in state polygon
        joins <- sf::st_join(points, state_polygon) %>% filter(!is.na(ID))
    } else {
        joins <- NA
    }
    return(joins)
}

