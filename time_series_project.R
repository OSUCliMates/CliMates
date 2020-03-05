library(tidync)
library(sf)
library(tidyverse)
library(maps)

create_windows <- function(yrs){
    if(yrs== 4){
        timemin <- seq(0,14244,by = yrs*365.25 + 1)
        timemax <- seq(yrs*365.25,14244 + yrs*365.25,by = yrs*365.25 + 1)
        list <- list(timemins = timemin,timemaxs = timemax)
    }else{ #just ignore leap years for the moment 
        timemin <- seq(0,14244,by = yrs*365 + 1)
        timemax <- seq(yrs*365,14244 + yrs*365,by = yrs*365 + 1)
        list <- list(timemins = timemin,timemaxs = timemax)
    }
    return(list)
}

create_moving_window <- function(years,lag){
    return("do this later??")
}

get_state <- function(state="oregon", timemin=0, timemax=1){
    state1 <- map("county", region = state, plot = F, fill = T)
    maxlon <- max(state1$x, na.rm = T)
    minlon <- min(state1$x, na.rm = T)
    maxlat <- max(state1$y, na.rm = T)
    minlat <- min(state1$y, na.rm = T)
    #filename <- "../st505/ERA-Interim/historical/MAXT.nc"
    filename <- "MAXT.nc"
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
    adf <- df %>% 
        group_by(time) %>% 
        summarize(avg = mean(MAXT)) %>% 
        as.data.frame() %>%
        select(time, avg)
    return(adf)
}


#######################################################################
# This script defines two functions: get.alpha() and get.amplitudes().
# get.alpha() takes in a data frame where the first column is a 
# column of time indices (integers) and the second column is a column
# of maximum (or minimum) temperatures. get.alpha then outputs the 
# amplitude of an nls regression equation of the form
# y = C + alpha*sin(2*pi/365.25)
# get.amplitudes() applies get.alpha() to a list of such data frames
######################################################################


#function to apply to a list of maxt (or mint) dataframes:
get.alpha <- function(frame){
    
    #define dataframe for nls
    df <- data.frame(
        temp = frame[,2],
        t <- 1:length(frame[,2])
    )
    
    #fit nls model
    fit <- nls(temp ~ beta0  + alpha*sin((2*pi/365.25)*t + omega),
               data=df,
               start = list(
                   beta0=mean(frame[,2]), 
                   alpha=sd(frame[,2]), 
                   omega=4.3
               )
    )
    
    #return amplitude
    return(summary(fit)$coefficients[2,1])
}

#function that applies amplitude finder to a list of data frames 
#where column one is a time index and column 2 is the maxtemp
get.amplitudes <- function(inlist){
    return(purrr::map_dbl(inlist,get.alpha))
}


amplitudes_by_state <- function(window_year = 4, state="oregon"){
    windows <- create_windows(window_year)
    dfs <- purrr::pmap(windows,~get_state(state = state,timemin=.x,timemax=.y))
    adfs <- purrr::map(dfs,averaging)
    amps <- get.amplitudes(adfs)
    return(amps)
}

state.name
oregon_amplitudes <- amplitudes_by_state(window_year = 1)
Alabama_amplitudes <- amplitudes_by_state(state = "Alabama")

data.frame(t = 1:39,oregon_amplitudes) %>% 
    ggplot(aes(x = t, y = oregon_amplitudes)) + geom_line()

