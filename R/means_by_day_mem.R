# for CESM-LENS
# this function takes in netcdf file and lat/long of rectangle of interest
# returns list with summary stat organized by member and day. 
# could be used with other summary stats and temps instead of prec. 
library(tidyverse)

by_day_mem <- function(file, lonmin, lonmax, latmin, latmax){
    loc_of_int <- tidync(file) %>% # filters out location of interest
        hyper_filter(lon = lon > lonmin & lon < lonmax,  # sim to get_state.R
                     lat = lat > latmin & lat < latmax) %>%
        hyper_tibble() %>%
        mutate(lon2 = ifelse(lon>180, lon-360,lon)) # updates longitude to be values humans are used to
    file_w_summary <- loc_of_int %>%
        group_by(mem, time) %>%
        summarise(daymean = (mean(PREC)*8640000)) #change summary stat here if wanted
                                                    # *8640000 changes prec from m/s to cm/day
    return(file_w_summary)
}

prec_means <- by_day_mem("/home/ST505/CESM-LENS/historical/PREC.nc",
                        235.3, 
                        236.4, 
                        42.0016,
                        46.2585)