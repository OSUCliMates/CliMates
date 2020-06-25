
library(tidync)

# find all the unique lat/long pairs for era dataset 
era_filename <- get_filename()
era_lat_lon <- tidync(era_filename) %>%
  hyper_filter(time = time == 0) %>% 
  hyper_tibble() %>% 
  dplyr::select(lat,lon) %>% 
  dplyr::mutate(lon = ifelse(lon>180,lon-360,lon),
                dataset = "era")
  
# find all the unique lat/long pairs for the lens dataset
lens_filename <- get_filename(data = "CESM")
lens_lat_lon <- tidync(lens_filename) %>% 
  hyper_filter(time = time == 25550,
               mem = mem ==1) %>% 
  hyper_tibble() %>% 
  dplyr::select(lat,lon) %>% 
  dplyr::mutate(dataset = "lens",
                lon = ifelse(lon>180,lon-360,lon))

#combine into 1 dataset
lat_lon_df <- rbind(era_lat_lon,lens_lat_lon)

write_csv(lat_lon_df,"~/CliMates/Output/lat_lon_pairs.csv")
