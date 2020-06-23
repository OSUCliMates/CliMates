library(rgeos)
library(devtools)
library(here)
library(ncdf4)

# Load in our functions 
devtools::load_all()

devtools::document()



era_dat <- get_filename() %>%
    get_state_data()

origin <- as.Date("1979-01-01")

oregon <- era_dat %>% 
    mutate(date = as.Date(time, origin=origin))

oregon_prec <- oregon %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(cum_prec = cumsum(PREC))

library(lubridate)
percentiles <- oregon_prec %>%
    mutate(quantile = quantile(cum_prec,.50)) %>%
    filter(cum_prec == quantile) %>%
    group_by(year,quantile) %>%
    summarize(min_date = min(date)) %>%
    mutate(month_day = format(min_date,"%m-%d"))



percentiles %>%
    ggplot() +
    geom_line(aes(x = year, y = month_day))




total_prec <- oregon %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarize(total_prec = sum(PREC))

ggplot(total_prec) +
    geom_line(aes(x = year, y = total_prec))


# get file metadata 
cesmlens <- get_filename(data = "CESM",specific="rcp",type="PREC")
cesmlens_data <- nc_open(cesmlens)
try(print(prec))

# Get CESM RCP Oregon data 
cesm_rcp <- cesmlens %>%
    get_state_data(stat = "oregon",timemax = 3)

# Get ERA Precip data 
era <- get_filename(data = "ERA",type ="PREC")%>%
    get_state_data(stat = "oregon",timemax = 3)


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
    geom_sf(data = cesm_rcp, aes(fill = PREC), color = "red") +
    geom_sf(data = era, aes(fill = PREC), color = "blue") 

#Just plot outlines of all oregon watersheds
ggplot(simp) + geom_sf()





