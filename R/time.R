
# assumes a time column
#TODO change origin for other datasets , this is just for ERA
add_calendar_year_col <- function(data_frame, origin="1979-1-1"){
  new_data <- data_frame %>%
    mutate(calendar_year = as.Date(time, origin = "1979-1-1"))
  return(new_data)
}

# assumes a "time" column
# thanks to Kate for code and ideas
add_water_year_col <- function(data_frame){
  new_data <- data_frame %>% 
    mutate(water_year = (time - 274)%%365)
  return(waterday)
}

# just for individual days 
calculate_water_year <- function(day){
  return((day - 274)%%365)
}