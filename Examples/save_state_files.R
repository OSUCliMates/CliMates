# If we want to make the Shiny side data load faster, make smaller datasets already filtered by state. 
# This will save an individual rds file for each state
# just for era for now
era_file <- get_filename()

for(state in state.name[1:8]){
  era_state <- get_state_data(filename = era_file, state = state)
  if(!is.null(nrow(era_state))){
    era_state_counties <- era_state %>%
      separate(col = ID, 
               sep = ",", 
               into = c("State","County")) %>% 
      select(!State) %>%
      mutate(calendar_date = as.Date(time,origin="1979-1-1"))
    state_filename <- paste("~/Climates/Output/era_states/",state,".csv", sep = "")
    write_rds(era_state_counties, file = state_filename)
  }
}




