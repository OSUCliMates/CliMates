# If we want to make the Shiny side data load faster, make smaller datasets already filtered by state. 
# just for era for now
era_file <- get_filename()

for(state in state.name[1:3]){
  era_state <- get_state_data(filename = era_file, state = state)
  if(!is.null(nrow(era_state))){
    era_state <- era_state %>%
      separate(col = ID, 
               sep = ",", 
               into = c("State","County")) %>% 
      select(!State)
    state_filename <- paste("~/Climates/Output/era_states/",state,".csv", sep = "")
    write.csv(era_state, file = state_filename)
  }
}







