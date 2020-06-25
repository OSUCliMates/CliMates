#' Get ONI info for a given dataset 
#' 
#' Thanks to Jess for the code 
#' 
#' @param dataset specify 1 = era, 2 = cesm, 3 = liu
#' 
#' 

get_oni <- function(dataset){
  yearmin <- dplyr::case_when(
    dataset ==1 ~ 1979, 
    dataset ==2 ~ 1979, # Change,
    dataset ==3 ~ 2000 # Change
  )
  ONI <- read.csv("~/CliMates/External Data/El Nino Metrics.csv")
  colnames(ONI) <- c("Year", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  ONI <- ONI %>% 
    filter(Year >= yearmin) %>% 
    pivot_longer(cols = -Year,
                 names_to = "Month",
                 values_to="ONI" ) 
  return(ONI)
}



