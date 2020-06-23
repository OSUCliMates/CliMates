library(tidync)
library(dplyr)
library(ggplot2)
library(purrr)
library(ncmeta)

#filenames
files <- list(
    maxt_historic = "/home/ST505/CESM-LENS/historical/MAXT.nc",
    mint_historic = "/home/ST505/CESM-LENS/historical/MINT.nc",
    prec_historic = "/home/ST505/CESM-LENS/historical/PREC.nc",
    maxt_rcp85    = "/home/ST505/CESM-LENS/rcp85/MAXT.nc",
    mint_rcp85    = "/home/ST505/CESM-LENS/rcp85/MINT.nc",
    prec_rcp85    = "/home/ST505/CESM-LENS/rcp85/PREC.nc"
)

#create members vector and view
members <- tidync(files$maxt_historic)%>%
    hyper_filter(time = time==25550)%>%
    hyper_tibble()%>%
    select(mem)%>%
    unique()
members <- members$mem
print(members)

min_time <- tidync(filepath)%>%
    hyper_filter(mem = mem==1)%>%
    hyper_tibble()%>%
    select(time)%>%
    min()

get_yearly_average_MAXT <- function(year,member){
    tidync(files$maxt_historic)%>%
        hyper_filter(mem  = mem==member)%>%
        hyper_filter(time = (time-min_time)%/%365==year-1920)%>%
        hyper_tibble()%>%
        summarise(avg_precip = mean(MAXT))
}

