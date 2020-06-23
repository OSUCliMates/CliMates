#' Get .nc filepath for data 
#'
#' @param data String containing data name, can be "ERA","Liu",or"CESM"
#' @param specific which dataset to pull - just for CESM and Liu options include historical and rcp for CESM and historical and endOfCentury for Liu 
#' @param type Can be MAXT,MINT,PREC, defaults to PREC
#' @param year_range for Liu-etal only: year range in 2000-2013, defaults whole range. 
#' @return Filepath (or string of filenames) to access .nc file of choosing 

# only works for ERA,CESM, still working on Liu - missing historical data?
# Working directory needs to be CliMates directory. 
get_filename <- function(data = "ERA",specific = "historical",type = "PREC", year_range = c(2000,2013)){
  path <- dplyr::case_when(
    data == "CESM" & specific == "historical" & type == "MAXT" ~ "../../ST505/CESM-LENS/historical/MAXT.nc",
    data == "CESM" & specific == "historical" & type == "MINT" ~ "../../ST505/CESM-LENS/historical/MINT.nc",
    data == "CESM" & specific == "historical" & type == "PREC" ~ "../../ST505/CESM-LENS/historical/PREC.nc",
    data == "CESM" & specific == "rcp" & type == "MAXT" ~ "../../ST505/CESM-LENS/rcp85/MAXT.nc",
    data == "CESM" & specific == "rcp" & type == "MINT" ~ "../../ST505/CESM-LENS/rcp85/MINT.nc",
    data == "CESM" & specific == "rcp" & type == "PREC" ~ "../../ST505/CESM-LENS/rcp85/PREC.nc",  
    data == "ERA" & type == "MAXT" ~ "../../ST505/ERA-Interim/historical/MAXT.nc",
    data == "ERA" & type == "MINT" ~ "../../ST505/ERA-Interim/historical/MINT.nc",
    data == "ERA" & type == "PREC" ~ "../../ST505/ERA-Interim/historical/PREC.nc",
  )
  if(data == "Liu" & specific == "historical"){
    liu_path <- "../../ST505/WRF-Liu-etal/historical"
    range <- year_range[1]:year_range[2]
    files <- list.files(liu_path)
    detect <- str_detect(files,type)
  }
  if(!is.na(path)){
    return(path)
  } else{
    return("Invalid specification of data ")
  }
}



