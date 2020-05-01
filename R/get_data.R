#' Get .nc filepath for data 
#'
#' @param data String containing data name, can be "ERA","Liu",or"CESM"
#' @param specific which dataset to pull - just for CESM and Liu
#' @param type Can be MAXT,MINT,PREC
#' @return Filepath 
#' 

# only works for ERA,CESM, still working on Liu - missing historical data?
get_data <- function(data = "ERA",specific = "",type = "PREC", year = ""){
  path <- case_when(
    data == "CESM" & specific == "historical" & type == "MAXT" ~ "../ST505/CESM-LENS/historical/MAXT.nc",
    data == "CESM" & specific == "historical" & type == "MINT" ~ "../ST505/CESM-LENS/historical/MINT.nc",
    data == "CESM" & specific == "historical" & type == "PREC" ~ "../ST505/CESM-LENS/historical/PREC.nc",
    data == "CESM" & specific == "rcp" & type == "MAXT" ~ "../ST505/CESM-LENS/rcp85/MAXT.nc",
    data == "CESM" & specific == "rcp" & type == "MINT" ~ "../ST505/CESM-LENS/rcp85/MINT.nc",
    data == "CESM" & specific == "rcp" & type == "PREC" ~ "../ST505/CESM-LENS/rcp85/PREC.nc",  
    data == "ERA" & type == "MAXT" ~ "../ST505/ERA-Interim/historical/MAXT.nc",
    data == "ERA" & type == "MINT" ~ "../ST505/ERA-Interim/historical/MINT.nc",
    data == "ERA" & type == "PREC" ~ "../ST505/ERA-Interim/historical/PREC.nc",
  )
  if(!is.na(path)){
    return(path)
  } else{
    return("Invalid specification of data ")
  }
}
