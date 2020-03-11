create_windows <- function(yrs){
    if(yrs== 4){
        timemin <- seq(0,14244,by = yrs*365.25 + 1)
        timemax <- seq(yrs*365.25,14244 + yrs*365.25,by = yrs*365.25 + 1)
        list <- list(timemins = timemin,timemaxs = timemax)
    }else{ #just ignore leap years for the moment 
        timemin <- seq(0,14244,by = yrs*365 + 1)
        timemax <- seq(yrs*365,14244 + yrs*365,by = yrs*365 + 1)
        list <- list(timemins = timemin,timemaxs = timemax)
    }
    return(list)
}