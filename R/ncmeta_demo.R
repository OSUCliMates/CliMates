# demo of ncmeta package
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

filepath <- files$prec_historic

#get metadata summary
nc_meta(filepath)

#get a quick summary of the variables
nc_vars(filepath)

#if we want to know more about the variable PREC, we use its index, 0:
nc_var(filepath,0)

#let's look at attributes
nc_atts(filepath)

#what are PREC's units?
nc_att(filepath,0,3)#returns a tibble; the value column is what we want:
nc_att(filepath,0,3)$value

#what about the other attributes for PREC?
nc_att(filepath,0,0)$value
nc_att(filepath,0,1)$value
nc_att(filepath,0,2)$value

#this function doesn't seem super helpful
nc_axes(filepath)
nc_axis(filepath,8)

#view the dimensions of an ncdf file.
#note: this one doesn't include PREC, which is the multi-dimensional grid.
nc_dims(filepath)

