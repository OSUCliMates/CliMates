library(tidync)
library(tidyverse)

maxtemp <- tidync("MAXT.nc")
############################################################
#Indexing independent variables (time, latitude, longitude)#
############################################################
time.dex <- maxtemp %>% activate("D0") %>% hyper_array()
lat.dex <- maxtemp %>% activate("D1") %>% hyper_array()
lon.dex <- maxtemp %>% activate("D0") %>% hyper_array()

###################################################################
# loop creates a time series of average max temp ACROSS ALL PIXELS#
###################################################################

#initialize an empty vector of average temps
averages <- numeric()


#time loop 
for(t in time.dex$time){
  
  #update frame (a grid of maximum temperatures)
  frame <- maxtemp %>% hyper_filter(time=time==t) %>% hyper_array()
  #take mean and add to vector
  averages <- c(averages, mean(frame$MAXT))
}

#construct data frame to be written as .csv
time_parts <- RNetCDF::utcal.nc(time.units$value, time.dex$time)%>%
  select(c(1,2,3))
outframe <- data.frame(year=time_parts[,1], 
                       month=time_parts[,2],
                       day=time_parts[,3],
                       avg.max.temp=averages)

#save vector as csv:
write.csv(outframe, 'average-Max-Temperature.csv')
