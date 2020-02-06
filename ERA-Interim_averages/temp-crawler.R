library(tidync)
library(tidyverse)

#Create maxt tibble 
maxt <- hyper_tibble("MAXT.nc")

############################################################
#Indexing independent variables (time, latitude, longitude)#
############################################################
slice0 <- maxt %>% filter(time==0)
lat.dex <- slice0 %>% select(lat) %>% distinct() 
lon.dex <- slice0 %>% select(lon) %>% distinct()
lat.dex <- lat.dex$lat
lon.dex <- lon.dex$lon
remove(slice0)
slice1 <- maxt %>% filter(lat==lat.dex[1]&lon==lon.dex[1])
time.dex <- slice1$time
remove(slice1)

#############################################################################
#nested for loop creates a time series of average max temp ACROSS ALL PIXELS#
#############################################################################
#load maxt into environment
maxt <- hyper_tibble("MAXT.nc")
#initialize an empty vector of average temps
avg.max.temp <- numeric


#time loop 
for(t in time.dex){
  
  #constrains scope of the nested loop to a single time
  time.slice <- maxt %>% filter(time==t)#narrows scope to a given time 
  #initialize the running sum of max temperatures for a given time
  sum.temp <- 0
  
  #latitude loop
  for (i in 1:length(lat.dex)){
    
    #constrains scope of the nested loop to a single latitude
    lat.slice <- time.slice %>% filter(lat==lat.dex[i]) 
    
    #longitude loop 
    for (j in 1:length(lon.dex)){
      
      #records single data point
      dmy <- lat.slice %>% filter(lon==lon.dex[j])  
      
      #adds max temp from single data point to running sum of temps for a given time point
      sum.temp <- sum.temp + dmy$MAXT
    }
  }
  #create average and records to vector
  avg.max.temp[t+1] <- sum.temp/(length(lat.dex)*length(lon.dex))
}
#save vector as csv:
write.csv(avg.max.temp, 'average-Max-Temperature.csv')

remove(maxt)