# # psuedo code for windowing that I think is wrong
# 
# store current date(?)
# 
# if year = year & month <= month
# KEEP
# elseif year=year &month=month & day <day
# KEEP
# 
# for (i in 1:3)
#     if year =year - i
# KEEP
# 
# 
# if year=year-4 & month > month
# KEEP
# if year=year-4 and month=month and day>= day 
# KEEP

# Kate's code from maxt-series.Rmd
#library(tidyverse)

#library(ggfortify)
#library(gridExtra)

#maxtfile <- "ERA-Interim_averages/average-Max-Temperature.csv"
#maxt <- read_csv(maxtfile)

#dvec <- character(length=length(maxt$avg.max.temp))
#for (i in 1:length(maxt$avg.max.temp)){
#    dvec[i] <- paste(maxt$year[i],maxt$month[i],maxt$day[i],sep="-")
#}
#maxt <- maxt %>%
#    mutate(date=lubridate::ymd(dvec),maxtemp=avg.max.temp)
#maxt <- select(.data=maxt,date,maxtemp)
#head(maxt)
#remove(dvec)

# new route
#t <- 1:length(maxt$date) #makes t index
#combined <- cbind(maxt, t) #adds t index

#step1 <- combined[which(combined$t>0),] #sorts out rows with t>first day
#new <- step1[which(step1$t<1462),] #sorts out rows with t<date of interest

#for (i in 1463:length(t)){
#    indx <- i
#    sec.indx <- indx-i
#    step1 <- combined[which(combined$t>sec.indx),]
#    step2 <- step1[which(step1$t<indx),]
#    new <- cbind #?? how to make the data frames go together
#}




