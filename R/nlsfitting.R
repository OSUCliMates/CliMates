#######################################################################
# This script defines two functions: get.alpha() and get.amplitudes().
# get.alpha() takes in a data frame where the first column is a 
# column of time indices (integers) and the second column is a column
# of maximum (or minimum) temperatures. get.alpha then outputs the 
# amplitude of an nls regression equation of the form
# y = C + alpha*sin(2*pi/365.25)
# get.amplitudes() applies get.alpha() to a list of such data frames
######################################################################


#function to apply to a list of maxt (or mint) dataframes:
get.alpha <- function(frame){

    #define dataframe for nls
    df <- data.frame(
        temp = frame[,2],
        t <- 1:length(frame[,2])
    )
    
    #fit nls model
    fit <- nls(temp ~ beta0  + alpha*sin((2*pi/365.25)*t + omega),
               data=df,
               start = list(
                   beta0=mean(frame[,2]), 
                   alpha=sd(frame[,2]), 
                   omega=4.3
                   )
               )
    
    #return amplitude
    return(summary(fit)$coefficients[2,1])
}

#function that applies amplitude finder to a list of data frames 
#where column one is a time index and column 2 is the maxtemp
get.amplitudes <- function(inlist){
    return(map(inlist,get.alpha))
}
