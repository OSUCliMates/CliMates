#' Average the max temperature across a state
#'
#' @param df Data frame containing a states max temperatures 
#' @param type Do we take the mean or median? 
#' @return A data frame of averaged max temperature for each day 

averaging <- function(df, type = "mean"){
    if(type == "mean"){
        adf <- df %>% 
            group_by(time) %>% 
            summarize(avg = mean(MAXT)) %>% 
            as.data.frame() %>%
            select(time, avg)
    }else{
        adf <- df %>% 
            group_by(time) %>% 
            summarize(avg = median(MAXT)) %>% 
            as.data.frame() %>%
            select(time, avg)
    }
    return(adf)
}