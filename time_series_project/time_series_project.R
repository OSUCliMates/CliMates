library(tidync)
library(sf)
library(tidyverse)
library(maps)
library(gridExtra)
source("R/get_state.R")
source("R/nlsfitting.R")
source("R/averaging.R")
source("R/discrete_windows.R")
source("R/amplitudes_by_state.R")

# example of using the get_state function and how to use plot to show it
test <- get_state(state = "oregon")
plot(test)
plot(test["MAXT"])

#oregon, new york, nebraska, flordia

## Time series project working on getting the individual states
ny_amplitudes <- amplitudes_by_state(state = "New York", window_year = 1)
or_amplitudes <- amplitudes_by_state(state = "oregon", window_year = 1)
ne_amplitudes <- amplitudes_by_state(state = "nebraska", window_year = 1)
fl_amplitudes <- amplitudes_by_state(state = "florida", window_year = 1)

four_states_amplitudes <- data.frame(ny_amplitudes,
                                     or_amplitudes,
                                     ne_amplitudes,
                                     fl_amplitudes)
write.csv(four_states_amplitudes, 'Output/NyOrNeFl_amplitudes.csv')


time <- 1:39
fllm <- lm(fl_amplitudes~time) 
nylm <- lm(ny_amplitudes~time) 
orlm <- lm(or_amplitudes~time)
nelm <- lm(ne_amplitudes~time) 

florida_residuals <- residuals(fllm)
new_york_residuals <- residuals(nylm)
oregon_residuals <- residuals(nylm)
nebraska_residuals <- residuals(nelm)
stateresids <- data.frame(florida_residuals,new_york_residuals,
                          oregon_residuals,nebraska_residuals)

### Facet wrap plots for time series project presentation
gather(four_states_amplitudes) %>% 
    mutate(year = rep(1:39,4)) %>% 
    ggplot(aes(x = year, y = value)) + geom_line() + 
    facet_wrap(~key)+ 
    ylab("Max Temperature in Kelvin") + 
    xlab("Years after 1979")
## free scales
gather(four_states_amplitudes) %>% 
    mutate(year = rep(1:39,4)) %>% 
    ggplot(aes(x = year, y = value)) +
    geom_point() + 
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~key, scales = "free") + 
    ylab("Max Temperature in Kelvin") + 
    xlab("Years after 1979")
gather(stateresids) %>% 
    mutate(year = rep(1:39,4)) %>% 
    ggplot(aes(x = year, y = value)) + geom_line() + 
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_wrap(~key)+ 
    ylab("Residual") + 
    xlab("Years after 1979")


### Output states
averaged_florida <- get_averaged_state("florida")
write.csv(averaged_florida, 'Output/averaged_florida.csv')
averaged_oregon <- get_averaged_state("oregon")
write.csv(averaged_oregon, 'Output/averaged_oregon.csv')
averaged_nebraska <- get_averaged_state("nebraska")
write.csv(averaged_nebraska, 'Output/averaged_nebraska.csv')
averaged_new_york <- get_averaged_state("new york")
write.csv(averaged_new_york, 'Output/averaged_new_york.csv')



## Working on getting a loop 
usstates <- state.name[-c(2,8,11)] # get rid of hawaii and alaska 
# and delaware (too small)
all_states_mean <- purrr::map(usstates,~amplitudes_by_state(state = .,
                                                            window_year = 1))

delaware <- amplitudes_by_state(state = "delaware",window_year = 1)
windows <- create_windows(window_year)
dfs <- purrr::pmap(windows,~get_state(state = state,timemin=.x,timemax=.y))
dadfs <- purrr::map(dfs,~averaging(df = .x, type = type))
amps <- get.amplitudes(dadfs)


delawareget <- get_state(state = "delaware")

oregon_amplitudes_mean <- amplitudes_by_state(window_year = 1)
oregon_amplitudes_median <- amplitudes_by_state(window_year = 1, type = "median")







#### Plot averaged data in each window: 
plot_windows <- function(window_year = 4, state="oregon", type = "mean"){
    windows <- create_windows(window_year)
    dfs <- purrr::pmap(windows,~get_state(state = state,timemin=.x,timemax=.y))
    adfs <- purrr::map(dfs,~averaging(df = .x, type = type))
    p <- purrr::map(adfs, ~ggplot(., aes(x = time, y = avg)) + geom_line())
    return(p)
}


#### This used for plotting the 39 different windows raw averaged data: 
plot_windows2 <- function(window_year = 4, state="oregon", type = "mean"){
    windows <- create_windows(window_year)
    dfs <- purrr::pmap(windows,~get_state(state = state,timemin=.x,timemax=.y))
    adfs <- purrr::map(dfs,~averaging(df = .x, type = type))
    all <- purrr::reduce(adfs,rbind)
    return(all)
}
windows <- all %>% 
    mutate(window_num = time%/% 365) %>% 
    filter(window_num < 39) %>% 
    mutate(time2 = c(rep(1:365,39)))
## Plots: 
windows %>%  
    ggplot(aes(x = time2, y = avg)) + geom_line() + 
    facet_wrap(~window_num) +
    ggtitle("Max temperature for Oregon") + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())+ 
    ylab("Max Temperature in Kelvin") + 
    xlab("")


raw_datas_plots <- plot_windows(window_year = 1)
grid.arrange(grobs=raw_datas_plots) 
raw_datas_plots4 <- plot_windows(window_year = 4)
grid.arrange(grobs=raw_datas_plots4) + ggtitle("Windows for Oregon, window 4")
raw_datas_plots4_ny <- plot_windows(window_year = 4, state = "new york")
grid.arrange(grobs=raw_datas_plots4_ny) 
raw_datas_plots1_ny <- plot_windows(window_year = 1, state = "new york")
grid.arrange(grobs=raw_datas_plots1_ny) + ggtitle("Windows for New york, window 1")



jpeg("orAmplitudes.jpg", width = 350, height = "350")
data.frame(t = 1:39,oregon_amplitudes_mean,oregon_amplitudes_median) %>% 
    ggplot() +
    geom_line(aes(x = t, y = oregon_amplitudes_mean), color = "blue") +
    geom_line(aes(x = t, y = oregon_amplitudes_median), color = "red")
dev.off()


data.frame(t = 1:39,oregon_amplitudes_mean,oregon_amplitudes_median) %>% 
    ggplot(aes(x = t, y = oregon_amplitudes_mean)) +
    geom_line() + geom_smooth(method = "lm", se = F)
data.frame(t = 1:39,ny_amplitudes) %>% 
    ggplot(aes(x = t, y = ny_amplitudes)) +
    geom_line() + geom_smooth(method = "lm", se = F)

oregon_a_dfs <- data.frame(t = 1:39,oregon_amplitudes_mean,oregon_amplitudes_median)
mod1 <- lm(oregon_amplitudes_mean ~t, data = oregon_a_dfs)
summary(mod1)