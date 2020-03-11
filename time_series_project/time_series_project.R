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
usstates <- state.name[-c(2,11)] # get rid of hawaii and alaska 

oregon_amplitudes_mean <- amplitudes_by_state(window_year = 1)
oregon_amplitudes_median <- amplitudes_by_state(window_year = 1, type = "median")


ny_amplitudes <- amplitudes_by_state(state = "New York", window_year = 1)
or_amplitudes <- amplitudes_by_state(state = "oregon", window_year = 1)
ne_amplitudes <- amplitudes_by_state(state = "nebraska", window_year = 1)
fl_amplitudes <- amplitudes_by_state(state = "florida", window_year = 1)

four_states_amplitudes <- data.frame(ny_amplitudes,
                                     or_amplitudes,
                                     ne_amplitudes,
                                     fl_amplitudes)

write.csv(four_states_amplitudes, 'Output/NyOrNeFl_amplitudes.csv')







all_states_mean <- purrr::map(usstates,~amplitudes_by_state(state = .))

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

plot_windows <- function(window_year = 4, state="oregon", type = "mean"){
    windows <- create_windows(window_year)
    dfs <- purrr::pmap(windows,~get_state(state = state,timemin=.x,timemax=.y))
    adfs <- purrr::map(dfs,~averaging(df = .x, type = type))
    p <- purrr::map(adfs, ~ggplot(., aes(x = time, y = avg)) + geom_line())
    return(p)
}


raw_datas_plots <- plot_windows(window_year = 1)
grid.arrange(grobs=raw_datas_plots) + ggtitle("Windows for Oregon, window 1")


raw_datas_plots4 <- plot_windows(window_year = 4)
grid.arrange(grobs=raw_datas_plots4) + ggtitle("Windows for Oregon, window 4")


raw_datas_plots4_ny <- plot_windows(window_year = 4, state = "new york")
grid.arrange(grobs=raw_datas_plots4_ny) 

raw_datas_plots1_ny <- plot_windows(window_year = 1, state = "new york")
grid.arrange(grobs=raw_datas_plots1_ny) + ggtitle("Windows for New york, window 1")
