source("R/get_state_data.R")
source("R/nlsfitting.R")
source("R/averaging.R")
source("R/discrete_windows.R")
amplitudes_by_state <- function(window_year = 4, state="oregon", type = "mean"){
    windows <- create_windows(window_year)
    dfs <- purrr::pmap(windows,~get_state(state = state,timemin=.x,timemax=.y))
    adfs <- purrr::map(dfs,~averaging(df = .x, type = type))
    amps <- get.amplitudes(adfs)
    return(amps)
}


get_averaged_state <- function(state="oregon", type = "mean"){
    df <- get_state(state = state)
    adf <- averaging(df)
    return(adf)
}