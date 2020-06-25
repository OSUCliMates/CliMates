# Code for all quadrants of Oregon to be stored here

library(tidyverse)
library(printr)
library(ggplot2)
library(ggrepel)
library(colorspace)
library(tidync)
library(maps)
library(ncdf4)
library(fields)
library(pracma)
library(raster)
library(RColorBrewer)
library(ggmap)
library(lubridate)
options(scipen=50)

ERAfile <- "~/../ST505/ERA-Interim/historical/PREC.nc"

# Note: Change [2] to -125 to include stations in ocean
SEPts <- c(-117, -120.5, 41.5, 44)
SWPts <- c(-120.5, -124, 41.5, 44)
NWPts <- c(-120.5, -124, 44, 46)
NEPts <- c(-117, -120.5, 44, 46)
allORpts <- c(-117, -124, 41.5, 46)

# Function to create data frame for given points
quadDF <- function(filename, points){
    DF <- tidync(filename) %>% 
        hyper_filter(lat = dplyr::between(lat, abs(points[3]),abs(points[4])),
                     lon = dplyr::between(lon-360, points[2], points[1])) %>% 
        hyper_tibble()
    DF <- DF %>%
        mutate(lon2 = ifelse(lon>=180,lon-360,lon),
               time2 = as.Date(time,origin="1979-1-1 00:00:0.0"),
               locat = interaction(lat, lon))
    return(DF)
}

# Creating column to identify which points belong to which quadrant (there is probably an easier way to do this in dplyr?)
SE <- quadDF(ERAfile, SEPts) %>%
    mutate(quadrant = "SE")
SW <- quadDF(ERAfile, SWPts) %>%
    mutate(quadrant = "SW")
NW <- quadDF(ERAfile, NWPts) %>%
    mutate(quadrant = "NW")
NE <- quadDF(ERAfile, NEPts) %>%
    mutate(quadrant = "NE")

# Bind all above into one df (raw ERA values)
allOR <- bind_rows(SE, SW, NW, NE) %>%
    subset(select=-c(lon, time))

# Where the points are in the state
ggplot(data=allOR)+
    geom_point(mapping=aes(x=lon2, y=lat, color=quadrant)) +
    borders("state")+
    coord_cartesian(xlim=c(-125, -116), ylim = c(41, 46))

# Save wide dataset before manip for Shiny purposes
write.csv(allOR, "AllORERAShinyWide.csv")

# Manipulation to create DF with ave prop rain days over entire quadrants' stations for a given month/year (for each quadrant)
AvePropONI <- allOR %>% mutate(Month=as.character(format(as.Date(time2), "%m")),
                          Year=as.numeric(format(as.Date(time2), "%Y"))) %>%
    group_by(Year, Month, locat, quadrant) %>%
    summarise(propRainDays=sum(PREC>(6*10^-8))/n()) %>%            # Number of rain days/total days recorded for all locations, months, years
    group_by(quadrant, Month, Year) %>%                            
    summarise(avePropRainDays = mean(propRainDays))                # Average proportion rain days averaged over the quadrant (value for each month & year)

# Dataframe for oceanic Nino index
ONI <- read.csv("~/CliMates/External Data/El Nino Metrics.csv")
colnames(ONI) <- c("Year", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
ONI <- ONI %>%
    filter(Year>=1979) %>%            # Earliest year available in ERA dataset
    pivot_longer(cols = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), names_to = "Month", values_to="ONI") %>%
    filter(Month != "Year")

AvePropONI <- left_join(ONI, AvePropONI)

# Function to create average proportion rain days and ONI plot
# Takes quadrant of interest (char string), and years in which to facet plots
plot1 <- function(quad, yearmin, yearmax){
    trimmed <- AvePropONI %>%
        filter(quadrant %in% quad & between(Year, yearmin, yearmax))
    
    plot <- ggplot(trimmed) +
                geom_col(aes(x=Month, fill=ONI, y=1.5), width=1)+
                geom_col(aes(x=Month, fill=ONI, y=-1), width=1)+
                scale_fill_distiller(palette="Spectral")+
                scale_color_manual(values = c("Black", "Blue", "Red", "Forest Green"))+
                geom_line(mapping=aes(x=Month, y=avePropRainDays, color=quadrant, group=quadrant))+
                theme(axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())+
                scale_y_continuous(name="Average proportion of rain days", limits=c(-1,1.5))+
                facet_wrap(~Year)+
                xlab("Month")+
                labs(fill="Oceanic Niño Index")+
                ggtitle("Average proportion of rain days with Oceanic Niño Index")
    
    View(trimmed)
    return(plot)
}


# Test plot 1
plot1(c("SE", "SW"), 1980, 1985)

# Function to create deseasonalized average proportion rain days and ONI plot
# Takes quadrant of interest (char string), and years in which to facet plots
DesAvePropONI <- AvePropONI %>%
    group_by(Month) %>%
    mutate(DevFromAve=avePropRainDays-mean(avePropRainDays, na.rm=TRUE),
           devFromAveOMI=ONI-mean(ONI, na.rm=TRUE))

plot2 <- function(quad, yearmin, yearmax){
    trimmed <- DesAvePropONI %>%
        filter(quadrant %in% quad & between(Year, yearmin, yearmax))
    
    plot <- ggplot(trimmed) +
                geom_col(aes(x=Month, fill=devFromAveOMI, y=0.75), width=1)+
                geom_col(aes(x=Month, fill=devFromAveOMI, y=-0.75), width=1)+
                geom_hline(aes(yintercept=0), linetype="dashed")+
                scale_fill_distiller(palette="Purples", direction =1)+
                geom_line(mapping=aes(x=Month, y=DevFromAve, color=quadrant, group=quadrant))+
                scale_color_manual(values=c("Black", "Blue", "Red", "Forest Green"))+
                theme(axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())+
                scale_y_continuous(name="Average proportion of rain days", limits=c(-0.75, 0.75))+
                facet_wrap(~Year)+
                xlab("Month")+
                labs(fill="Oceanic Niño Index", color="Quadrant of OR")+
                ggtitle("Deseasonalized average proportion of rain days with Oceanic Niño Index")
    return(plot)
}

# Test plot 2
plot2(c("SE", "NW"), 1980, 1990)

# Save allOR as .csv for shiny app makin'
write.csv(DesAvePropONI, "AllORERAShinyData.csv")




##### Connected scatterplot for yearly average prop rain days and ONI

# DF for ave prop rain days
allORCon <- allOR %>% mutate(Month=as.character(format(as.Date(time2), "%m")),
                          Year=as.numeric(format(as.Date(time2), "%Y"))) %>%
    group_by(Year, locat, quadrant) %>%
    summarise(propRainDays=sum(PREC>(6*10^-8))/n()) %>%            # Number of rain days/total days recorded for all locations, months, years
    group_by(quadrant, Year) %>%                            
    summarise(avePropRainDays = mean(propRainDays))   

# Dataframe for oceanic Nino index
ONICon <- ONI %>%
    group_by(Year) %>%
    summarise(AveONI = mean(ONI))

allORCon <- left_join(ONICon, allORCon)


test <- allORCon %>% 
    filter(quadrant %in% c("NE"))

ggplot(test, aes(x=Year, y=avePropRainDays, label=Year))+
    geom_line()+
    geom_point()

#### Going to try to plot variance of monthly precip by year
# Manipulation to create DF with ave prop rain days over entire quadrants' stations for a given month/year (for each quadrant)
QuadORVar <- allOR %>% mutate(Month=as.character(format(as.Date(time2), "%m")),
                          Year=as.numeric(format(as.Date(time2), "%Y"))) %>%
    group_by(Year, Month, quadrant, locat) %>%
    summarise(TotMoPrecipPt=sum(PREC)) %>%      # Total precip for month, by quadrant
    group_by(quadrant, Year, Month) %>%
    summarise(TotMoPrecipAveQuad=mean(TotMoPrecipPt)) %>%
    group_by(quadrant, Year) %>%
    mutate(varTotPrecip = var(TotMoPrecipAveQuad))

QuadORTot <- allOR %>% mutate(Month=as.character(format(as.Date(time2), "%m")),
                             Year=as.numeric(format(as.Date(time2), "%Y"))) %>%
    group_by(Year, quadrant, locat) %>%
    summarise(TotYrPrecipPt=sum(PREC)) %>%
    group_by(quadrant, Year) %>%
    summarise(TotYrPrecipAveQuad = mean(TotYrPrecipPt)) %>%
    group_by(quadrant) %>%
    mutate(CumuPrecip=cumsum(TotYrPrecipAveQuad))


ggplot(QuadORVar)+
    geom_line(aes(x=Year, y=varTotPrecip, color=quadrant, group=quadrant), size=0.8)+
    scale_color_manual(values=c("Black", "Blue", "Red", "Forest Green"))+
    labs(color="Quadrant of OR", 
         y="Variance in Total Monthly Precipitation (m)")+
    ggtitle("Variability of total monthly precipitation by year")

ggplot(QuadORTot)+
    geom_line(aes(x=Year, y=TotYrPrecipAveQuad, color=quadrant, group=quadrant), size=0.8)+
    scale_color_manual(values=c("Black", "Blue", "Red", "Forest Green"))+
    labs(color="Quadrant of OR", 
         y="Total yearly precipitation (m)")+
    ggtitle("Total yearly precipitation")

ggplot()+
    geom_line(data=QuadORTot, aes(x=Year, y=CumuPrecip, color=quadrant, group=quadrant), size=0.8)+
    scale_color_manual(values=c("Black", "Blue", "Red", "Forest Green"))+
    labs(color="Quadrant of OR",
         y="Cumulative precipitation (m)")+
    ggtitle("Cumulative precipitation")

