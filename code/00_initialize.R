### these are all the packages we use throughout the project (there may be extra here)
library(PanelMatch)
library(httr)
library(readxl)
library(SearchTrees)
library(jsonlite)
library(gstat)
library(scales)
library(ebal)
library(did)
library(stargazer)
library(raster)
library(miceadds)
library(vroom)
library(ggeffects)
library(tidycensus)
library(rnoaa)
library(spdep)
library(AER)
library(maptools)
library(tigris)
library(rgdal)
library(spatialreg)
library(sqldf)
library(lubridate)
library(rgdal)
library(tidyverse)
library(data.table)
library(kevostools)

### this is a quick function that allows us to clear memory without losing things we
### want to keep from one script to the next

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")
options("modelsummary_format_numeric_latex" = "plain")

cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}

