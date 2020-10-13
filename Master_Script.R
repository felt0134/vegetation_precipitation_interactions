#master

rm(list=ls())

#set working drectory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#load packages. This will get you through dataframe projection, not the plotting.
library(dplyr)
library(splitstackshape)
library(raster)
library(gstat)
library(rgdal)
library(tidyr)
library(reshape2)
library(ggplot2)

#
source("Functions.R")
source("vegetation_precipitation_driver.R")

#need to re-set working directory after the driver
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

source("core_loop.R")
source("coefficients_dataframe_production.R")
#source("global_fits.R")
