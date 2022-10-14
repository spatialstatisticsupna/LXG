## Load libraries
suppressPackageStartupMessages(
  {
    library(sp)
    library(sf)
    library(geodata)
    library(rgdal)
    library(raster)
    library(mapview)
    library(rsat)
    library(spdplyr)
    library(caret)
    library(xgboost)
  }
)
## Download satellite data and derive indexes from spectral bands
source('1.Download_Gen_Index.R')
## Build active fire distance and dates and LandCover rasters
source('2.ActiveFires_FireDates.R')
## Build differenced index rasterStack and input dataset
source('3.Generate_DataSet.R')
## Build classification reference, transform the data set and apply XGBOOST
source('4.TrainingTest.R')
## Show results
source('5.showResults.R')

## NOT RUN
library(deldir) # in order to have RStudio warn that they need to be downloaded 
                # (there is no need to import these packages)
library(rgeos)