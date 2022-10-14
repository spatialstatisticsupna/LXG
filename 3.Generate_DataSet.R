# Read RasterBrick corresponding to the MOD09GA product cloud masks
cld.mask.t <- rsat_get_raster(ptgal, "MOD09GA", "CloudMask") %>% 
  disaggregate(fact= c(2,2))

names(cld.mask.t) <- sprintf("X%s",format(dates(ptgal)[-length(dates(ptgal))],"%Y%j"))

# Read RasterBrick corresponding to the MYD09GA product cloud masks
cld.mask.a <- rsat_get_raster(ptgal, "MYD09GA", "CloudMask") %>% 
  disaggregate(fact= c(2,2))

names(cld.mask.a) <- sprintf("X%s",format(dates(ptgal)[-length(dates(ptgal))],"%Y%j"))

# Names of the indexes to difference
indexes <- c('NBR1', 'NBR2', 'MVI', 'MIRBI', 'NIR', 'NDVI')

# Load function to compute differenced indexes
source('compIndex.R')

# Initialize RasterStack to save the information
finalVariables <- stack()

for (indx in indexes){
  
  # compute differenced index
  index_dat <- compIndex(index = indx,
                         notBurnableMasckPG = notBurnable_mask,
                         cld.mask.t = cld.mask.t,
                         cld.mask.a = cld.mask.a,
                         Artoi = ptgal,
                         DoI = actFire_date_rl
                         )  
  
  # add differenced index to the finalVariables stack 
  finalVariables <- addLayer(finalVariables, index_dat)
  
}

# Add distance and LandCover to the finalVariables stack
finalVariables <- addLayer(finalVariables, dist_actFire_rl, landCoverBurnable_rl)

# The finalVariables RasterStack will be used as input for the next step

#####
# EFFIS burned areas
#####

# Read SPolyDF of burned areas corresponding to the DoI (october 2017) from the 
# EFFIS wildfire database
ptgal_BA_spolydf <- readOGR('./incenESPT') %>%
  mutate(firedateMonth = as.numeric(format(as.Date(FIREDATE), "%Y%m"))) %>%
  spTransform(crs(finalVariables)) %>%
  raster::intersect(finalVariables[[1]]) %>%
  filter(firedateMonth == datOfInt)
# see effis.pdf 
# https://effis.jrc.ec.europa.eu/apps/data.request.form/

#####
# MCD64A1 MODIS burned areas
#####

# build rasters from the .tif files for the MCD64A1 (burned area) product
ModisBAfiles <- list.files('./Data/MCD64A1/201710/',full.names = T)
north_BA_rl <- raster(ModisBAfiles[1])
south_BA_rl <- raster(ModisBAfiles[2])

# Combine rasters
ns_BA_rl <- raster::mosaic(north_BA_rl, south_BA_rl, fun=max)

# Extract the pixels intersecting the region of interest
Mod_BA_RoI_rl <- raster::intersect(ns_BA_rl, finalVariables[[1]])

#change variable name for active fires
actFires <- actFires_RDoI_spdf

# compute point process intensity
source("Z.cov.int.nokm.R")

library(spatstat)
library(raster)

actFire.intensity <- Z.cov.int.nokm(finalVariables,actFires)
names(actFire.intensity) <- "aF.int"

# save workspace needed as input for the training phase
save.image("Data/input_4.TrainingTest_ndvi2009.RData")
