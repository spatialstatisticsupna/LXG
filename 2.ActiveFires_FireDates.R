# Month for which burned area is going to be mapped (date of interest)
datOfInt <- '201710'

# Read rtoi generated in the previous step
rtoi.path <- file.path("D:/galicia/rtoi/PPXG/")
ptgal <- read_rtoi(rtoi.path)

# data available within the rtoi can be listed using
# rsat_list_data(ptgal)

# define reference RasterLayer for the RoI (Region of Interest) taking NA values
ref_regOfInt_rl <- rsat_get_raster(ptgal, "MOD09GA", "NBR1")[[1]] %>% setValues(NA)


# Active fires (from MODIS and VIIRS).
# Read active fires SpatialPointsDataFrame:
# position (in UTM coordinates) and fire date
modis_AF_spdf <- readOGR('./Data/activeFires/AF_MOD') # From MODIS sensor
viirs_AF_spdf <- readOGR('./Data/activeFires/AF_VII') # From VIIRS sensor
# Download from: https://firms.modaps.eosdis.nasa.gov/download/create.php

# adapting VIIRS data size may be necessary
# viirs<-viirs[,1:14]

# Change column names in order to combine both data frames
names(viirs_AF_spdf) <- names(modis_AF_spdf)

# Combine VIIRS and MODIS data into a single SpatialPointsDataFrame
AFfull_spdf <- rbind(modis_AF_spdf, viirs_AF_spdf)

# Extract from the SPDF containing all active fires the observations corresponding
# to the RToI, and change the date format to a numeric julian format
actFires_RDoI_spdf <- AFfull_spdf %>% 
  mutate(date = as.numeric(format(as.Date(ACQ_DATE), "%Y%m"))) %>% 
  filter(date== datOfInt) %>% 
  spTransform(crs(ref_regOfInt_rl)) %>% 
  raster::intersect(ref_regOfInt_rl) %>% 
  mutate(ACQ_DATE = as.numeric(format(as.Date(ACQ_DATE), "%Y%j"))) 

# transform the SPDF into a raster containing the fire detection data for each pixel
actFires_RDoI_rl <- rasterize(actFires_RDoI_spdf, ref_regOfInt_rl, field = actFires_RDoI_spdf$ACQ_DATE, 
                       update=TRUE, updateValue="NA")


# freq(actFires_RDoI_rl)
# can be used to see the number of active fires for each day

source("2.1_Landcover_Notburnablemask.R")
# Change extent for the rasters containing the not burnable mask and the burnable 
# LandCover to make them overlap with the active fire raster
extent(notBurnable_mask) <- extent(actFires_RDoI_rl)
extent(landCoverBurnable_rl) <- extent(notBurnable_mask)

# apply mask to extract not burnable pixels
actFires_RDoI_rl <- actFires_RDoI_rl * notBurnable_mask

# create new raster taking 1 in the active fires instead of the observation date
actFires_mask <- reclassify(actFires_RDoI_rl, matrix(c(-Inf,Inf,1)))

# Distance to nearest active fire in kilometers (will be used later)
dist_actFire_rl <- distance(actFires_mask)/1000

# rename raster containing distances to active fires
names(dist_actFire_rl) <- 'distAF'

## Voronoi diagram assigning to each polygon the nearest active fires observation date

library(dismo)

# Build SPolyDF assigning the active fire observation date corresponding to its
# center to each polygon
voronoi_actFires_sPolydf <- voronoi(actFires_RDoI_spdf) %>% 
  spTransform(crs(ref_regOfInt_rl))

# Build raster containing the nearest fires observation date based on the Voronoi diagram
actFire_date_rl <- rasterize(voronoi_actFires_sPolydf, ref_regOfInt_rl, 
                 field = voronoi_actFires_sPolydf$ACQ_DATE )

# The active fire dates will be used to obtain the previous and posterior time
# series used for computing the differenced indexes
