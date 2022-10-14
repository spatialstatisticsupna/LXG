## LANDCOVER
## Obtain LandCover raster from the MCD12Q1 Modis LandCover product
lc_RoI_rl <- rsat_get_raster(ptgal,"MCD12Q1","MCD12Q1_LC_Type1")

# This classes can't be burned (based on the MODIS burned area mapping algorithm)
#       - 12: croplands
#       - 13: Urban
#       - 14: 40-60 % croplands
#       - 15: snow and ice
#       - 16: barren
#       - 17: water

# build filter matrix 
BnB_filt <- matrix(c(1, 11, 1,
                11, 256, NA),
              byrow = T,
              nrow = 2)

# obtain raster for the RoI containing 1 for pixels corresponding to burnable
# areas and NA for the pixels corresponding to not burnable areas
notBurnable_mask <- reclassify(lc_RoI_rl, BnB_filt)

# Obtain LandCover for burnable pixels
landCoverBurnable_rl <- lc_RoI_rl * notBurnable_mask

# rm(lc_RoI_rl)

# change LandCover raster name
names(landCoverBurnable_rl) <- 'LandCover'







