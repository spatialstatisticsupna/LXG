library(xgboost)
library(raster)
library(spdplyr)
load("Data/results_pp_rep.RData")

seed = 53
set.seed(seed)

fire_model_med <- xgboost(data = fire$train_mat,
                          label = fire$train.df$lclass,
                          params = best_param,
                          nrounds = max(hyper_grid_2[, 7]))

predict_val <- predict(fire_model_med, fire$test_mat)

# predict using the trained model
predict_all <- predict(fire_model_med, fire$FullDataset_mat)
# transform values to classes
predict_all <- ifelse(predict_all > 0.5, 1, 0)

##############
# Build prediction and classification rasters
##############
## Build prediction rasters
# add predictions to the FullDataset DataFrame
FullDataset.df$pred <- predict_all
# Start from the reference raster
ref_raster_rl <- finalVariables[[1]] %>% setValues(NA)
pred_raster_rl <- ref_raster_rl
names(pred_raster_rl) <- "Prediction"
# obtain indexes within the raster for pixels with each prediction (1 and 0)
ind_PosPred <-
  as.numeric(rownames(FullDataset.df[which(FullDataset.df$pred == 1), ]))
ind_NegPred <-
  as.numeric(rownames(FullDataset.df[which(FullDataset.df$pred == 0), ]))
# substitute values into the raster
pred_raster_rl[ind_PosPred] <- 1
pred_raster_rl[ind_NegPred] <- 0

## Build classification raster
# start from reference raster
cl_raster_rl <- ref_raster_rl
names(cl_raster_rl) <- "Reference"
# obtain indexes within the raster for pixels from each class (1 and 0)
ind_PosCl <-
  as.numeric(rownames(FullDataset.df[which(FullDataset.df$lclass == 1), ]))
ind_NegCl <-
  as.numeric(rownames(FullDataset.df[which(FullDataset.df$lclass == 0), ]))
# substitute values into the raster
cl_raster_rl[ind_PosCl] <- 2
cl_raster_rl[ind_NegCl] <- 0

# add both rasters
clasif_raster <- cl_raster_rl + pred_raster_rl
# TN = 0, FP = 1, FN = 2, TP = 3

##############
# prepare MCD64A1 classification raster
##############

# mask NAs from the classification reference
NA_mask <- reclassify(cl_raster_rl, matrix(c(-Inf, Inf, 1)))

# apply Mask to MODIS burned area raster
extent(Mod_BA_RoI_rl) <- extent(ref_raster_rl)
Mod_BA_Masked_rl <- Mod_BA_RoI_rl * NA_mask

## recodify burned area raster
BA_filt <- matrix(c(-Inf,-1, 0,-1, 1, 0,
                    1, Inf, 1),
                  byrow = T,
                  nrow = 3)
Ref_ModisBA_rl <- reclassify(Mod_BA_Masked_rl, BA_filt)
# rename
names(Ref_ModisBA_rl) <- "MCD64A1"

# recodify MODIS burned area raster
Ref_ModisBA02_rl <- reclassify(Ref_ModisBA_rl, matrix(c(1, 2), nrow = 1))

# add pred vs MCD
clasif_raster_MCD <- pred_raster_rl + Ref_ModisBA02_rl

##########
# Comparative graphs
##########

cl_ref_mcd_raster <- addLayer(clasif_raster,clasif_raster_MCD)

extent(cl_ref_mcd_raster) <- extent(c(xmin(cl_ref_mcd_raster), 
                                      xmax(cl_ref_mcd_raster), 
                                      ymin(cl_ref_mcd_raster), 
                                      ymax(cl_ref_mcd_raster))/1000)
projection(cl_ref_mcd_raster) <- gsub("units=m", "units=km", projection(cl_ref_mcd_raster))


scale = list("SpatialPolygonsRescale", layout.scale.bar(height = 0.1), 
             offset = c(-760,4850), scale = 50, fill=c("transparent","black"))
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(-77e1,46e2),
          scale = 40)
l3 = list("sp.text", c(-76e1,4840), "0",scale=50)
l4 = list("sp.text", c(-71e1,4840), "50 km",scale=50)
rect = list("panel.rect",xleft=-710, ybottom=4425,xright=-620, ytop=4550, alpha=1)

labelat <- c(0.375,1.125,1.875,2.625,3.375)
labeltext<- c("TN","FP","FN","TP","NA")

spplot(cl_ref_mcd_raster[[1]],
       main = c("PPXG prediction vs. reference classification",
                "PPXG prediction vs. MCD64A1 classification")[1],
       col.regions=c(terrain.colors(3)[1],rainbow(12)[c(3,1,8)],"white"),
       at = c(0,0.75,1.5,2.25,3,3.75),
       scales=list(draw=F),
       sp.layout=list(scale,l3,l4,l2,rect),#,which=1),
       par.settings = list(fontsize = list(text = 11)),
       colorkey = list(
         labels=list(
           at = labelat,
           labels = labeltext
         ),
         at = c(0,0.75,1.5,2.25,3,3.75),
         # pretty = TRUE,
         height = 0.2
       ),
       maxpixels = 687124
       # skip = c(TRUE, TRUE, FALSE)
)

spplot(cl_ref_mcd_raster[[2]],
       main = c("PPXG prediction vs. reference classification", 
                "PPXG prediction vs. MCD64A1 classification")[2],
       col.regions=c(terrain.colors(3)[1],rainbow(12)[c(3,1,8)],"white"),
       at = c(0,0.75,1.5,2.25,3,3.75),
       scales=list(draw=T),
       sp.layout=list(scale,l3,l4,l2,rect),#,which=1),
       par.settings = list(fontsize = list(text = 11)),
       colorkey = list(
         labels=list(
           at = labelat,
           labels = labeltext
         ),
         at = c(0,0.75,1.5,2.25,3,3.75),
         # pretty = TRUE,
         height = 0.2
       ),
       maxpixels = 687124
       # skip = c(TRUE, TRUE, FALSE)
)

## Zoom
ext <- extent(-710,-620,4425,4550)
zoom_classif <- crop(cl_ref_mcd_raster,ext)

## show
osx = -705
osy = 4543
scale = list("SpatialPolygonsRescale", layout.scale.bar(height = 0.1), 
             offset = c(osx,osy), scale = 10, fill=c("transparent","black"),
             first = FALSE)
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(-705,4430),
          scale = 15,first=FALSE)
l3 = list("sp.text", c(osx,osy+3), "0",scale=50)
l4 = list("sp.text", c(osx + 11,osy+3), "10 km",scale=50)

labelat <- c(0.375,1.125,1.875,2.625,3.375)
labeltext<- c("TN","FP","FN","TP","NA")

spplot(zoom_classif[[1]],
      main = c("PPXG prediction vs. reference classification", 
               "PPXG prediction vs. MCD64A1 classification")[1],
      col.regions=c(terrain.colors(3)[1],rainbow(12)[c(3,1,8)],"white"),
      at = c(0,0.75,1.5,2.25,3,3.75),
      scales=list(draw=T),
      sp.layout=list(scale,l3,l4,l2),#,which=1),
      par.settings = list(fontsize = list(text = 12)),
      colorkey = list(
        labels=list(
          at = labelat,
          labels = labeltext
        ),
        at = c(0,0.75,1.5,2.25,3,3.75),
        # pretty = TRUE,
        height = 0.2
      )
)

spplot(zoom_classif[[2]],
       main = c("PPXG prediction vs. reference classification",
                "PPXG prediction vs. MCD64A1 classification")[2],
       col.regions=c(terrain.colors(3)[1],rainbow(12)[c(3,1,8)],"white"),
       at = c(0,0.75,1.5,2.25,3,3.75),
       scales=list(draw=T),
       sp.layout=list(scale,l3,l4,l2),#,which=1),
       par.settings = list(fontsize = list(text = 12)),
       colorkey = list(
         labels=list(
           at = labelat,
           labels = labeltext
         ),
         at = c(0,0.75,1.5,2.25,3,3.75),
         # pretty = TRUE,
         height = 0.2
       )
)
