## Import Libraries
suppressPackageStartupMessages({
  library(raster)
  library(spdplyr)
  library(caret)
  library(xgboost)
  library(rsat)
  library(Matrix)
})

if (dir.exists("Data/") &
    !(exists("finalVariables") &
      exists("ptgal_BA_spolydf") & exists("Mod_BA_RoI_rl"))) {
  load("Data/input_4.TrainingTest.RData")
} else{
  message(
    "Data/ folder not found, the following process will not work unless the previous steps have already been executed"
  )
}


################################
### Classification Reference ###
################################

# extract BA pixels within the RoI
ref_raster_rl <-  finalVariables[[1]] %>% setValues(NA)
ptgal_largeBA_rl <- rasterize(ptgal_BA_spolydf, ref_raster_rl)

# create burned area pixel mask
ptgal_largeBA_mask <-
  reclassify(ptgal_largeBA_rl, matrix(c(-Inf, Inf, 1)))

# mask low dNBR pixels
lowdNBR1_filt <- matrix(c(-Inf, 0.1, NA,
                          0.1, Inf, 1),
                        byrow = T,
                        nrow = 2)

lowdNBR1_mask <-
  reclassify(finalVariables[[1]], lowdNBR1_filt, right = FALSE)

# burned pixel mask (pixels from large burned areas with dNBR1 > 0.1)
burnedPixel_mask <- ptgal_largeBA_mask * lowdNBR1_mask

# high dNBR filter (dNBR > 0.15)
highdNBR1_filt <- matrix(c(-Inf, 0.15, 1,
                           0.15, Inf, NA),
                         byrow = T,
                         nrow = 2)
## inverse filter
inverse_filt <- matrix(c(NA, 1,
                         1, NA),
                       byrow = T,
                       nrow = 2)

## pixels not chosen as burned
inv_burnedPixel_mask <- reclassify(burnedPixel_mask, inverse_filt)

## high dNBR pixel mask (dNBR > 0.15)
highdNBR1_mask <- reclassify(finalVariables[[1]], highdNBR1_filt)

## not burned pixel mask (pixels not chosen as burned with dNBR < 0.15)
unburnedPixel_mask <- inv_burnedPixel_mask * highdNBR1_mask

###############
### XGBOOST ###
###############
## Transform input dataset to a dataframe in order to train the XGBOOST algorithm

# remove dNBR1 variable and LandCover
ind.names <- names(finalVariables)[!names(finalVariables)  %in% c("d.NBR1","LandCover")]
finalVariables <- finalVariables[[ind.names]]

# add point process intensity
finalVariables <- addLayer(finalVariables,actFire.intensity)

# Burned pixels

Burned_rb <- finalVariables * burnedPixel_mask

Burned.df <- as.data.frame(Burned_rb, na.rm = TRUE)
names(Burned.df) <- names(finalVariables)

# Add class
Burned.df$lclass <- rep('1', nrow(Burned.df))

# Unburned pixels

Unburned_rb <- finalVariables * unburnedPixel_mask

Unburned.df <- as.data.frame(Unburned_rb, na.rm = TRUE)
names(Unburned.df) <- names(finalVariables)

# Add class

Unburned.df$lclass <- rep('0', nrow(Unburned.df))

# Final dataset

FullDataset.df <- rbind(Burned.df, Unburned.df)

## Build training and test datasets


# fix seed
set.seed(2022)

trainIndex <-
  createDataPartition(FullDataset.df$lclass, p = 0.75, list = F)

trainingSet <- FullDataset.df[trainIndex, ]

testSet <- FullDataset.df[-trainIndex, ]

## XGBOOST model

# Change data format to the one required by XGBOOST
fire <- list()

fire$train.df <- trainingSet
fire$test.df <- testSet

fire$train_mat <- sparse.model.matrix(lclass ~ ., data = fire$train.df)

fire$test_mat <-sparse.model.matrix(lclass ~ ., data = fire$test.df)


# Select best hyperparameters

# Parameter that allows to work with unbalanced classes
pos <- length(which(trainingSet$lclass == 1))
neg <- length(which(trainingSet$lclass == 0))


hyper_grid_2 <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
  max_depth = c(5, 10, 15),
  min_child_weight = seq(1, 9, 2),
  subsample = 0.75,
  colsample_bytree = 0.75,
  rmse = 0,           # a place to dump RMSE results
  trees = 0          # a place to dump required number of trees
)

totalIter <- nrow(hyper_grid_2)

# apply cross validation for each hyperparameter combination
for (i in seq_len(nrow(hyper_grid_2))) {
  a <- Sys.time()
  message(paste('Iteration', i, 'of', totalIter))
  m <- xgb.cv(
    data = fire$train_mat,
    label = fire$train.df$lclass,
    nrounds = 100,
    objective = "binary:logistic",
    eval_metric = "error",
    early_stopping_rounds = 5,
    nfold = 10,
    verbose = TRUE,
    params = list(
      eta = hyper_grid_2$eta[i],
      max_depth = hyper_grid_2$max_depth[i],
      min_child_weight = hyper_grid_2$min_child_weight[i],
      subsample = hyper_grid_2$subsample[i],
      colsample_bytree = hyper_grid_2$colsample_bytree[i],
      scale_pos_weight = neg / pos
    )
  )
  hyper_grid_2$rmse[i] <- max(m$evaluation_log$test_error_mean)
  hyper_grid_2$trees[i] <- m$best_iteration
  message(paste('Execution time:', Sys.time() - a))
}


# obtain best hyperparameters from the cross validation
best_param <- list(
  booster = "gbtree",
  eta = hyper_grid_2[order(hyper_grid_2$rmse, decreasing = T)[1], 1],
  max_depth = hyper_grid_2[order(hyper_grid_2$rmse, decreasing = T)[1], 2],
  min_child_weight = hyper_grid_2[order(hyper_grid_2$rmse, decreasing = T)[1], 3],
  subsample = hyper_grid_2[order(hyper_grid_2$rmse, decreasing = T)[1], 4],
  colsample_bytree = hyper_grid_2[order(hyper_grid_2$rmse, decreasing = T)[1], 5],
  objective = "binary:logistic",
  eval_metric = "error",
  scale_pos_weight = neg / pos
)

## REPEAT
nrep = 100
source("proc.conf_mat.R")
source("metric.R")
metrics.test <- data.frame() 
metrics.full <- data.frame()
metrics.mod <- data.frame()
pcts.test <- data.frame()
pcts.full <- data.frame()
pcts.mod <- data.frame()
conf_mats.test <- data.frame()
conf_mats.full <- data.frame()
conf_mats.mod <- data.frame()
imp_mats <- data.frame()
## prepare MCD64A1 reference vector
ref_raster_rl <- finalVariables[[1]] %>% setValues(NA)
NA_mask <- cover(burnedPixel_mask,unburnedPixel_mask)
extent(Mod_BA_RoI_rl) <- extent(ref_raster_rl)
Mod_BA_Masked_rl <- Mod_BA_RoI_rl * NA_mask
BA_filt <- matrix(c(-Inf,-1, 0,-1, 1, 0,
                    1, Inf, 1),
                  byrow = T,
                  nrow = 3)
Ref_ModisBA_rl <- reclassify(Mod_BA_Masked_rl, BA_filt)
names(Ref_ModisBA_rl) <- "MCD64A1"
ref_mod_pos <- Ref_ModisBA_rl[as.numeric
                              (rownames(FullDataset.df[which(FullDataset.df$lclass == 1), ]))]
ref_mod_neg <- Ref_ModisBA_rl[as.numeric
                              (rownames(FullDataset.df[which(FullDataset.df$lclass == 0), ]))]
modis_ref_vect <- c(ref_mod_pos, ref_mod_neg)
##
for (seed in 1:nrep) {
  set.seed(seed = seed)
  fire_model <- xgboost(data = fire$train_mat,
                        label = fire$train.df$lclass,
                        params = best_param,
                        nrounds = max(hyper_grid_2[, 7]))
  ##  TEST
  predict_val <- predict(fire_model, fire$test_mat)
  predict_val <- ifelse(predict_val > 0.5, 1, 0)
  conf_mat_test <-
    caret::confusionMatrix(as.factor(predict_val),
                           as.factor(fire$test.df$lclass),
                           positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_test,
                       metrics = metrics.test,
                       pcts = pcts.test,
                       conf_mats = conf_mats.test)
  
  metrics.test <- pcm[[1]]
  pcts.test <- pcm[[2]]
  conf_mats.test <- pcm[[3]]
  
  ## FULL
  fire$FullDataset_mat <-sparse.model.matrix(lclass ~ ., data = FullDataset.df)
  predict_all <- predict(fire_model, fire$FullDataset_mat)
  predict_all <- ifelse(predict_all > 0.5, 1, 0)
  conf_mat_full <- caret::confusionMatrix(as.factor(predict_all),
                                          as.factor(FullDataset.df$lclass),
                                          positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_full,
                       metrics = metrics.full,
                       pcts = pcts.full,
                       conf_mats = conf_mats.full)
  metrics.full <- pcm[[1]]
  pcts.full <- pcm[[2]]
  conf_mats.full <- pcm[[3]]
  
  
  conf_mat_Modis <- caret::confusionMatrix(as.factor(predict_all),
                                           as.factor(modis_ref_vect), 
                                           positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_Modis,
                       metrics = metrics.mod,
                       pcts = pcts.mod,
                       conf_mats = conf_mats.mod)
  
  metrics.mod <- pcm[[1]]
  pcts.mod <- pcm[[2]]
  conf_mats.mod <- pcm[[3]]
  
  importance_matrix <- xgb.importance(model = fire_model)
  imp_mats[seed,importance_matrix$Feature] <- importance_matrix$Gain
}

save(metrics.test,metrics.full,metrics.mod,
     pcts.test,pcts.full,pcts.mod,
     conf_mats.test, conf_mats.full, conf_mats.mod,
     imp_mats,
     file = "acc_metrics_pp.RData")

save.image(file="Data/results.RData")

