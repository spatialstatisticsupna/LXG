## Import Libraries
suppressPackageStartupMessages({
  library(terra)
  library(caret)
  library(xgboost)
  library(Matrix)
})

source("proc.conf_mat.R")
source("metric.R")

################################
### Classification Reference ###
################################

finalVariables <- rast("Data/Modis_DataSet.tif")
FullDataset.df <- as.data.frame(finalVariables,na.rm = T)

## Build training and test datasets

# fix seed
set.seed(2022)

trainIndex <-
  createDataPartition(FullDataset.df$effis, p = 0.75, list = F)

var.sel <- names(FullDataset.df) %in% c("d.NBR1","d.NBR2",
                                        "d.MVI","d.MIRBI","d.NIR",
                                        "d.NDVI","distAF","aF.int","effis")
trainingSet <- FullDataset.df[trainIndex, var.sel]

testSet <- FullDataset.df[-trainIndex, var.sel]

## XGBOOST model

# Change data format to the one required by XGBOOST
fire <- list()

fire$train.df <- trainingSet
fire$test.df <- testSet

fire$train_mat <- sparse.model.matrix(effis ~ ., data = fire$train.df)

fire$test_mat <-sparse.model.matrix(effis ~ ., data = fire$test.df)


# Select best hyperparameters

# Parameter that allows to work with unbalanced classes
pos <- length(which(trainingSet$effis == 1))
neg <- length(which(trainingSet$effis == 0))


hyper_grid_2 <- expand.grid(
  eta = c(0.1, 0.2, 0.3, 0.4, 0.5),
  max_depth = c(1, 5, 10),
  min_child_weight = seq(1, 9, 2),
  subsample = c(0.75,1),
  colsample_bytree = c(0.75,1),
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
    label = fire$train.df$effis,
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
  hyper_grid_2$rmse[i] <- m$evaluation_log$test_error_mean[m$best_iteration]
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
# best_param <- list(
#   booster = "gbtree",
#   eta = 0.1,
#   max_depth = 5,
#   min_child_weight = 5,
#   subsample = 0.75,
#   colsample_bytree = 0.75,
#   objective = "binary:logistic",
#   eval_metric = "error",
#   scale_pos_weight = neg / pos
# )

## REPEAT
nrep = 100
source("proc.conf_mat.R")
source("metric.R")
metrics.test <- data.frame() 
metrics.full <- data.frame()
metrics.mod <- data.frame()
metrics.lclass <- data.frame()
metrics.fcci <- data.frame()
pcts.test <- data.frame()
pcts.full <- data.frame()
pcts.mod <- data.frame()
pcts.lclass <- data.frame()
pcts.fcci <- data.frame()
conf_mats.test <- data.frame()
conf_mats.full <- data.frame()
conf_mats.mod <- data.frame()
conf_mats.lclass <- data.frame()
conf_mats.fcci <- data.frame()
imp_mats <- data.frame()
modis_ref_vect <- FullDataset.df$mcd64a1
fcci_ref_vect <- FullDataset.df$fCCI51
lclass_ref_vect <- FullDataset.df$lclass
##
set.seed(2022)
for (seed in 1:nrep) {
  set.seed(seed = seed)
  fire_model <- xgboost(data = fire$train_mat,
                        label = fire$train.df$effis,
                        params = best_param,
                        nrounds = max(hyper_grid_2[, 7]))
  ##  TEST
  predict_val <- predict(fire_model, fire$test_mat)
  predict_val <- ifelse(predict_val > 0.5, 1, 0)
  conf_mat_test <-
    caret::confusionMatrix(as.factor(predict_val),
                           as.factor(fire$test.df$effis),
                           positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_test,
                metrics = metrics.test,
                pcts = pcts.test,
                conf_mats = conf_mats.test)
  
  metrics.test <- pcm[[1]]
  pcts.test <- pcm[[2]]
  conf_mats.test <- pcm[[3]]
  
  ## FULL
  fire$FullDataset_mat <-sparse.model.matrix(effis ~ ., data = FullDataset.df[,var.sel])
  predict_all <- predict(fire_model, fire$FullDataset_mat)
  predict_all <- ifelse(predict_all > 0.5, 1, 0)
  conf_mat_full <- caret::confusionMatrix(as.factor(predict_all),
                           as.factor(FullDataset.df$effis),
                           positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_full,
                metrics = metrics.full,
                pcts = pcts.full,
                conf_mats = conf_mats.full)
  metrics.full <- pcm[[1]]
  pcts.full <- pcm[[2]]
  conf_mats.full <- pcm[[3]]
  
  ## MCD64A1
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
  
  ## lclass
  conf_mat_lclass <- caret::confusionMatrix(as.factor(predict_all),
                                           as.factor(lclass_ref_vect), 
                                           positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_lclass,
                       metrics = metrics.lclass,
                       pcts = pcts.lclass,
                       conf_mats = conf_mats.lclass)
  
  metrics.lclass <- pcm[[1]]
  pcts.lclass <- pcm[[2]]
  conf_mats.lclass <- pcm[[3]]
  
  ## FireCCI5.1
  conf_mat_fcci <- caret::confusionMatrix(as.factor(predict_all),
                                            as.factor(fcci_ref_vect), 
                                            positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat_fcci,
                       metrics = metrics.fcci,
                       pcts = pcts.fcci,
                       conf_mats = conf_mats.fcci)
  
  metrics.fcci <- pcm[[1]]
  pcts.fcci <- pcm[[2]]
  conf_mats.fcci <- pcm[[3]]
  
  importance_matrix <- xgb.importance(model = fire_model)
  imp_mats[seed,importance_matrix$Feature] <- importance_matrix$Gain
}

lr.metrics.test <- data.frame()
lr.metrics.lclass <- data.frame()
lr.metrics.mod <- data.frame()
lr.metrics.fcci <- data.frame()
lr.pcts.test <- data.frame()
lr.pcts.lclass <- data.frame()
lr.pcts.mod <- data.frame()
lr.pcts.fcci <- data.frame()
lr.conf_mats.test <- data.frame()
lr.conf_mats.lclass <- data.frame()
lr.conf_mats.mod <- data.frame()
lr.conf_mats.fcci <- data.frame()
for (seed in 1:100) {
  set.seed(seed = seed)
  ## Logistic Regression
  trainPos <- trainingSet[trainingSet$effis == 1,]
  trainNeg <- trainingSet[trainingSet$effis == 0,]
  trainNegB <- trainNeg[sample(1:nrow(trainNeg),nrow(trainPos)),]
  
  # fit
  trainBalanced <- rbind(trainPos,trainNegB)
  weightsB <- rep(1,nrow(trainBalanced))
  weightsB[trainBalanced$effis == 0] <- sum(trainingSet$effis == 0)/sum(trainingSet$effis == 1)
  weights <- rep(1,nrow(trainingSet))
  weights[trainingSet$effis == 1] <- sum(trainingSet$effis == 0)/sum(trainingSet$effis == 1)
  logrB <- glm(effis ~ .,
               data = trainBalanced,
               family = binomial(link="logit"),
               weights = weightsB)
  xtable::xtable(summary(logrB)$coefficients,digits = c(0,5,5,5,5))
  summary(logrB)
  
  # test
  probs <- predict(logrB,testSet,type = "response")
  predicted.classes <- ifelse(probs > 0.5, 1, 0)
  conf_mat.effis <- confusionMatrix(as.factor(predicted.classes),
                                    as.factor(testSet$effis),
                                    positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat.effis,
                       metrics = lr.metrics.test,
                       pcts = lr.pcts.test,
                       conf_mats = lr.conf_mats.test)
  
  lr.metrics.test <- pcm[[1]]
  lr.pcts.test <- pcm[[2]]
  lr.conf_mats.test <- pcm[[3]]
  
  # predict full dataset
  probs <- predict(logrB,FullDataset.df,type = "response")
  predicted.classes <- ifelse(probs > 0.5, 1, 0)
  
  # lclass
  conf_mat.lclass <- confusionMatrix(as.factor(predicted.classes),
                                     as.factor(FullDataset.df$lclass),
                                     positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat.lclass,
                       metrics = lr.metrics.lclass,
                       pcts = lr.pcts.lclass,
                       conf_mats = lr.conf_mats.lclass)
  
  lr.metrics.lclass <- pcm[[1]]
  lr.pcts.lclass <- pcm[[2]]
  lr.conf_mats.lclass <- pcm[[3]]
  
  # MCD64A1
  conf_mat.mod <- confusionMatrix(as.factor(predicted.classes),
                                  as.factor(FullDataset.df$mcd64a1),
                                  positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat.mod,
                       metrics = lr.metrics.mod,
                       pcts = lr.pcts.mod,
                       conf_mats = lr.conf_mats.mod)
  
  lr.metrics.mod <- pcm[[1]]
  lr.pcts.mod <- pcm[[2]]
  lr.conf_mats.mod <- pcm[[3]]
  
  # FireCCI5.1
  conf_mat.fcci <- confusionMatrix(as.factor(predicted.classes),
                                   as.factor(FullDataset.df$fCCI51),
                                   positive = "1")
  
  pcm <- proc.conf_mat(conf_mat = conf_mat.fcci,
                       metrics = lr.metrics.fcci,
                       pcts = lr.pcts.fcci,
                       conf_mats = lr.conf_mats.fcci)
  
  lr.metrics.fcci <- pcm[[1]]
  lr.pcts.fcci <- pcm[[2]]
  lr.conf_mats.fcci <- pcm[[3]]
}

save(metrics.test, metrics.full, metrics.mod, metrics.lclass, metrics.fcci,
     pcts.test, pcts.full,pcts.mod, pcts.lclass, pcts.fcci,
     conf_mats.test, conf_mats.full, conf_mats.mod, conf_mats.lclass, conf_mats.fcci,
     imp_mats,
     lr.metrics.test, lr.metrics.mod, lr.metrics.lclass, lr.metrics.fcci,
     lr.pcts.test, lr.pcts.mod, lr.pcts.lclass, lr.pcts.fcci,
     lr.conf_mats.test, lr.conf_mats.mod, lr.conf_mats.lclass, lr.conf_mats.fcci,
     file = "Data/acc_metrics_2.RData")

save.image(file="Data/results_2.RData")
