# LXG
This repository provides the R code to reproduce the computational analysis of the paper: "Logistic regression versus machine learning for detecting burned areas using satellite images" (Militino et al., 2023).

## Table of contents

- [R code](#R-code)
- [Additional data](#Additional-data)

## R code

The script [*LXG_terra.R*](https://github.com/spatialstatisticsupna/PPXG/blob/main/LXG_terra.R) contains the implementation of the models described in the paper.

## Additional data

The `Data/` folder contains the following:

- `MODIS_DataSet.tif`: This file contains the `raster` data set used to compare the described models.

- `acc_metrics.RData`: This file comprises R objects that store the accuracy metrics for every simulation conducted.
