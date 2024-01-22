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

# Acknowledgements
This work has been funded by the project PID 2020-113125RB-I00 of the Spanish Research Agency (MCIN/ AEI/ 10.13039/501100011033) and Ayudas predoctorales UPNA 2022-2023.

![image](https://github.com/spatialstatisticsupna/LXG/blob/main/micin-aei.jpg)

# References

Militino, A. F., Goyena, H., Pérez-Goya, U. and Ugarte, M.D (2024). Logistic regression versus machine learning for detecting burned areas using satellite images _Environmental and Ecological Statistics_, 
 https://doi.org/10.1007/s10651-023-00590-7.
