# PPXG
This repository provides the R code to reproduce the computational analysis of the paper: "Refining the identification of burned areas in Iberian peninsula regions with spatial point processes and machine learning" (Militino et al., 2022).

## Table of contents

- [R code](#R-code)
- [Alternative workflow](#Alternative-workflow)
- [Auxiliary data](#Auxiliary-data)

## R code
First, we need to sign up in the web services of 
[EarthExplorer](https://ers.cr.usgs.gov/register/), and replace the strings `"USERNAME"` and `"PASSWORD"` with our
own credentials.

The script [*Main.R*](https://github.com/spatialstatisticsupna/PPXG/blob/main/Main.R) calls the following five R scripts required to get the final estimates of burned areas and the graphs:

[**1.Download_Gen_Index.R**](https://github.com/spatialstatisticsupna/PPXG/blob/main/1.%20Download_Gen_Index.R) It downloads the multispectral images corresponding to the region and time of interest and computes the spectral indexes.

[**2.ActiveFires_FireDates.R**](https://github.com/spatialstatisticsupna/PPXG/blob/main/2.ActiveFires_FireDates.R) It uses the active fire data from the VIIRS and MODIS sensors to obtain fire dates and distances to active fires for each pixel. Since burnable LandCover information is needed to compute those dates and distances, it is also obtained at this point, using [*2.1_Landcover_Notburnablemask*](https://github.com/spatialstatisticsupna/PPXG/blob/main/2.1_Landcover_Notburnablemask.R).

[**3.Generate_DataSet.R**](https://github.com/spatialstatisticsupna/PPXG/blob/main/3.Generate_DataSet.R) It combines the active fire dates and the burnable LandCover information from script 2 with the multispectral indexes obtained in script 1 for computing the diferenced multispectral indexes using [*CompIndex.R*](https://github.com/spatialstatisticsupna/PPXG/blob/main/compIndex.R). The raster of burnable LandCover data, the raster of distances to the active fires, and the rasters of differenced spectral indexes are stacked in a rasterstack object. This script also uploads theEFFIS wildfire data and the burned areas from the MODIS MCD64A1 burned area product. In order to execute this step we need to download the EFFIS burned area data (see [effis.pdf](https://github.com/spatialstatisticsupna/PPXG/blob/main/effis.pdf))

[**4.TrainingTest.R**](https://github.com/spatialstatisticsupna/PPXG/blob/main/4.TrainingTest.R) It starts generating the classification reference from the dNBR index and the EFFIS burned areas. Later, it adds the classification reference data to the rasterstack generated in the previous step and it transforms it to the text format requiered for XGBoost. Finally, it selects the best hyperparameter combination using cross validation and provides the results.

[**5.PlotResults.R**](https://github.com/spatialstatisticsupna/PPXG/blob/main/5.showResults.R) Plots the figures of the article.

## Alternative workflow

The whole procedure is time and disk consuming for downloading and processing the multi-spectral images. Additionally, it is necessary to sign up to EarthExplorer and download the non-raster data. We provide an alternative workflow for running the XGBoost training process without executing the previous scripts.

To run XGBoost from step 4 (skipping scripts 1-3) we simply download the repository and execute `4.TrainingTest.R` to generate the results. This file will automatically read the auxiliary data from the `Data/` folder.

## Auxiliary data

The `Data/` folder contains the following:

- `input_4.TrainingTest.RData`: This file contains the input data needed for the XGBoost training process. More specifically, the  `rasterStack` of the derived indexes, the `spatialPolygonsDataFrame` of burned areas downloaded from the EFFIS wildfire database, and the MODIS MCD64A1 `raster`.

- `results.RData`: This file contains the R objects needed to show the comparative plots, and the data used for generating the tables.

- The `MCD64A1/` folder contains the MCD64A1 MODIS burned area rasters obtained by transforming the Burn Date layer from the .hdf files downloaded via `rsat` to .tif files.

- The `activeFires/` folder contains the VIIRS active fire and MODIS thermal anomalies `spatialPointsDataFrame`s obtained from [FIRMS](https://firms.modaps.eosdis.nasa.gov/download/create.php).
