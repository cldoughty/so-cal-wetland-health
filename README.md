# so-cal-wetland-health
This repository includes code to extract and analyze Landsat-based timeseries of wetland NDVI and habitat composition over 1984-2019 for Southern California. The information contained in this repository demonstrates how timeseries were developed and preprocessed in Google Earth Engine (GEE) and analyzed in R. We used this approach to assess trends in wetland health and drivers of wetland change in over 30 wetlands along the Southern California coast. 

## Getting Started
You will need [GEE](https://earthengine.google.com/), [R](https://www.r-project.org/) and [R studio](https://www.rstudio.com/) to run the contents of this repository.

## Analyses and files included in this repository
Landsat Timeseries - the **SoCalTimeseries.js** GEE script is for preprocessing Landsat timeseries and exporting wetland NDVI and habitat composition as a csv. This script is based on a shared feature collection of wetland boundaries ("TS_Estuary_Boundaries_092720_fixed") for which the user selects one wetland at a time. The script will process timeseries per site and export a csv to a user-defined Google Drive folder.

Environmental Trends - **R_Environmental Time Series.r** processes input data stored in the "files" folder to create plots and estimate trends in regional environmental drivers.

Wetland NDVI and Habitat Timeseries and Trends - **R_Time Series Trend Analysis.r** takes post-processed annual timeseries for each wetland site stored in the "files" folder and estimates trends in wetland condidions. Trends are then compared to site characteristics and human drivers. This script also plots and exports timeseries of NDVI and habitat area for each site.

Drivers of Wetland NDVI and Area - **R_Environmental Driver HGAMs.r** uses the annual timeseries including annual environmental variables for each wetland site stored in the "files" folder to perform Heirarchial Generalized Additive Models (HGAMs) to describe the complex, nonlinear relationship between wetland conditions of NDVI and area to environmental drivers.

To run these scripts in R studio, ensure your working directory and output directory are correct and include the "files" folder from this repository.

## Suggested Citation
Doughty, C.L., Tracking the Health of Southern California's Coastal Wetlands using the Landsat Archive (so-cal-wetland-health), (2018), GitHub repository, https://github.com/cldoughty/so-cal-wetland-health
