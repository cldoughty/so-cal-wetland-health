# so-cal-wetland-health
This repository includes code to extract and analyze Landsat-based timeseries of wetland NDVI and habitat composition over 1984-2019 for Southern California. The information contained in this repository demonstrates how timeseries were developed and preprocessed in Google Earth Engine (GEE) and analyzed in R. We used this approach to assess trends in wetland health and drivers of wetland change in over 30 wetlands along the Southern California coast. 

## Getting Started

To run this model, you will need [GEE](https://earthengine.google.com/), [R](https://www.r-project.org/) and [R studio](https://www.rstudio.com/). To run this model, download this repository to a local directory or `git clone` this repository using [git](https://git-scm.com/downloads).

## Files included in this repository
* **SoCalTimeseries.js** - GEE script for preprocessing Landsat timeseries and exporting wetland NDVI and habiata composition as a csv.
* **something.r** – R script for estimating Sen's Slope from Lansat timeseries
* **something.r** – R script for estimating Sen's Slope from Lansat timeseries
* **something.csv** - Data needed......

## It's a process


With the repository data saved to your local working directory, open the R script **SCWRP_RSU_slr_model.r**. With the script open in R studio, set the working directory `setwd()` to the same local working directory where you saved the repository. Here, you can also define where you want the model outputs to be saved. These should be the only changes required for the model script to run. 

The code in the model consists of two parts. In part 1, we read in **SLR_Model_Inputs.csv**, **Hypsometry.csv**, and **Zbreaks_Archetypes.csv**, then extract the input data that we need to make intermediate calculations (changes in elevation and water level) that get fed into part 2. Part 2 consists of a loop, where we go through our list of sites from the input data one at a time. This is where we manipulate the site-specific hypsometric curve and the water levels delimiting marsh zones in order to mimic changes caused by accretion, tidal inlet dynamics and sea level rise. Based on the site-specific hypsometric curve, the model calculates the amount of area for each marsh zone under current conditions and 2 future SLR scenarios: 0.6 m by 2050 and 1.7 m by 2100. The area calculations are then saved to the output directory as a subfolder for each site.

To run the model script in R studio, ensure your working directory and output directory are correct, select the entire script `cntl+a` and click `run`.

## Suggested Citation
Doughty, C.L., Tracking the Health of Southern California's Coastal Wetlands using the Landsat Archive (so-cal-wetland-health), (2018), GitHub repository, https://github.com/cldoughty/so-cal-wetland-health
