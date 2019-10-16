# phenoSynth <a href='http://phenocam.nau.edu/phenosynth/'><img src='./www/phenoSynth.png' align="right" height="139" /></a>
## An interactive tool to evaluate phenology data across data sources, spatial, and temporal scales

[![Open Source Love svg2](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badges/)[![GitHub forks](https://img.shields.io/github/forks/Naereen/StrapDown.js.svg?style=social&label=Fork&maxAge=2592000)](https://GitHub.com/Naereen/StrapDown.js/network/)



With over a decade of National Phenology Network and PhenoCam near-surface imagery records, point-based datasets have reached a critical mass with which to evaluate phenological change. However, these datasets often represent single species or vegetation types, making them challenging to scale spatially and extract regional-to-continental scale trends. PhenoCam imagery can record large landscapes and multiple vegetation types, creating opportunities to evaluate spatial overlap between these high-precision, point-based observations of single species or vegetation types and remotely-sensed, satellite-derived data products that have coarse spatial and temporal-extents and heterogeneous vegetation cover. Many challenges exist within points-to-pixels phenology extrapolation. In heterogeneously vegetated areas, Region of Interest (ROI) vegetation types extracted for PhenoCam analyses are sometimes underrepresented or absent in coarse satellite-derived vegetation cover classifications, which can create a mismatch in phenological signals. Further, PhenoCam field of view can span multiple remotely sensed pixels, yet observe a small portion of them, again creating potential mismatch between datasets and time series. Key factors for scaling phenological data are to evaluate ROI representativeness at landscape levels and the spatial fraction of remotely sensed pixels observed by PhenoCam.

PhenoSynth is an open-repository Shiny(R) interface that addresses these factors and allows users to visualize and interact with phenological data across multiple sources including MODIS and eventually LandSat. This tool provides an interface to investigate ‘apples-to-apples’ overlap in vegetation classification, and evaluate agreement in phenological indices and time series across observational datasets, facilitating the scaling of phenological data to regional and continental levels.


### Easiest way to directly interact with R code is via runGitHub
The following command will check for all the required R packages, install them if needed and run the PhenoSynth app directly from GitHub. However, the latest version of RStudio should be already installed. The app requires pre-installed GDAL and basic image rendering libraries (png, jpeg, tif, etc.) on the operating system.

```{r, echo=TRUE}

library(shiny)

runGitHub("phenoSynth", "katharynduffy")

```

###  Notes to current users:
We are currently in the process of
1) Integrating NPN data
2) Building a cached data archive for users

Please hang in there with us while we get these off the ground and let us know if you have any requests or feedback.


The R shiny app has been developed and maintained by [Katharyn Duffy and Kyle Enns for APIS Case 2](https://esto.nasa.gov/files/solicitations/AIST_16/ROSES2016_AIST_A41_awards.html#Jeffrey) since June, 2018.

Most recent release is available from: https://github.com/katharynduffy/phenoSynth
