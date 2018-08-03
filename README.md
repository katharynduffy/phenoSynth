
## PhenoSynth: An interactive tool to evaluate phenology data across spatial and temporal scales



### Easiest way is to use runGitHub
The following command will check for all the required R packages, install them if needed and run the drawROI app directly from GitHub. However, latest version of R should be already installed. The app requires pre-installed GDAL and basic image rendering libraries (png, jpeg, tif, etc.) on the operating system.

```{r, echo=TRUE}

library(shiny)

runGitHub("phenoRemote", "katharynduffy")

```


The R package is developed and maintained by [Katharyn Duffy and Kyle Enns for APIS Case 2](https://github.com/katharynduffy) since June, 2018.

Most recent release is available from: https://github.com/katharynduffy/phenoRemote
