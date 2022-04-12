# Example calibration of Simplace simulation with CroptimizeR

A short example how to use CroptimizeR to calibrate a Simplace simulation. 
The simulation uses Lintul2 model to calculate potential yield. 

Parameters to calibrate are LUE, SLA and RGRL by using measured yield from five locations (i.e. five "situations" in CroptimizeR nomenclature).  

## Install Simplace

See https://www.simplace.net/

## Install simplace and Croptimizer packages

```{r}
install.packages('simplace', repos=c('http://r-forge.r-project.org','http://cran.r-project.org'))
devtools::install_github("SticsRPacks/CroptimizR@*release")
```

## Run the optimisation

Download the content of this folder (including subdirectories). Then set the working directory to the downloaded directory (e.g. by using `setwd()`) and run the script `runCroptimizer.R`
