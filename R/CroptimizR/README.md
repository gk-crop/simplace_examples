# Example calibration of Simplace simulation with CroptimizR

A short example how to use CroptimizR to calibrate a Simplace simulation. 
The simulation uses Lintul2 model to calculate potential yield. 

Parameters to calibrate are LUE, SLA and RGRL by using measured yield from five locations (i.e. five "situations" in CroptimizR nomenclature).  

## Install Simplace

See https://www.simplace.net/

## Install simplace and CroptimizR packages

```{r}
install.packages('simplace')
devtools::install_github("SticsRPacks/CroptimizR@*release")
```

## Run the optimisation

Download the content of this folder (including subdirectories). Then set the working directory to the downloaded directory (e.g. by using `setwd()`) and run the script `runCroptimizR.R`
