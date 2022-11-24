# Example calibration of Simplace simulation using R's optim function

A short example how to use optim function to calibrate a Simplace simulation. 
The simulation uses Lintul2 model to calculate potential yield. 

Parameters to calibrate are LUE, SLA and RGRL by using measured yield from tree locations.  

## Install Simplace

See https://www.simplace.net/

```{r}
install.packages('simplace', repos=c('http://r-forge.r-project.org','http://cran.r-project.org'))
```

## Run the optimisation

Download the content of this folder (including subdirectories). Then set the working directory to the downloaded directory (e.g. by using `setwd()`) and run the scripts.
