#' ---
#' title: Testing R's `optim` function for yield calibration with Simplace
#' subtitle: Calibration for different locations - setup with individual data files.
#' author: Gunther Krauss
#' output: pdf_document
#' df_print: kable
#' ---


#' Calibrate a biomass model with respect to measured yields 
#' by changing the parameters LUE, SLA and RGRL.
#' 
#' Simulations are performed for three locations for a period of 13 years.
#' 
#' In this setup, the weather data for each location are in different files, 
#' so that the simulations for each locations needs an own project id and 
#' can't be run in parallel by Simplace.



#' # Preparation - set up Simplace

#' Load the library simplace
library(simplace)

#' Set directories and files

InstallDir <- simplace::findFirstSimplaceInstallation() # Simplace java files
WorkDir <-  "simulation/" # solution and data files
OutputDir <- "output/"   # simplace output directory

SolutionFile <- paste0(WorkDir, "solution/YieldSlaLueRgrl.sol.xml") # Solution
 
ProjectData <- read.delim(paste0(WorkDir, "data/climatezones.csv"), 
                  sep = ";", header=TRUE) #read climatezone IDs for different environments 

Zones <- unique(ProjectData$ClimateZone)


#' Initialise the java virtual machine and Simplace and create a project from 
#' the given solution file.

simplace <- initSimplace(InstallDir,WorkDir,OutputDir,
                         javaparameters="-Xmx2048m")
openProject(simplace, SolutionFile)  


#' 
#' # Define the optimisation function
#'
#'
#' Read observations.
#' 
obs <- read.delim("data/observations.csv", sep = ";", header=TRUE)


#' `optim` needs a function that 
#' 
#'  - has a numeric vector as input (the parameters)
#'  - returns a scalar numeric value (the target value that has to be minimized)
#'  
#'  We create a function named `yield` that
#'  
#'  - takes the values from the input vector and assigns it to the parameters
#'  - takes an additional parameter with a list of zones
#'  - loops over each of the given zones and
#'    - create a simulation for the specific zone with the parameters
#'    - run the simulation and retrieve the results for the zone
#'    - add the zone results to the results of all zones
#'  - merge results with the observed data
#'  - calculate the RMSE of observed yield and simulated yield
#' 

yield <- function(v, Zones)
{
  parameter <- list()
  parameter$vLUE <- v[1]
  parameter$vSLA <- v[2]
  parameter$vRGRL <- v[3]
  allresults <- NULL
  for(Zone in Zones)
  {  
    parameter$vClimateZone <- Zone
    parameter$projectid <- Zone
    sim <- createSimulation(simplace, parameter)
    runSimulations(simplace) 
    result <- getResult(simplace,"LintulSum", sim)
    res <- resultToDataframe(result,expand=FALSE)
    allresults <- rbind(allresults, res)
  }
  merged <- merge(allresults, obs, 
                  by=c("ClimateZone","Year"))
  val <- sqrt(mean((merged$Yield - merged$ObservedYield)^2)) 
  val
}


#' 
#' # Run the Calibration
#' 
setLogLevel("ERROR")



#' Calibration for first zone only
optim(c(2.8,0.02,0.009),yield, control=list(maxit=500), Zones=Zones[1])


#' Calibration for all zones
system.time(
  optim(c(2.8,0.02,0.009),yield, control=list(maxit=500), Zones=Zones) |> print()
)



#' 
#' # Finalising
#' 
#' Close the simplace project
#' 
closeProject(simplace)


