#' ---
#' title: Testing R's `optim` function for yield calibration with Simplace
#' subtitle: Calibration for a sinlge location.
#' author: Gunther Krauss
#' output: pdf_document
#' df_print: kable
#' ---

#' Calibrate a biomass model with respect to measured yields 
#' by changing the parameters LUE, SLA and RGRL.
#' 
#' Simulations are performed on one location for a period of 13 years.


#' 
#' # Preparation - set up Simplace and read observations
#' 

#' Load the library simplace
library(simplace)

#' Set directories and files

InstallDir <- simplace::findFirstSimplaceInstallation() # Simplace java files
WorkDir <-  "simulation/" # solution and data files
OutputDir <- "output/"   # simplace output directory

SolutionFile <- paste0(WorkDir, "solution/YieldSlaLueRgrl.sol.xml") # Solution
 
#' Initialise the java virtual machine and Simplace and create a project from 
#' the given solution file.

simplace <- initSimplace(InstallDir,WorkDir,OutputDir,
                         javaparameters="-Xmx2048m")
openProject(simplace, SolutionFile)  

setLogLevel("ERROR")

#' Read observations.
#' 
obs <- read.delim("data/observations_C02.csv", sep = ";", header=TRUE)

#' 
#' # Define the optimisation function
#'
#' `optim` needs a function that 
#' 
#'  - has a numeric vector as input (the parameters)
#'  - returns a scalar numeric value (the target value that has to be minimized)
#'  
#'  We create a function named `yield` that
#'  
#'  - takes the values from the input vector and assigns it to the parameters
#'  - creates a simulation with the parameters
#'  - run the simulation and retrieve the results
#'  - merge results with the observed data
#'  - calculate the RMSE of observed yield and simulated yield
#' 

yield <- function(v)
{
  parameter <- list()
  parameter$vLUE <- v[1]
  parameter$vSLA <- v[2]
  parameter$vRGRL <- v[3]
  sim <- createSimulation(simplace, parameter)
  runSimulations(simplace) 
  result <- getResult(simplace,"LintulSum", sim)
  allresults <- resultToDataframe(result,expand=FALSE)
  merged <- merge(allresults, obs, 
                  by=c("Year"))
  val <- sqrt(mean((merged$Yield - merged$ObservedYield)^2)) 
  val
}

#' 
#' # Run the Calibration
#' 
optim(c(2.8,0.02,0.009),yield, control=list(maxit=500))

#' 
#' # Finalising
#' 
#' Close the simplace project
#' 
closeProject(simplace)


