#' ---
#' title: Using R's `optim` function for yield calibration with Simplace
#' subtitle: Calibration for a sinlge location.
#' author: Gunther Krauss
#' output: pdf_document
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

installdir <- findFirstSimplaceInstallation() # Simplace java files
workdir <-  "simulation/" # solution and data files
outputdir <- "output/"   # simplace output directory

solution <- paste0(workdir, "solution/YieldSlaLueRgrl.sol.xml") # Solution

#' Initialise the java virtual machine and Simplace and create a project from
#' the given solution file.

simplace <- initSimplace(installdir, workdir, outputdir,
                         javaparameters = "-Xmx2048m")
openProject(simplace, solution)
setLogLevel("ERROR")

#' Read observations.
#'
observations <- read.delim("data/observations_C02.csv",
                           sep = ";", header = TRUE)

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

yield <- function(p) {
  parameter <- list()
  parameter$vLUE <- p[1]
  parameter$vSLA <- p[2]
  parameter$vRGRL <- p[3]
  sim <- createSimulation(simplace, parameter)
  runSimulations(simplace)
  result <- getResult(simplace, "Yields", sim)
  allresults <- resultToDataframe(result, expand = FALSE)
  merged <- merge(allresults, observations, by = c("Year"))
  val <- sqrt(mean((merged$Yield - merged$ObservedYield)^2))
  val
}

#'
#' # Run the Calibration
#'

optim(c(2.8, 0.02, 0.009), yield, control = list(maxit = 500))

#'
#' # Finalising
#'
#' Close the simplace project
#'

closeProject(simplace)
