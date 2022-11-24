#' ---
#' title: Using R's `optim` function for yield calibration with Simplace
#' subtitle: Calibration for different locations - individual data files.
#' author: Gunther Krauss
#' output: pdf_document
#' ---

#' Calibrate a biomass model with respect to measured yields
#' by changing the parameters LUE, SLA and RGRL.
#'
#' Simulations are performed for three locations for a period of 13 years.
#'
#' In this setup, the weather data for each location are in different files,
#' so that the simulations for each locations needs an own project id and
#' can't be run in parallel by Simplace.

#'
#' # Preparation - set up Simplace
#'
#' Load the library simplace

library(simplace)

#' Set directories and files

installdir <- findFirstSimplaceInstallation() # Simplace java files
workdir <-  "simulation/" # solution and data files
outputdir <- "output/"   # simplace output directory

solution <- paste0(workdir, "solution/YieldSlaLueRgrl.sol.xml") # Solution
zonedata <- read.delim(paste0(workdir, "data/climatezones.csv"),
                 sep = ";", header = TRUE) # climate zone IDs of locations
zones <- unique(zonedata$ClimateZone)

#' Initialise the java virtual machine and Simplace and create a project from
#' the given solution file.

simplace <- initSimplace(installdir, workdir, outputdir,
                         javaparameters = "-Xmx2048m")
openProject(simplace, solution)
setLogLevel("ERROR")

#'
#' # Define the optimisation function
#'
#' Read observations.

observations <- read.delim("data/observations.csv", sep = ";", header = TRUE)

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

yield <- function(p, zones) {
  parameter <- list()
  parameter$vLUE <- p[1]
  parameter$vSLA <- p[2]
  parameter$vRGRL <- p[3]
  allresults <- NULL
  for (zone in zones) {
    parameter$vClimateZone <- zone
    parameter$projectid <- zone
    sim <- createSimulation(simplace, parameter)
    runSimulations(simplace)
    result <- getResult(simplace, "Yields", sim)
    res <- resultToDataframe(result, expand = FALSE)
    allresults <- rbind(allresults, res)
  }
  merged <- merge(allresults, observations, by = c("ClimateZone", "Year"))
  val <- sqrt(mean((merged$Yield - merged$ObservedYield)^2))
  val
}

#'
#' # Run the Calibration
#'
#' Calibration for first zone only

optim(c(2.8, 0.02, 0.009), yield, control = list(maxit = 500), zones = zones[1])

#' Calibration for all zones

optim(c(2.8, 0.02, 0.009), yield, control = list(maxit = 500), zones = zones)

#'
#' # Finalising
#'
#' Close the simplace project
#'

closeProject(simplace)
