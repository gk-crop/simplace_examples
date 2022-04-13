#' ---
#' title: Calibration of Simplace simulation with CroptimizeR
#' author: Gunther Krauss
#' date: April 12, 2022
#' ---

#' # Preparations for Simplace
#' Load the wrapper function that runs Simplace simulations
source("SimplaceFunction.R")
model_function <- SimplaceFunction

#' Give more memory to the java process
options(java.parameters = "-Xmx6g")

#' Define the model options for Simplace
model_options <- within(list(), {

  InstallDir <- simplace::findFirstSimplaceInstallation() # Simplace java files
  WorkDir <- paste0(getwd(), "/simulation/") # solution and data files
  OutputDir <- paste0(getwd(), "/output/")   # Simplace output directory

  SolutionFile <- paste0(WorkDir, "solution/YieldSlaLueRgrl.sol.xml") # Solution

  # Define situations - e. g. load them from a project data file
  ProjectDataFile <- paste0(WorkDir, "data/climatezones.csv")
  ProjectData <- if (is.null(ProjectDataFile)) {
      NULL
    } else {
      readr::read_delim(ProjectDataFile, delim = ";")
    }
  Situations <- unique(ProjectData$CLIMATEZONE)

  # Supply the variable name that defines which situation will run
  SituationVariable <- "vCLIMATEZONE"

  # Id of MEMORY output that provides the simulation results
  OutputId <- "LintulSum"

  # Initialise Simplace framework and store the handle
  SimplaceInstance <- simplace::initSimplace(InstallDir, WorkDir, OutputDir)

})

#' Set Check- and LogLevel and load the simulation
simplace::setCheckLevel(model_options$SimplaceInstance, "OFF")
simplace::openProject(model_options$SimplaceInstance,
                      model_options$SolutionFile)
simplace::setLogLevel("WARN")

#'
#' # Preparations for CroptimizeR
#'

#' Read the observed data
obs <- readr::read_delim("data/observations.csv", delim = ";")
obs_list <- split(obs, f = obs$CLIMATEZONE)
names(obs_list) <- model_options$Situations

#' Define calibration parameters and their range
param_info <- list(lb = c(vLUE =  0, vSLA = 0.0, vRGRL = 0.00001),
                   ub = c(vLUE = 10, vSLA = 0.1, vRGRL = 0.05))

#' Set the options for CroptimizeR
optim_options <- list(path_results = paste0(getwd(), "/output"),   # path where
                   # to store the results (graph and Rdata)
                   nb_rep = 2,     # Number of repetitions of the minimization;
                   ranseed = 1234, # set random seed so that each execution give
                   # the same results. If you want randomization, don't set it.
                   maxeval = 50)

#'
#' # Runing the optimisation
#'

#' Run the optimisation process by CroptimizeR
CroptimizR::estim_param(obs_list = obs_list,
                        model_function = model_function,
                        model_options = model_options,
                        optim_options = optim_options,
                        param_info = param_info)

#'
#' # Cleanup
#'

#' Close the simplace project (simulation)
simplace::setLogLevel("INFO")
simplace::closeProject(model_options$SimplaceInstance)

#'
#' # Analyze the optimisation result
#'

#' ...
