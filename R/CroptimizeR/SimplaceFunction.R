#' Define the optimisation function according to the CroptimizeR specification

SimplaceFunction <- function(param_values=NULL,
                             sit_names=NULL,
                             model_options,
                             sit_var_dates_mask=NULL, ...) {

  # Create empty result, set error to true
  results <-  list(sim_list = list(), error = TRUE)

  # Perform simulations, don't break if Simplace breaks but return error=TRUE
  tryCatch({

    # fetch parameters from CroptimizeR and add the values to the parameter list
    # If required some (pre)processing or check of parameters could be done here
    # if the Simplace solution can't handle some parameters directly
    param <- list()
    for (iparam in names(param_values)) {
      param[[iparam]] <- param_values[iparam]
    }

    # Create and run simulation for each situation
    for (situation in model_options$Situations) {
      # Add the situation to the parameters
      param$projectid <- situation
      param[[model_options$SituationVariable]] <- situation

      # create the simulation with new parameters
      id <- simplace::createSimulation(model_options$SimplaceInstance, param)

      # run the simulation
      simplace::runSimulations(model_options$SimplaceInstance)

      # fetch the results and convert them to dataframe
      res <- simplace::getResult(model_options$SimplaceInstance,
                                 model_options$OutputId, id)
      df <- simplace::resultToDataframe(res)

      # Add the date column as CroptimizeR requires it
      # If required, some additional (post)processing can be performed here if
      # the Simplace output does not comply to the CroptimizeR standards
      df$Date <- df$CURRENT.DATE

      # Add the result of the situation
      results$sim_list[[situation]] <- df
    }

    # if every simulation succeeded, set error to FALSE
    results$error <- FALSE
  },
  error = function(e) {
      print(paste("Something went wrong in Simplace: ", e))
    }
  )

  # return the results
  results
}
