enhanceWithBoundaries <- function(fun, l_bound, u_bound, 
                                  penalty_value=Inf, 
                                  boundary_fun=NULL,
                                  param_pos=1, 
                                  ...) {
  function(...) {
    invalid <- FALSE
    if(!is.null(boundary_fun)) {
      invalid <- boundary_fun(...elt(param_pos))
    }
    else {
      invalid <- !all(l_bound <= ...elt(param_pos) & ...elt(param_pos) <= u_bound)
    }
    if(invalid) {
      penalty_value
    }
    else {
      fun(...)
    }
  }
}

