hyperbolic.discounting <-
  function(x, t, parameters)
  {
    return(x / (1 + parameters[['k']] * t))
  }

