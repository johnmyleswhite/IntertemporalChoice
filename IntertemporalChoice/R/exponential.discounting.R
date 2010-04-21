exponential.discounting <-
  function(x, t, parameters)
  {
    return(x * parameters[['delta']] ^ t)
  }

