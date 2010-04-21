parameterize.value.model <-
  function(value.model = 'exponential', parameters = list())
  {
    value.models <- list('exponential' = exponential.discounting,
                         'hyperbolic' = hyperbolic.discounting)
  
    if (! validate.parameters(value.model, 'null', parameters))
    {
      stop('Required parameters could not be validated.')
    }
  
    return(function (X, T) {return(value.models[[value.model]](X, T, parameters))})
  }

