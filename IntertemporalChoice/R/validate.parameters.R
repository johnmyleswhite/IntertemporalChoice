validate.parameters <-
  function(value.model, choice.model, parameters)
  {
    required.parameters <- list('exponential' = c('delta'),
                                'hyperbolic' = c('k'),
                                'restricted-logistic' = c('value-function'),
                                'kable-restricted-logistic' = c('value-function'),
                                'null' = c())
    
    expected.parameters <- union(required.parameters[[value.model]],
                                 required.parameters[[choice.model]])
    
    for (parameter in expected.parameters)
    {
      if (is.null(parameters[[parameter]]))
      {
        warning(paste('Value of required parameter ',
                      parameter,
                      ' is NULL.',
                      sep = ''))
        return(FALSE)
      }  
    }
    
    return(TRUE)
  }

