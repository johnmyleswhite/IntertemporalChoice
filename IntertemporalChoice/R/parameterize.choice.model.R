parameterize.choice.model <-
  function(choice.model = 'restricted-logistic', parameters = list())
  {
    choice.models <- list('restricted-logistic' = restricted.logistic.choice,
                          'kable-restricted-logistic' = kable.restricted.logistic.choice)
  
    if (! validate.parameters('null', choice.model, parameters))
    {
      stop('Required parameters could not be validated.')
    }
  
    return(function (X1, T1, X2, T2) {return(choice.models[[choice.model]](X1, T1, X2, T2, parameters))})
  }

