fit.discounting.model <-
  function(choice.data,
           value.model = 'exponential',
           choice.model = 'restricted-logistic',
           X1.colname = 'X1',
           T1.colname = 'T1',
           X2.colname = 'X2',
           T2.colname = 'T2',
           EarlierOptionChosen.colname = 'EarlierOptionChosen')
  {
    if (sum(apply(choice.data, 1, function (r) {any(is.na(r))})) > 0)
    {
      stop('NA\'s encountered in choice.data. Aborting analysis.')
    }
    
    if (value.model == 'exponential')
    {
      if (choice.model == 'restricted-logistic')
      {
        optimum <- optimize(function(d)
                            {
                              value.function <- parameterize.value.model(value.model = 'exponential',
                                                                         parameters = list('delta' = d))
                              choice.function <- parameterize.choice.model(choice.model = 'restricted-logistic',
                                                                           parameters = list('value-function' = value.function))
                              return(choice.sequence.log.probability(choice.data, choice.function, X1.colname = X1.colname, T1.colname = T1.colname, X2.colname = X2.colname, T2.colname = T2.colname, EarlierOptionChosen.colname = EarlierOptionChosen.colname))
                            },
                            c(0, 1),
                            maximum = TRUE)
        value.function <- parameterize.value.model(value.model = 'exponential',
                                                   parameters = list('delta' = optimum$maximum))
        choice.function <- parameterize.choice.model(choice.model = 'restricted-logistic',
                                                     parameters = list('value-function' = value.function))
        output <- list('parameters' = list('delta' = optimum$maximum),
                       'fitted_model' = choice.function,
                       'log_likelihood' = optimum$objective)
        return(output)
      }
      
      if (choice.model == 'kable-restricted-logistic')
      {
        optimum <- optimize(function(d)
                            {
                              value.function <- parameterize.value.model(value.model = 'exponential',
                                                                         parameters = list('delta' = d))
                              choice.function <- parameterize.choice.model(choice.model = 'kable-restricted-logistic',
                                                                           parameters = list('value-function' = value.function))
                              return(choice.sequence.log.probability(choice.data, choice.function, X1.colname = X1.colname, T1.colname = T1.colname, X2.colname = X2.colname, T2.colname = T2.colname, EarlierOptionChosen.colname = EarlierOptionChosen.colname))
                            },
                            c(0, 1),
                            maximum = TRUE)
        value.function <- parameterize.value.model(value.model = 'exponential',
                                                   parameters = list('delta' = optimum$maximum))
        choice.function <- parameterize.choice.model(choice.model = 'kable-restricted-logistic',
                                                     parameters = list('value-function' = value.function))
        output <- list('parameters' = list('delta' = optimum$maximum),
                       'fitted_model' = choice.function,
                       'log_likelihood' = optimum$objective)
        return(output)
      }
    }
    
    if (value.model == 'hyperbolic')
    {
      if (choice.model == 'restricted-logistic')
      {
        optimum <- optimize(function(k)
                            {
                              value.function <- parameterize.value.model(value.model = 'hyperbolic',
                                                                         parameters = list('k' = k))
                              choice.function <- parameterize.choice.model(choice.model = 'restricted-logistic',
                                                                           parameters = list('value-function' = value.function))
                              return(choice.sequence.log.probability(choice.data, choice.function, X1.colname = X1.colname, T1.colname = T1.colname, X2.colname = X2.colname, T2.colname = T2.colname, EarlierOptionChosen.colname = EarlierOptionChosen.colname))
                            },
                            c(0, 10),
                            maximum = TRUE)
        value.function <- parameterize.value.model(value.model = 'hyperbolic',
                                                   parameters = list('k' = optimum$maximum))
        choice.function <- parameterize.choice.model(choice.model = 'restricted-logistic',
                                                     parameters = list('value-function' = value.function))
        output <- list('parameters' = list('k' = optimum$maximum),
                       'fitted_model' = choice.function,
                       'log_likelihood' = optimum$objective)
        return(output)
      }
      
      if (choice.model == 'kable-restricted-logistic')
      {
        optimum <- optimize(function(k)
                            {
                              value.function <- parameterize.value.model(value.model = 'hyperbolic',
                                                                         parameters = list('k' = k))
                              choice.function <- parameterize.choice.model(choice.model = 'kable-restricted-logistic',
                                                                           parameters = list('value-function' = value.function))
                              return(choice.sequence.log.probability(choice.data, choice.function, X1.colname = X1.colname, T1.colname = T1.colname, X2.colname = X2.colname, T2.colname = T2.colname, EarlierOptionChosen.colname = EarlierOptionChosen.colname))
                            },
                            c(0, 10),
                            maximum = TRUE)
        value.function <- parameterize.value.model(value.model = 'hyperbolic',
                                                   parameters = list('k' = optimum$maximum))
        choice.function <- parameterize.choice.model(choice.model = 'kable-restricted-logistic',
                                                     parameters = list('value-function' = value.function))
        output <- list('parameters' = list('k' = optimum$maximum),
                       'fitted_model' = choice.function,
                       'log_likelihood' = optimum$objective)
        return(output)
      }
    }
  }
