choice.sequence.log.probability <-
  function(choice.data,
           parameterized.choice.model,
           X1.colname = 'X1',
           T1.colname = 'T1',
           X2.colname = 'X2',
           T2.colname = 'T2',
           EarlierOptionChosen.colname = 'EarlierOptionChosen')
  {
    log.probability <- 0
    
    for (i in 1:nrow(choice.data))
    {
      p <- parameterized.choice.model(choice.data[i, X1.colname],
                                      choice.data[i, T1.colname],
                                      choice.data[i, X2.colname],
                                      choice.data[i, T2.colname])
      
      if (choice.data[i, EarlierOptionChosen.colname] == 1)
      {
        log.probability <- log.probability + log(p)
      }
      else
      {
        log.probability <- log.probability + log(1 - p)
      }
    }
    
    return(log.probability)
  }

