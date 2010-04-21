predict.discounting.model <-
  function(choice.data,
           choice.function,
           X1.colname = 'X1',
           T1.colname = 'T1',
           X2.colname = 'X2',
           T2.colname = 'T2',
           EarlierOptionChosen.colname = 'EarlierOptionChosen')
  {
    for (i in 1:nrow(choice.data))
    {
      p <- choice.function(choice.data[i, X1.colname],
                           choice.data[i, T1.colname],
                           choice.data[i, X2.colname],
                           choice.data[i, T2.colname])
      if (p > 0.5)
      {
        choice.data[i, EarlierOptionChosen.colname] <- 1
      }
      else
      {
        choice.data[i, EarlierOptionChosen.colname] <- 0
      }
    }
    
    return(choice.data)
  }

