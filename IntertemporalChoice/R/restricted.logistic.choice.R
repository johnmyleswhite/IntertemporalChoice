restricted.logistic.choice <-
  function(X1, T1, X2, T2, parameters)
  {
    V1 <- parameters[['value-function']](X1, T1)
    V2 <- parameters[['value-function']](X2, T2)
    dV <- V1 - V2
    z <- dV
    p <- 1 / (1 + exp(-z))
    return(p)
  }

