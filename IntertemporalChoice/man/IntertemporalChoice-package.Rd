\name{IntertemporalChoice-package}
\alias{IntertemporalChoice-package}
\alias{IntertemporalChoice}
\docType{package}
\title{
Functions to fit microeconomic models of intertemporal discounting to choice data.
}
\description{
Microeconomics provides two canonical models of the value of delayed goods: the exponential discounting model and the hyperbolic discounting model. This package provides tools to estimate the parameters for both of these models from empirical choice data. Functions are also included to make predictions from fitted models and to simulate choice behavior for ideal agents with specified parameters.
}
\details{
\tabular{ll}{
Package: \tab IntertemporalChoice\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-04-21\cr
License: \tab Artistic-2.0\cr
LazyLoad: \tab yes\cr
}
}
\author{
John Myles White

Maintainer: John Myles White <jmw@johnmyleswhite.com>
}
\references{
David Laibson, ``Golden Eggs and Hyperbolic Discounting'', Quarterly Journal of Economics, 1997
}
\keyword{ package }
\seealso{
}
\examples{
library('IntertemporalChoice')

n <- 100

choice.data <- data.frame(X1 = rep(10, n),
                          T1 = rep(0, n),
                          X2 = rep(11, n),
                          T2 = (1:n) / 52,
                          EarlierOptionChosen = rep(NA, n))

value.function <- parameterize.value.model(value.model = 'exponential',
                                           parameters = list('delta' = 0.75))

choice.function <- parameterize.choice.model(choice.model = 'restricted-logistic',
                                             parameters = list('value-function' = value.function))

choice.data <- simulate.discounting.model(choice.data, choice.function)

exponential.fit <- fit.discounting.model(choice.data)

predicted.data <- predict.discounting.model(choice.data, exponential.fit$fitted_model)

rmse <- sqrt(mean(predicted.data$EarlierOptionChosen
                  - choice.data$EarlierOptionChosen))
}
