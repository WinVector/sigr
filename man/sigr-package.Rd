\name{sigr-package}
\alias{sigr-package}
\alias{sigr}
\docType{package}
\title{
sigr: a package for formatting significances
}
\description{
    sigr is a package for succinctly formatting significance summaries of
    various models and tests.  The main purpose is unified reporting and planning
    of experimental results, working around issue such as the difficulty of
    extracting model significances (such as with lm/glm).
}
\details{
\tabular{ll}{
Package: \tab sigr\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2016-10-26\cr
License: \tab GNU General Public License version 3\cr
}
}
\author{
John Mount and Nina Zumel

Maintainer: <contact@win-vector.com>
}

\keyword{ package }

\examples{
d <- data.frame(x=c(1,2,3,4,5,6,7,7),
       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
model <- glm(y~x,data=d,family=binomial)
summary(model)
sigr::formatChiSqTest(model,pLargeCutoff=1,format='ascii')$formatStr
}
