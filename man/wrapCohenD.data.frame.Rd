% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CohenD.R
\name{wrapCohenD.data.frame}
\alias{wrapCohenD.data.frame}
\title{Wrap Cohen's D (effect size between groups).}
\usage{
\method{wrapCohenD}{data.frame}(x, Column1Name, Column2Name, ..., na.rm = FALSE)
}
\arguments{
\item{x}{data.frame}

\item{Column1Name}{character column 1 name}

\item{Column2Name}{character column 2 name}

\item{...}{extra arguments (not used)}

\item{na.rm}{if TRUE remove NAs}
}
\value{
formatted string and fields
}
\description{
Wrap Cohen's D (effect size between groups).
}
\examples{

d <- data.frame(x = c(1,1,2,2,3,3,4,4),
                y = c(1,2,3,4,5,6,7,7))
render(wrapCohenD(d,'x','y'))

}
