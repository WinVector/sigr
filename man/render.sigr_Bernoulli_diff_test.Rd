% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DifferenceDistribution.R
\name{render.sigr_Bernoulli_diff_test}
\alias{render.sigr_Bernoulli_diff_test}
\title{Format sigr_Bernoulli_diff_test (test of difference of Bernoulli processes).}
\usage{
\method{render}{sigr_Bernoulli_diff_test}(
  statistic,
  ...,
  format,
  statDigits = 4,
  sigDigits = 4,
  pLargeCutoff = 0.05,
  pSmallCutoff = 1e-05
)
}
\arguments{
\item{statistic}{wrapped cor.test.}

\item{...}{extra arguments (not used)}

\item{format}{if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)}

\item{statDigits}{integer number of digits to show in summaries.}

\item{sigDigits}{integer number of digits to show in significances.}

\item{pLargeCutoff}{value to declare non-significance at or above.}

\item{pSmallCutoff}{smallest value to print}
}
\value{
formatted string
}
\description{
Format sigr_Bernoulli_diff_test (test of difference of Bernoulli processes).
}
\examples{

Bernoulli_diff_stat(2000, 5000, 100, 200)
Bernoulli_diff_stat(2000, 5000, 100, 200, 0.1)
Bernoulli_diff_stat(2000, 5000, 100, 199)
Bernoulli_diff_stat(2000, 5000, 100, 199, 0.1)


}
