% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROC.R
\name{sensitivity_and_specificity_s12p12n}
\alias{sensitivity_and_specificity_s12p12n}
\title{Compute the shape1_pos, shape2_pos, shape1_neg, shape2_neg graph.}
\usage{
sensitivity_and_specificity_s12p12n(
  Score,
  ...,
  shape1_pos,
  shape2_pos,
  shape1_neg,
  shape2_neg
)
}
\arguments{
\item{Score}{vector of sensitivities to evaluate}

\item{...}{force later arguments to bind by name.}

\item{shape1_pos}{beta shape1 parameter for positive examples}

\item{shape2_pos}{beta shape2 parameter for positive examples}

\item{shape1_neg}{beta shape1 parameter for negative examples}

\item{shape2_neg}{beta shape1 parameter for negative examples}
}
\value{
Score, Specificity and Sensitivity data frame
}
\description{
Compute specificity and sensitivity given specificity and model fit parameters.
}
\examples{

library(wrapr)

empirical_data <- rbind(
  data.frame(
    Score = rbeta(1000, shape1 = 3, shape2 = 2),
    y = TRUE),
  data.frame(
    Score = rbeta(1000, shape1 = 5, shape2 = 4),
    y = FALSE)
)

unpack[shape1_pos = shape1, shape2_pos = shape2] <-
  fit_beta_shapes(empirical_data$Score[empirical_data$y])

shape1_pos
shape2_pos

unpack[shape1_neg = shape1, shape2_neg = shape2] <-
  fit_beta_shapes(empirical_data$Score[!empirical_data$y])

shape1_neg
shape2_neg

ideal_roc <- sensitivity_and_specificity_s12p12n(
  seq(0, 1, 0.1),
  shape1_pos = shape1_pos,
  shape1_neg = shape1_neg,
  shape2_pos = shape2_pos,
  shape2_neg = shape2_neg)


empirical_roc <- build_ROC_curve(
  modelPredictions = empirical_data$Score,
  yValues = empirical_data$y
)

# # should look very similar
# library(ggplot2)
# ggplot(mapping = aes(x = 1 - Specificity, y = Sensitivity)) +
#   geom_line(data = empirical_roc, color='DarkBlue') +
#   geom_line(data = ideal_roc, color = 'Orange')

}
