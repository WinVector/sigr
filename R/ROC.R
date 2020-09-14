


#' calculate ROC curve.
#'
#' Based on:
#'  \url{https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html}
#'
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues truth values (not empty, same length as model predictions)
#' @param ... force later arguments to bind by name.
#' @param na.rm logical, if TRUE remove NA values.
#' @param yTarget value considered to be positive.
#' @return the ROC graph of Score (model score), Sensitivity, and Specificity. Guaranteed to have the (0, 0) and (1, 1) (1-Specificity,Sensitivity) endpoints.
#'
#' @examples
#'
#' sigr::build_ROC_curve(1:4, c(TRUE,FALSE,TRUE,TRUE))
#'
#' @export
build_ROC_curve <- function(modelPredictions, yValues,
                    ...,
                    na.rm = FALSE,
                    yTarget = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::build_ROC_curve")
  if(!is.numeric(modelPredictions)) {
    stop("sigr::calcAUC modelPredictions must be numeric")
  }
  yValues <- yValues==yTarget
  if(!is.logical(yValues)) {
    stop("sigr::build_ROC_curve yValues must be logical")
  }
  if(length(modelPredictions)!=length(yValues)) {
    stop("sigr::build_ROC_curve must have length(modelPredictions)==length(yValues)")
  }
  if(na.rm) {
    goodPosns <- (!is.na(modelPredictions)) & (!is.na(yValues))
    modelPredictions <- modelPredictions[goodPosns]
    yValues <- yValues[goodPosns]
  }
  x <- NULL
  y <- NULL
  positive_prevalence <- 0
  if(length(yValues) <= 0) {
      modelPredictions <- c(NA_real_, NA_real_)
      x = c(0, 1)
      y = c(0, 1)
  } else {
    positive_prevalence <- mean(yValues)
    ord <- order(modelPredictions, decreasing=TRUE)
    yValues <- yValues[ord]
    modelPredictions <- modelPredictions[ord]
    # FPR is the x-axis, TPR the y.
    x <- cumsum(!yValues)/max(1,sum(!yValues)) # FPR = x-axis
    y <- cumsum(yValues)/max(1,sum(yValues))   # TPR = y-axis
    # each point should be fully after a bunch of points or fully before a
    # decision level. remove dups to achieve this.
    dup <- c(modelPredictions[-1]>=modelPredictions[-length(modelPredictions)],
             FALSE)
    x <- x[!dup]
    y <- y[!dup]
    modelPredictions <- modelPredictions[!dup]
    # And add in ideal endpoints just in case
    if((x[[1]] > 0) || (y[[1]] > 0)) {
      x <- c(0, x)
      y <- c(0, y)
      modelPredictions <- c(NA_real_, modelPredictions)
    }
    if((x[[length(x)]] < 1) || (y[[length(x)]] < 1)) {
      x <- c(x, 1)
      y <- c(y, 1)
      modelPredictions <- c(modelPredictions, NA_real_)
    }
  }
  data.frame(
    Score = modelPredictions,
    Sensitivity = y,
    Specificity = 1 - x)
}


#' Add ROC derived columns.
#'
#' Add ROC columns derived from sensitivity and specificity.
#'
#' @param d input data frame, must at lest of columns Sensitivity and Specificity
#' @param positive_prevalence scalar, the prevalence of the positive class or prior odds
#' @return extended data frame with more columns
#'
#' @examples
#'
#' d <- data.frame(pred = 1:4, truth = c(TRUE,FALSE,TRUE,TRUE))
#' roc <- build_ROC_curve(d$pred, d$truth)
#' add_ROC_derived_columns(roc, mean(d$truth))
#'
#' @export
add_ROC_derived_columns <- function(d, positive_prevalence) {
  # standard definitions
  d$FalsePositiveRate <- 1 - d$Specificity
  d$TruePositiveRate <- d$Sensitivity
  d$TrueNegativeRate <- 1 - d$FalsePositiveRate
  d$FalseNegativeRate <- 1 - d$TruePositiveRate
  # to prevalences (rates normalized by total population, not by class)
  d$false_positive_prevalence = d$FalsePositiveRate * (1 - positive_prevalence)
  d$true_positive_prevalence = d$TruePositiveRate * positive_prevalence
  d$true_negative_prevalence = d$TrueNegativeRate * (1 - positive_prevalence)
  d$false_negative_prevalence = d$FalseNegativeRate * positive_prevalence
  # return value
  d
}



#' calculate AUC.
#'
#' Based on:
#'  \url{https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html}
#'
#' @param modelPredictions numeric predictions (not empty), ordered (either increasing or decreasing)
#' @param yValues truth values (not empty, same length as model predictions)
#' @param ... force later arguments to bind by name.
#' @param na.rm logical, if TRUE remove NA values.
#' @param yTarget value considered to be positive.
#' @return area under curve
#'
#' @examples
#'
#' sigr::calcAUC(1:4, c(TRUE,FALSE,TRUE,TRUE)) # should be 2/3
#'
#' @export
calcAUC <- function(modelPredictions, yValues,
                    ...,
                    na.rm = FALSE,
                    yTarget = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::calcAUC")
  d <- build_ROC_curve(
    modelPredictions = modelPredictions,
    yValues = yValues,
    na.rm = na.rm,
    yTarget = yTarget)
  # sum areas of segments (triangle topped vertical rectangles)
  n <- nrow(d)
  area <- sum( ((d$Sensitivity[-1]+d$Sensitivity[-n])/2) * abs(d$Specificity[-1]-d$Specificity[-n]) )
  area
}

