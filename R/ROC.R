
#' calculate AUC.
#'
#' Based on:
#'  \url{https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html}
#'
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues truth values (not empty, same length as model predictions)
#' @param ... force later arguments to bind by name.
#' @param na.rm logical, if TRUE remove NA values.
#' @param yTarget value considered to be positive.
#' @return the ROC graph as an x,y data frame (with additional derived columns)
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
  d <- NULL
  positive_prevalence <- 0
  if(length(yValues) <= 0) {
    d <- data.frame(
      x = c(0, 1),
      y = c(0, 1))
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
    # also remove the 0 and 1 x-points if present, so when we add in there
    # are no dups
    extreme <- ((x <= 0) & (y <=0)) | ((x >= 1) & (y>=1))
    x <- x[!extreme]
    y <- y[!extreme]
    # And add in ideal endpoints just in case (redundancy here is not a problem).
    x <- c(0, x, 1)
    y <- c(0, y, 1)
    d <- data.frame(
      x = x,
      y = y)
  }
  # add derived columns
  d$FalsePositiveRate <- d$x
  d$TruePositiveRate <- d$y
  d$Sensitivity <- d$TruePositiveRate
  d$Specificity <- 1 - d$FalsePositiveRate
  d$TrueNegativeRate <- 1 - d$FalsePositiveRate
  d$FalseNegativeRate <- 1 - d$TruePositiveRate
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
#' @param modelPredictions numeric predictions (not empty)
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
  area <- sum( ((d$y[-1]+d$y[-n])/2) * (d$x[-1]-d$x[-n]) )
  area
}

