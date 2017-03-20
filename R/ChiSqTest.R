

#' Format a chi-square test (quality of categorical prediction)
#'
#' @param statistic wrapped T-test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_chisqtest <- function(statistic,
                              ...,
                              format,
                              sigDigits=2,
                              pLargeCutoff=0.05,
                              pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_ttest unexpected arguments")
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  pString <- render(wrapSignificance(statistic$pValue,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],'Chi-Square Test',fsyms['endB'],
                      ' summary: ',
                      fsyms['startI'],'pseudo-',fsyms['RSq'],fsyms['endI'],'=',
                      sprintf('%.2g',statistic$pseudoR2),
                      ' (',
                      fsyms['chiSq'],'(',
                      statistic$delta_df,
                      ',', fsyms['startI'],'N',fsyms['endI'],
                      '=',(1+statistic$df.null),')=',
                      sprintf('%.2g',statistic$delta_deviance),
                      ', ',pString,').')
  formatStr
}

#' Wrap quality of a categorical prediction roughly in "APA Style" ( American Psychological Association ).
#'
#' @param x numeric, data.frame or lm where to get model or data to score.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapChiSqTestImpl}},  \code{\link{wrapChiSqTest.glm}},  and \code{\link{wrapChiSqTest.data.frame}}
#' @export
wrapChiSqTest <- function(x,...) UseMethod('wrapChiSqTest')


#' Format quality of a logistic regression roughly in "APA Style"
#'  ( American Psychological Association ).
#'
#' @param df.null null degrees of freedom.
#' @param df.residual residual degrees of freedom.
#' @param null.deviance null deviance
#' @param deviance residual deviance
#' @return wrapped statistic
#'
#' @importFrom stats pchisq
#'
#' @examples
#'
#' wrapChiSqTestImpl(df.null=7,df.residual=6,
#'     null.deviance=11.09035,deviance=10.83726)
#'
#' @export
wrapChiSqTestImpl <- function(df.null,df.residual,
                                    null.deviance,deviance) {
  delta_deviance <- null.deviance - deviance
  delta_df <- df.null - df.residual
  sig <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
  pseudoR2 <- 1.0 - deviance/null.deviance # pseudo R-squared
  r <- list(test="Chi-Square test",
            df.null=df.null,
            df.residual=df.residual,
            null.deviance=null.deviance,
            deviance=deviance,
            pseudoR2=pseudoR2,
            pValue=sig,
            sig=sig,
            delta_deviance=delta_deviance,
            delta_df=delta_df)
  class(r) <- c('sigr_chisqtest', 'sigr_statistic')
  r
}

#' Format ChiSqTest from model.
#'
#' @param x glm logistic regression model
#' @param ... extra arguments (not used)
#' @return wrapped test
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x,data=d,family=binomial)
#' summary(model)
#' render(wrapChiSqTest(model),pLargeCutoff=1,format='ascii')
#'
#'
#' @export
wrapChiSqTest.glm <- function(x,
                                ...) {
  logisticRegressionModel <- x
  if(length(list(...))) {
    stop('wrapChiSqTest.glm extra arguments')
  }
  if(!'glm' %in% class(logisticRegressionModel)) {
    stop('wrapChiSqTest.glm expected class glm')
  }
  if(logisticRegressionModel$family$family!='binomial') {
    warning('wrapChiSqTest.glm: model family was not binomial')
  }
  if(logisticRegressionModel$family$link!='logit') {
    warning('wrapChiSqTest.glm: model link was not logit')
  }
  if(!logisticRegressionModel$converged) {
    warning("wrapChiSqTest.glm: model did not converge")
  }
  wrapChiSqTestImpl(logisticRegressionModel$df.null,
                  logisticRegressionModel$df.residual,
                  logisticRegressionModel$null.deviance,
                  logisticRegressionModel$deviance)
}



#' Format ChiSqTest from data.
#'
#' @param x data frame containing columns to compare
#' @param predictionColumnName character name of prediction column
#' @param yColumnName character name of column containing dependent variable
#' @param nParameters number of variables in model
#' @param meany (optional) mean of y
#' @param ... extra arguments (not used)
#' @return wrapped test
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x,data=d,family=binomial)
#' summary(model)
#' d$pred <- predict(model,type='response',newdata=d)
#' render(wrapChiSqTest(d,'pred','y'),pLargeCutoff=1)
#'
#' @export
wrapChiSqTest.data.frame <- function(x,
                                       predictionColumnName, yColumnName,
                                       nParameters=1,
                                       meany=mean(x[[yColumnName]]),
                                       ...) {
  d <- x
  y <- d[[yColumnName]]
  if(!is.logical(y)) {
    stop("wrapr::wrapChiSqTest.data.frame y column must be logical")
  }
  predictions <- d[[predictionColumnName]]
  if(!is.numeric(predictions)) {
    stop("wrapr::wrapChiSqTest.data.frame prediction column must be numeric")
  }
  if(length(list(...))) {
    stop('wrapChiSqTest.data.frame extra arguments')
  }
  n <- length(y)
  df.null <- n-1
  df.residual <- n-(1+nParameters)
  null.deviance <- calcDeviance(meany,y)
  deviance <- calcDeviance(predictions,y)
  wrapChiSqTestImpl(df.null,df.residual,
                  null.deviance,deviance)
}

