

#' Format quality of a linear regression roughly in "APA Style" ( American Psychological Association ).
#'
#' @param x numeric, data.frame or lm where to get model or data to score.
#' @param ... extra arguments
#'
#' @seealso \code{\link{formatChiSqTestImpl}},  \code{\link{formatChiSqTest.glm}},  and \code{\link{formatChiSqTest.data.frame}}
#' @export
formatChiSqTest <- function(x,...) UseMethod('formatChiSqTest')


#' Format quality of a logistic regression roughly in "APA Style"
#'  ( American Psychological Association ).
#'
#' @param df.null null degrees of freedom.
#' @param df.residual residual degrees of freedom.
#' @param null.deviance null deviance
#' @param deviance residual deviance
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @importFrom stats pchisq
#'
#' @examples
#'
#' cat(sigr::formatChiSqTestImpl(df.null=7,df.residual=6,
#'     null.deviance=11.09035,deviance=10.83726)$formatStr)
#'
#' @export
formatChiSqTestImpl <- function(df.null,df.residual,
                                    null.deviance,deviance,
                                    format,
                                    pLargeCutoff=0.05,
                                    pSmallCutoff=1.0e-5) {
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  delta_deviance <- null.deviance - deviance
  delta_df <- df.null - df.residual
  sig <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
  pseudoR2 <- 1.0 - deviance/null.deviance # pseudo R-squared
  pString <- formatSignificance(sig,
                                symbol='p',
                                format=format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  list(test="Chi-Square test",
       df.null=df.null,
       df.residual=df.residual,
       null.deviance=null.deviance,
       deviance=deviance,
       format=format,
       pLargeCutoff=pLargeCutoff,
       pSmallCutoff=pSmallCutoff,
       pseudoR2=pseudoR2,
       pValue=sig,
       formatStr=paste0(fsyms['startB'],'Chi-Square Test',fsyms['endB'],
                        ' summary: ',
                        fsyms['startI'],'pseudo-',fsyms['RSq'],fsyms['endI'],'=',
                        sprintf('%.2g',pseudoR2),
                        ' (',
                        fsyms['chiSq'],'(',
                        delta_df,
                        ',', fsyms['startI'],'N',fsyms['endI'],'=',(1+df.null),')=',
                        sprintf('%.2g',delta_deviance),
                        ', ',pString,').'))
}

#' Format ChiSqTest from model.
#'
#' @param x glm logistic regression model
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x,data=d,family=binomial)
#' summary(model)
#' sigr::formatChiSqTest(model,pLargeCutoff=1,format='ascii')$formatStr
#'
#'
#' @export
formatChiSqTest.glm <- function(x,
                                ...,
                                format,
                                pLargeCutoff=0.05,
                                pSmallCutoff=1.0e-5) {
  logisticRegressionModel <- x
  if(length(list(...))) {
    stop('formatChiSqTest.glm extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  if(!'glm' %in% class(logisticRegressionModel)) {
    stop('formatChiSqTest.glm expected class glm')
  }
  if(logisticRegressionModel$family$family!='binomial') {
    warning('formatChiSqTestFromModel: model family was not binomial')
  }
  if(logisticRegressionModel$family$link!='logit') {
    warning('formatChiSqTestFromModel: model link was not logit')
  }
  if(!logisticRegressionModel$converged) {
    warning("formatChiSqTestFromModel: model did not converge")
  }
  formatChiSqTestImpl(logisticRegressionModel$df.null,
                  logisticRegressionModel$df.residual,
                  logisticRegressionModel$null.deviance,
                  logisticRegressionModel$deviance,
                  format,pLargeCutoff,pSmallCutoff)
}



#' Format ChiSqTest from data.
#'
#' @param x data frame containing columns to compare
#' @param predictionColumnName character name of prediction column
#' @param yColumnName character name of column containing dependent variable
#' @param nParameters number of variables in model
#' @param meany (optional) mean of y
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x,data=d,family=binomial)
#' summary(model)
#' d$pred <- predict(model,type='response',newdata=d)
#' sigr::formatChiSqTest(d,'pred','y',pLargeCutoff=1)$formatStr
#'
#' @export
formatChiSqTest.data.frame <- function(x,
                                       predictionColumnName,yColumnName,
                                       nParameters=1,
                                       meany=mean(x[[yColumnName]]),
                                       ...,
                                       format,
                                       pLargeCutoff=0.05,
                                       pSmallCutoff=1.0e-5) {
  d <- x
  y <- d[[yColumnName]]
  predictions <- d[[predictionColumnName]]
  if(length(list(...))) {
    stop('formatChiSqTest.data.frame extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  n <- length(y)
  df.null <- n-1
  df.residual <- n-(1+nParameters)
  null.deviance <- calcDeviance(meany,y)
  deviance <- calcDeviance(predictions,y)
  formatChiSqTestImpl(df.null,df.residual,
                  null.deviance,deviance,
                  format,pLargeCutoff,pSmallCutoff)
}

