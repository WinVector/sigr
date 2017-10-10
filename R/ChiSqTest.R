

#' Format a chi-square test (quality of categorical prediction)
#'
#' @param statistic wrapped T-test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param statDigits integer number of digits to show in summaries.
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_chisqtest <- function(statistic,
                              ...,
                              format,
                              statDigits=2,
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
  statStr <- paste0('%.',statDigits,'g')
  pString <- render(wrapSignificance(statistic$pValue,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],'Chi-Square Test',fsyms['endB'],
                      ' summary: ',
                      fsyms['startI'],'pseudo-',fsyms['RSq'],fsyms['endI'],'=',
                      sprintf(statStr,statistic$pseudoR2),
                      ' (',
                      fsyms['chiSq'],'(',
                      statistic$delta_df,
                      ',', fsyms['startI'],'N',fsyms['endI'],
                      '=',(1+statistic$df.null),')=',
                      sprintf(statStr,statistic$delta_deviance),
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
#' @param x glm logistic regression model (glm(family=binomial))
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


#' Format ChiSqTest from model summary.
#'
#' @param x summary(glm(family=binomial)) object.
#' @param ... extra arguments (not used)
#' @return wrapped test
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x,data=d,family=binomial)
#' sum <- summary(model)
#' render(wrapChiSqTest(sum),pLargeCutoff=1,format='ascii')
#'
#'
#' @export
wrapChiSqTest.summary.glm <- function(x,
                              ...) {
  logisticRegressionModelSummary <- x
  if(length(list(...))) {
    stop('wrapChiSqTest.summary.glm extra arguments')
  }
  if(!'summary.glm' %in% class(logisticRegressionModelSummary)) {
    stop('wrapChiSqTest.summary.glm expected class summary.glm')
  }
  if(logisticRegressionModelSummary$family$family!='binomial') {
    warning('wrapChiSqTest.summary.glm: model family was not binomial')
  }
  if(logisticRegressionModelSummary$family$link!='logit') {
    warning('wrapChiSqTest.summary.glm: model link was not logit')
  }
  wrapChiSqTestImpl(logisticRegressionModelSummary$df.null,
                    logisticRegressionModelSummary$df.residual,
                    logisticRegressionModelSummary$null.deviance,
                    logisticRegressionModelSummary$deviance)
}





#' Format ChiSqTest from data.
#'
#' @param x data frame containing columns to compare
#' @param predictionColumnName character name of prediction column
#' @param yColumnName character name of column containing dependent variable
#' @param nParameters number of variables in model
#' @param meany (optional) mean of y
#' @param ... extra arguments (not used)
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped test
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'       y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x, data=d, family=binomial)
#' summary(model)
#' d$pred <- predict(model,type='response',newdata=d)
#' render(wrapChiSqTest(d,'pred','y'),pLargeCutoff=1)
#'
#' @export
wrapChiSqTest.data.frame <- function(x,
                                     predictionColumnName, yColumnName,
                                     nParameters= 1,
                                     meany= mean(x[[yColumnName]]),
                                     ...,
                                     na.rm= FALSE) {
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
  nNA <- sum(is.na(predictions) | is.na(y))
  if(na.rm) {
    goodPosns <- (!is.na(predictions)) & (!is.na(y))
    predictions <- predictions[goodPosns]
    y <- y[goodPosns]
  }
  n <- length(y)
  df.null <- n-1
  df.residual <- n-(1+nParameters)
  null.deviance <- calcDeviance(meany,y)
  deviance <- calcDeviance(predictions,y)
  res <- wrapChiSqTestImpl(df.null,df.residual,
                  null.deviance,deviance)
  res$nNA <- nNA
  res$n <- n
  res
}


#' Format ChiSqTest from anova of logistic model.
#'
#' @param x result from stats::anova(stats::glm(family=binomial))
#' @param ... extra arguments (not used)
#' @return list of formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x1= c(1,2,3,4,5,6,7,7),
#'                 x2= c(1,0,3,0,5,0,7,0),
#'                 y= c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
#' model <- glm(y~x1+x2, data=d, family=binomial)
#' summary(model)
#' render(wrapChiSqTest(model),
#'        pLargeCutoff=1, format='ascii')
#' anov <- anova(model)
#' print(anov)
#' lapply(sigr::wrapChiSqTest(anov),
#'        function(ti) {
#'          sigr::render(ti,
#'                       pLargeCutoff= 1,
#'                       pSmallCutoff= 0,
#'                       statDigits=4,
#'                       sigDigits=3,
#'                       format='ascii')
#'        })
#'
#' @export
wrapChiSqTest.anova <- function(x,
                            ...) {
  n <- length(x$Df)
  dfNull <- x$`Resid. Df`[[1]]
  devNull <- x$`Resid. Dev`[[1]]
  res <- lapply(2:n,
                function(i) {
                  wrapChiSqTestImpl(dfNull,  x$`Resid. Df`[[i]],
                                    devNull,  x$`Resid. Dev`[[i]])
                })
  names(res) <- attr(x,"row.names")[2:n]
  res
}

