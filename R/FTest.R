

#' Format quality of a linear regression roughly in "APA Style" ( American Psychological Association ).
#'
#' @param x numeric, data.frame or lm where to get model or data to score.
#' @param ... extra arguments
#'
#' @seealso \code{\link{formatFTestImpl}},  \code{\link{formatFTest.lm}},  and \code{\link{formatFTest.data.frame}}
#' @export
formatFTest <- function(x,...) UseMethod('formatFTest')

#' Format quality of a linear regression roughly in "APA Style" ( American Psychological Association ).
#'
#' @param numdf degrees of freedom 1.
#' @param dendf degrees of freedom 2.
#' @param FValue observe F test statistic
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @importFrom stats pf
#'
#' @examples
#'
#' cat(sigr::formatFTestImpl(numdf=2,dendf=55,FValue=5.56)$formatStr)
#'
#' @export
formatFTestImpl <- function(numdf,dendf,FValue,
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
  Funscaled <- FValue*(numdf/dendf)
  R2 <- 1 - 1/(Funscaled+1)
  pValue <-  stats::pf(FValue,
                       numdf,
                       dendf,
                       lower.tail=FALSE)
  pString <- formatSignificance(pValue,'p', format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  list(
    test='F Test',
    numdf=numdf,
    dendf=dendf,
    FValue=FValue,
    format=format,
    pLargeCutoff=pLargeCutoff,
    pSmallCutoff=pSmallCutoff,
    R2=R2,
    pValue=pValue,
    formatStr=
      paste0(fsyms['startB'],'F Test',fsyms['endB'],
             ' summary: (',fsyms['RSq'],'=',
             sprintf('%.2g',R2),
             ', ',fsyms['startI'],'F',fsyms['endI'],'(',numdf,',',dendf,')=',
             sprintf('%.2g',FValue),
             ', ',pString,').')
  )
}

#' Format FTest from model.
#'
#' @param x lm model
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' model <- lm(y~x,data=d)
#' summary(model)
#' sigr::formatFTest(model)$formatStr
#'
#'
#' @export
formatFTest.lm <- function(x,
                           ...,
                           format,
                           pLargeCutoff=0.05,
                           pSmallCutoff=1.0e-5) {
  linearRegressionModel <- x
  if(length(list(...))) {
    stop('formatFTest.lm extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  if(!'lm' %in% class(linearRegressionModel)) {
    stop('formatFTestFromModel expected class lm')
  }
  fitSummary <- summary(linearRegressionModel)
  fstats <- fitSummary$fstatistic
  FValue <- fstats[['value']]
  numdf <-  fstats[['numdf']]
  dendf <- fstats[['dendf']]
  formatFTestImpl(numdf,dendf,FValue,format,pLargeCutoff,pSmallCutoff)
}

#' Format FTest from data.
#'
#' @param x data frame containing columns to compare
#' @param predictionColumnName character name of prediction column
#' @param yColumnName character name of column containing dependent variable
#' @param nParameters number of variables in model
#' @param meany (optional) mean of y
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx")
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' model <- lm(y~x,data=d)
#' summary(model)
#' d$pred <- predict(model,newdata=d)
#' sigr::formatFTest(d,'pred','y')$formatStr
#'
#' @export
formatFTest.data.frame <- function(x,
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
    stop('formatFTest.data.frame extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  if(!is.numeric(predictions)) {
    stop('formatFTestFromData expected numeric argument')
  }
  n <- length(y)
  rss1 <- sum((y-meany)^2)
  rss2 <- sum((y-predictions)^2)
  p1 <- 1
  p2 <- 1 + nParameters
  FValue = ((rss1-rss2)/(p2-p1))/(rss2/(n-p2))
  formatFTestImpl(p2-p1,n-p2,FValue,format,pLargeCutoff,pSmallCutoff)
}
