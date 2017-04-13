


#' Wrap F-test (significane of a linear relation).
#'
#' @param x numeric, data.frame or lm where to get model or data to score.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapFTestImpl}},  \code{\link{wrapFTest.lm}},  and \code{\link{wrapFTest.data.frame}}
#' @export
wrapFTest <- function(x,...) UseMethod('wrapFTest')

#' Format an F-test
#'
#' @param statistic wrapped test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_ftest <- function(statistic,
                              ...,
                              format,
                              sigDigits=2,
                              pLargeCutoff=0.05,
                              pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_ftest unexpected arguments")
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
  formatStr =
    paste0(fsyms['startB'],'F Test',fsyms['endB'],
           ' summary: (',fsyms['RSq'],'=',
           sprintf('%.2g',statistic$R2),
           ', ',fsyms['startI'],'F',fsyms['endI'],
           '(',statistic$numdf,',',statistic$dendf,')=',
           sprintf('%.2g',statistic$FValue),
           ', ',pString,').')
  formatStr
}

#' Wrap F-test (significane of a linear relation).
#'
#' @param numdf degrees of freedom 1.
#' @param dendf degrees of freedom 2.
#' @param FValue observed F test statistic
#' @return wrapped statistic
#'
#' @importFrom stats pf
#'
#' @examples
#'
#' wrapFTestImpl(numdf=2,dendf=55,FValue=5.56)
#'
#' @export
wrapFTestImpl <- function(numdf, dendf, FValue) {
  Funscaled <- FValue*(numdf/dendf)
  R2 <- 1 - 1/(Funscaled+1)
  pValue <- stats::pf(FValue,
                      numdf,
                      dendf,
                      lower.tail = FALSE)
  r <- list(
    test='F Test',
    numdf=numdf,
    dendf=dendf,
    FValue=FValue,
    R2=R2,
    pValue=pValue)
  class(r) <- c('sigr_ftest', 'sigr_statistic')
  r
}

#' Wrap quality statistic of a linear regression.
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
#' sigr::wrapFTest(model)
#'
#'
#' @export
wrapFTest.lm <- function(x,
                           ...,
                           format,
                           pLargeCutoff=0.05,
                           pSmallCutoff=1.0e-5) {
  linearRegressionModel <- x
  if(length(list(...))) {
    stop('wrapFTest.lm extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  if(!'lm' %in% class(linearRegressionModel)) {
    stop('wrapFTest.lm expected class lm')
  }
  fitSummary <- summary(linearRegressionModel)
  fstats <- fitSummary$fstatistic
  FValue <- fstats[['value']]
  numdf <-  fstats[['numdf']]
  dendf <- fstats[['dendf']]
  wrapFTestImpl(numdf,dendf,FValue)
}

#' Wrap quality statistic of a linear relation from data.
#'
#' @param x data frame containing columns to compare
#' @param predictionColumnName character name of prediction column
#' @param yColumnName character name of column containing dependent variable
#' @param nParameters number of variables in model
#' @param meany (optional) mean of y
#' @param ... extra arguments (not used)
#' @param na.rm logical, if TRUE remove NA values
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
#' sigr::wrapFTest(d,'pred','y')
#'
#' @export
wrapFTest.data.frame <- function(x,
                                 predictionColumnName, yColumnName,
                                 nParameters= 1,
                                 meany= mean(x[[yColumnName]]),
                                 ...,
                                 na.rm= FALSE,
                                 format,
                                 pLargeCutoff= 0.05,
                                 pSmallCutoff= 1.0e-5) {
  d <- x
  y <- d[[yColumnName]]
  if(!is.numeric(y)) {
    stop("wrapr::wrapFTest.data.frame y column must be numeric")
  }
  predictions <- d[[predictionColumnName]]
  if(!is.numeric(predictions)) {
    stop("wrapr::wrapFTest.data.frame prediction column must be numeric")
  }
  if(length(list(...))) {
    stop('wrapFTest.data.frame extra arguments')
  }
  nNA <- sum(is.na(predictionColumnName) | is.na(yColumnName))
  if(na.rm) {
    goodPosns <- (!is.na(predictionColumnName)) & (!is.na(yColumnName))
    predictionColumnName <- predictionColumnName[goodPosns]
    yColumnName <- yColumnName[goodPosns]
  }
  n <- length(predictionColumnName)
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  if(!is.numeric(predictions)) {
    stop('wrapFTestFromData expected numeric argument')
  }
  n <- length(y)
  rss1 <- sum((y-meany)^2)
  rss2 <- sum((y-predictions)^2)
  p1 <- 1
  p2 <- 1 + nParameters
  FValue = ((rss1-rss2)/(p2-p1))/(rss2/(n-p2))
  res <- wrapFTestImpl(p2-p1,n-p2,FValue)
  res$n <- n
  res$nNA <- nNA
  res
}
