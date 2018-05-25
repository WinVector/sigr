


#' Wrap F-test (significane identity relation).
#'
#' @param x numeric, data.frame or lm where to get model or data to score.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapFTestImpl}},  \code{\link{wrapFTest.lm}},  and \code{\link{wrapFTest.data.frame}}
#' @export
wrapFTest <- function(x,...) {
  UseMethod('wrapFTest')
}

#' Format an F-test
#'
#' @param statistic wrapped test
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
render.sigr_ftest <- function(statistic,
                              ...,
                              format,
                              statDigits=3,
                              sigDigits=2,
                              pLargeCutoff=0.05,
                              pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_ftest")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  pString <- render(wrapSignificance(statistic$pValue,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  statStr <- paste0('%.',statDigits,'g')
  formatStr =
    paste0(fsyms['startB'],'F Test',fsyms['endB'],
           ' summary: (',fsyms['RSq'],'=',
           sprintf(statStr,statistic$R2),
           ', ',fsyms['startI'],'F',fsyms['endI'],
           '(',statistic$numdf,',',statistic$dendf,')=',
           sprintf(statStr,statistic$FValue),
           ', ',pString,').')
  formatStr
}

#' Wrap F-test (significane of identity relation).
#'
#' @param numdf degrees of freedom 1.
#' @param dendf degrees of freedom 2.
#' @param FValue observed F test statistic
#' @param ... not used, force later arguments to bind by name
#' @param format optional, suggested format
#' @return wrapped statistic
#'
#' @importFrom stats pf
#'
#' @examples
#'
#' wrapFTestImpl(numdf=2,dendf=55,FValue=5.56)
#'
#' @export
wrapFTestImpl <- function(numdf, dendf, FValue,
                          ...,
                          format = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFTestImpl")
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
    pValue=pValue,
    format=format)
  class(r) <- c('sigr_ftest', 'sigr_statistic')
  r
}

#' Wrap quality statistic of identity r regression.
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
                           format = NULL,
                           pLargeCutoff=0.05,
                           pSmallCutoff=1.0e-5) {
  linearRegressionModel <- x
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFTest.lm")
  if(!'lm' %in% class(linearRegressionModel)) {
    stop('wrapFTest.lm expected class lm')
  }
  fitSummary <- summary(linearRegressionModel)
  fstats <- fitSummary$fstatistic
  FValue <- fstats[['value']]
  numdf <-  fstats[['numdf']]
  dendf <- fstats[['dendf']]
  wrapFTestImpl(numdf,dendf,FValue,
                format = format)
}



#' Wrap quality statistic of linear regression summary.
#'
#' @param x summary.lm summary(lm()) object
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
#' sum <- summary(model)
#' sigr::wrapFTest(sum)
#'
#'
#' @export
wrapFTest.summary.lm <- function(x,
                         ...,
                         format = NULL,
                         pLargeCutoff=0.05,
                         pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFTest.summary.lm")
  fitSummary <- x
  if(!'summary.lm' %in% class(fitSummary)) {
    stop('wrapFTest.summary.lm expected class summary.lm')
  }
  fstats <- fitSummary$fstatistic
  FValue <- fstats[['value']]
  numdf <-  fstats[['numdf']]
  dendf <- fstats[['dendf']]
  wrapFTestImpl(numdf,dendf,FValue,
                format = format)
}



#' Wrap quality statistic of identity relation from data.
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
                                 format = NULL,
                                 pLargeCutoff= 0.05,
                                 pSmallCutoff= 1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFTest.data.frame")
  d <- x
  y <- d[[yColumnName]]
  if(!is.numeric(y)) {
    stop("wrapr::wrapFTest.data.frame y column must be numeric")
  }
  predictions <- d[[predictionColumnName]]
  if(!is.numeric(predictions)) {
    stop("wrapr::wrapFTest.data.frame prediction column must be numeric")
  }
  nNA <- sum(is.na(predictionColumnName) | is.na(yColumnName))
  if(na.rm) {
    goodPosns <- (!is.na(predictionColumnName)) & (!is.na(yColumnName))
    predictionColumnName <- predictionColumnName[goodPosns]
    yColumnName <- yColumnName[goodPosns]
  }
  n <- length(predictionColumnName)
  if(!is.numeric(predictions)) {
    stop('wrapFTestFromData expected numeric argument')
  }
  n <- length(y)
  rss1 <- sum((y-meany)^2)
  rss2 <- sum((y-predictions)^2)
  p1 <- 1
  p2 <- 1 + nParameters
  FValue = ((rss1-rss2)/(p2-p1))/(rss2/(n-p2))
  res <- wrapFTestImpl(p2-p1,n-p2,FValue,
                       format = format)
  res$n <- n
  res$nNA <- nNA
  res
}


#' Wrap quality statistic of a linear relation from anova.
#'
#' @param x result from stats::anova(stats::lm())
#' @param ... extra arguments (not used)
#' @return list of formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x1 = c(1,2,3,4,5,6,7,7),
#'                 x2 = c(1,0,3,0,5,6,0,7),
#'                 y =  c(1,1,2,2,3,3,4,4))
#' model <- lm(y~x1+x2, data=d)
#' summary(model)
#' sigr::wrapFTest(model)
#' anov <- stats::anova(model)
#' print(anov)
#' lapply(sigr::wrapFTest(anov),
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
wrapFTest.anova <- function(x,
                            ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFTest.anova")
  n <- length(x$Df)
  res <- lapply(seq_len(n-1),
                function(i) {
                  wrapFTestImpl(x$Df[[i]], x$Df[[n]], x$`F value`[[i]])
                })
  names(res) <- attr(x,"row.names")[seq_len(n-1)]
  res
}

#' Wrap quality statistic of a linear relation from ezANOVA (package ez).
#'
#' Please see \url{https://github.com/WinVector/sigr/issues/1#issuecomment-322311947} for an example.
#'
#' @param x list result from ezANOVA (package ez).
#' @param ... extra arguments (not used)
#' @return list of formatted string and fields
#'
#'
#' @export
wrapFTestezANOVA <- function(x,
                            ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFTestezANOVA")
  fs <- lapply(seq_len(nrow(x$ANOVA)),
               function(rowIndex) {
                 row <- x$ANOVA[rowIndex, , drop=FALSE]
                 sigr::wrapFTestImpl(row$DFn, row$DFd, row$F)
               })
  names(fs) <- x$ANOVA$Effect
  fs
}

