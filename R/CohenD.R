

#' Format Cohen-D (effect size between groups)
#'
#' @param statistic wrapped effsize::cohen.d
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param statDigits integer number of digits to show in summaries (not yet implemented).
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_cohend <- function(statistic,
                              ...,
                              format,
                              statDigits=3,
                              sigDigits=2,
                              pLargeCutoff=1,
                              pSmallCutoff=0) {
  if(length(list(...))>0) {
    stop("render.sigr_cohend unexpected arguments")
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  cohen_d <- statistic$cohen_d
  stat_format_str <- paste0('%.',statDigits,'g')
  pString <- render(wrapSignificance(1-cohen_d$conf.level,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],cohen_d$method,fsyms['endB'],
                      ': ', sprintf(stat_format_str,cohen_d$estimate),
                      ' ("',cohen_d$magnitude,
                      '", interval [', sprintf(stat_format_str,cohen_d$conf.int[[1]]),
                      ', ', sprintf(stat_format_str,cohen_d$conf.int[[2]]),'] at ',
                      pString,').')
  formatStr
}

#' Wrap Cohen's D (effect size between groups).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link[effsize]{cohen.d}}, \code{\link{wrapCohenD.effsize}}, and \code{\link{wrapCohenD.data.frame}}
#' @export
wrapCohenD <- function(x,...) UseMethod('wrapCohenD')


#' Wrap Cohen's D (effect size between groups).
#'
#' @param x effsize::cohen.d result
#' @param ... extra arguments (not used)
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' cohen_d <- effsize::cohen.d(d$x,d$y)
#' render(wrapCohenD(cohen_d))
#'
#' @export
wrapCohenD.effsize <- function(x,
                            ...) {
  if(length(list(...))) {
    stop('wrapCohenD extra arguments')
  }
  if(!'effsize' %in% class(x)) {
    stop('wrapCohenD expected class effsize')
  }
  r <- list(cohen_d=x,
            test='cohen.d')
  class(r) <- c('sigr_cohend', 'sigr_statistic')
  r
}

#' Wrap Cohen's D (effect size between groups).
#'
#' @param x data.frame
#' @param Column1Name character column 1 name
#' @param Column2Name character column 2 name
#' @param ... extra arguments passed to effsize::cohen.d
#' @param na.rm logical, if TRUE remove NA values
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' render(wrapCohenD(d,'x','y'))
#'
#' @importFrom effsize cohen.d
#'
#' @export
wrapCohenD.data.frame <- function(x,
                                 Column1Name,
                                 Column2Name,
                                 ...,
                                 na.rm= FALSE) {
  if(!'data.frame' %in% class(x)) {
    stop('sigr::wrapCohenD expected class data.frame')
  }
  if(!is.numeric(x[[Column1Name]])) {
    stop("sigr::wrapCohenD expected column 1 to be numeric")
  }
  if(!is.numeric(x[[Column2Name]])) {
    stop("sigr::wrapCohenD expected column 2 to be numeric")
  }
  c1 <- x[[Column1Name]]
  c2 <- x[[Column2Name]]
  nNA <- sum(is.na(c1) | is.na(c2))
  if(na.rm) {
    goodPosns <- (!is.na(c1)) & (!is.na(c2))
    c1 <- c1[goodPosns]
    c2 <- c2[goodPosns]
  }
  n <- length(c1)
  cohen_d <- cohen.d(c1,c2,...)
  r <- list(cohen_d=cohen_d,
            test='cohen.d',
            Column1Name=Column1Name,
            Column2Name=Column2Name,
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_cohend', 'sigr_statistic')
  r
}
