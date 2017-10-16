

#' Format a T-test (difference in means by group)
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
render.sigr_ttest <- function(statistic,
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
  tt <- statistic$tt
  stat_format_str <- paste0('%.',statDigits,'g')
  pString <- render(wrapSignificance(tt$p.value,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],tt$method,fsyms['endB'],
                     ', ',tt$alternative,
                     ': (',fsyms['startI'],'t',fsyms['endI'],
                     '=',paste(sprintf(stat_format_str,tt$statistic),collapse=','),
                     ', ',fsyms['startI'],'df',fsyms['endI'],'=',
                     sprintf(stat_format_str,tt$parameter),', ',
                     pString,').')
  formatStr
}

#' Wrap t.test (difference in means by group).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapTTest.htest}}, and  \code{\link{wrapTTest.data.frame}}
#' @export
wrapTTest <- function(x,...) UseMethod('wrapTTest')


#' Wrap t.test (difference in means by group).
#'
#' @param x t.test result
#' @param ... extra arguments (not used)
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' tt <- t.test(d$x,d$y)
#' render(wrapTTest(tt),pLargeCutoff=1)
#' # confirm not rescaling, as a correlation test would
#' render(wrapTTest(t.test(d$x,2*d$y)),pLargeCutoff=1)
#'
#' @export
wrapTTest.htest <- function(x,
                          ...) {
  if(length(list(...))) {
    stop('wrapTTest extra arguments')
  }
  if(!'htest' %in% class(x)) {
    stop('wrapTTest expected class htest')
  }
  r <- list(tt=x,
       test='t.test')
  class(r) <- c('sigr_ttest', 'sigr_statistic')
  r
}

#' Wrap t.test (difference in means by group).
#'
#' @param x data.frame
#' @param Column1Name character column 1 name
#' @param Column2Name character column 2 name
#' @param ... extra arguments passed to ttest
#' @param y passed to \code{\link[stats]{t.test}}
#' @param alternative passed to \code{\link[stats]{t.test}}
#' @param mu passed to \code{\link[stats]{t.test}}
#' @param paired passed to \code{\link[stats]{t.test}}
#' @param var.equal passed to \code{\link[stats]{t.test}}
#' @param conf.level passed to \code{\link[stats]{t.test}}
#' @param na.rm logical, if TRUE remove NA values
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' render(wrapTTest(d,'x','y'),pLargeCutoff=1)
#' # confirm p not order depedent
#' render(wrapTTest(d,'y','x'),pLargeCutoff=1)
#'
#' @importFrom stats t.test
#'
#' @export
wrapTTest.data.frame <- function(x,
                                 Column1Name,
                                 Column2Name,
                                 ...,
                                 y = NULL,
                                 alternative = c("two.sided", "less", "greater"),
                                 mu = 0, paired = FALSE, var.equal = FALSE,
                                 conf.level = 0.95,
                                 na.rm= FALSE) {
  if(!'data.frame' %in% class(x)) {
    stop('sigr::wrapTTest expected class data.frame')
  }
  if(!is.numeric(x[[Column1Name]])) {
    stop("sigr::wrapTTest expected column 1 to be numeric")
  }
  if(!is.numeric(x[[Column2Name]])) {
    stop("sigr::wrapTTest expected column 2 to be numeric")
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
  tt <- stats::t.test(c1,c2,
                      alternative =alternative,
                      mu = mu, paired = paired, var.equal =  var.equal,
                      conf.level = conf.level, ...)
  r <- list(tt=tt,
            test='t.test',
            Column1Name=Column1Name,
            Column2Name=Column2Name,
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_ttest', 'sigr_statistic')
  r
}


#' Wrap t.test (difference in means by group).
#'
#' @param x numeric population 1
#' @param pop2 numeric population 2
#' @param ... extra arguments passed to ttest
#' @param y passed to \code{\link[stats]{t.test}}
#' @param alternative passed to \code{\link[stats]{t.test}}
#' @param mu passed to \code{\link[stats]{t.test}}
#' @param paired passed to \code{\link[stats]{t.test}}
#' @param var.equal passed to \code{\link[stats]{t.test}}
#' @param conf.level passed to \code{\link[stats]{t.test}}
#' @param na.rm logical, if TRUE remove NA values
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' render(wrapTTest(d$x, d$y), pLargeCutoff=1)
#' # confirm p not order depedent
#' render(wrapTTest(d$y, d$x),pLargeCutoff=1)
#'
#' @importFrom stats t.test
#'
#' @export
wrapTTest.numeric <- function(x,
                              pop2,
                              ...,
                              y = NULL,
                              alternative = c("two.sided", "less", "greater"),
                              mu = 0, paired = FALSE, var.equal = FALSE,
                              conf.level = 0.95,
                              na.rm= FALSE) {
  if(!is.numeric(x)) {
    stop("sigr::wrapTTest expected x to be numeric")
  }
  if(!is.numeric(pop2)) {
    stop("sigr::wrapTTest expected pop2 to be numeric")
  }
  c1 <- x
  c2 <- pop2
  nNA <- sum(is.na(c1) | is.na(c2))
  if(na.rm) {
    goodPosns <- (!is.na(c1)) & (!is.na(c2))
    c1 <- c1[goodPosns]
    c2 <- c2[goodPosns]
  }
  n <- length(c1)
  tt <- stats::t.test(c1,c2,
                      alternative = alternative,
                      mu = mu, paired = paired, var.equal = var.equal,
                      conf.level = conf.level, ...)
  r <- list(tt=tt,
            test='t.test',
            Column1Name='x',
            Column2Name='pop2',
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_ttest', 'sigr_statistic')
  r
}
