

#' Format a T-test (difference in means by group)
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
render.sigr_ttest <- function(statistic,
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
  tt <- statistic$tt
  pString <- render(wrapSignificance(tt$p.value,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],tt$method,fsyms['endB'],
                     ', ',tt$alternative,
                     ': (',fsyms['startI'],'t',fsyms['endI'],
                     '=',paste(sprintf('%.2g',tt$statistic),collapse=','),
                     ', ',fsyms['startI'],'df',fsyms['endI'],'=',
                     sprintf('%.2g',tt$parameter),', ',
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
#' @param ... extra arguments
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
                            ...) {
  if(!'data.frame' %in% class(x)) {
    stop('wrapTTest expected class data.frame')
  }
  tt <- t.test(x[[Column1Name]],x[[Column2Name]],...)
  r <- list(tt=tt,
            test='t.test',
            Column1Name=Column1Name,
            Column2Name=Column2Name)
  class(r) <- c('sigr_ttest', 'sigr_statistic')
  r
}
