
# TODO: add S3 generic access (like FTest).

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
#' @param tt t.test result
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
wrapTTest <- function(tt,
                          ...) {
  if(length(list(...))) {
    stop('wrapTTest extra arguments')
  }
  if(!'htest' %in% class(tt)) {
    stop('wrapTTest expected class htest')
  }
  r <- list(tt=tt,
       test='t.test')
  class(r) <- c('sigr_ttest', 'sigr_statistic')
  r
}
