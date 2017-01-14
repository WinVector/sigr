
# TODO: S3 genericize wrapping.

#' Format cor.test (test of liner correlation).
#'
#' @param statistic wrapped cor.test.
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' ct <- cor.test(d$x,d$y)
#' wrapCorTest(ct)
#'
#'
#' @export
render.sigr_cortest <- function(statistic,
                          ...,
                          format,
                          sigDigits=2,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5) {
  if(length(list(...))) {
    stop('render.sigr_cortest extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  ct <- statistic$ct
  pString <- render(wrapSignificance(ct$p.value,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],ct$method,fsyms['endB'],
                ': (',fsyms['startI'],'r',fsyms['endI'],
                '=',sprintf('%.2g',ct$estimate),
                ', ',pString,').')
  formatStr
}

#' Wrap cor.test (test of liner correlation).
#'
#' @param ct cor.test result
#' @param ... extra arguments (not used)
#' @return wrapped stat
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' ct <- cor.test(d$x,d$y)
#' wrapCorTest(ct)
#'
#'
#' @export
wrapCorTest <- function(ct,
                          ...) {
  if(length(list(...))) {
    stop('wrapCorTest extra arguments')
  }
  r <- list(ct=ct,
            test='cor.test')
  class(r) <- c('sigr_cortest', 'sigr_statistic')
  r
}
