

#' Format a pwr-test
#'
#' @param statistic wrapped test from pwr package
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
render.sigr_pwr_htest <- function(statistic,
                              ...,
                              format,
                              statDigits=4,
                              sigDigits=4,
                              pLargeCutoff=1,
                              pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_pwr_htest")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  statStr <- paste0('%.',statDigits,'g')
  pwr <- statistic$pwr
  sigString <- render(wrapSignificance(pwr$sig.level,
                                       symbol='sig'),
                      format=format,
                      pLargeCutoff=pLargeCutoff,
                      pSmallCutoff=pSmallCutoff)
  powString <-  render(wrapSignificance(1-pwr$power,
                                        symbol='1-pwr'),
                       format=format,
                       pLargeCutoff=pLargeCutoff,
                       pSmallCutoff=pSmallCutoff)
  others <- setdiff(names(pwr),
                    c('sig.level', 'power', 'method', 'note'))
  fmt <- vapply(others,
                function(ni) {
                  vi <- pwr[[ni]]
                  if(is.numeric(vi)) {
                    paste0(ni, ': ', sprintf(statStr, vi))
                  } else {
                    paste0(ni, ': ', vi)
                  }
                }, character(1))
  formatStr <- paste0(fsyms['startB'],pwr$method,fsyms['endB'],
                      " ", sigString,
                      ", ", powString,
                      "; ", paste(fmt, collapse = ', '),
                      '.')
  formatStr
}

#' Wrap pwr test (difference in means by group).
#'
#' @param x test from pwr package
#' @param ... extra arguments
#'
#' @seealso \code{\link[pwr]{pwr.2p.test}}
#' @export
wrapPWR <- function(x,...) {
  UseMethod('wrapPWR')
}


#' Wrap pwr test.
#'
#' @param x pwr test result
#' @param ... extra arguments (not used)
#' @return formatted string and fields
#'
#' @examples
#'
#' if(require("pwr", quietly = TRUE)) {
#'   # Example from pwr package
#'   #  Exercise 6.1 p. 198 from Cohen (1988)
#'   test <- pwr::pwr.2p.test(h=0.3,n=80,sig.level=0.05,alternative="greater")
#'   wrapPWR(test)
#' }
#'
#' @export
wrapPWR.power.htest <- function(x,
                                ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapPWR.power.htest")
  if(!('power.htest' %in% class(x))) {
    stop('wrapTTest expected class htest')
  }
  r <- list(pwr=x,
            test='pwr test')
  class(r) <- c('sigr_pwr_htest', 'sigr_statistic')
  r
}

