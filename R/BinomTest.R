

#' Format binom.test (test of rate of a Bernoulli experiment).
#'
#' @param statistic wrapped binom.test.
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param statDigits integer number of digits to show in summaries.
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' bt <- binom.test(7, 10, 0.5)
#' wrapBinomTest(bt)
#'
#'
#' @export
render.sigr_binomtest <- function(statistic,
                                  ...,
                                  format,
                                  statDigits=4,
                                  sigDigits=4,
                                  pLargeCutoff=0.05,
                                  pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_binomtest")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  stat_format_str <- paste0('%.',statDigits,'g')
  bt <- statistic$bt
  alt <- bt$alternative
  hyp_rate <- bt$null.value[["probability of success"]]
  r <- bt$estimate
  n <- bt$parameter[["number of trials"]]
  k <- bt$statistic[["number of successes"]]
  pString <- render(wrapSignificance(bt$p.value,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],bt$method,fsyms['endB'],
                      ': (',
                      alt,
                      ' ',
                      sprintf(stat_format_str,hyp_rate),
                      '; ',
                      k, "/", n, "=",
                      '=',sprintf(stat_format_str,r),
                      '; ',pString,').')
  formatStr
}


#' Wrap binom.test (test of Bernoulli rate).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapBinomTest.htest}}, and  \code{\link{wrapBinomTest.data.frame}}
#' @export
wrapBinomTest <- function(x,...) {
  UseMethod('wrapBinomTest')
}



#' Wrap binom.test (test of Bernoulli rate).
#'
#' @param x binom.test result
#' @param ... extra arguments (not used)
#' @return wrapped stat
#'
#' @examples
#'
#' bt <- binom.test(7, 10, 0.5)
#' wrapBinomTest(bt)
#'
#'
#' @export
wrapBinomTest.htest <- function(x,
                                ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapBinomTest.htest")
  r <- list(bt=x,
            test='binom.test',
            nNA = 0)
  class(r) <- c('sigr_binomtest', 'sigr_statistic')
  r
}

#' Wrap binom.test (test of Bernoulli rate).
#'
#' @param x data.frame
#' @param ColumnName character name of measurment column
#' @param SuccessValue value considered a success (positive)
#' @param ... extra arguments passed to binom.test
#' @param p number, hypothesized probability of success.
#' @param alternative passed to \code{\link[stats]{binom.test}}
#' @param conf.level passed to \code{\link[stats]{binom.test}}
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped stat
#'
#' @examples
#'
#' d <- data.frame(x = c(rep(0, 3), rep(1, 7)))
#' wrapBinomTest(d, "x", 1)
#' d <- data.frame(x = c(rep(0, 15), rep(1, 35)))
#' wrapBinomTest(d, "x", 1)
#'
#'
#' @importFrom stats binom.test
#'
#' @export
wrapBinomTest.data.frame <- function(x,
                                     ColumnName,
                                     SuccessValue,
                                     ...,
                                     p = 0.5,
                                     alternative = c("two.sided", "less", "greater"),
                                     conf.level = 0.95,
                                     na.rm= FALSE) {
  col <- x[[ColumnName]]
  nNA <- sum(is.na(col))
  if(na.rm) {
    col <- col[!is.na(col)]
  }
  n <- length(col)
  x <- sum(col == SuccessValue)
  bt <- stats::binom.test(x, n,
                          p = p,
                          alternative = alternative,
                          conf.level = conf.level)
  r <- list(bt=bt,
            test='binom.test',
            pValue = p,
            nNA=nNA)
  class(r) <- c('sigr_binomtest', 'sigr_statistic')
  r
}
