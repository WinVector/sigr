

#' Format binom.test (test of rate of a Binomial/Bernoulli experiment).
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
  r <- bt$estimate
  n <- bt$parameter[["number of trials"]]
  k <- bt$statistic[["number of successes"]]
  ci <- bt$conf.int
  cl <- attr(ci, "conf.level", exact = TRUE)
  if(statistic$show_p) {
    alt <- bt$alternative
    hyp_rate <- bt$null.value[["probability of success"]]
    pString <- render(wrapSignificance(bt$p.value,
                                       symbol='p'),
                      format=format,
                      pLargeCutoff=pLargeCutoff,
                      pSmallCutoff=pSmallCutoff)
    formatStr <- paste0(fsyms['startB'],bt$method,fsyms['endB'],
                        ': (',
                        k, "/", n, "=",
                        sprintf(stat_format_str,r),
                        "~c(", sprintf(stat_format_str,cl), ")",
                        "[", sprintf(stat_format_str,ci[[1]]), ", ", sprintf(stat_format_str,ci[[2]]), "]",
                        ", ",
                        alt,
                        ' ',
                        sprintf(stat_format_str,hyp_rate),
                        '; ',pString,').')
  } else {
    formatStr <- paste0(fsyms['startB'],bt$method,fsyms['endB'],
                        ': (',
                        k, "/", n, "=",
                        sprintf(stat_format_str,r),
                        "~c(", sprintf(stat_format_str,cl), ")",
                        "[", sprintf(stat_format_str,ci[[1]]), ", ", sprintf(stat_format_str,ci[[2]]), "]",
                        ').')
  }
  formatStr
}


#' Wrap binom.test (test of Binomial/Bernoulli rate).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapBinomTest.htest}}, \code{\link{wrapBinomTestS}}, \code{\link{wrapBinomTest.logical}}, \code{\link{wrapBinomTest.numeric}}, \code{\link{wrapBinomTest.data.frame}}
#' @export
wrapBinomTest <- function(x,...) {
  UseMethod('wrapBinomTest')
}



#' Wrap binom.test (test of Binomial/Bernoulli rate).
#'
#' @param x binom.test result
#' @param ... not used, just for argument compatibility
#' @return wrapped stat
#'
#' @seealso \code{\link{wrapBinomTest}}, \code{\link{wrapBinomTest.htest}}, \code{\link{wrapBinomTestS}}, \code{\link{wrapBinomTest.logical}}, \code{\link{wrapBinomTest.numeric}}, \code{\link{wrapBinomTest.data.frame}}
#'
#' @examples
#'
#' bt <- binom.test(7, 10, 0.5)
#' wrapBinomTest(bt)
#'
#' @export
wrapBinomTest.htest <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapBinomTest.htest")
  r <- list(bt=x,
            test='binom.test',
            nNA = 0,
            show_p = TRUE)
  class(r) <- c('sigr_binomtest', 'sigr_statistic')
  r
}


#' Wrap binom.test (test of Binomial/Bernoulli rate) from summary.
#'
#' @param x numeric scalar, number of successes.
#' @param n numeric scalar, number of trials.
#' @param ... extra arguments passed to binom.test
#' @param p number, hypothesized probability of success.
#' @param alternative passed to \code{\link[stats]{binom.test}}
#' @param conf.level passed to \code{\link[stats]{binom.test}}
#' @return wrapped stat
#'
#' @seealso \code{\link{wrapBinomTest}}, \code{\link{wrapBinomTest.htest}}, \code{\link{wrapBinomTestS}}, \code{\link{wrapBinomTest.logical}}, \code{\link{wrapBinomTest.numeric}}, \code{\link{wrapBinomTest.data.frame}}
#'
#' @examples
#'
#' wrapBinomTestS(3, 7, p = 0.5)
#' wrapBinomTestS(300, 700, p = 0.5)
#'
#'
#' @importFrom stats binom.test
#'
#' @export
wrapBinomTestS <- function(x, n,
                           ...,
                           p = NA,
                           alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapBinomTestS")
  if((!is.numeric(x))||(length(x)!=1)) {
    stop("sigr::wrapBinomTestS x must be a numeric scalar")
  }
  if((!is.numeric(n))||(length(n)!=1)) {
    stop("sigr::wrapBinomTestS n must be a numeric scalar")
  }
  nNA <- 0
  bt <- stats::binom.test(x, n,
                          p = ifelse(is.na(p), 0.5, p),
                          alternative = alternative,
                          conf.level = conf.level)
  r <- list(bt=bt,
            test='binom.test',
            nNA=nNA,
            show_p = !is.na(p))
  class(r) <- c('sigr_binomtest', 'sigr_statistic')
  r
}



#' Wrap binom.test (test of Binomial/Bernoulli rate).
#'
#' @param x logical, vector of trials.
#' @param ... extra arguments passed to binom.test
#' @param p number, hypothesized probability of success.
#' @param alternative passed to \code{\link[stats]{binom.test}}
#' @param conf.level passed to \code{\link[stats]{binom.test}}
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped stat
#'
#' @seealso \code{\link{wrapBinomTest}}, \code{\link{wrapBinomTest.htest}}, \code{\link{wrapBinomTestS}}, \code{\link{wrapBinomTest.logical}}, \code{\link{wrapBinomTest.numeric}}, \code{\link{wrapBinomTest.data.frame}}
#'
#' @examples
#'
#' x = c(rep(FALSE, 3), rep(TRUE, 7))
#' wrapBinomTest(x)
#' x = c(rep(FALSE, 15), rep(TRUE, 35))
#' wrapBinomTest(x)
#'
#'
#' @importFrom stats binom.test
#'
#' @export
wrapBinomTest.logical <- function(x,
                                  ...,
                                  p = NA,
                                  alternative = c("two.sided", "less", "greater"),
                                  conf.level = 0.95,
                                  na.rm= FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapBinomTest.logical")
  nNA <- sum(is.na(x))
  if(na.rm) {
    x <- x[!is.na(x)]
  }
  if(any(is.na(x))) {
    stop("sigr::wrapBinomTest saw NAs")
  }
  n <- length(x)
  x <- sum(x)
  wrapBinomTestS(x, n,
                 p = p,
                 alternative = alternative,
                 conf.level = conf.level)
}


#' Wrap binom.test (test of Binomial/Bernoulli rate).
#'
#' @param x numeric, vector of trials.
#' @param SuccessValue value considered a success (positive)
#' @param ... extra arguments passed to binom.test
#' @param p number, hypothesized probability of success.
#' @param alternative passed to \code{\link[stats]{binom.test}}
#' @param conf.level passed to \code{\link[stats]{binom.test}}
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped stat
#'
#' @seealso \code{\link{wrapBinomTest}}, \code{\link{wrapBinomTest.htest}}, \code{\link{wrapBinomTestS}}, \code{\link{wrapBinomTest.logical}}, \code{\link{wrapBinomTest.numeric}}, \code{\link{wrapBinomTest.data.frame}}
#'
#' @examples
#'
#' x = c(rep(0, 3), rep(1, 7))
#' wrapBinomTest(x, 1)
#' x = c(rep(0, 15), rep(1, 35))
#' wrapBinomTest(x, 1)
#'
#'
#' @importFrom stats binom.test
#'
#' @export
wrapBinomTest.numeric <- function(x,
                                  SuccessValue = TRUE,
                                  ...,
                                  p = NA,
                                  alternative = c("two.sided", "less", "greater"),
                                  conf.level = 0.95,
                                  na.rm= FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapBinomTest.numeric")
  wrapBinomTest.logical(x==SuccessValue,
                        p = p,
                        alternative = alternative,
                        conf.level =conf.level,
                        na.rm= na.rm)
}



#' Wrap binom.test (test of Binomial/Bernoulli rate).
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
#' @seealso \code{\link{wrapBinomTest}}, \code{\link{wrapBinomTest.htest}}, \code{\link{wrapBinomTestS}}, \code{\link{wrapBinomTest.logical}}, \code{\link{wrapBinomTest.numeric}}, \code{\link{wrapBinomTest.data.frame}}
#'
#' @examples
#'
#' d <- data.frame(x = c(rep(0, 3), rep(1, 7)))
#' wrapBinomTest(d, "x", 1, p = 0.5)
#' d <- data.frame(x = c(rep(0, 15), rep(1, 35)))
#' wrapBinomTest(d, "x", 1, p = 0.5)
#'
#'
#' @importFrom stats binom.test
#'
#' @export
wrapBinomTest.data.frame <- function(x,
                                     ColumnName,
                                     SuccessValue = TRUE,
                                     ...,
                                     p = NA,
                                     alternative = c("two.sided", "less", "greater"),
                                     conf.level = 0.95,
                                     na.rm= FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapBinomTest.data.frame")
  wrapBinomTest.logical(x[[ColumnName]]==SuccessValue,
                        p = p,
                        alternative = alternative,
                        conf.level =conf.level,
                        na.rm= na.rm)
}

