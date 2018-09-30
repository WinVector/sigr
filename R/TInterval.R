
#' @importFrom stats var
NULL

#' Format a Student-T tolerance-style interval around an estimate of a mean.
#'
#' Report sample size (n), sample mean, bias-corrected standard deviation estimate
#' (assuming normality, using a chi-square distribution correction
#' from \url{https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation#Bias_correction}),
#' and a Student t-test tolerance-style confidence interval.
#'
#' @param statistic wrapped TInterval.
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
#' set.seed(2018)
#' d <- rnorm(100) + 3.2
#' TInterval(d)
#'
#' @export
render.sigr_tinterval <- function(statistic,
                                  ...,
                                  format,
                                  statDigits=4,
                                  sigDigits=4,
                                  pLargeCutoff=0.05,
                                  pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_tinterval")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  stat_format_str <- paste0('%.',statDigits,'g')
  formatStr <- paste0(fsyms['startB'],statistic$test,fsyms['endB'],
                      ': (n=',
                      sprintf(stat_format_str, statistic$sample_size),
                      " ~ ",
                      sprintf(stat_format_str, statistic$sample_mean),
                      "+-",
                      sprintf(stat_format_str, statistic$corrected_dev),
                      "*Z: c(",
                      sprintf(stat_format_str, sqrt(statistic$conf.level)),
                      ")",
                      "[", sprintf(stat_format_str, statistic$interval_low), ", ", sprintf(stat_format_str, statistic$interval_high), "]",
                      ').')
  formatStr
}


#' Wrap TInterval (test of Binomial/Bernoulli rate).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{TIntervalS}}, \code{\link{TInterval.numeric}}, \code{\link{TInterval.data.frame}}
#' @export
TInterval <- function(x, ...) {
  UseMethod('TInterval')
}




#' Student-T tolerance-style interval around an estimate of a mean from summary.
#'
#' @param sample_size numeric scalar integer, size of sample.
#' @param sample_mean numeric scalar, mean of sample.
#' @param sample_var numeric scalar, variance of sample (Bessel-corrected).
#' @param ... extra arguments passed to TInterval.
#' @param nNA number of NAs seen.
#' @param conf.level confidence level to draw interval
#' @return wrapped stat
#'
#' @seealso \code{\link{TInterval}}, \code{\link{TIntervalS}}, \code{\link{TInterval.numeric}}, \code{\link{TInterval.data.frame}}
#'
#' @examples
#'
#' set.seed(2018)
#' d <- rnorm(100) + 3.2
#' TIntervalS(length(d), mean(d), stats::var(d))
#'
#' @export
TIntervalS <- function(sample_size,
                       sample_mean,
                       sample_var,
                       ...,
                       nNA = 0,
                       conf.level = 0.95) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::TIntervalS")
  if((!is.numeric(sample_size))||(length(sample_size)!=1)) {
    stop("sigr::TIntervalS sample_size must be a numeric scalar")
  }
  if((!is.numeric(sample_mean))||(length(sample_mean)!=1)) {
    stop("sigr::TIntervalS sample_mean must be a numeric scalar")
  }
  if((!is.numeric(sample_var))||(length(sample_var)!=1)) {
    stop("sigr::TIntervalS sample_var must be a numeric scalar")
  }
  prob_tail <- (1-conf.level)/2
  width_t <- stats::qt(prob_tail, df = sample_size-1,
                       ncp = 0, lower.tail = FALSE)
  # see https://en.wikipedia.org/wiki/Student%27s_t-distribution
  width <- width_t*sqrt(sample_var/sample_size)
  # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
  c4n <- sqrt(2/(sample_size-1))*gamma(sample_size/2)/gamma((sample_size-1)/2)
  corrected_dev <- sqrt(sample_var)/c4n
  r <- list(test='TInterval',
            sample_size = sample_size,
            sample_mean = sample_mean,
            sample_var = sample_var,
            corrected_dev = corrected_dev,
            nNA = nNA,
            conf.level = conf.level,
            interval_low = sample_mean - width,
            interval_high = sample_mean + width)
  class(r) <- c('sigr_tinterval', 'sigr_statistic')
  r
}



#' Student-T tolerance-style interval around an estimate of a mean from observations.
#'
#' @param x logical, vector of observations.
#' @param ... extra arguments passed to TInterval
#' @param conf.level confidence level to draw interval
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped stat
#'
#' @seealso \code{\link{TInterval}}, \code{\link{TIntervalS}}, \code{\link{TInterval.numeric}}, \code{\link{TInterval.data.frame}}
#'
#' @examples
#'
#' set.seed(2018)
#' d <- rnorm(100) + 3.2
#' TInterval(d)
#'
#'
#' @export
TInterval.numeric <- function(x,
                              ...,
                              conf.level = 0.95,
                              na.rm = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::TInterval.logical")
  nNA <- sum(is.na(x))
  if(na.rm) {
    x <- x[!is.na(x)]
  }
  if(any(is.na(x))) {
    stop("sigr::TInterval saw NAs")
  }
  TIntervalS(sample_size = length(x),
             sample_mean = mean(x),
             sample_var = var(x),
             nNA = nNA,
             conf.level = conf.level)
}




#' Student-T tolerance-style interval around an estimate of a mean from a data.frame.
#'
#' @param x data.frame
#' @param ColumnName character name of measurment column
#' @param ... extra arguments passed to TInterval
#' @param conf.level confidence level to draw interval
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped stat
#'
#' @seealso \code{\link{TInterval}}, \code{\link{TIntervalS}}, \code{\link{TInterval.numeric}}, \code{\link{TInterval.data.frame}}
#'
#' @examples
#'
#' set.seed(2018)
#' d <- data.frame(x = rnorm(100) + 3.2)
#' TInterval(d, "x")
#'
#'
#' @export
TInterval.data.frame <- function(x,
                                 ColumnName,
                                 ...,
                                 conf.level = 0.95,
                                 na.rm= FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::TInterval.data.frame")
  TInterval.numeric(x[[ColumnName]],
                    conf.level = conf.level,
                    na.rm = na.rm)
}

