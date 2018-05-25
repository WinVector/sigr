




#' Format Cohen-D (effect size between groups)
#'
#' @param statistic CohenD-approximation
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
render.sigr_cohend <- function(statistic,
                              ...,
                              format,
                              statDigits=3,
                              sigDigits=2,
                              pLargeCutoff=1,
                              pSmallCutoff=0) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_cohend")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  stat_format_str <- paste0('%.',statDigits,'g')
  formatStr <- paste0(fsyms['startB'],statistic$test,fsyms['endB'],
                      ': ', sprintf(stat_format_str,statistic$cohen_d))
  formatStr
}

#' Wrap Cohen's D (effect size between groups).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapCohenD.data.frame}}
#' @export
wrapCohenD <- function(x,...) {
  UseMethod('wrapCohenD')
}


# NOT following strict definition, ad-hoc sensible calculation.
calcCohenD <- function(pop1, pop2) {
  pooledSD <- sd(c(pop1 - mean(pop1), pop2 - mean(pop2)))
  popDiff <- mean(pop2) - mean(pop1)
  CohenD <- popDiff/pooledSD
  CohenD
}


#' Wrap Cohen's D (effect size between groups).
#'
#' @param x data.frame
#' @param Column1Name character column 1 name
#' @param Column2Name character column 2 name
#' @param ... extra arguments (not used)
#' @param na.rm if TRUE remove NAs
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x = c(1,1,2,2,3,3,4,4),
#'                 y = c(1,2,3,4,5,6,7,7))
#' render(wrapCohenD(d,'x','y'))
#'
#' @export
wrapCohenD.data.frame <- function(x,
                                 Column1Name,
                                 Column2Name,
                                 ...,
                                 na.rm = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapCohenD.data.frame")
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
  cohen_d <- calcCohenD(c1, c2)
  r <- list(cohen_d=cohen_d,
            test='Cohen_D_approx',
            Column1Name=Column1Name,
            Column2Name=Column2Name,
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_cohend', 'sigr_statistic')
  r
}


#' Wrap Cohen's D (effect size between groups).
#'
#' @param x numeric reference or control measurments
#' @param treatment numeric treatment or group-2 measurements
#' @param ... extra arguments (not used)
#' @param na.rm if TRUE remove NAs
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x = c(1,1,2,2,3,3,4,4),
#'                 y = c(1,2,3,4,5,6,7,7))
#' render(wrapCohenD(d$x, d$y))
#'
#' @export
wrapCohenD.numeric <- function(x,
                               treatment,
                               ...,
                               na.rm = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapCohenD.numeric")
  if(!is.numeric(x)) {
    stop('sigr::wrapCohenD expected class x to be numeric')
  }
  if(!is.numeric(treatment)) {
    stop("sigr::wrapCohenD expected class treatment to be numeric")
  }
  c1 <- x
  c2 <- treatment
  nNA <- sum(is.na(c1) | is.na(c2))
  if(na.rm) {
    goodPosns <- (!is.na(c1)) & (!is.na(c2))
    c1 <- c1[goodPosns]
    c2 <- c2[goodPosns]
  }
  n <- length(c1)
  cohen_d <- calcCohenD(c1, c2)
  r <- list(cohen_d=cohen_d,
            test='Cohen_D_approx',
            Column1Name='control',
            Column2Name='treatment',
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_cohend', 'sigr_statistic')
  r
}
