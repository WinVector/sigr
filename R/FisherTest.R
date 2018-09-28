

#' Format fisher.test (test of categorical independence).
#'
#' @param statistic wrapped Fisher test
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param statDigits integer number of digits to show in summaries.
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c('b','a','a','a','b','b','b'),
#'                 y=c('1','1','1','2','2','2','2'))
#' ft <- fisher.test(table(d))
#' wrapFisherTest(ft)
#'
#'
#' @export
render.sigr_fishertest <- function(statistic,
                          ...,
                          format,
                          statDigits=4,
                          sigDigits=4,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_fishertest")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  stat_format_str <- paste0('%.',statDigits,'g')
  ft <- statistic$ft
  pString <- render(wrapSignificance(ft$p.value,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
 formatStr <- paste0(fsyms['startB'],ft$method,fsyms['endB'],
                ': (',fsyms['startI'],'odds.ratio',fsyms['endI'],
                '=',sprintf(stat_format_str,ft$estimate),
                ', ',pString,').')
 formatStr
}

#' Wrap fisher.test (test of categorical independence).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapFisherTest.htest}}, and  \code{\link{wrapFisherTest.data.frame}}
#' @export
wrapFisherTest <- function(x,...) {
  UseMethod('wrapFisherTest')
}


#' Wrap fisher.test (test of categorical independence).
#'
#' @param x fisher.test result
#' @param ... extra arguments (not used)
#' @return wrapped test.
#'
#' @examples
#'
#' d <- data.frame(x=c('b','a','a','a','b','b','b'),
#'                 y=c('1','1','1','2','2','2','2'))
#' ft <- fisher.test(table(d))
#' wrapFisherTest(ft)
#'
#'
#' @export
wrapFisherTest.htest <- function(x,
                           ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFisherTest.htest")
  r <- list(ft=x,
       test='fisher.test')
  class(r) <- c('sigr_fishertest', 'sigr_statistic')
  r
}


#' Wrap fisher.test (test of categorical independence).
#'
#' @param x data.frame
#' @param Column1Name character column 1 name
#' @param Column2Name character column 2 name
#' @param ... extra arguments (not used)
#' @param na.rm logical, if TRUE remove NA values
#' @param workspace passed to \code{\link[stats]{fisher.test}}
#' @param hybrid passed to \code{\link[stats]{fisher.test}}
#' @param control passed to \code{\link[stats]{fisher.test}}
#' @param or passed to \code{\link[stats]{fisher.test}}
#' @param alternative passed to \code{\link[stats]{fisher.test}}
#' @param conf.int passed to \code{\link[stats]{fisher.test}}
#' @param conf.level passed to \code{\link[stats]{fisher.test}}
#' @param simulate.p.value passed to \code{\link[stats]{fisher.test}}
#' @param B passed to \code{\link[stats]{fisher.test}}
#' @return wrapped test.
#'
#' @examples
#'
#' d <- data.frame(x=c('b','a','a','a','b','b','b'),
#'                 y=c('1','1','1','2','2','2','2'))
#' wrapFisherTest(d, 'x', 'y')
#'
#'
#' @importFrom stats fisher.test
#'
#' @export
wrapFisherTest.data.frame <- function(x,
                                      Column1Name,
                                      Column2Name,
                                      ...,
                                      na.rm= FALSE,
                                      workspace = 200000, hybrid = FALSE,
                                      control = list(), or = 1, alternative = "two.sided",
                                      conf.int = TRUE, conf.level = 0.95,
                                      simulate.p.value = FALSE, B = 2000) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFisherTest.data.frame")
  c1 <- x[[Column1Name]]
  c2 <- x[[Column2Name]]
  nNA <- sum(is.na(c1) | is.na(c2))
  if(na.rm) {
    goodPosns <- (!is.na(c1)) & (!is.na(c2))
    c1 <- c1[goodPosns]
    c2 <- c2[goodPosns]
  }
  n <- length(c1)
  ft <- stats::fisher.test(x=table(c1,c2),
                           y=NULL,
                           workspace = workspace, hybrid = hybrid,
                           control = control, or = or, alternative = alternative,
                           conf.int = conf.int, conf.level = conf.level,
                           simulate.p.value = simulate.p.value, B = B)
  r <- list(ft=ft,
            test='fisher.test',
            Column1Name=Column1Name,
            Column2Name=Column2Name,
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_fishertest', 'sigr_statistic')
  r
}


#' Wrap fisher.test (test of categorical independence).
#'
#' @param x data.frame
#' @param ... extra arguments (not used)
#' @param workspace passed to \code{\link[stats]{fisher.test}}
#' @param hybrid passed to \code{\link[stats]{fisher.test}}
#' @param control passed to \code{\link[stats]{fisher.test}}
#' @param or passed to \code{\link[stats]{fisher.test}}
#' @param alternative passed to \code{\link[stats]{fisher.test}}
#' @param conf.int passed to \code{\link[stats]{fisher.test}}
#' @param conf.level passed to \code{\link[stats]{fisher.test}}
#' @param simulate.p.value passed to \code{\link[stats]{fisher.test}}
#' @param B passed to \code{\link[stats]{fisher.test}}
#' @return wrapped test.
#'
#' @examples
#'
#' d <- data.frame(x=c('b','a','a','a','b','b','b'),
#'                 y=c('1','1','1','2','2','2','2'))
#' t <- table(d)
#' wrapFisherTest(t)
#'
#'
#' @importFrom stats fisher.test
#'
#' @export
wrapFisherTest.table <- function(x,
                                ...,
                                 workspace = 200000, hybrid = FALSE,
                                 control = list(), or = 1, alternative = "two.sided",
                                 conf.int = TRUE, conf.level = 0.95,
                                 simulate.p.value = FALSE, B = 2000) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::wrapFisherTest.table")
  n <- sum(x)
  dimnames <- attr(t, 'dimnames')
  Column1Name <- names(dimnames)[[1]]
  Column2Name <- names(dimnames)[[2]]
  ft <- stats::fisher.test(x,
                           workspace = workspace, hybrid = hybrid,
                           control = control, or = or, alternative = alternative,
                           conf.int = conf.int, conf.level = conf.level,
                           simulate.p.value = simulate.p.value, B = B)
  r <- list(ft=ft,
            test='fisher.test',
            Column1Name=Column1Name,
            Column2Name=Column2Name,
            n=n,
            nNA=0)
  class(r) <- c('sigr_fishertest', 'sigr_statistic')
  r
}



