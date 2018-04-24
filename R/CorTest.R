
# TODO: S3 genericize wrapping.

#' Format cor.test (test of liner correlation).
#'
#' @param statistic wrapped cor.test.
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
                          statDigits=2,
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
    format <- "ascii"
  }
  fsyms <- syms[format,]
  stat_format_str <- paste0('%.',statDigits,'g')
  ct <- statistic$ct
  pString <- render(wrapSignificance(ct$p.value,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],ct$method,fsyms['endB'],
                ': (',fsyms['startI'],'r',fsyms['endI'],
                '=',sprintf(stat_format_str,ct$estimate),
                ', ',pString,').')
  formatStr
}


#' Wrap cor.test (test of liner correlation).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapCorTest.htest}}, and  \code{\link{wrapCorTest.data.frame}}
#' @export
wrapCorTest <- function(x,...) UseMethod('wrapCorTest')



#' Wrap cor.test (test of liner correlation).
#'
#' @param x cor.test result
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
wrapCorTest.htest <- function(x,
                          ...) {
  if(length(list(...))) {
    stop('wrapCorTest.htest extra arguments')
  }
  r <- list(ct=x,
            test='cor.test')
  class(r) <- c('sigr_cortest', 'sigr_statistic')
  r
}

#' Wrap cor.test (test of liner correlation).
#'
#' @param x data.frame
#' @param Column1Name character column 1 name
#' @param Column2Name character column 2 name
#' @param ... extra arguments passed to cor.test
#' @param alternative passed to \code{\link[stats]{cor.test}}
#' @param method passed to \code{\link[stats]{cor.test}}
#' @param exact passed to \code{\link[stats]{cor.test}}
#' @param conf.level passed to \code{\link[stats]{cor.test}}
#' @param continuity passed to \code{\link[stats]{cor.test}}
#' @param na.rm logical, if TRUE remove NA values
#' @return wrapped stat
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' wrapCorTest(d,'x','y')
#'
#'
#' @importFrom stats cor.test
#'
#' @export
wrapCorTest.data.frame <- function(x,
                                   Column1Name,
                                   Column2Name,
                                   ...,
                                   alternative = c("two.sided", "less", "greater"),
                                   method = c("pearson", "kendall", "spearman"),
                                   exact = NULL, conf.level = 0.95, continuity = FALSE,
                                   na.rm= FALSE) {
  if(!is.numeric(x[[Column1Name]])) {
    stop("wrapr::wrapCorTest.data.frame column 1 must be numeric")
  }
  if(!is.numeric(x[[Column2Name]])) {
    stop("wrapr::wrapCorTest.data.frame column 2 must be numeric")
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
  ct <- stats::cor.test(x=c1,y=c2,
                        alternative = alternative,
                        method = method,
                        exact = exact, conf.level = conf.level, continuity = continuity,
                        ...)
  r <- list(ct=ct,
            test='cor.test',
            Column1Name=Column1Name,
            Column2Name=Column2Name,
            n=n,
            nNA=nNA)
  class(r) <- c('sigr_cortest', 'sigr_statistic')
  r
}
