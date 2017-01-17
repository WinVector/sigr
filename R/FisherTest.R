
# TODO: S3 generizize wrapping

#' Format fisher.test (test of categorial indendence).
#'
#' @param statistic wrapped Fisher test
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param sigDigits integer number of digits to show
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
                          sigDigits=2,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5) {
  if(length(list(...))) {
    stop('render.sigr_fishertest extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  ft <- statistic$ft
  pString <- render(wrapSignificance(ft$p.value,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
 formatStr <- paste0(fsyms['startB'],ft$method,fsyms['endB'],
                ': (',fsyms['startI'],'odds.ratio',fsyms['endI'],
                '=',sprintf('%.2g',ft$estimate),
                ', ',pString,').')
 formatStr
}

#' Wrap fisher.test (test of categorial indendence).
#'
#' @param x numeric, data.frame or test.
#' @param ... extra arguments
#'
#' @seealso \code{\link{wrapFisherTest.htest}}, and  \code{\link{wrapFisherTest.data.frame}}
#' @export
wrapFisherTest <- function(x,...) UseMethod('wrapFisherTest')


#' Wrap fisher.test (test of categorial indendence).
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
  if(length(list(...))) {
    stop('wrapFisherTest.htest extra arguments')
  }
  r <- list(ft=x,
       test='fisher.test')
  class(r) <- c('sigr_fishertest', 'sigr_statistic')
  r
}


#' Wrap fisher.test (test of categorial indendence).
#'
#' @param x data.frame
#' @param Column1Name character column 1 name
#' @param Column2Name character column 2 name
#' @param ... extra arguments
#' @return wrapped test.
#'
#' @examples
#'
#' d <- data.frame(x=c('b','a','a','a','b','b','b'),
#'                 y=c('1','1','1','2','2','2','2'))
#' wrapFisherTest(d,'x','y')
#'
#'
#' @importFrom stats fisher.test
#'
#' @export
wrapFisherTest.data.frame <- function(x,
                                      Column1Name,
                                      Column2Name,
                                 ...) {
  if(length(list(...))) {
    stop('wrapFisherTest.data.frame extra arguments')
  }
  ft <- fisher.test(table(x[[Column1Name]],x[[Column2Name]]),...)
  r <- list(ft=ft,
            test='fisher.test',
            Column1Name=Column1Name,
            Column2Name=Column2Name)
  class(r) <- c('sigr_fishertest', 'sigr_statistic')
  r
}


