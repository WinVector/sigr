

#' Format fisher.test.
#'
#' @param ft cor.test result
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c('b','a','a','a','b','b','b'),
#'                 y=c('1','1','1','2','2','2','2'))
#' ft <- fisher.test(table(d))
#' sigr::formatFisherTest(ft)$formatStr
#'
#'
#' @export
formatFisherTest <- function(ft,
                          ...,
                          format,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5) {
  if(length(list(...))) {
    stop('formatFisherTest extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  pString <- formatSignificance(ft$p.value,
                                symbol='p',
                                format=format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  list(ft=ft,
       test='fisher.test',
       formatStr=
         paste0(fsyms['startB'],ft$method,fsyms['endB'],
                ': (',fsyms['startI'],'odds.ratio',fsyms['endI'],
                '=',sprintf('%.2g',ft$estimate),
                ', ',pString,').'))
}
