#' Format cor.test.
#'
#' @param ct cor.test result
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string and fields
#'
#' @examples
#'
#' d <- data.frame(x=c(1,2,3,4,5,6,7,7),
#'                 y=c(1,1,2,2,3,3,4,4))
#' ct <- cor.test(d$x,d$y)
#' sigr::formatCorTest(ct)$formatStr
#'
#'
#' @export
formatCorTest <- function(ct,
                           ...,
                           format,
                           pLargeCutoff=0.05,
                           pSmallCutoff=1.0e-5) {
  if(length(list(...))) {
    stop('formatCorTest extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  pString <- formatSignificance(ct$p.value,'p', format,
                                pLargeCutoff,
                                pSmallCutoff)
  list(ct=ct,
       test='cor.test',
       formatStr=
         paste0(fsyms['startB'],ct$method,fsyms['endB'],
                ': (',fsyms['startI'],'r',fsyms['endI'],
                '=',sprintf('%.2g',ct$estimate),
                ', ',pString,').'))
}
