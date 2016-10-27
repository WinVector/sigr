
#' Format t.test.
#'
#' @param tt t.test result
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
#' tt <- t.test(d$x,d$y)
#' sigr::formatTTest(tt,pLargeCutoff=1,format='ascii')$formatStr
#' # confirm not rescaling, as a correlation test would
#' sigr::formatTTest(t.test(d$x,2*d$y),pLargeCutoff=1,format='ascii')$formatStr
#'
#' @export
formatTTest <- function(tt,
                          ...,
                          format,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5) {
  if(length(list(...))) {
    stop('formatTTest extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  if(!'htest' %in% class(tt)) {
    stop('formatTTest expected class htest')
  }
  fsyms <- syms[format,]
  pString <- formatSignificance(tt$p.value,
                                symbol='p',
                                format=format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  list(tt=tt,
       test='t.test',
       formatStr=
         paste0(fsyms['startB'],tt$method,fsyms['endB'],
                ', ',tt$alternative,
                ': (',fsyms['startI'],'t',fsyms['endI'],
                '=',paste(sprintf('%.2g',tt$statistic),collapse=','),
                ', ',fsyms['startI'],'df',fsyms['endI'],'=',
                sprintf('%.2g',tt$parameter),', ',
                pString,').'))
}
