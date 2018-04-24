
#' Format summary roughly in "APA Style" ( American Psychological Association ).
#'
#' @param statistic sigr summary statistic
#' @param ... extra arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param statDigits integer number of digits to show in summaries.
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @seealso \code{\link{render.sigr_significance}}, \code{\link{render.sigr_ftest}}
#' @export
render <- function(statistic,...,
                   format,
                   statDigits=2,
                   sigDigits=2,
                   pLargeCutoff=0.05,
                   pSmallCutoff=1.0e-5) UseMethod('render')


#' Format a significance
#'
#' @param statistic wrapped significance
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param statDigits integer number of digits to show in summaries (not used in significance reports).
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' cat(render(wrapSignificance(1/300),format='html'))
#'
#' @export
render.sigr_significance <- function(statistic,
                          ...,
                          format,
                          statDigits=2,
                          sigDigits=2,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_significance unexpected arguments")
  }
  sig <- statistic$significance
  symbol <- statistic$symbol
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  pString <- paste0(fsyms['startI'],symbol,fsyms['endI'])
  if(is.na(sig)||is.infinite(sig)||is.nan(sig)) {
    pString <- paste0(pString,fsyms['eq'],'n.a.')
  } else if(sig>=pLargeCutoff) {
    pString <- paste0(pString,fsyms['eq'],'n.s.')
  } else {
    sigStr <- paste0('%.',sigDigits,'g')
    if(sig<pSmallCutoff) {
      pString <- paste0(pString,fsyms['lt'],sprintf(sigStr,pSmallCutoff))
    } else {
      pString <- paste0(pString,fsyms['eq'],sprintf(sigStr,sig))
    }
  }
  #attr(pString,'format') <- format
  pString
}

#' Format
#'
#' @param x sigr wrapper to print
#' @param ... extra arguments (not used)
#' @return formatted string
#' @examples
#'
#' format(wrapSignificance(1/300))
#'
#' @export
format.sigr_statistic <- function(x, ...) {
  render(x,format='ascii')
}

#' Print
#'
#' @param x sigr wrapper to print
#' @param ... extra arguments
#' @return formatted string
#' @examples
#'
#' print(wrapSignificance(1/300))
#'
#' @export
print.sigr_statistic <- function(x, ...) {
  print(render(x,format='ascii'), ...)
}

#' Wrap a significance
#'
#' @param significance numeric the significance value.
#' @param symbol the name of the value (e.g. "p", "t", ...).
#' @return wrapped significance
#'
#' @examples
#'
#' wrapSignificance(1/300)
#'
#' @export
wrapSignificance <- function(significance,
                               symbol='p') {
  r <- list(significance=significance,
       symbol=symbol)
  class(r) <- c('sigr_significance', 'sigr_statistic')
  r
}
