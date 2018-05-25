
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
render <- function(statistic,
                   ...,
                   format,
                   statDigits=2,
                   sigDigits=2,
                   pLargeCutoff=0.05,
                   pSmallCutoff=1.0e-5) {
  UseMethod('render')
}

# get render args out of ...
get_render_args <- function(args, strict, stat, fnname) {
  res <- list(
    # format = "ascii", # if commented out trigger format special case
    statDigits=2,
    sigDigits=2,
    pLargeCutoff=0.05,
    pSmallCutoff=1.0e-5
  )
  for(ni in names(res)) {
    # first from stat, then override from args
    if(ni %in% names(stat)) {
      res[[ni]] <- stat[[ni]]
    }
    if(ni %in% names(args)) {
      res[[ni]] <- args[[ni]]
      args[[ni]] <- NULL
    }
  }
  # if format not set, look for in stat and then environment
  if(!("format" %in% names(res))) {
    # format is a special case if not set in initial res
    if("format" %in% names(args)) {
      res$format <- args$format
      args$format <- NULL
    } else if("format" %in% names(stat)) {
      res$format <- stat$format
    } else {
      res$format <- getRenderingFormat()
    }
  }
  if(!isTRUE(res$format %in% formats)) {
    res$format <- "ascii"
  }
  if(strict) {
    if(length(args)>0) {
      stop(paste("unexpected arguments", fnname))
    }
  }
  res
}


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
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_significance")
  sig <- statistic$significance
  symbol <- statistic$symbol
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
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


#' as.character
#'
#' @param x sigr wrapper to print
#' @param ... extra arguments for sigr::render
#' @return formatted string
#'
#' @examples
#'
#' as.character(wrapSignificance(1/300))
#'
#' @export
as.character.sigr_statistic <- function(x, ...) {
  render_args <- get_render_args(list(...), TRUE, x, "sigr::as.character.sigr_statistic")
  render(x,
         format = render_args$format,
         statDigits = render_args$statDigits,
         sigDigits = render_args$sigDigits,
         pLargeCutoff = render_args$pLargeCutoff,
         pSmallCutoff = render_args$pSmallCutoff)
}

#' Format
#'
#' @param x sigr wrapper to print
#' @param ... extra arguments for sigr::render
#' @return formatted string
#'
#' @examples
#'
#' format(wrapSignificance(1/300))
#'
#' @export
format.sigr_statistic <- function(x, ...) {
  render_args <- get_render_args(list(...), TRUE, x, "sigr::format.sigr_statistic")
  render(x,
         format = render_args$format,
         statDigits = render_args$statDigits,
         sigDigits = render_args$sigDigits,
         pLargeCutoff = render_args$pLargeCutoff,
         pSmallCutoff = render_args$pSmallCutoff)
}

#' Print
#'
#' @param x sigr wrapper to print
#' @param ... extra arguments for sigr::render and print
#' @return formatted string
#' @examples
#'
#' print(wrapSignificance(1/300))
#'
#' @export
print.sigr_statistic <- function(x, ...) {
  render_args <- get_render_args(list(...), FALSE, x, "sigr::print.sigr_statistic")
  print(render(x,
               format = render_args$format,
               statDigits = render_args$statDigits,
               sigDigits = render_args$sigDigits,
               pLargeCutoff = render_args$pLargeCutoff,
               pSmallCutoff = render_args$pSmallCutoff),
        ...)
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
