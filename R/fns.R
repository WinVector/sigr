
# set up per-format symbols
formats <- c('markdown','html','ascii','latex', 'docx', 'pandoc')
symCodes <- c('startB','endB','startI','endI','eq','lt',
              'RSq','chiSq')
syms <- matrix('',nrow=length(formats),ncol=length(symCodes),
               dimnames=list(formats,symCodes))
syms[,'eq'] <- '='
syms[,'lt'] <- '<'
syms[,'RSq'] <- 'R2'
syms[,'chiSq'] <- 'X2'
syms['html','startI'] <- '<i>'
syms['html','endI'] <- '</i>'
syms['html','startB'] <- '<b>'
syms['html','endB'] <- '</b>'
syms['html','lt'] <- '&lt;'
syms['html','RSq'] <- '<i>R<sup>2</sup></i>'
syms['html','chiSq'] <- '<i>&chi;<sup>2</sup></i>'
syms['latex','startI'] <- '\\textit{'
syms['latex','endI'] <- '}'
syms['latex','startB'] <- '\\textbf{'
syms['latex','endB'] <- '}'
syms['latex','lt'] <- ' $\\lt$ '
syms['latex','RSq'] <- '$R^2$'
syms['latex','chiSq'] <- '$\\chi^2$'
syms['markdown',] <- syms['html',]
syms['markdown','startI'] <- '*'
syms['markdown','endI'] <- '*'
syms['markdown','startB'] <- '**'
syms['markdown','endB'] <- '**'
syms['markdown','RSq'] <- '<i>R^2^</i>'
syms['markdown','chiSq'] <- '<i>&chi;^2^</i>'
syms['pandoc',] <- syms['markdown',]
syms['docx',] <- syms['markdown',]

#' Detect rendering format (using knitr).
#'
#'
#' @return rendering format
#'
#' @examples
#'
#' getRenderingFormat()
#'
#' @export
getRenderingFormat <- function() {
  fmt <- NULL
  if(requireNamespace("knitr",quietly=TRUE)) {
    fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  }
  if(is.null(fmt)) {
    fmt <- 'ascii'
  }
  fmt
}

#' Print a significance neatly
#'
#' @param sig numeric the significance value.
#' @param symbol the name of the value (e.g. p, t, ...).
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' cat(sigr::formatSignificance(1/300))
#'
#' @export
formatSignificance <- function(sig,symbol='p',
                              format,
                              pLargeCutoff=0.05,
                              pSmallCutoff=1.0e-5) {
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  pString <- paste0(fsyms['startI'],'p',fsyms['endI'])
  if(sig>=pLargeCutoff) {
    pString <- paste0(pString,fsyms['eq'],'n.s.')
  } else {
    if(sig<pSmallCutoff) {
      pString <- paste0(pString,fsyms['lt'],sprintf('%.3g',pSmallCutoff))
    } else {
      pString <- paste0(pString,fsyms['eq'],sprintf('%.3g',sig))
    }
  }
  #attr(pString,'format') <- format
  pString
}

#' @importFrom parallel parLapplyLB
NULL

# maybe run parallel
plapply <- function(workList,worker,parallelCluster) {
  if(is.null(parallelCluster) || (!requireNamespace("parallel",quietly=TRUE))) {
    res <- lapply(workList,worker)
  } else {
    res <- parallel::parLapplyLB(parallelCluster,workList,worker)
  }
  res
}


#' Calculate deviance.
#'
#' @param pred numeric predictions
#' @param y logical truth
#' @return deviance
#'
#' @examples
#'
#' sigr::calcDeviance(1:4,c(TRUE,FALSE,TRUE,TRUE))
#'
#' @export
calcDeviance <- function(pred,y) {
  eps <- 1.0e-6
  -2*sum(log(pmin(1-eps,pmax(eps,ifelse(y,pred,1-pred)))))
}


#' Calculate sum of squared error.
#'
#' @param pred numeric predictions
#' @param y numeric truth
#' @return sum of squared error
#'
#' @examples
#'
#' sigr::calcSSE(1:4,c(TRUE,FALSE,TRUE,TRUE))
#'
#' @export
calcSSE <- function(pred,y) {
  sum((y-pred)^2)
}



#' calculate AUC.
#'
#' Based on:
#'  http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues logical truth (not empty, same lenght as model predictions)
#' @return area under curve
#'
#' @examples
#'
#' sigr::calcAUC(1:4,c(TRUE,FALSE,TRUE,TRUE))
#'
#' @export
calcAUC <- function(modelPredictions,yValues) {
  ord <- order(modelPredictions, decreasing=TRUE)
  yValues <- yValues[ord]
  modelPredictions <- modelPredictions[ord]
  # FPR is the x-axis, TPR the y.
  dF <- data.frame(
    x=cumsum(!yValues)/max(1,sum(!yValues)), # FPR
    y=cumsum(yValues)/max(1,sum(yValues)),   # TPR
    score=modelPredictions
  )
  # each point shoudl be fully after a bunch of points or fully before.
  # no some points are in situations.
  dup <- c(dF$score[-1]>=head(dF$score,-1),FALSE)
  dF <- dF[!dup,]
  x <- dF$x
  y <- dF$y
  # extend out to 0-1 (some corner cases)
  if((length(x)<=0)||(min(x)>0)) {
    x <- c(0,x)
    y <- c(0,y)
  }
  if(max(x)<1) {
    x <- c(x,1)
    y <- c(y,1)
  }
  # sum areas of segments
  sum( ((y[-1]+head(y,-1))/2) * (x[-1]-head(x,-1)) )
}




