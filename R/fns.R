
# set up per-format symbols
formats <- c('markdown','html','ascii','latex', 'docx', 'pandoc',
             'markdown_github')
markdownSynonyms <- c('markdown_github')
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
for(fi in markdownSynonyms) {
  syms[fi,] <- syms['markdown',]
}

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
  if(!isTRUE(fmt %in% formats)) {
    fmt <- "ascii"
  }
  fmt
}


#' @importFrom parallel parLapplyLB
NULL

# maybe run parallel
plapply <- function(workList, worker, parallelCluster) {
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
#' @param na.rm logical, if TRUE remove NA values
#' @param eps numeric, smoothing term
#' @return deviance
#'
#' @examples
#'
#' sigr::calcDeviance(1:4,c(TRUE,FALSE,TRUE,TRUE))
#'
#' @export
calcDeviance <- function(pred, y,
                         na.rm= FALSE,
                         eps= 1.0e-6) {
  if(!is.numeric(pred)) {
    stop("sigr::calcDeviance pred must be numeric")
  }
  if(!is.logical(y)) {
    stop("sigr::calcDeviance y must be logical")
  }
  if((length(pred)==1)&&(length(y)>1)) {
    pred <- rep(pred, length(y))
  }
  if(length(pred)!=length(y)) {
    stop("sigr::calcDeviance must have length(pred)==length(y)")
  }
  if(na.rm) {
    goodPosns <- (!is.na(pred)) & (!is.na(y))
    pred <- pred[goodPosns]
    y <- y[goodPosns]
  }
  -2*sum(log(pmin(1-eps,pmax(eps,ifelse(y,pred,1-pred)))))
}


#' Calculate sum of squared error.
#'
#' @param pred numeric predictions
#' @param y numeric truth
#' @param na.rm logical, if TRUE remove NA values
#' @return sum of squared error
#'
#' @examples
#'
#' sigr::calcSSE(1:4,c(1,0,1,1))
#'
#' @export
calcSSE <- function(pred, y,
                    na.rm= FALSE) {
  if(!is.numeric(pred)) {
    stop("sigr::calcSSE pred must be numeric")
  }
  if(!is.numeric(y)) {
    stop("sigr::calcSSE y must be numeric")
  }
  if((length(pred)==1)&&(length(y)>1)) {
    pred <- rep(pred, length(y))
  }
  if(length(pred)!=length(y)) {
    stop("sigr::calcSSE must have length(pred)==length(y)")
  }
  if(na.rm) {
    goodPosns <- (!is.na(pred)) & (!is.na(y))
    pred <- pred[goodPosns]
    y <- y[goodPosns]
  }
  sum((y-pred)^2)
}


