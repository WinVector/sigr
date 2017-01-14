
mkPermWorker <- function(modelValues,yValues,scoreFn) {
  force(yValues)
  force(modelValues)
  force(scoreFn)
  n <- length(modelValues)
  if(is.null(yValues)) {
    yValues <- logical(n)
  }
  function(x) {
    perm <- sample.int(n,n,replace=FALSE)
    score <- scoreFn(modelValues,yValues[perm])
    score
  }
}


#' Format an empirical test (quality of categorical prediction)
#'
#' @param statistic wrapped T-test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_permtest <- function(statistic,
                                  ...,
                                  format,
                                  sigDigits=2,
                                  pLargeCutoff=0.05,
                                  pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_emptest unexpected arguments")
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  pString <- render(wrapSignificance(statistic$pValue,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],'Studentized permutation test',fsyms['endB'],
                      ': ',statistic$test,', ',
                      ' summary: ',pString)
  formatStr
}


#' Empirical permuation test of significance of scoreFn(modelValues,yValues) >= scoreFn(modelValues,perm(yValues)).
#'
#' Treat permutaiton re-samples as similar to bootstrap replications.
#'
#' @param modelValues numeric array of predictions.
#' @param yValues numeric/logical array of outcomes, depedendent, or truth values
#' @param scoreFn function with signature scoreFn(modelValues,yValues) returning scalar numeric score.
#' @param ... not used, forces later arguments to be bound by name
#' @param returnScores logical if TRUE return detailed permutedScores
#' @param nRep integer number of repititions to perform
#' @param parallelCluster optional snow-style parallel cluster.
#' @return summaries
#'
#' @examples
#'
#' set.seed(25325)
#' y <- 1:5
#' m <- c(1,1,2,2,2)
#' cor.test(m,y,alternative='greater')
#' f <- function(modelValues,yValues) cor(modelValues,yValues)
#' permutationScoreModel(m,y,f)
#'
#' @export
#'
permutationScoreModel <- function(modelValues,yValues,
                                  scoreFn,
                                  ...,
                                  returnScores=FALSE,
                                  nRep=100,
                                  parallelCluster=NULL) {
  if(length(list(...))>0) {
    stop('permutationScoreModel unexpected extra arguments')
  }
  observedScore <- scoreFn(modelValues,yValues)
  permWorker <- mkPermWorker(modelValues=modelValues,
                             yValues=yValues,
                             scoreFn=scoreFn)
  permutedScores <- plapply(1:nRep,permWorker,parallelCluster)
  permutedScores <- as.numeric(permutedScores)
  meanv <- mean(permutedScores)
  sdv <- sqrt(sum((permutedScores-meanv)^2)/(length(permutedScores)-1))
  z <- (observedScore-meanv)/sdv
  df = length(permutedScores)-1
  pValue <- stats::pt(z,df=df,lower.tail=FALSE)
  pFreq <- sum(permutedScores>=observedScore)/length(permutedScores)
  ret <- list(fnName='permutationScoreModel',
              test='is observed score greater than permuted score',
              observedScore=observedScore,
              df=df,
              z=z,
              mean=meanv,
              sd=sdv,
              pValue=pValue,
              pFreq=pFreq)
  if(returnScores) {
    ret$permutedScores <- permutedScores
  }
  class(ret) <- c('sigr_permtest', 'sigr_statistic')
  ret
}




mkResampleWorker <- function(modelValues,
                              yValues,
                              scoreFn) {
  force(yValues)
  force(modelValues)
  force(scoreFn)
  n <- length(modelValues)
  if(is.null(yValues)) {
    yValues <- logical(n)
  }
  function(x) {
    samp <- sample.int(n,n,replace=TRUE)
    score <- scoreFn(modelValues[samp],yValues[samp])
    score
  }
}

#' Studentized bootstrap variance estimate for scoreFn(yValues,modelValues).
#'
#' @param modelValues numeric array of predictions (model to test).
#' @param yValues numeric/logical array of outcomes, depedendent, or truth values
#' @param scoreFn function with signature scoreFn(modelValues,yValues) returning scalar numeric score.
#' @param ... not used, forces later arguments to be bound by name
#' @param returnScores logical if TRUE return detailed resampledScores
#' @param nRep integer number of repititions to perform
#' @param parallelCluster optional snow-style parallel cluster.
#' @return summaries
#'
#' @examples
#'
#' set.seed(25325)
#' y <- 1:5
#' m1 <- c(1,1,2,2,2)
#' cor.test(m1,y,alternative='greater')
#' f <- function(modelValues,yValues) {
#'  if((sd(modelValues)<=0)||(sd(yValues)<=0)) {
#'    return(0)
#'  }
#'  cor(modelValues,yValues)
#' }
#' s <- sigr::resampleScoreModel(m1,y,f)
#' print(s)
#' z <- (s$observedScore-0)/s$sd # should check size of z relative to bias!
#' pValue <- pt(z,df=length(y)-2,lower.tail=FALSE)
#' pValue
#'
#'
#' @export
#'
#
resampleScoreModel <- function(modelValues,
                               yValues,
                               scoreFn,
                               ...,
                               returnScores=FALSE,
                               nRep=100,
                               parallelCluster=NULL) {
  if(length(list(...))>0) {
    stop('resampleScoreModelPair unexpected extra arguments')
  }
  observedScore <- scoreFn(modelValues,yValues)
  resampleWorker <- mkResampleWorker(modelValues=modelValues,
                                          yValues=yValues,
                                          scoreFn=scoreFn)
  resampledScores <- plapply(1:nRep,resampleWorker,parallelCluster)
  resampledScores <- as.numeric(resampledScores)
  meanv <- mean(resampledScores)
  sdv <- sqrt(sum((resampledScores-meanv)^2)/(length(resampledScores)-1))
  ret <- list(fnName='resampleScoreModel',
              observedScore=observedScore,
              bias=meanv-observedScore,
              sd=sdv)
  if(returnScores) {
    ret$resampledScores <- resampledScores
  }
  ret
}






mkResampleDiffWorker <- function(model1Values,
                                 model2Values,
                                 yValues,
                                 scoreFn) {
  force(yValues)
  force(model1Values)
  force(model2Values)
  force(scoreFn)
  n <- length(model1Values)
  if(is.null(yValues)) {
    yValues <- logical(n)
  }
  if(is.null(model2Values)) {
    model2Values <- numeric(n)
  }
  function(x) {
    samp <- sample.int(n,n,replace=TRUE)
    score1 <- scoreFn(model1Values[samp],yValues[samp])
    score2 <- scoreFn(model2Values[samp],yValues[samp])
    list(score1=score1,score2=score2,diff=score1-score2)
  }
}

# convert a non-empty list of single row data frames with the identical non-empty
# numeric column structure into a data frame (without bringing in dplyr/tidyr
# dependency, and faster than do.call(rbind,rows)).
listToDataFrame <- function(rows) {
  n <- length(rows)
  cols <- names(rows[[1]])
  d <- data.frame(x=numeric(n))
  colnames(d) <- cols[[1]]
  for(ci in cols) {
    d[[ci]] <- vapply(rows,function(ri) { ri[[ci]] },numeric(1))
  }
  d
}


#' Format an empirical test (quality of categorical prediction)
#'
#' @param statistic wrapped T-test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_emptest <- function(statistic,
                                ...,
                                format,
                                sigDigits=2,
                                pLargeCutoff=0.05,
                                pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_emptest unexpected arguments")
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  pString <- render(wrapSignificance(statistic$eValue,
                                     symbol='e'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'],'Studentized empirical test',fsyms['endB'],
                      ': ',statistic$test,', ',
                      ' summary: ',pString)
  formatStr
}


#' Studentized estimate of how often a difference is below zero.
#'
#' @param resampledDiffs numeric vector resampled observations
#' @return estimated probbility of seeing a re-sampled difference below zero.
#'
#' @examples
#'
#' set.seed(2352)
#' resampledDiffs <- rnorm(10)+1
#' estimateDifferenceZeroCrossing(resampledDiffs)
#'
#' @export
#'
estimateDifferenceZeroCrossing <- function(resampledDiffs) {
  meanv <- mean(resampledDiffs)
  sdv <- sqrt(sum((resampledDiffs-meanv)^2)/(length(resampledDiffs)-1))
  z <- meanv/sdv
  df = length(resampledDiffs)-1
  eFreq <- sum(resampledDiffs<=0)/length(resampledDiffs)
  eValue <- stats::pt(z,df=df,lower.tail=FALSE)
  ret <- list(fnName='estimateDifferenceZeroCrossing',
              test="is difference greater than zero on re-samples",
              z=z,
              meanv=meanv,
              sd=sdv,
              eValue=eValue,
              eFreq=eFreq)
  class(ret) <- c('sigr_emptest', 'sigr_statistic')
  ret
}

#' Studentized bootstrap test of strength of scoreFn(yValues,model1Values) > scoreFn(yValues,model1Values).
#'
#' True confidence intervals are harder to get right (see
#' "An Introduction to the Bootstrap", Bradely Efron,
#' and Robert J. Tibshirani, Chapman & Hall/CRC, 1993.),
#' but we will settle for simple p-value estimates.
#'
#' @param model1Values numeric array of predictions (model to test).
#' @param model2Values numeric array of predictions (reference model).
#' @param yValues numeric/logical array of outcomes, depedendent, or truth values
#' @param scoreFn function with signature scoreFn(modelValues,yValues) returning scalar numeric score.
#' @param ... not used, forces later arguments to be bound by name
#' @param returnScores logical if TRUE return detailed resampledScores
#' @param nRep integer number of repititions to perform
#' @param parallelCluster optional snow-style parallel cluster.
#' @return summaries
#'
#' @examples
#'
#' set.seed(25325)
#' y <- 1:5
#' m1 <- c(1,1,2,2,2)
#' m2 <- c(1,1,1,1,2)
#' cor(m1,y)
#' cor(m2,y)
#' f <- function(modelValues,yValues) {
#'   if((sd(modelValues)<=0)||(sd(yValues)<=0)) {
#'     return(0)
#'   }
#'   cor(modelValues,yValues)
#' }
#' resampleScoreModelPair(m1,m2,y,f)
#'
#' @export
#'
#
resampleScoreModelPair <- function(model1Values,
                                   model2Values,
                                   yValues,
                                   scoreFn,
                                   ...,
                                   returnScores=FALSE,
                                   nRep=100,
                                   parallelCluster=NULL) {
  if(length(list(...))>0) {
    stop('resampleScoreModelPair unexpected extra arguments')
  }
  observedScore1 <- scoreFn(model1Values,yValues)
  observedScore2 <- scoreFn(model2Values,yValues)
  resampleWorker <- mkResampleDiffWorker(model1Values=model1Values,
                                         model2Values=model2Values,
                                         yValues=yValues,
                                         scoreFn=scoreFn)
  detailedResampledScores <- plapply(1:nRep,resampleWorker,parallelCluster)
  observedDiff <- observedScore1-observedScore2
  detailedResampledScores <- listToDataFrame(detailedResampledScores)
  resampledDiffs <- detailedResampledScores$diff
  ret <- estimateDifferenceZeroCrossing(resampledDiffs)
  ret$observedScore1 <- observedScore1
  ret$observedScore2 <- observedScore2
  ret$observedDiff <- observedDiff
  if(returnScores) {
    ret$detailedResampledScores <- detailedResampledScores
  }
  ret
}

