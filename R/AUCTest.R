
#' @importFrom stats pnorm pt sd
NULL


#' Format an AUC-test (quality of a probability score)
#'
#' @param statistic wrapped AUC test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_aucpermtest <- function(statistic,
                              ...,
                              format,
                              sigDigits=2,
                              pLargeCutoff=0.05,
                              pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_auctest unexpected arguments")
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
  scoreString <- sprintf('%.2g',statistic$eScore$observedScore)
  formatStr <- paste0(fsyms['startB'],'AUC test alt. hyp. AUC>AUC(permuted)',fsyms['endB'],
                      ': (',fsyms['startI'],'AUC',fsyms['endI'],
                      '=',scoreString,
                      ', ',fsyms['startI'],'s.d.',fsyms['endI'],'=',sprintf('%.2g',statistic$eScore$sd),
                      ', ',pString,').')
  formatStr
}

#' Perform AUC permutation test.
#'
#' Estimate signficance of AUC by permuation test.
#'
#' @param d data.frame
#' @param modelName character model column name
#' @param yName character outcome column name
#' @param yTarget target to match to y
#' @param ... extra arguments (not used)
#' @param returnScores logical if TRUE return detailed permutedScores
#' @param nrep number of permutation repititions to estimate p values.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return AUC statistic
#'
#' @examples
#'
#' set.seed(25325)
#' d <- data.frame(x1=c(1,2,3,4,5,6,7,7),
#'                 y=c(FALSE,TRUE,FALSE,FALSE,
#'                     TRUE,TRUE,FALSE,TRUE))
#' permTestAUC(d,'x1','y',TRUE)
#'
#'
#' @export
permTestAUC <- function(d,
                      modelName,
                      yName,
                      yTarget,
                      ...,
                      returnScores=FALSE,
                      nrep=100,
                      parallelCluster=NULL) {
  if(length(list(...))) {
    stop('permTestAUC extra arguments')
  }
  eScore <- permutationScoreModel(modelValues=d[[modelName]],
                                  yValues=d[[yName]]==yTarget,
                                  scoreFn=calcAUC,
                                  returnScores=returnScores,
                                  nRep=nrep,
                                  parallelCluster=parallelCluster)
  pValue = eScore$pValue
  r <- list(pValue=pValue,
            test='AUC permutation test',
            eScore=eScore)
  class(r) <- c('sigr_aucpermtest', 'sigr_statistic')
  r
}





#' Format an AUC-test (quality of a probability score)
#'
#' @param statistic wrapped AUC test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_aucresamptest <- function(statistic,
                                    ...,
                                    format,
                                    sigDigits=2,
                                    pLargeCutoff=0.05,
                                    pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_aucresamptest unexpected arguments")
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  eScore <- statistic$eScore
  pString <- render(wrapSignificance(statistic$pValue,
                                     symbol='p'),
                    format=format,
                    sigDigits=sigDigits,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  scoreString <- sprintf('%.2g',eScore$observedScore)
  formatStr <- paste0(fsyms['startB'],'AUC test alt. hyp. AUC>0.5',fsyms['endB'],
                ': (',fsyms['startI'],'AUC',fsyms['endI'],
                '=',scoreString,
                ', ',fsyms['startI'],'s.d.',fsyms['endI'],'=',sprintf('%.2g',eScore$sd),
                ', ',pString,').')
  formatStr
}

#' Wrap AUC resampling test results.
#'
#' Estimate signficance of AUC by resampling test.
#'
#' @param d data.frame
#' @param modelName character model column name
#' @param yName character outcome column name
#' @param yTarget target to match to y
#' @param ... extra arguments (not used)
#' @param returnScores logical if TRUE return detailed resampledScores.
#' @param nrep number of permutation repititions to estimate p values.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @return AUC statistic
#'
#' @examples
#'
#' set.seed(25325)
#' d <- data.frame(x1=c(1,2,3,4,5,6,7,7),
#'                 y=c(FALSE,TRUE,FALSE,FALSE,
#'                     TRUE,TRUE,FALSE,TRUE))
#' resampleTestAUC(d,'x1','y',TRUE)
#'
#'
#' @export
resampleTestAUC <- function(d,
                      modelName,
                      yName,
                      yTarget,
                      ...,
                      returnScores=FALSE,
                      nrep=100,
                      parallelCluster=NULL) {
  if(length(list(...))) {
    stop('formatAUCresample extra arguments')
  }
  eScore <- resampleScoreModel(modelValues=d[[modelName]],
                               yValues=d[[yName]]==yTarget,
                               scoreFn=calcAUC,
                               returnScores=returnScores,
                               nRep=nrep,
                               parallelCluster=parallelCluster)
  z <- (eScore$observedScore-0.5)/eScore$sd # should check size of z relative to bias!
  pValue <- stats::pt(z,df=nrow(d)-2,lower.tail=FALSE)
  r <- list(pValue=pValue,
            test='AUC resampling test',
            eScore=eScore,
            z=z,
            pValue=pValue)
  class(r) <- c('sigr_aucresamptest', 'sigr_statistic')
  r
}






#' Format an AUC-test (quality of a probability score)
#'
#' @param statistic wrapped AUC test
#' @param ... not used, force use of named binding for later arguments
#' @param format if set the format to return ("html", "latex", "markdown", "ascii")
#' @param sigDigits integer number of digits to show
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#'
#' @export
render.sigr_aucpairtest <- function(statistic,
                                    ...,
                                    format,
                                    sigDigits=2,
                                    pLargeCutoff=0.05,
                                    pSmallCutoff=1.0e-5) {
  if(length(list(...))>0) {
    stop("render.sigr_aucpairtest unexpected arguments")
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  eScore <- statistic$eScore
  eString <- render(wrapSignificance(statistic$eValue,
                                                 symbol='e'),
                                format=format,
                                sigDigits=sigDigits,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  scoreString1 <- sprintf('%.2g',eScore$observedScore1)
  scoreString2 <- sprintf('%.2g',eScore$observedScore2)
  scoreString <- paste0(scoreString1,';',scoreString2)
  formatStr <- paste0(fsyms['startB'],'AUC test resampled AUC1>AUC2',fsyms['endB'],
                ': (',fsyms['startI'],'AUCs',fsyms['endI'],
                '=',scoreString,
                ', ',fsyms['startI'],'s.d.',fsyms['endI'],'=',sprintf('%.2g',eScore$sd),
                ', ',eString,').')
  formatStr
}

#' Test AUC pair results.
#'
#' Estimate signficance of difference in two AUCs by resampling.
#'
#' @param d data.frame
#' @param model1Name character model 1 column name
#' @param model2Name character model 2 column name
#' @param yName character outcome column name
#' @param yTarget target to match to y
#' @param ... extra arguments (not used)
#' @param returnScores logical if TRUE return detailed resampledScores
#' @param nrep number of re-sample repetition to estimate p value.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return AUC pair test
#'
#' @examples
#'
#' set.seed(25325)
#' d <- data.frame(x1=c(1,2,3,4,5,6,7,7),
#'                 x2=1,
#'                 y=c(FALSE,TRUE,FALSE,FALSE,
#'                     TRUE,TRUE,FALSE,TRUE))
#' testAUCpair(d,'x1','x2','y',TRUE)
#'
#'
#' @export
testAUCpair <- function(d,
                          model1Name,
                          model2Name,
                          yName,
                          yTarget,
                          ...,
                          returnScores=FALSE,
                          nrep=100,
                          parallelCluster=NULL) {
  if(length(list(...))) {
    stop('formatAUC extra arguments')
  }
  eScore <- resampleScoreModelPair(model1Values=d[[model1Name]],
                                   model2Values=d[[model2Name]],
                                   yValues=d[[yName]]==yTarget,
                                   scoreFn=calcAUC,
                                   returnScores=returnScores,
                                   nRep=nrep,
                                   parallelCluster=parallelCluster)
  eValue <- eScore$eValue
  r <- list(eValue=eValue,
       test='AUC results',
       eScore=eScore)
  class(r) <- c('sigr_aucpairtest', 'sigr_statistic')
  r
}
