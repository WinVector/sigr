
#' @importFrom stats pnorm pt sd
NULL



#' Format AUC permutation test results.
#'
#' Estimate signficance of AUC by permuation test.
#'
#' @param d data.frame
#' @param modelName character model column name
#' @param yName character outcome column name
#' @param yTarget target to match to y
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param returnScores logical if TRUE return detailed permutedScores
#' @param nrep number of permutation repititions to estimate p values.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return formatted string and fields
#'
#' @examples
#'
#' set.seed(25325)
#' d <- data.frame(x1=c(1,2,3,4,5,6,7,7),
#'                 y=c(FALSE,TRUE,FALSE,FALSE,
#'                     TRUE,TRUE,FALSE,TRUE))
#' sigr::formatAUC(d,'x1','y',TRUE)$formatStr
#'
#'
#' @export
formatAUC <- function(d,
                      modelName,
                      yName,
                      yTarget,
                      ...,
                      format,
                      returnScores=FALSE,
                      nrep=100,
                      pLargeCutoff=0.05,
                      pSmallCutoff=1.0e-5,
                      parallelCluster=NULL) {
  if(length(list(...))) {
    stop('formatAUC extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  eScore <- permutationScoreModel(modelValues=d[[modelName]],
                                  yValues=d[[yName]]==yTarget,
                                  scoreFn=calcAUC,
                                  returnScores=returnScores,
                                  nRep=nrep,
                                  parallelCluster=parallelCluster)
  pValue = eScore$pValue
  pString <- formatSignificance(pValue,
                                symbol='p',
                                format=format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  scoreString <- sprintf('%.2g',eScore$observedScore)
  list(pValue=pValue,
       test='AUC differnce test',
       eScore=eScore,
       scoreString=scoreString,
       pString=pString,
       formatStr=
         paste0(fsyms['startB'],'AUC test alt. hyp. AUC>AUC(permuted)',fsyms['endB'],
                ': (',fsyms['startI'],'AUC',fsyms['endI'],
                '=',scoreString,
                ', ',fsyms['startI'],'s.d.',fsyms['endI'],'=',sprintf('%.2g',eScore$sd),
                ', ',pString,').'))
}


#' Format AUC resampling test results.
#'
#' Estimate signficance of AUC by permuation test.
#'
#' @param d data.frame
#' @param modelName character model column name
#' @param yName character outcome column name
#' @param yTarget target to match to y
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param returnScores logical if TRUE return detailed resampledScores.
#' @param nrep number of permutation repititions to estimate p values.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @return formatted string and fields
#'
#' @examples
#'
#' set.seed(25325)
#' d <- data.frame(x1=c(1,2,3,4,5,6,7,7),
#'                 y=c(FALSE,TRUE,FALSE,FALSE,
#'                     TRUE,TRUE,FALSE,TRUE))
#' sigr::formatAUCresample(d,'x1','y',TRUE)$formatStr
#'
#'
#' @export
formatAUCresample <- function(d,
                      modelName,
                      yName,
                      yTarget,
                      ...,
                      format,
                      returnScores=FALSE,
                      nrep=100,
                      pLargeCutoff=2.0,
                      pSmallCutoff=1.0e-5,
                      parallelCluster=NULL) {
  if(length(list(...))) {
    stop('formatAUCresample extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  eScore <- resampleScoreModel(modelValues=d[[modelName]],
                               yValues=d[[yName]]==yTarget,
                               scoreFn=calcAUC,
                               returnScores=returnScores,
                               nRep=nrep,
                               parallelCluster=parallelCluster)
  z <- (eScore$observedScore-0.5)/eScore$sd # should check size of z relative to bias!
  pValue <- stats::pt(z,df=nrow(d)-2,lower.tail=FALSE)
  pString <- formatSignificance(pValue,
                                symbol='p',
                                format=format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  scoreString <- sprintf('%.2g',eScore$observedScore)
  list(pValue=pValue,
       test='AUC resampling test',
       eScore=eScore,
       scoreString=scoreString,
       z=z,
       pValue=pValue,
       pString=pString,
       formatStr=
         paste0(fsyms['startB'],'AUC test alt. hyp. AUC>0.5',fsyms['endB'],
                ': (',fsyms['startI'],'AUC',fsyms['endI'],
                '=',scoreString,
                ', ',fsyms['startI'],'s.d.',fsyms['endI'],'=',sprintf('%.2g',eScore$sd),
                ', ',pString,').'))
}





#' Format AUC comparison results.
#'
#' Estimate signficance of difference in two AUCs by resampling.
#'
#' @param d data.frame
#' @param model1Name character model 1 column name
#' @param model2Name character model 2 column name
#' @param yName character outcome column name
#' @param yTarget target to match to y
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param returnScores logical if TRUE return detailed resampledScores
#' @param nrep number of re-sample repetition to estimate p value.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return formatted string and fields
#'
#' @examples
#'
#' set.seed(25325)
#' d <- data.frame(x1=c(1,2,3,4,5,6,7,7),
#'                 x2=1,
#'                 y=c(FALSE,TRUE,FALSE,FALSE,
#'                     TRUE,TRUE,FALSE,TRUE))
#' sigr::formatAUCpair(d,'x1','x2','y',TRUE)$formatStr
#'
#'
#' @export
formatAUCpair <- function(d,
                          model1Name,
                          model2Name,
                          yName,
                          yTarget,
                          ...,
                          format,
                          returnScores=FALSE,
                          nrep=100,
                          pLargeCutoff=0.05,
                          pSmallCutoff=1.0e-5,
                          parallelCluster=NULL) {
  if(length(list(...))) {
    stop('formatAUC extra arguments')
  }
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!(format %in% formats)) {
    stop(paste("format",format,"not recognized"))
  }
  fsyms <- syms[format,]
  eScore <- resampleScoreModelPair(model1Values=d[[model1Name]],
                                   model2Values=d[[model2Name]],
                                   yValues=d[[yName]]==yTarget,
                                   scoreFn=calcAUC,
                                   returnScores=returnScores,
                                   nRep=nrep,
                                   parallelCluster=parallelCluster)
  eValue <- eScore$eValue
  eString <- formatSignificance(eValue,
                                symbol='e',
                                format=format,
                                pLargeCutoff=pLargeCutoff,
                                pSmallCutoff=pSmallCutoff)
  scoreString1 <- sprintf('%.2g',eScore$observedScore1)
  scoreString2 <- sprintf('%.2g',eScore$observedScore2)
  scoreString <- paste0(scoreString1,';',scoreString2)
  list(eValue=eValue,
       eString=eString,
       test='AUC results',
       eScore=eScore,
       scoreString1=scoreString1,
       scoreString2=scoreString2,
       scoreString=scoreString,
       formatStr=
         paste0(fsyms['startB'],'AUC test resampled AUC1>AUC2',fsyms['endB'],
                ': (',fsyms['startI'],'AUCs',fsyms['endI'],
                '=',scoreString,
                ', ',fsyms['startI'],'s.d.',fsyms['endI'],'=',sprintf('%.2g',eScore$sd),
                ', ',eString,').'))
}
