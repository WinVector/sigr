
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

#' Empirical permuation test of significance of scoreFn(modelValues,yValues) > scoreFn(modelValues,perm(yValues)).
#'
#' Treat permutaiton re-samples as similar to bootstrap replications.
#'
#' @param modelValues numeric array of predictions.
#' @param yValues numeric/logical array of outcomes, depedendent, or truth values
#' @param scoreFn function with signature scoreFn(modelValues,yValues) returning scalar numeric score.
#' @param ... not used, forces later arguments to be bound by name
#' @param nRep integer number of repititions to perform
#' @param parallelCluster optional snow-style parallel cluster.
#' @return summaries
#'
#' @examples
#'
#' y <- 1:5
#' m <- c(1,1,2,2,2)
#' cor.test(m,y)
#' f <- function(modelValues,yValues) cor(modelValues,yValues)
#' permutationScoreModel(m,y,f)
#'
#' @export
#'
permutationScoreModel <- function(modelValues,yValues,
                                  scoreFn,
                                  ...,
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
  v <- as.numeric(permutedScores)
  meanv <- mean(v)
  sdv <- sqrt(sum((v-meanv)^2)/(length(v)-1))
  z <- (observedScore-meanv)/sdv
  df = length(v)-1
  pValue <- pt(z,df=df,lower.tail=FALSE)
  pFreq <- sum(v>=observedScore)/length(v)
  list(fnName='permutationScoreModel',
       test='is observed score greater than permuted score',
       observedScore=observedScore,
       df=df,
       z=z,
       mean=meanv,
       sd=sdv,
       pValue=pValue,
       pFreq=pFreq)
}

mkResampleDiffWorkder <- function(model1Values,
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
  modelPooled <- c(model1Values,model2Values)
  yPooled <- c(yValues,yValues)
  function(x) {
    samp <- sample.int(2*n,n,replace=TRUE)
    score1 <- scoreFn(modelPooled[samp],yPooled[samp])
    samp <- sample.int(2*n,n,replace=TRUE)
    score2 <- scoreFn(modelPooled[samp],yPooled[samp])
    score1-score2
  }
}

#' Studentized bootstrap test of significance of
#'  scoreFn(yValues,model1Values) - scoreFn(yValues,model1Values) > pooled difference.
#'
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
#' @param nRep integer number of repititions to perform
#' @param parallelCluster optional snow-style parallel cluster.
#' @return summaries
#'
#' @examples
#'
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
                                   nRep=100,
                                   parallelCluster=NULL) {
  if(length(list(...))>0) {
    stop('resampleScoreModelPair unexpected extra arguments')
  }
  observedScore1 <- scoreFn(model1Values,yValues)
  observedScore2 <- scoreFn(model2Values,yValues)
  resampleWorker <- mkResampleDiffWorkder(model1Values=model1Values,
                                          model2Values=model2Values,
                                          yValues=yValues,
                                          scoreFn=scoreFn)
  resampledScores <- plapply(1:nRep,resampleWorker,parallelCluster)
  v <- as.numeric(resampledScores)
  meanv <- mean(v)
  sdv <- sqrt(sum((v-meanv)^2)/(length(v)-1))
  z <- (observedScore1-observedScore2-meanv)/sdv
  df = length(v)-1
  pFreq <- sum(v>=observedScore1-observedScore2)/length(v)
  pValue <- pt(z,df=df,lower.tail=FALSE)
  list(fnName='resampleScoreModelPair',
       test="is score1-score2 greater than pooled differences",
       observedScore1=observedScore1,
       observedScore2=observedScore2,
       z=z,
       mean=meanv,
       sd=sdv,
       pValue=pValue,
       pFreq=pFreq)
}

