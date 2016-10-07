
mkPermWorker <- function(yValues,modelValues,scoreFn) {
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

permutationScoreModel <- function(yValues,modelValues,
                                  scoreFn,
                                  nRep=100,
                                  parallelCluster=NULL) {
  observedScore <- scoreFn(modelValues,yValues)
  permWorker <- mkPermWorker(yValues,modelValues,scoreFn)
  permutedScores <- plapply(1:nRep,permWorker,parallelCluster)
  v <- as.numeric(permutedScores)
  z <- (observedScore-mean(v))/sd(v)
  pNorm <- pnorm(z,lower.tail=FALSE)
  df = length(v)-1
  pValue <- pt(z,df=df,lower.tail=FALSE)
  pFreq <- sum(v>=observedScore)/length(v)
  list(observedScore=observedScore,
       df=df,
       #permutedScores=v,
       z=z,
       mean=mean(v),
       sd=sd(v),
       pNorm=pNorm,
       pValue=pValue,
       pFreq=pFreq)
}

mkResampleDiffWorkder <- function(yValues,
                              model1Values,
                              model2Values,
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


resampleScoreModelPair <- function(yValues,
                                   model1Values,
                                   model2Values,
                                   scoreFn,
                                   nRep=100,
                                   parallelCluster=NULL) {
  observedScore1 <- scoreFn(model1Values,yValues)
  observedScore2 <- scoreFn(model2Values,yValues)
  resampleWorker <- mkResampleDiffWorkder(yValues,
                                   model1Values,
                                   model2Values,
                                   scoreFn)
  resampledScores <- plapply(1:nRep,resampleWorker,parallelCluster)
  v <- as.numeric(resampledScores)
  z <- (observedScore1-observedScore2-mean(v))/sd(v)
  pNorm <- pnorm(z,lower.tail=FALSE)
  df = length(v)-1
  pFreq <- sum(v>=observedScore1-observedScore2)/length(v)
  pValue <- pt(z,df=df,lower.tail=FALSE)
  list(observedScore1=observedScore1,
       observedScore2=observedScore2,
       #resampledScoreDiffs=v,
       z=z,
       mean=mean(v),
       sd=sd(v),
       pValue=pValue,
       pFreq=pFreq)
}

