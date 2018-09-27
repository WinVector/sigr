
#' @importFrom stats dbinom convolve
NULL

#' Compute the distribution of differences of replacement samples of two Bernoulli experiments.
#'
#' Compute the distribution of \code{max(1, nBeffective/nAeffective)*sum(a) - max*1, nAeffective/nBeffective)*sum(b)}
#' where \code{a} is a 0/1 vector of length \code{nAeffective} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)},
#' and \code{b} is a 0/1 vector of length \code{nBeffective} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)}.
#' The idea is: under this scaling differnces in success rates between the two processes are easilly observed as differences
#' in counts returned by the scaled processes.
#' The method be used to get the exact probability of a given difference under the null hypothesis that
#' both the A and B processes have the same success rate.
#'
#' Note the intent that we are measuring the results of an A/B test with \code{nA \%\% nB == 0}
#' (no padding needed),  or  \code{nA >> nB}, or \code{nB >> nA}. The larger sample is padded so
#' the smaller sample divides evenly into it (allowing faster calculation methods).  The sizes of the
#' effective samples are given by \code{nAeffective} and \code{nBeffective}.  The padding will
#' slighly over-estimate confidences due the increased sample size, but if \code{nA} and \code{nB} are
#' not near each other- this will be a small effect.
#'
#'
#' @param nA number of A experiments.
#' @param kA number of A successes observed.
#' @param nB number of B experiments.
#' @param kB number of B successes observed.
#' @return experiment summaries
#'
#' @examples
#'
#' res <- Bernoulli_diff_dist(5000, 2000, 200, 100)
#' res$distribution[res$distribution$diff==ceiling(res$effective_diff), ]$prob_le
#' # above is the chance of a-b being at least as small (b being at least that
#' # much better) as observed if the two samples were drawn from a pooled process with
#' # no actual rate difference.
#' # That can work as a significance of a 1-sided test.
#' # two sided:
#' res$distribution[res$distribution$diff==ceiling(res$effective_diff), ]$prob_le +
#'    res$distribution[res$distribution$diff==floor(-res$effective_diff), ]$prob_ge
#'
#' @export
#'
Bernoulli_diff_dist <- function(nA, kA, nB, kB) {
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if((nA<=0)||(!is.wholenumber(nA))) {
    stop("nA must be a positive integer")
  }
  if((kA<0)||(kA>nA)||(!is.wholenumber(kA))) {
    stop("kA must be an integer in the range [0, nA]")
  }
  if((nB<=0)||(!is.wholenumber(nB))) {
    stop("nB must be a positive integer")
  }
  if((kB<0)||(kB>nB)||(!is.wholenumber(kB))) {
    stop("kB must be an integer in the range [0, nB]")
  }
  if(nB>nA) {
    res <- Bernoulli_diff_dist(nB, kB, nA, kA)
    res$nA <- nA
    res$kA <- kA
    res$nB <- nB
    res$kB <- kB
    res$nBeffective <- res$nAeffective
    res$nAeffective <- nA
    res$effective_diff <- -res$effective_diff
    d <- res$distribution
    d$diff <- -d$diff
    d <- d[order(d$diff), , drop = FALSE]
    rownames(d) <- NULL
    d$prob_le <- cumsum(d$prob)
    d$prob_lt <- d$prob_le - d$prob
    d$prob_ge <- rev(cumsum(rev(d$prob)))
    d$prob_gt <- d$prob_ge - d$prob
    res$distribution <- d
    return(res)
  }
  probi <- (kA+kB)/(nA+nB)
  # pad A-process so B process count divides into it.
  nAeffective <- nA + (nA %% nB)
  v1 <- dbinom(0:nAeffective, prob = probi, size = nAeffective)
  v2 <- dbinom(0:nB, prob = probi, size = nB)
  npad <- nAeffective/nB - 1
  if(npad>0) {
    # work process B as observing nAeffective/nB success for each success
    pad <- numeric(npad)
    v2 <- unlist(lapply(v2, function(vi) c(vi, pad)))[1:(nAeffective+1)]
  }
  probs <- convolve(v1, v2, type = "open")
  # check <- numeric(nAeffective + nB + 1)
  # for(i in 0:nAeffective) {
  #   for(j in 0:nB) {
  #     check[[i - j + nB + 1]] <- check[[i - j + nB + 1]] + v1[[i+1]]*v2[[j+1]]
  #   }
  # }
  # diff <- max(abs(probs-check))
  d <- data.frame(
    diff = (-nAeffective):nAeffective,
    prob = probs)
  d$prob_le <- cumsum(d$prob)
  d$prob_lt <- d$prob_le - d$prob
  d$prob_ge <- rev(cumsum(rev(d$prob)))
  d$prob_gt <- d$prob_ge - d$prob
  list(nA = nA, kA = kA,  nB = nB, kB = kB,
       probi = probi,
       nAeffective = nAeffective,
       nBeffective = nB,
       effective_diff = (nAeffective/nA)*kA - (npad+1)*kB,
       distribution = d)
}
