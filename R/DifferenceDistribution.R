
#' @importFrom stats dbinom convolve
NULL

#' Compute the distribution of differences of replacement samples of two Bernoulli experiments.
#'
#' Compute the distribution of \code{sum(a) - (nA/nB)*sum(b)}
#' where \code{a} is a 0/1 vector of length \code{nA + (nA %% nB)} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)},
#' and \code{b} is a 0/1 vector of length \code{nB} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)}.
#' Can be used to get the exact probability of a given difference under the null hypothesis that
#' both the A and B processes have the same success rate.
#'
#' Note the intent that we are measuring the results of an A/B test with \code{nA %% nB == 0}
#' (no padding needed) or
#' \code{nA >> nB} (padding is small relative to experiment size).  The padding so that the length of the B-process divides into the length of the
#' A-process is to allow all the differences of counts to be integers (allows a faster computation).
#'
#'
#' @param nA number of A experiments.
#' @param kA number of A successes observed.
#' @param nB number of B experiments (must be no larger than nA).
#' @param kB number of B successes observed.
#' @return experiment summaries
#'
#' @examples
#'
#' Bernoulli_diff_dist(5, 2, 2, 1)
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
    stop("must have nB no larger than nA")
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
       nAeffective = nAeffective,
       effective_diff = (nAeffective/nA)*kA - (npad+1)*kB,
       distribution = d)
}
