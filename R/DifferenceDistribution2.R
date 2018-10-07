

# Replacement for DifferenceDistribution


#' @importFrom stats dbinom convolve
NULL

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  (length(x)==1) && (abs(x - round(x)) < tol)
}

is.pos_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  is.wholenumber(x) && (x>0)
}


#' Build a table of observation probabilities for a difference of scaled Binomial counts.
#'
#' Must have nA*multA == nB*multB.
#'
#' @param nA number of events in process A
#' @param multA nA success multiplier
#' @param nB number of event sin process B
#' @param multB nB success multiplier
#' @param probi common success probability.
#' @return table of event probabilities
#'
#' @examples
#'
#' scaled_binomial_difference_table(3, 2, 2, 3, 0.5)
#' scaled_binomial_difference_table(1, 2, 2, 1, 0.5)
#'
#' @noRd
#'
scaled_binomial_difference_table <- function(nA, multA,
                                             nB, multB,
                                             probi) {
  if(!is.pos_wholenumber(nA)) {
    stop("sigr::scaled_binomial_difference_table nA must be a positive integer")
  }
  if(!is.pos_wholenumber(multA)) {
    stop("sigr::scaled_binomial_difference_table multA must be a positive integer")
  }
  if(!is.pos_wholenumber(nB)) {
    stop("sigr::scaled_binomial_difference_table nB must be a positive integer")
  }
  if(!is.pos_wholenumber(multB)) {
    stop("sigr::scaled_binomial_difference_table multB must be a positive integer")
  }
  if((probi<0)||(probi>1)) {
    stop("sigr::scaled_binomial_difference_table probi must in the interval [0,1]")
  }
  if(abs(nA*multA - nB*multB)>1.e-3) {
    stop("scaled_binomial_difference_table: (nA*multA - nB*multB) must be zero")
  }
  vA <- dbinom(0:nA, prob = probi, size = nA)
  if(multA>1) {
    pad <- numeric(multA-1)
    vA <- unlist(lapply(vA, function(vi) c(vi, pad)))[1:(nA*multA+1)]
  }
  if(length(vA)!=(nA*multA+1)) {
    stop("sigr::scaled_binomial_difference_table bad vA length")
  }
  vB <- dbinom(0:nB, prob = probi, size = nB)
  if(multB>1) {
    pad <- numeric(multB-1)
    vB <- unlist(lapply(vB, function(vi) c(vi, pad)))[1:(nB*multB+1)]
  }
  if(length(vB)!=(nB*multB+1)) {
    stop("sigr::scaled_binomial_difference_table bad vA length")
  }
  probs <- convolve(vA, vB, type = "open")
  probs <- pmax(probs, 0)
  probs <- probs/sum(probs)
  d <- data.frame(
    diff = (-nA*multA):(nA*multA),
    prob = probs)
  d$prob_le <- cumsum(d$prob)
  d$prob_lt <- d$prob_le - d$prob
  d$prob_ge <- rev(cumsum(rev(d$prob)))
  d$prob_gt <- d$prob_ge - d$prob
  d
}


calc_probs <- function(d,
                       nA, multA,
                       nB, multB,
                       test_rate_difference) {
  if(!is.pos_wholenumber(nA)) {
    stop("sigr::scaled_binomial_difference_table nA must be a positive integer")
  }
  if(!is.pos_wholenumber(multA)) {
    stop("sigr::scaled_binomial_difference_table multA must be a positive integer")
  }
  if(!is.pos_wholenumber(nB)) {
    stop("sigr::scaled_binomial_difference_table nB must be a positive integer")
  }
  if(!is.pos_wholenumber(multB)) {
    stop("sigr::scaled_binomial_difference_table multB must be a positive integer")
  }
  if((test_rate_difference<0)||(test_rate_difference>1)) {
    stop("sigr::scaled_binomial_difference_table test_rate_difference must in the interval [0,1]")
  }
  if(abs(nA*multA - nB*multB)>1.e-3) {
    stop("scaled_binomial_difference_table: (nA*multA - nB*multB) must be zero")
  }
  # avoid rounding issues as seen in extras/RateDiffs.Rmd
  # Bernoulli_diff_stat(82, 200, 55, 100)
  en <- round(max(nA, nB)*test_rate_difference, digits = 7)
  i1 <- match(ceiling(-en), d$diff)
  if(is.na(i1)) {
    i1 <- 1
  } else {
    if((i1>1)&&(d$diff[[i1]] > (-en))) {
      i1 <- i1 - 1
    }
  }
  i2 <- match(floor(en), d$diff)
  if(is.na(i2)) {
    i2 <- nrow(d)
  } else {
    if((i2<nrow(d))&&(d$diff[[i2]]<en)) {
      i2 <- i2 + 1
    }
  }
  test_sig <- d$prob_le[[i1]] + d$prob_ge[[i2]]
  test_sig
}


# TODO: new padding/shrinking algo for Bernoulli_diff_stat().
#
# For experiment sizes, A, B p we are roughly mixing
# an experiment of size A, rate p scaled by max(1, B/A)
# with an experiment of size B, rate p scaled by max(1, B/A).
#
# So the variance of the rate estimate is going to be
#
# max(1, B/A)^2 * p*(1-p) * A / A^2 + max(1, A/B)^2 * p*(1-p) * B / B^2
# = p*(1-p) * ( max(1, B/A)^2 / A + max(1, A/B)^2 / B )
#
# the p*(1-p) term is not varying, with A, B so define
#
# f(A, B) := max(1, B/A)^2 / A + max(1, A/B)^2 / B
#
# Our current padding scheme when A>B is to run experiments of size
# ( A + (B - (A %% B)), B ) and
# ( A - (A %% B), B ).
#
# Instead we should run more variations.
#
# 1) Write A/B as a short continued fraction. Take the first or second
#    convergent of this (the last one where at least one of the numerator or
#    denominator or small).  A/B ~ x/y so y A ~ x B.
# 2) Solve (by rounding) for a, b such that y (A + a) = x (B + b)
#    (easy look at floor(y A / ( x B)) and ceiling(y A / (x B))
#     and pick b integral and sign satifying and then a for equality).
# 3) push best over and best under (according to f()) as the bounding estimates.
#
#



