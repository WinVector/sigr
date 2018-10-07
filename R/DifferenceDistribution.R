
#' @importFrom stats dbinom convolve
NULL


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


Bernoulli_diff_dist_impl <- function(nA, nB,
                                     probi, test_rate_difference) {
  calc_probs <- function(nA, nB) {
    if(nB>nA) {
      stop("calc_probs needs nA>=nB")
    }
    if((nA %% nB)!=0) {
      stop("calc_probs needs (nA%%nB)==0")
    }
    # pad A-process so B process count divides into it.
    v1 <- dbinom(0:nA, prob = probi, size = nA)
    v2 <- dbinom(0:nB, prob = probi, size = nB)
    npad <- nA/nB - 1
    if(npad>0) {
      # work process B as observing nA/nB success for each success
      pad <- numeric(npad)
      v2 <- unlist(lapply(v2, function(vi) c(vi, pad)))[1:(nA+1)]
    }
    probs <- convolve(v1, v2, type = "open")
    probs <- pmax(probs, 0)
    probs <- probs/sum(probs)
    # check <- numeric(nA + nB + 1)
    # for(i in 0:nA) {
    #   for(j in 0:nB) {
    #     check[[i - j + nB + 1]] <- check[[i - j + nB + 1]] + v1[[i+1]]*v2[[j+1]]
    #   }
    # }
    # diff <- max(abs(probs-check))
    data.frame(
      diff = (-nA):nA,
      prob = probs)
  }
  if(nB<=nA) {
    d <- calc_probs(nA, nB)
  } else {
    d <- calc_probs(nB, nA)
    d$diff <- -d$diff
    d <- d[order(d$diff), , drop = FALSE]
    rownames(d) <- NULL
  }
  d$prob_le <- cumsum(d$prob)
  d$prob_lt <- d$prob_le - d$prob
  d$prob_ge <- rev(cumsum(rev(d$prob)))
  d$prob_gt <- d$prob_ge - d$prob
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
  testres <- list(nA = nA,
                  nB = nB,
                  probi = probi,
                  test_rate_difference = test_rate_difference,
                  distribution = d,
                  kind = "two sided",
                  test_sig = test_sig)
  testres
}

#' Compute the distribution of differences of replacement samples of two Binomial or Bernoulli experiments.
#'
#' Assuming \code{max(nA, nB) \%\% min(nA, nB) == 0}:
#' compute the distribution of differences of weighted sums between
#' \code{max(1, nB/nA)*sum(a)}
#' and  \code{max(1, nA/nB)*sum(b)}
#' where \code{a} is a 0/1 vector of length \code{nA} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)},
#' and \code{b} is a 0/1 vector of length \code{nB} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)}.
#' Then return the significance of a direct two-sided test that the absolute value of this difference is at least as large
#' as the test_rate_difference (if supplied) or the empirically observed rate difference \code{abs(nB*kA - nA*kB)/(nA*nB)}.
#' The idea is: under this scaling differences in success rates between the two processes are easily observed as differences
#' in counts returned by the scaled processes.
#' The method can be used to get the exact probability of a given difference under the null hypothesis that
#' both the \code{A} and \code{B} processes have the same success rate \code{(kA+kB)/(nA+nB)}.
#' When \code{nA} and \code{nB} don't divide evenly into to each
#' other two calculations are run with the larger process is alternately padded and truncated to look like a larger or smaller
#' experiment that meets the above conditions.  This gives us a good range of significances.
#'
#' Note the intent is that we are measuring the results of an A/B test with \code{max(nA, nB) \%\% min(nA, nB) == 0}
#' (no padding needed), or \code{max(nA,nB) >> min(nA,nB)} (padding is small effect).
#'
#' The idea of converting a rate problem into a counting problem follows from reading Wald's \emph{Sequential Analysis}.
#'
#' For very small p-values the calculation is sensitive to rounding in the observed ratio-difference,
#' as an arbitrarily small change in test-rate can move an entire set of observed differences in or out of the significance calculation.
#'
#'
#' @param kA number of A successes observed.
#' @param nA number of A experiments.
#' @param kB number of B successes observed.
#' @param nB number of B experiments.
#' @param test_rate_difference numeric, difference in rate of A-B to test.  Note: it is best to specify this prior to looking at the data.
#' @param common_rate rate numeric, assumed null-rate.
#' @return Bernoulli difference test statistic.
#'
#' @examples
#'
#' Bernoulli_diff_stat(2000, 5000, 100, 200)
#' Bernoulli_diff_stat(2000, 5000, 100, 200, 0.1)
#' Bernoulli_diff_stat(2000, 5000, 100, 199)
#' Bernoulli_diff_stat(2000, 5000, 100, 199, 0.1)
#' Bernoulli_diff_stat(100, 200, 2000, 5000)
#'
#' # sigr adjusts experiment sizes when lengths
#' # don't divide into each other.
#' Bernoulli_diff_stat(100, 199, 2000, 5000)
#' Bernoulli_diff_stat(100, 199, 2000, 5000)$pValue
#'
#' @export
#'
Bernoulli_diff_stat <- function(kA, nA, kB, nB,
                                test_rate_difference, common_rate) {
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
  used_observed_rate <- FALSE
  if(missing(common_rate) || is.null(common_rate)) {
    used_observed_rate <- TRUE
    probi <- (kA+kB)/(nA+nB)
  } else {
    probi <- common_rate
  }
  used_observed_rate_diff <- FALSE
  if(missing(test_rate_difference) || is.null(test_rate_difference)) {
    # test_rate_difference = abs(kA/nA - kB/nB)
    test_rate_difference <- abs(nB*kA - nA*kB)/(nA*nB)
    used_observed_rate_diff <- TRUE
  }
  divides_even <- (nA==nB) ||
    ((nA>nB)&&((nA %% nB)==0)) ||
    ((nB>nA)&&((nB %% nA)==0))
  Aadjusted <- FALSE
  Badjusted <- FALSE
  subres <- list()
  pValue <- NA
  pHigh <- NA
  pLow <- NA
  if(divides_even) {
    subres <- list(
      Bernoulli_diff_dist_impl(nA, nB, probi, test_rate_difference))
    pValue <- subres[[1]]$test_sig
    pHigh <- pValue
    pLow <- pValue
  } else {
    if(nA>=nB) {
      Aadjusted <- TRUE
      residue = nA %% nB
      subres <- list(
        Bernoulli_diff_dist_impl(nA + (nB - residue), nB, probi, test_rate_difference),
        Bernoulli_diff_dist_impl(nA - residue, nB, probi, test_rate_difference)
      )
    } else {
      Badjusted <- TRUE
      residue = nB %% nA
      subres <- list(
        Bernoulli_diff_dist_impl(nA, nB + (nA - residue), probi, test_rate_difference),
        Bernoulli_diff_dist_impl(nA, nB - residue, probi, test_rate_difference)
      )
    }
    pHigh <- max(subres[[1]]$test_sig, subres[[2]]$test_sig)
    pLow <- min(subres[[1]]$test_sig, subres[[2]]$test_sig)
    pValue <- (pHigh + pLow)/2
  }
  r <- list(test='Bernoulli_diff_test',
            kind = "two sided",
            kA = kA,
            nA = nA,
            kB = kB,
            nB = nB,
            probi = probi,
            test_rate_difference = test_rate_difference,
            used_observed_rate = used_observed_rate,
            used_observed_rate_diff = used_observed_rate_diff,
            divides_even = divides_even,
            Aadjusted = Aadjusted,
            Badjusted = Badjusted,
            subres = subres,
            pValue = pValue,
            pHigh = pHigh,
            pLow = pLow)
  class(r) <- c('sigr_Bernoulli_diff_test', 'sigr_statistic')
  r
}


#' Format sigr_Bernoulli_diff_test (test of difference of Bernoulli processes).
#'
#' @param statistic wrapped cor.test.
#' @param ... extra arguments (not used)
#' @param format if set the format to return ("html", "latex", "markdown", "ascii", "docx", ...)
#' @param statDigits integer number of digits to show in summaries.
#' @param sigDigits integer number of digits to show in significances.
#' @param pLargeCutoff value to declare non-significance at or above.
#' @param pSmallCutoff smallest value to print
#' @return formatted string
#'
#' @examples
#'
#' Bernoulli_diff_stat(2000, 5000, 100, 200)
#' Bernoulli_diff_stat(2000, 5000, 100, 200, 0.1)
#' Bernoulli_diff_stat(2000, 5000, 100, 199)
#' Bernoulli_diff_stat(2000, 5000, 100, 199, 0.1)
#'
#'
#' @export
render.sigr_Bernoulli_diff_test <- function(statistic,
                                            ...,
                                            format,
                                            statDigits=4,
                                            sigDigits=4,
                                            pLargeCutoff=0.05,
                                            pSmallCutoff=1.0e-5) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::render.sigr_cortest")
  if (missing(format) || is.null(format)) {
    format <- getRenderingFormat()
  }
  if(!isTRUE(format %in% formats)) {
    format <- "ascii"
  }
  fsyms <- syms[format,]
  stat_format_str <- paste0('%.',statDigits,'g')
  if(statistic$divides_even) {
    pString <- render(wrapSignificance(statistic$pValue,
                                       symbol='p'),
                      format=format,
                      pLargeCutoff=pLargeCutoff,
                      pSmallCutoff=pSmallCutoff)
  } else {
    pStringH <- render(wrapSignificance(statistic$pHigh,
                                        symbol='pH'),
                       format=format,
                       pLargeCutoff=pLargeCutoff,
                       pSmallCutoff=pSmallCutoff)
    pStringL <- render(wrapSignificance(statistic$pLow,
                                        symbol='pL'),
                       format=format,
                       pLargeCutoff=pLargeCutoff,
                       pSmallCutoff=pSmallCutoff)
    pString <- paste0(pStringL, ", ", pStringH)
  }
  formatStr <- paste0(fsyms['startB'], "Bernoulli difference test",fsyms['endB'],
                      ': (A=',
                      statistic$kA, "/", statistic$nA, "=", sprintf(stat_format_str,statistic$kA/statistic$nA),
                      ', B=',
                      statistic$kB, "/", statistic$nB, "=", sprintf(stat_format_str,statistic$kB/statistic$nB),
                      ", ", ifelse(statistic$used_observed_rate, "=", "~"), sprintf(stat_format_str,statistic$probi),
                      ", ", ifelse(statistic$used_observed_rate_diff, "post ", "prior "), sprintf(stat_format_str,statistic$test_rate_difference),
                      " ", statistic$kind,
                      '; ',pString,').')
  formatStr
}





