
#' @importFrom stats dbinom convolve
NULL

#' Compute the distribution of differences of replacement samples of two Bernoulli experiments.
#'
#' Compute the distribution of \code{max(1, nBeffective/nAeffective)*sum(a) - max(1, nAeffective/nBeffective)*sum(b)}
#' where \code{a} is a 0/1 vector of length \code{nAeffective} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)},
#' and \code{b} is a 0/1 vector of length \code{nBeffective} with each item 1 with independent probability \code{(kA+kB)/(nA+nB)}.
#' The idea is: under this scaling differences in success rates between the two processes are easily observed as differences
#' in counts returned by the scaled processes.
#' The method be used to get the exact probability of a given difference under the null hypothesis that
#' both the A and B processes have the same success rate.
#'
#' Note the intent that we are measuring the results of an A/B test with \code{nA \%\% nB == 0}
#' (no padding needed),  or  \code{nA >> nB}, or \code{nB >> nA}. The larger sample is padded so
#' the smaller sample divides evenly into it (allowing faster calculation methods).  The sizes of the
#' effective samples are given by \code{nAeffective} and \code{nBeffective}.  The padding will
#' slightly over-estimate confidences due the increased sample size, but if \code{nA} and \code{nB} are
#' not near each other- this will be a small effect.  However, padding does represent a downward
#' bias on significance estimates.
#'
#'
#' @param kA number of A successes observed.
#' @param nA number of A experiments.
#' @param kB number of B successes observed.
#' @param nB number of B experiments.
#' @param test_rate numeric, difference in rate of A-B to test.  Note: it is best to specify this prior to looking at the data.
#' @return Bernoulli difference test statistic.
#'
#' @examples
#'
#' Bernoulli_diff_dist(2000, 5000, 100, 200, 0.1)
#' Bernoulli_diff_dist(2000, 5000, 100, 200)
#' Bernoulli_diff_dist(100, 200, 2000, 5000, 0.1)
#'
#' # let sigr extend the A experiment to estimate
#' # biased down
#' kA <- 2000
#' nA <- 5000
#' kB <- 100
#' nB <- 199
#' Bernoulli_diff_dist(kA, nA, kB, nB)
#' # user truncating the A experiment
#' # biased up (modulo rounding issues)
#' nAEffective <- floor(nA/nB)*nB
#' kAEffectiveF <- floor((kA/nA)*nAEffective)
#' Bernoulli_diff_dist(kAEffectiveF, nAEffective, kB, nB)
#' kAEffectiveC <- ceiling((kA/nA)*nAEffective)
#' Bernoulli_diff_dist(kAEffectiveC, nAEffective, kB, nB)
#'
#' @export
#'
Bernoulli_diff_dist <- function(kA, nA, kB, nB,
                                test_rate) {
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
  if(missing(test_rate) || is.null(test_rate)) {
    test_rate = abs(kA/nA - kB/nB)
    used_observed_rate <- TRUE
  }
  calc_probs <- function(kA, nA, kB, nB) {
    if(nB>nA) {
      stop("calc_probs needs nA>=nB")
    }
    probi <- (kA+kB)/(nA+nB)
    # pad A-process so B process count divides into it.
    nAeffective <- nA
    residue <- nA %% nB
    if(residue!=0) {
      nAeffective <- nA + (nB - (nA %% nB))
    }
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
    list(d = d, nAeffective = nAeffective, probi = probi)
  }
  if(nB<=nA) {
    pres <- calc_probs(kA, nA, kB, nB)
    d <- pres$d
    nAeffective <- pres$nAeffective
    nBeffective <- nB
  } else {
    pres <- calc_probs(kB, nB, kA, nA)
    d <- pres$d
    d$diff <- -d$diff
    d <- d[order(d$diff), , drop = FALSE]
    rownames(d) <- NULL
    nAeffective <- nA
    nBeffective <- pres$nAeffective
  }
  probi <- pres$probi
  d$prob_le <- cumsum(d$prob)
  d$prob_lt <- d$prob_le - d$prob
  d$prob_ge <- rev(cumsum(rev(d$prob)))
  d$prob_gt <- d$prob_ge - d$prob
  en <- max(nAeffective, nBeffective)*test_rate
  i1 <- match(ceiling(-en), d$diff)
  if(is.na(i1)) {
    i1 <- 1
  }
  i2 <- match(floor(en), d$diff)
  if(is.na(i2)) {
    i2 <- nrow(d)
  }
  test_sig <- ifelse(d$diff[[i1]]>-en, d$prob_lt[[i1]], d$prob_le[[i1]]) +
    ifelse(d$diff[[i2]] < en, d$prob_gt[[i2]], d$prob_ge[[i2]])
  testres <- list(kA = kA, nA = nA, kB = kB, nB = nB,
                  probi = probi,
                  nAeffective = nAeffective,
                  nBeffective = nBeffective,
                  used_observed_rate = used_observed_rate,
                  test_rate = test_rate,
                  distribution = d,
                  padded = (nA!=nAeffective) || (nB!=nBeffective),
                  kind = "two_sided",
                  test_sig = test_sig)
  r <- list(testres=testres,
            pValue = test_sig,
            test='Bernoulli_diff_test')
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
#' Bernoulli_diff_dist(2000, 5000, 100, 200, 0.1)
#' Bernoulli_diff_dist(2000, 5000, 100, 199)
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
  testres <- statistic$testres
  pString <- render(wrapSignificance(testres$test_sig,
                                     symbol='p'),
                    format=format,
                    pLargeCutoff=pLargeCutoff,
                    pSmallCutoff=pSmallCutoff)
  formatStr <- paste0(fsyms['startB'], "Bernoulli difference test",fsyms['endB'],
                      ': (A', ifelse(testres$nA==testres$nAeffective, "=", paste0("+",testres$nAeffective-testres$nA,"~")),
                      testres$kA, "/", testres$nA, "=", sprintf(stat_format_str,testres$kA/testres$nA),
                      ', B', ifelse(testres$nB==testres$nBeffective, "=", paste0("+",testres$nBeffective-testres$nB,"~")),
                      testres$kB, "/", testres$nB, "=", sprintf(stat_format_str,testres$kB/testres$nB),
                      ", ", ifelse(testres$used_observed_rate, "post ", "prior "), sprintf(stat_format_str,testres$test_rate),
                      " ", testres$kind,
                      '; ',pString,').')
  formatStr
}
