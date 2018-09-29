TestingRateDifferences
================

Let's take a quick look at a very important and common experimental problem: checking if the difference in success rates of two [Bernoulli processes](https://en.wikipedia.org/wiki/Bernoulli_process) is statistically significant. This can arise in A/B testing situations such as online advertising, sales, and manufacturing.

We already have a [free video course on a Bayesian treatment of planning and evaluating A/B tests](http://www.win-vector.com/blog/2016/02/free-video-course-applied-bayesian-ab-testing-in-r/) (including a [free Shiny application](https://github.com/WinVector/CampaignPlanner_v3)). Let's now take a look at the should be simple task of just building a summary statistic that includes a classic frequentist significance.

Suppose our data comes from collecting outcomes from two processes: A and B, and is as follows.

``` r
# build example data
set.seed(2018)
A <- rep(0, 200)
A[sample.int(length(A), 82)] <- 1
B <- rep(0, 100)
B[sample.int(length(B), 55)] <- 1

kA <- sum(A)
nA <- length(A)
kB <- sum(B)
nB <- length(B)

df <- data.frame(
  outcome = c(A, B), 
  treatment = c(rep("A", length(A)), 
                rep("B", length(B))),
  stringsAsFactors = FALSE)
table(df)
```

    ##        treatment
    ## outcome   A   B
    ##       0 118  45
    ##       1  82  55

``` r
abs_rate_difference <- abs(kA/nA - kB/nB)
print(abs_rate_difference)
```

    ## [1] 0.14

A natural question is: how likely is such a large difference of success rates (ratios of 1's in the records) assuming a null-hypothesis that both processes are in fact the same and generating 1's at the common observed rate. This is a classic significance question.

With the [`sigr`](https://CRAN.R-project.org/package=sigr) [`R`](https://www.r-project.org) package we can answer this directly with the difference in [Bernilli processes difference statistic](https://winvector.github.io/sigr/reference/Bernoulli_diff_dist.html) (requires `sigr` version `1.0.2`, or newer). Right or wrong frequentist summaries are supposed to be easy and quick to derive, here is the difference in rates of Bernoulli processes as a one-liner.

``` r
library("sigr")

s <- Bernoulli_diff_dist(kA, nA, kB, nB)
s
```

    ## [1] "Bernoulli difference test: (A=82/200=0.41, B=55/100=0.55, post 0.14 two_sided; p=0.02404)."

``` r
s$pValue
```

    ## [1] 0.02404322

As always, we want the p-value to be small as it represents the odds of our experiment being invalid under one very specific source of error (there are many more ways and experiment can go wrong, and failing to fail in this particular way does not give odds the result is correct).

One can try to estimate this quantity directly through re-sampling techniques, but estimating small significances is computationally expensive.

``` r
set.seed(2018)

mk_resample <- function(A, B) {
  force(A)
  force(B)
  kA <- sum(A)
  nA <- length(A)
  kB <- sum(B)
  nB <- length(B)
  univ <- c(A, B)
  nU <- length(univ)
  nrun <- 1000000
  abs_rate_difference_times_nab <- abs(nB*kA - nA*kB)

  function(...) {
    nge <- 0
    for(i in seq_len(nrun)) {
      sA <- univ[sample.int(nU, nA, replace = TRUE)]
      sB <- univ[sample.int(nU, nB, replace = TRUE)]
      obs <- abs(nB*sum(sA) - nA*sum(sB))
      nge <- nge + (obs>=abs_rate_difference_times_nab)
    }
    nge/nrun
  }
}
f <- mk_resample(A, B)

cl <- parallel::makeCluster(parallel::detectCores())
res <- as.numeric(parallel::parLapply(cl, seq_len(100), f))
parallel::stopCluster(cl)

mean(res)
```

    ## [1] 0.02401745

More commonly one just throws out some domain knowledge and uses a ready-made test. For instance if we ignore the fact that the Bernoulli process generates only 0/1 we can use a classic t-test to estimate the significance of the observed difference.

``` r
wrapTTest(A, B)
```

    ## [1] "Welch Two Sample t-test, two.sided: (t=-2.297, df=195.7, p=0.02269)."

One can work further on the above by adding "continuity corrections" and so on.

Or, if we ignore distinction between treatment and outcome we can use a Fisher independence test.

``` r
wrapFisherTest(df, "treatment", "outcome")
```

    ## [1] "Fisher's Exact Test for Count Data: (odds.ratio=1.755, p=0.0268)."

All of the above methods are standard and serviceable. Notice the difference in Bernoulli processes calculation was closest to the empirical estimate. And, there is some value in a quick direct calculation of the probabilities arising from the actual problem at hand. Approximations and assumptions we have not needed do not need to be later explained.
