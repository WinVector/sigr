TestingRateDifferences
================

``` r
library("sigr")
```

``` r
set.seed(23523)
A <- rep(0, 200)
A[sample.int(length(A), 82)] <- 1
B <- rep(0, 100)
B[sample.int(length(B), 55)] <- 1

kA <- sum(A)
nA <- length(A)
kB <- sum(B)
nB <- length(B)

diff <- abs(kA/nA - kB/nB)
print(diff)
```

    ## [1] 0.14

``` r
mk_resample <- function(A, B, diff) {
  force(A)
  force(B)
  force(diff)
  univ <- c(A, B)
  nA <- length(A)
  nB <- length(B)
  nU <- length(univ)
  nrun <- 1000000
  function(...) {
    nge <- 0
    for(i in seq_len(nrun)) {
      sA <- univ[sample.int(nU, nA, replace = TRUE)]
      sB <- univ[sample.int(nU, nB, replace = TRUE)]
      nge <- nge + (abs(sum(sA)/nA - sum(sB)/nB)>=diff)
    }
    nge/nrun
  }
}
f <- mk_resample(A, B, diff)

cl <- parallel::makeCluster(parallel::detectCores())
res <- as.numeric(parallel::parLapply(cl, seq_len(100), f))
mean(res)
```

    ## [1] 0.02012466

``` r
s <- Bernoulli_diff_dist(kA, nA, kB, nB)
s
```

    ## [1] "Bernoulli difference test: (A=82/200=0.41, B=55/100=0.55, post 0.14 two_sided; p=0.01934)."

``` r
s$pValue
```

    ## [1] 0.01934292

``` r
wrapTTest(A, B)
```

    ## [1] "Welch Two Sample t-test, two.sided: (t=-2.297, df=195.7, p=0.02269)."
