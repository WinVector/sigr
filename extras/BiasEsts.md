Bias Ests
================

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
library("wrapr")
library("cdata")
library("ggplot2")
library("glmnet")
```

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-16

``` r
set.seed(32535)

naive_sd_fun <- function(x) {
  # also equals (sum(x)/length(x))*(1-sum(x)/length(x))
  sqrt(mean((mean(x)-x)^2))
}

Bessel_sd_fun <- function(x) {
  sd(x)
}

eval_scale_adjustment_table <- function(scale_adjustment_table, p, sd_fun = naive_sd_fun) {
  n <- length(scale_adjustment_table)-1
  obs <- 0:n
  probs <- dbinom(obs, size = n, prob = p)
  evals <- vapply(obs,
                function(ki) {
                  sd_fun(c(rep(1, ki), rep(0, n-ki)))*scale_adjustment_table[[ki+1]]
                }, numeric(1))
  sd_target <- sqrt(p*(1-p))
  sum(probs*evals)/sd_target
}


# related to Wald mini/max type ideas and:
# http://www.win-vector.com/blog/2014/07/frequenstist-inference-only-seems-easy/
# https://mathoverflow.net/questions/177574/existence-of-solutions-of-a-polynomial-system 
# think this is under-determined, so could ask for symmetry or all coefs near 1.
# also a lot like a Chebyshev polynomial (or integral of one), or upside down beta.
# can also try priors like beta(0.5,0.5)
# could also solve for affine version- where we allow addition in addition to scaling.
solve_for_scaling_table <- function(n, sd_fun = naive_sd_fun, excess_resolution = 128) {
  if(n<2) {
    return(rep(1, n+1))
  }
  obs <- 1:(n-1)
  vars <- paste0("s_", 1:(n-1))
  
  #ps <- seq(1/n, (n-1)/n, by = 1/(excess_resolution*n))
  ps <- seq(1/(excess_resolution*n), (excess_resolution*n-1)/(excess_resolution*n), by = 1/(excess_resolution*n))
  d <- data.frame(target = sqrt(ps*(1-ps)))
  for(ki in seq_len(length(vars))) {
    k <- obs[[ki]]
    var <- vars[[ki]]
    d[[var]] <- 0
    for(ii in seq_len(length(ps))) {
      pi <- ps[[ii]]
      prob <- dbinom(k, size = n, prob = pi)
      est <- sd_fun(c(rep(1, k), rep(0, n-k)))
      cij <- prob*est
      d[[var]][[ii]] <- cij
      d$target[[ii]] <- d$target[[ii]] - cij # regularize towards zero
    }
  }
  #m <- lm(mk_formula("target", vars, intercept = FALSE), data= d)
  #soln <- as.numeric(m$coefficients) + 1
  m <- glmnet(as.matrix(d[, vars, drop = FALSE]), d$target, 
              alpha=0, lambda=1e-6, family = "gaussian", intercept = FALSE,
              weights = dbeta(ps, 0.5, 0.5),
              # lower.limits = -10,
              standardize = FALSE)
  soln <- as.numeric(m$beta) + 1
  mx <- max(soln)
  c(mx, soln, mx)
}

solve_for_scaling_table(10)
```

    ##  [1] 2.0310041 2.0310041 0.4903253 1.1946009 1.1509679 0.9374478 1.1532905
    ##  [8] 1.1913828 0.4926127 2.0301965 2.0310041

``` r
solve_for_scaling_table(5)
```

    ## [1] 2.0171146 2.0171146 0.7335087 0.7361976 2.0158886 2.0171146

``` r
tab <- solve_for_scaling_table(5)

adjs <- data.frame(p = seq(0, 1, by = 0.01))
adjs$joint_scaled <- vapply(
  adjs$p,
  function(pi) {
    eval_scale_adjustment_table(tab, pi)
  }, numeric(1))
adjs$Bessel_scaled <- vapply(
  adjs$p,
  function(pi) {
    eval_scale_adjustment_table(rep(1, length(tab)), pi, sd_fun = Bessel_sd_fun)
  }, numeric(1))
adjs$unscaled <- vapply(
  adjs$p,
  function(pi) {
    eval_scale_adjustment_table(rep(1, length(tab)), pi)
  }, numeric(1))


adjsp <- unpivot_to_blocks(
  adjs, 
  nameForNewKeyColumn = "method", 
  nameForNewValueColumn = "ratio", 
  columnsToTakeFrom = c("joint_scaled", "unscaled", "Bessel_scaled"))
adjsp <- adjsp[!is.na(adjsp$ratio), , drop = FALSE]
adjsp$method <- reorder(factor(adjsp$method), -adjsp$ratio)


ggplot(data = adjsp, mapping = aes(x = p, y = ratio, color = method)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype=2) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("ratio betweeen estimated and theoretical standard deviations")
```

![](BiasEsts_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
data <- as.data.frame(Titanic)

data <- data %.>% 
  select_rows_nse(., 
                  (Class == "Crew") & 
                    (Sex == "Female") & 
                    (Age == "Adult")) %.>%
  orderby(., "Survived")

print(data)
```

    ##    Class    Sex   Age Survived Freq
    ## 1:  Crew Female Adult       No    3
    ## 2:  Crew Female Adult      Yes   20

``` r
universe <- c(rep(0, data$Freq[data$Survived=="No"]),
              rep(1, data$Freq[data$Survived=="Yes"]))

print(universe)
```

    ##  [1] 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

``` r
summary1 <- function(x, scale_adjustment_table) {
  naive_var <- mean((mean(x)-x)^2)
  sd_est <- sd(x)
  # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
  # correctly correct for normal, not binomomial as we have here!
  if(length(scale_adjustment_table) != (1+length(x))) {
    stop("wrong sized scale_adjustment table")
  }
  data.frame(mean = mean(x),
             var = var(x),
             sd = sd_est,
             naive_var = naive_var,
             naive_sd = sqrt(naive_var),
             adj_sd = sqrt(naive_var)*scale_adjustment_table[[sum(x)+1]])
}

tabu <- solve_for_scaling_table(length(universe), naive_sd_fun)
print(tabu)
```

    ##  [1] 1.7446284 1.7446284 0.9121849 0.9460220 1.0601573 1.0767995 1.0429760
    ##  [8] 1.0150451 1.0091154 1.0163886 1.0250212 1.0294205 1.0289172 1.0239361
    ## [15] 1.0156549 1.0094069 1.0161304 1.0437471 1.0763276 1.0590666 0.9459633
    ## [22] 0.9131938 1.7441983 1.7446284

``` r
su <- summary1(
  universe, 
  tabu)
print(su)
```

    ##        mean       var        sd naive_var  naive_sd    adj_sd
    ## 1 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812 0.3185826

``` r
n <- length(universe)
Bessel_corrected_var <- (n/(n-1))*su$naive_var
print(Bessel_corrected_var)
```

    ## [1] 0.1185771

``` r
Bessel_corrected_sd <- sqrt(Bessel_corrected_var)
print(Bessel_corrected_sd)
```

    ## [1] 0.3443502

``` r
samp_size <- 5

mk_f <- function(universe, samp_size, summary1) {
  force(universe)
  force(samp_size)
  force(summary1)
  scale_adjustment_table <- 
    solve_for_scaling_table(samp_size, naive_sd_fun)
  f <- function(i) {
    sample <- universe[sample.int(length(universe), 
                                  samp_size, 
                                  replace = TRUE)]
    summary1(sample, scale_adjustment_table)
  }
}

tabs <- solve_for_scaling_table(samp_size, naive_sd_fun)
print(tabs)
```

    ## [1] 2.0171146 2.0171146 0.7335087 0.7361976 2.0158886 2.0171146

``` r
f <- mk_f(universe, samp_size, summary1)


cl <- parallel::makeCluster(parallel::detectCores())
res <- parallel::parLapply(cl, 1:100000, f)
parallel::stopCluster(cl)
res <- do.call(rbind, res)

sums <- as.data.frame(lapply(res, mean))
print(sums)
```

    ##       mean      var        sd naive_var  naive_sd    adj_sd
    ## 1 0.869094 0.113833 0.2387239 0.0910664 0.2135212 0.3498729

``` r
sqrt(su$mean*(1-su$mean))
```

    ## [1] 0.3367812
