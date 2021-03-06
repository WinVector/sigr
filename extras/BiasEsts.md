Bias Ests
================

``` r
library("rqdatatable")  # requires version rqdatatable 1.1.2 or newer and rquery 1.2.1 or newer
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
set.seed(2018)

naive_sd_fun <- function(x) {
  sqrt(mean((mean(x)-x)^2))
}

Bessel_sd_fun <- function(x) {
  sd(x)
}

eval_scale_adjustment_table_ratio <- function(n, p, sd_fun) {
  obs <- 0:n
  probs <- dbinom(obs, size = n, prob = p)
  evals <- vapply(obs,
                  function(ki) {
                    sd_fun(c(rep(1, ki), rep(0, n-ki)))
                  }, numeric(1))
  sd_target <- sqrt(p*(1-p))
  sum(probs*evals)/sd_target
}

eval_scale_adjustment_table_diff <- function(n, p, sd_fun) {
  obs <- 0:n
  probs <- dbinom(obs, size = n, prob = p)
  evals <- vapply(obs,
                  function(ki) {
                    sd_fun(c(rep(1, ki), rep(0, n-ki)))
                  }, numeric(1))
  sd_target <- sqrt(p*(1-p))
  sum(probs*evals)-sd_target
}


# related to Wald mini/max type ideas and:
# http://www.win-vector.com/blog/2014/07/frequenstist-inference-only-seems-easy/
# https://mathoverflow.net/questions/177574/existence-of-solutions-of-a-polynomial-system 
# think this is under-determined, so could ask for symmetry or all coefs near 1.
# also a lot like a Chebyshev polynomial (or integral of one), or upside down beta.
# can also try priors like beta(0.5,0.5)
# could also solve for affine version- where we allow addition in addition to scaling.
# must have a really nice form if we integrate the ps-term (likely a calculus of variations solution).
solve_for_scaling_table <- function(n, sd_fun = naive_sd_fun, 
                                    intercept = TRUE,
                                    excess_resolution = 16) {
  if(n<2) {
    return(rep(1, n+1))
  }
  obs <- 1:(n-1)
  vars <- paste0("s_", 1:(n-1))
  ps <- seq(1/(excess_resolution*n), 
            (excess_resolution*n-1)/(excess_resolution*n), 
            by = 1/(excess_resolution*n))
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
  const <- 0.0
  if(length(vars)<=1) {
    m <- lm(mk_formula("target", vars, intercept = intercept), data= d)
    soln <- as.numeric(m$coefficients[vars]) + 1
    if(intercept) {
      const = m$coefficients[["(Intercept)"]]
    } 
  } else {
    m <- glmnet(as.matrix(d[, vars, drop = FALSE]), d$target, 
                alpha=0, lambda=1e-6, family = "gaussian", intercept = intercept,
                weights = dbeta(ps, 0.5, 0.5),
                # lower.limits = -10,
                standardize = FALSE)
    soln <- as.numeric(m$beta) + 1
    if(intercept) {
      const = m$a0
    } 
  }
  multipliers = c(0, soln, 0)
  ests <- vapply(0:n,
                 function(k) {
                   sd_fun(c(rep(1, k), rep(0, n-k)))*multipliers[[k+1]] + const
                 }, numeric(1))
  function(x) {
    if(length(x)!=n) {
      stop("wrong length")
    }
    if(length(setdiff(x, c(0,1)))>0) {
      stop("non zero/one value")
    }
    k = sum(x)
    ests[[k+1]]
  }
}



plot_estimate_ratios <- function(n, ssoln) {
  adjs <- data.frame(p = seq(0, 1, by = 0.01))
  adjs$joint_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_ratio(n, pi, sd_fun = ssoln)
    }, numeric(1))
  adjs$Bessel_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_ratio(n, pi, sd_fun = Bessel_sd_fun)
    }, numeric(1))
  adjs$unscaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_ratio(n, pi, sd_fun = naive_sd_fun)
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
    ggtitle("ratio betweeen estimated and theoretical standard deviations",
            subtitle = paste0("n= ", n))
}

plot_estimate_diffs <- function(n, ssoln) {
  adjs <- data.frame(p = seq(0, 1, by = 0.01))
  adjs$joint_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_diff(n, pi, sd_fun = ssoln)
    }, numeric(1))
  adjs$Bessel_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_diff(n, pi, sd_fun = Bessel_sd_fun)
    }, numeric(1))
  adjs$unscaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_diff(n, pi, sd_fun = naive_sd_fun)
    }, numeric(1))
  
  
  adjsp <- unpivot_to_blocks(
    adjs, 
    nameForNewKeyColumn = "method", 
    nameForNewValueColumn = "difference", 
    columnsToTakeFrom = c("joint_scaled", "unscaled", "Bessel_scaled"))
  adjsp <- adjsp[!is.na(adjsp$difference), , drop = FALSE]
  adjsp$method <- reorder(factor(adjsp$method), -adjsp$difference)
  
  
  ggplot(data = adjsp, mapping = aes(x = p, y = difference, color = method)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red", linetype=2) + 
    scale_color_brewer(palette = "Dark2") + 
    ggtitle("difference betweeen estimated and theoretical standard deviations",
            subtitle = paste0("n=", n))
}


plot_estimates <- function(n, ssoln) {
  adjs <- data.frame(k = 0:n)
  adjs$joint_scaled <- vapply(0:n,
                              function(k) {
                                ssoln(c(rep(1, k), rep(0, n-k)))
                              }, numeric(1))
  adjs$Bessel_scaled <- vapply(0:n,
                               function(k) {
                                 sd(c(rep(1, k), rep(0, n-k)))
                               }, numeric(1))
  adjs$naive_sd <- vapply(0:n,
                          function(k) {
                            naive_sd_fun(c(rep(1, k), rep(0, n-k)))
                          }, numeric(1))
  adjp <- cdata::unpivot_to_blocks(adjs, 
                                   nameForNewKeyColumn = "estimation_method",
                                   nameForNewValueColumn = "estimate",
                                   columnsToTakeFrom = qc(joint_scaled, Bessel_scaled,  naive_sd))
  ggplot(data = adjp, mapping = aes(x = k, y = estimate, color = estimation_method, group = estimation_method)) +
    geom_point() +
    geom_linerange(aes(ymin = 0, ymax = estimate)) +
    scale_color_brewer(palette = "Dark2") + 
    facet_wrap(~estimation_method, ncol=1) +
    ggtitle("standard deviation estimates",
            subtitle = paste0("n=", n))
}

cl <- parallel::makeCluster(parallel::detectCores())

intercept <- TRUE

print(paste("*******", intercept))
```

    ## [1] "******* TRUE"

``` r
for(n in c(2, 3, 4, 5, 10, 20, 100)) {
  print("******")
  print(n)
  tab <- solve_for_scaling_table(n, intercept = intercept)
  plot_estimates(n, tab) %.>% print(.)
  plot_estimate_ratios(n, tab) %.>% print(.)
  plot_estimate_diffs(n, tab) %.>% print(.)
}
```

    ## [1] "******"
    ## [1] 2

<img src="BiasEsts_files/figure-markdown_github/run-1.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-2.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-3.png" width="768" />

    ## [1] "******"
    ## [1] 3

<img src="BiasEsts_files/figure-markdown_github/run-4.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-5.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-6.png" width="768" />

    ## [1] "******"
    ## [1] 4

<img src="BiasEsts_files/figure-markdown_github/run-7.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-8.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-9.png" width="768" />

    ## [1] "******"
    ## [1] 5

<img src="BiasEsts_files/figure-markdown_github/run-10.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-11.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-12.png" width="768" />

    ## [1] "******"
    ## [1] 10

<img src="BiasEsts_files/figure-markdown_github/run-13.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-14.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-15.png" width="768" />

    ## [1] "******"
    ## [1] 20

<img src="BiasEsts_files/figure-markdown_github/run-16.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-17.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-18.png" width="768" />

    ## [1] "******"
    ## [1] 100

<img src="BiasEsts_files/figure-markdown_github/run-19.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-20.png" width="768" /><img src="BiasEsts_files/figure-markdown_github/run-21.png" width="768" />

``` r
data <- as.data.frame(Titanic)

data <- data %.>% 
  select_rows(., 
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
summary1 <- function(x, tabu) {
  scale_corrected_sd <- tabu(x)
  naive_var <- mean((mean(x)-x)^2)
  sd_est <- sd(x)
  # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
  # correctly correct for normal, not binomomial as we have here!
  data.frame(mean = mean(x),
             var = var(x),
             sd = sd_est,
             naive_var = naive_var,
             naive_sd = sqrt(naive_var),
             scale_corrected_sd = scale_corrected_sd)
}



samp_size <- 5

mk_f <- function(universe, samp_size, summary1, intercept) {
  force(universe)
  force(samp_size)
  force(summary1)
  force(intercept)
  scale_adjustment_table <- 
    solve_for_scaling_table(samp_size, naive_sd_fun, intercept = intercept)
  f <- function(i) {
    sample <- universe[sample.int(length(universe), 
                                  samp_size, 
                                  replace = TRUE)]
    summary1(sample, scale_adjustment_table)
  }
}


f <- mk_f(universe, samp_size, summary1, intercept)

print("universe")
```

    ## [1] "universe"

``` r
tabu <- solve_for_scaling_table(length(universe), naive_sd_fun, intercept = intercept)
su <- summary1(universe, tabu)
n <- length(universe)

print("actual sd (non-sample)")
```

    ## [1] "actual sd (non-sample)"

``` r
print(su)
```

    ##        mean       var        sd naive_var  naive_sd scale_corrected_sd
    ## 1 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812          0.3384957

``` r
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
res <- parallel::parLapply(cl, 1:100000, f)
res <- do.call(rbind, res)
sums <- as.data.frame(lapply(res, mean))



res$Bessel_sd <- res$sd
res$Bessel_var <- res$var

rpv <- res %.>% 
  extend(., Bessel_var := Bessel_sd^2) %.>% 
  unpivot_to_blocks(.,
                    nameForNewKeyColumn = "estimation_method",
                    nameForNewValueColumn = "var_estimate",
                    columnsToTakeFrom = c("naive_var", "Bessel_var"))
efv <- project(rpv, 
               var_estimate = mean(var_estimate), 
               groupby = "estimation_method")  

rpv %.>%
  project(., variance_of_estimate = var(var_estimate),
          groupby = "estimation_method")
```

    ##    estimation_method variance_of_estimate
    ## 1:         naive_var          0.008772391
    ## 2:        Bessel_var          0.013706861

``` r
p3v <- ggplot(data = rpv, aes(x = var_estimate)) +
  geom_histogram(bins = 30)  + 
  geom_vline(xintercept = su$naive_var, color = "red", alpha = 0.5, size=3, linetype =3) + 
  geom_vline(xintercept = su$naive_var, color = "red", alpha = 0.5) + 
  geom_vline(data = efv, aes(xintercept = var_estimate), linetype=2) +
  facet_wrap(~estimation_method, ncol=1) +
  ggtitle("distribution of variance estimates by method",
          subtitle = "average shown in black, original universe variance value in red")
print(p3v)
```

<img src="BiasEsts_files/figure-markdown_github/run-22.png" width="768" />

``` r
rp <- unpivot_to_blocks(res,
                        nameForNewKeyColumn = "estimation_method",
                        nameForNewValueColumn = "sd_estimate",
                        columnsToTakeFrom = c(
                          "scale_corrected_sd", "naive_sd", "Bessel_sd"))

ef <- project(rp, 
              sd_estimate = mean(sd_estimate), 
              groupby = "estimation_method")

rp %.>%
  project(., variance_of_estimate = var(sd_estimate),
          groupby = "estimation_method")
```

    ##     estimation_method variance_of_estimate
    ## 1: scale_corrected_sd           0.05763954
    ## 2:           naive_sd           0.04554781
    ## 3:          Bessel_sd           0.05693477

``` r
rpb <- rp[rp$estimation_method %in% qc(Bessel_sd, naive_sd), , drop = FALSE]
efb <- ef[ef$estimation_method %in% qc(Bessel_sd, naive_sd), , drop = FALSE]
p3b <- ggplot(data = rpb, aes(x = sd_estimate)) +
  geom_histogram(bins = 30)  + 
  geom_vline(xintercept = su$naive_sd, color = "red", alpha = 0.5, size=2, linetype =3) + 
  geom_vline(xintercept = su$naive_sd, color = "red", alpha = 0.5) + 
  geom_vline(data = efb, aes(xintercept = sd_estimate), linetype=2) +
  facet_wrap(~estimation_method, ncol=1) +
  ggtitle("distribution of sd estimates by method",
          subtitle = "average shown in black, original universe sd value in red")
print(p3b)
```

<img src="BiasEsts_files/figure-markdown_github/run-23.png" width="768" />

``` r
rpa <- rp[rp$estimation_method %in% qc(Bessel_sd, naive_sd, scale_corrected_sd), , drop = FALSE]
efa <- ef[ef$estimation_method %in% qc(Bessel_sd, naive_sd, scale_corrected_sd), , drop = FALSE]
p3 <- ggplot(data = rpa, aes(x = sd_estimate)) +
  geom_histogram(bins = 30)  + 
  geom_vline(xintercept = su$naive_sd, color = "red", alpha = 0.5, size=2, linetype =3) + 
  geom_vline(xintercept = su$naive_sd, color = "red", alpha = 0.5) + 
  geom_vline(data = efa, aes(xintercept = sd_estimate), linetype=2) +
  facet_wrap(~estimation_method, ncol=1) +
  ggtitle("distribution of sd estimates by method",
          subtitle = "average shown in black, original universe sd value in red")
print(p3)
```

<img src="BiasEsts_files/figure-markdown_github/run-24.png" width="768" />

``` r
rp %.>%
  project(., variance_of_estimate = var(sd_estimate),
          groupby = "estimation_method")
```

    ##     estimation_method variance_of_estimate
    ## 1: scale_corrected_sd           0.05763954
    ## 2:           naive_sd           0.04554781
    ## 3:          Bessel_sd           0.05693477

``` r
parallel::stopCluster(cl)
```
