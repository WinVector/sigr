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
solve_for_scaling_table <- function(n, sd_fun = naive_sd_fun) {
  if(n<2) {
    return(rep(1, n+1))
  }
  obs <- 1:(n-1)
  vars <- paste0("s_", 1:(n-1))
  excess_resolution <- 2
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
              alpha=0, lambda=1e-8, family = "gaussian", intercept = FALSE,
              weights = dbeta(ps, 0.5, 0.5),
              lower.limits = -10,
              standardize = FALSE)
  soln <- as.numeric(m$beta) + 1
  mx <- max(soln)
  c(mx, soln, mx)
}

solve_for_scaling_table(100)
```

    ##   [1] 1.8213318 1.8181678 0.8109601 0.9197317 1.1129045 1.1003459 1.0233049
    ##   [8] 0.9844096 0.9908930 1.0128501 1.0262441 1.0252522 1.0163959 1.0082936
    ##  [15] 1.0053160 1.0068424 1.0097841 1.0113923 1.0106880 1.0083724 1.0059089
    ##  [22] 1.0045523 1.0047903 1.0062947 1.0082301 1.0096880 1.0100479 1.0091581
    ##  [29] 1.0073210 1.0051331 1.0032614 1.0022317 1.0022886 1.0033514 1.0050642
    ##  [36] 1.0069129 1.0083726 1.0090445 1.0087513 1.0075711 1.0058071 1.0039033
    ##  [43] 1.0023314 1.0014740 1.0015333 1.0024842 1.0040833 1.0059294 1.0075610
    ##  [50] 1.0085688 1.0086961 1.0079028 1.0063791 1.0045022 1.0027465 1.0015669
    ##  [57] 1.0012830 1.0019924 1.0035369 1.0055334 1.0074646 1.0088114 1.0091953
    ##  [64] 1.0084916 1.0068818 1.0048230 1.0029348 1.0018262 1.0019073 1.0032406
    ##  [71] 1.0054850 1.0079645 1.0098617 1.0104901 1.0095672 1.0073858 1.0047953
    ##  [78] 1.0029524 1.0028816 1.0049788 1.0086670 1.0124218 1.0142985 1.0128924
    ##  [85] 1.0083929 1.0031416 1.0010522 1.0055737 1.0166958 1.0286843 1.0312171
    ##  [92] 1.0162191 0.9893809 0.9780701 1.0183165 1.1039017 1.1204432 0.9175578
    ##  [99] 0.8058372 1.8213318 1.8213318

``` r
solve_for_scaling_table(10)
```

    ##  [1]  2.19586703  2.19317182 -0.06256223  1.89976626  0.86643521
    ##  [6]  0.88921078  0.85202395  1.91632855 -0.07192223  2.19586703
    ## [11]  2.19586703

``` r
solve_for_scaling_table(5)
```

    ## [1] 1.9093087 1.9093087 0.8005699 0.8037763 1.9078200 1.9093087

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

    ##  [1] 2.13590753 2.13590753 0.09422216 1.59689344 1.09879480 0.76217929
    ##  [7] 1.06407856 1.16815855 0.99387764 0.92022543 1.03808551 1.12478461
    ## [13] 1.03591093 0.90708466 0.95574848 1.14704538 1.17113062 0.89472372
    ## [19] 0.77706477 1.27998049 1.42456483 0.16594193 2.12171330 2.13590753

``` r
su <- summary1(
  universe, 
  tabu)
print(su)
```

    ##        mean       var        sd naive_var  naive_sd    adj_sd
    ## 1 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812 0.4797666

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

    ## [1] 1.9093087 1.9093087 0.8005699 0.8037763 1.9078200 1.9093087

``` r
f <- mk_f(universe, samp_size, summary1)


cl <- parallel::makeCluster(parallel::detectCores())
res <- parallel::parLapply(cl, 1:100000, f)
res <- do.call(rbind, res)

as.data.frame(lapply(res, mean))
```

    ##       mean      var        sd naive_var  naive_sd    adj_sd
    ## 1 0.869462 0.113391 0.2375928 0.0907128 0.2125095 0.3352917
