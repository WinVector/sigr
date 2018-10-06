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

eval_scale_adjustment_table_ratio <- function(scale_adjustment_table, 
                                        const,
                                        p, sd_fun = naive_sd_fun) {
  n <- length(scale_adjustment_table)-1
  obs <- 0:n
  probs <- dbinom(obs, size = n, prob = p)
  evals <- vapply(obs,
                function(ki) {
                  sd_fun(c(rep(1, ki), rep(0, n-ki)))*scale_adjustment_table[[ki+1]] + const
                }, numeric(1))
  sd_target <- sqrt(p*(1-p))
  sum(probs*evals)/sd_target
}

eval_scale_adjustment_table_diff <- function(scale_adjustment_table, 
                                        const,
                                        p, sd_fun = naive_sd_fun) {
  n <- length(scale_adjustment_table)-1
  obs <- 0:n
  probs <- dbinom(obs, size = n, prob = p)
  evals <- vapply(obs,
                function(ki) {
                  sd_fun(c(rep(1, ki), rep(0, n-ki)))*scale_adjustment_table[[ki+1]] + const
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
                                    intercept = FALSE,
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
  list(multipliers = c(0, soln, 0), intercept = intercept, const = const)
}

plot_multipliers <- function(ssoln) {
  tab <- ssoln$multipliers
  const <- ssoln$const
  const_str <- sprintf("%.4g", const)
  n <- length(tab)-1
  dtab <- data.frame(multiplier = tab)
  dtab$observed_count <- 0:(nrow(dtab)-1)
  breaks <- 0:n
  mod <- 10
  while(length(breaks)>20) {
    breaks <- sort(unique(breaks - breaks%%mod))
    mod <- mod*10
  }
  ggplot() + 
    geom_rect(aes(xmin=0, xmax=n, ymin=1-const, ymax=1), alpha = 0.5) +
    geom_point(data = dtab, 
               mapping = aes(x = observed_count,
                             y = multiplier),
               size=3) +
    geom_linerange(data = dtab,
                   mapping = aes(x = observed_count,
                                 ymin = 0,
                                 ymax = multiplier)) + 
    geom_hline(yintercept = const) + 
    geom_hline(yintercept = 1, color = "darkgray", linetype=2) + 
    ggtitle(paste0("std-deviation multiplier window, n=", n, " + ", const_str)) +
    scale_x_continuous(breaks = breaks)
}

plot_estimate_ratios <- function(ssoln) {
  tab <- ssoln$multipliers
  const <- ssoln$const
  const_str <- sprintf("%.4g", const)
  n <- length(tab)-1
  adjs <- data.frame(p = seq(0, 1, by = 0.01))
  adjs$joint_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_ratio(tab, const, pi)
    }, numeric(1))
  adjs$Bessel_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_ratio(rep(1, length(tab)), 0, pi, sd_fun = Bessel_sd_fun)
    }, numeric(1))
  adjs$unscaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_ratio(rep(1, length(tab)), 0, pi)
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
            subtitle = paste0("n= ", n, " + ", const_str))
}

plot_estimate_diffs <- function(ssoln) {
  tab <- ssoln$multipliers
  const <- ssoln$const
  const_str <- sprintf("%.4g", const)
  n <- length(tab)-1
  adjs <- data.frame(p = seq(0, 1, by = 0.01))
  adjs$joint_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_diff(tab, const, pi)
    }, numeric(1))
  adjs$Bessel_scaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_diff(rep(1, length(tab)), 0, pi, sd_fun = Bessel_sd_fun)
    }, numeric(1))
  adjs$unscaled <- vapply(
    adjs$p,
    function(pi) {
      eval_scale_adjustment_table_diff(rep(1, length(tab)), 0, pi)
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
            subtitle = paste0("n=", n, " + ", const_str))
}

cl <- parallel::makeCluster(parallel::detectCores())


for(intercept in c(FALSE, TRUE)) {
  print(paste("*******", intercept))
  for(n in c(2, 3, 4, 5, 10, 20, 100)) {
    print("******")
    print(n)
    tab <- solve_for_scaling_table(n, intercept = intercept)
    print(tab)
    plot_multipliers(tab) %.>% print(.)
    plot_estimate_ratios(tab) %.>% print(.)
    plot_estimate_diffs(tab) %.>% print(.)
  }
  
  data <- as.data.frame(Titanic)
  
  data <- data %.>% 
    select_rows_nse(., 
                    (Class == "Crew") & 
                      (Sex == "Female") & 
                      (Age == "Adult")) %.>%
    orderby(., "Survived")
  
  print(data)
  
  universe <- c(rep(0, data$Freq[data$Survived=="No"]),
                rep(1, data$Freq[data$Survived=="Yes"]))
  
  print(universe)
  
  
  
  summary1 <- function(x, tabu) {
    scale_adjustment_table <- tabu$multipliers
    const <- tabu$const
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
               adj_sd = sqrt(naive_var)*scale_adjustment_table[[sum(x)+1]] + const)
  }
  
  print("universe")
  tabu <- solve_for_scaling_table(length(universe), naive_sd_fun, intercept = intercept)
  print(tabu)
  su <- summary1(
    universe, 
    tabu)
  print(su)
  
  n <- length(universe)
  Bessel_corrected_var <- (n/(n-1))*su$naive_var
  print(Bessel_corrected_var)
  
  Bessel_corrected_sd <- sqrt(Bessel_corrected_var)
  print(Bessel_corrected_sd)
  
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
  
  
  res <- parallel::parLapply(cl, 1:100000, f)
  res <- do.call(rbind, res)
  
  print("aggregates")
  sums <- as.data.frame(lapply(res, mean))
  print(sums)
  print("sds of aggregates")
  sdss <- as.data.frame(lapply(res, sd))
  print(sdss)
  print("stddev est from aggregate mean")
  print(sqrt(su$mean*(1-su$mean)))
  
  p1 <- ggplot(data= res, aes(x = naive_sd)) +
    geom_density() + 
    geom_vline(xintercept = sums$naive_sd) +
    geom_vline(xintercept = su$naive_sd, color = "red") + 
    xlim(0, 1) +
    ggtitle(paste0("distribution of naive sd"),
            subtitle = "average shown in black, universe value in red")
  print(p1)

  p2 <- ggplot(data= res, aes(x = adj_sd)) +
    geom_density() + 
    geom_vline(xintercept = sums$adj_sd) +
    geom_vline(xintercept = su$naive_sd, color = "red") + 
    xlim(0, 1) +
    ggtitle(paste0("distribution of ", ifelse(intercept, "affine", "scale")," adjusted sd"),
            subtitle = "average shown in black, universe value in red")
  print(p2)
}
```

    ## [1] "******* FALSE"
    ## [1] "******"
    ## [1] 2
    ## $multipliers
    ## [1] 0.000000 2.208666 0.000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-1.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-2.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-3.png" width="768" />

    ## [1] "******"
    ## [1] 3
    ## $multipliers
    ## [1] 0.000000 1.598742 1.598715 0.000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-4.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-5.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-6.png" width="768" />

    ## [1] "******"
    ## [1] 4
    ## $multipliers
    ## [1]  0.00000000  2.28787190 -0.08011322  2.28791183  0.00000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-7.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-8.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-9.png" width="768" />

    ## [1] "******"
    ## [1] 5
    ## $multipliers
    ## [1] 0.0000000 2.0122346 0.7368832 0.7390467 2.0112480 0.0000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-10.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-11.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-12.png" width="768" />

    ## [1] "******"
    ## [1] 10
    ## $multipliers
    ##  [1] 0.0000000 2.0346467 0.4802704 1.2042416 1.1523668 0.9286623 1.1547608
    ##  [8] 1.2009477 0.4825954 2.0338301 0.0000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-13.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-14.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-15.png" width="768" />

    ## [1] "******"
    ## [1] 20
    ## $multipliers
    ##  [1] 0.0000000 1.8036584 0.8468381 0.9534401 1.0883555 1.0833304 1.0326424
    ##  [8] 1.0061200 1.0117913 1.0296074 1.0391542 1.0312949 1.0125694 1.0047259
    ## [15] 1.0313315 1.0843746 1.0897463 0.9523454 0.8464031 1.8041832 0.0000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-16.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-17.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-18.png" width="768" />

    ## [1] "******"
    ## [1] 100
    ## $multipliers
    ##   [1] 0.000000 1.211359 1.106920 1.048873 1.024846 1.017004 1.015263
    ##   [8] 1.015079 1.014813 1.014122 1.013120 1.012014 1.010964 1.010056
    ##  [15] 1.009318 1.008737 1.008284 1.007926 1.007632 1.007378 1.007150
    ##  [22] 1.006935 1.006730 1.006534 1.006345 1.006165 1.005995 1.005838
    ##  [29] 1.005693 1.005562 1.005444 1.005340 1.005248 1.005167 1.005098
    ##  [36] 1.005037 1.004985 1.004939 1.004899 1.004864 1.004832 1.004802
    ##  [43] 1.004775 1.004749 1.004724 1.004701 1.004680 1.004661 1.004645
    ##  [50] 1.004632 1.004623 1.004619 1.004620 1.004626 1.004637 1.004654
    ##  [57] 1.004677 1.004705 1.004738 1.004776 1.004818 1.004863 1.004913
    ##  [64] 1.004965 1.005021 1.005082 1.005147 1.005217 1.005294 1.005379
    ##  [71] 1.005473 1.005578 1.005695 1.005824 1.005968 1.006125 1.006295
    ##  [78] 1.006479 1.006675 1.006883 1.007106 1.007348 1.007618 1.007930
    ##  [85] 1.008308 1.008779 1.009375 1.010124 1.011034 1.012078 1.013168
    ##  [92] 1.014144 1.014804 1.015037 1.015194 1.016920 1.024769 1.048830
    ##  [99] 1.106932 1.211422 0.000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0

<img src="BiasEsts_files/figure-gfm/run-19.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-20.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-21.png" width="768" />

    ##    Class    Sex   Age Survived Freq
    ## 1:  Crew Female Adult       No    3
    ## 2:  Crew Female Adult      Yes   20
    ##  [1] 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [1] "universe"
    ## $multipliers
    ##  [1] 0.0000000 1.7507295 0.9059314 0.9453566 1.0628968 1.0781336 1.0423358
    ##  [8] 1.0139508 1.0086728 1.0166048 1.0253736 1.0295972 1.0290340 1.0241831
    ## [15] 1.0158508 1.0090520 1.0151156 1.0430754 1.0775732 1.0617949 0.9453608
    ## [22] 0.9069580 1.7502615 0.0000000
    ## 
    ## $intercept
    ## [1] FALSE
    ## 
    ## $const
    ## [1] 0
    ## 
    ##        mean       var        sd naive_var  naive_sd    adj_sd
    ## 1 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812 0.3183797
    ## [1] 0.1185771
    ## [1] 0.3443502
    ## [1] "aggregates"
    ##       mean      var        sd naive_var  naive_sd    adj_sd
    ## 1 0.870412 0.112782 0.2364982 0.0902256 0.2115304 0.3459839
    ## [1] "sds of aggregates"
    ##        mean       var        sd  naive_var  naive_sd    adj_sd
    ## 1 0.1502317 0.1168784 0.2384349 0.09350271 0.2132626 0.3717854
    ## [1] "stddev est from aggregate mean"
    ## [1] 0.3367812

<img src="BiasEsts_files/figure-gfm/run-22.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-23.png" width="768" />

    ## [1] "******* TRUE"
    ## [1] "******"
    ## [1] 2
    ## $multipliers
    ## [1] 0.000000 1.382903 0.000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ## [1] 0.1653139

<img src="BiasEsts_files/figure-gfm/run-24.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-25.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-26.png" width="768" />

    ## [1] "******"
    ## [1] 3
    ## $multipliers
    ## [1] 0.000000 1.049043 1.049043 0.000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##        s0 
    ## 0.1465111

<img src="BiasEsts_files/figure-gfm/run-27.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-28.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-29.png" width="768" />

    ## [1] "******"
    ## [1] 4
    ## $multipliers
    ## [1] 0.0000000 1.3858312 0.5018465 1.3858676 0.0000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##        s0 
    ## 0.1031395

<img src="BiasEsts_files/figure-gfm/run-30.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-31.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-32.png" width="768" />

    ## [1] "******"
    ## [1] 5
    ## $multipliers
    ## [1] 0.0000000 1.2637473 0.7857704 0.7868919 1.2632237 0.0000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##         s0 
    ## 0.09633953

<img src="BiasEsts_files/figure-gfm/run-33.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-34.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-35.png" width="768" />

    ## [1] "******"
    ## [1] 10
    ## $multipliers
    ##  [1] 0.0000000 1.2292346 0.7950156 0.8928657 0.9396809 0.9196452 0.9448599
    ##  [8] 0.8874666 0.7975359 1.2287340 0.0000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##         s0 
    ## 0.06806992

<img src="BiasEsts_files/figure-gfm/run-36.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-37.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-38.png" width="768" />

    ## [1] "******"
    ## [1] 20
    ## $multipliers
    ##  [1] 0.0000000 1.1478825 0.8861183 0.8646562 0.9156349 0.9380356 0.9320465
    ##  [8] 0.9215980 0.9200283 0.9257682 0.9302637 0.9279211 0.9216743 0.9206278
    ## [15] 0.9298107 0.9378306 0.9175794 0.8649898 0.8847579 1.1483603 0.0000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##         s0 
    ## 0.04929705

<img src="BiasEsts_files/figure-gfm/run-39.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-40.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-41.png" width="768" />

    ## [1] "******"
    ## [1] 100
    ## $multipliers
    ##   [1] 0.0000000 1.0820446 1.0156744 0.9785900 0.9650872 0.9628812 0.9646592
    ##   [8] 0.9671003 0.9690514 0.9703297 0.9710934 0.9715576 0.9718912 0.9721964
    ##  [15] 0.9725205 0.9728751 0.9732528 0.9736392 0.9740198 0.9743836 0.9747233
    ##  [22] 0.9750356 0.9753202 0.9755785 0.9758130 0.9760267 0.9762223 0.9764026
    ##  [29] 0.9765698 0.9767258 0.9768718 0.9770092 0.9771388 0.9772610 0.9773762
    ##  [36] 0.9774848 0.9775866 0.9776816 0.9777697 0.9778506 0.9779242 0.9779902
    ##  [43] 0.9780484 0.9780988 0.9781412 0.9781758 0.9782027 0.9782222 0.9782345
    ##  [50] 0.9782401 0.9782393 0.9782325 0.9782200 0.9782020 0.9781788 0.9781504
    ##  [57] 0.9781166 0.9780773 0.9780321 0.9779807 0.9779225 0.9778570 0.9777838
    ##  [64] 0.9777023 0.9776121 0.9775129 0.9774045 0.9772868 0.9771597 0.9770234
    ##  [71] 0.9768778 0.9767229 0.9765584 0.9763837 0.9761977 0.9759988 0.9757849
    ##  [78] 0.9755532 0.9753007 0.9750244 0.9747222 0.9743934 0.9740402 0.9736685
    ##  [85] 0.9732883 0.9729128 0.9725555 0.9722233 0.9719048 0.9715536 0.9710692
    ##  [92] 0.9702857 0.9689916 0.9670333 0.9645976 0.9628402 0.9650801 0.9786211
    ##  [99] 1.0157288 1.0820817 0.0000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##         s0 
    ## 0.01430012

<img src="BiasEsts_files/figure-gfm/run-42.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-43.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-44.png" width="768" />

    ##    Class    Sex   Age Survived Freq
    ## 1:  Crew Female Adult       No    3
    ## 2:  Crew Female Adult      Yes   20
    ##  [1] 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [1] "universe"
    ## $multipliers
    ##  [1] 0.0000000 1.1353235 0.8985872 0.8699394 0.9114213 0.9341853 0.9341982
    ##  [8] 0.9277480 0.9244943 0.9258603 0.9295613 0.9327943 0.9334894 0.9309085
    ## [15] 0.9263773 0.9235439 0.9264400 0.9342560 0.9355362 0.9118821 0.8688254
    ## [22] 0.8985333 1.1357127 0.0000000
    ## 
    ## $intercept
    ## [1] TRUE
    ## 
    ## $const
    ##         s0 
    ## 0.04589171 
    ## 
    ##         mean       var        sd naive_var  naive_sd    adj_sd
    ## s0 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812 0.3384957
    ## [1] 0.1185771
    ## [1] 0.3443502
    ## [1] "aggregates"
    ##      mean     var        sd naive_var  naive_sd    adj_sd
    ## 1 0.87031 0.11267 0.2361936  0.090136 0.2112579 0.3333504
    ## [1] "sds of aggregates"
    ##        mean      var        sd  naive_var  naive_sd    adj_sd
    ## 1 0.1507804 0.116947 0.2385019 0.09355762 0.2133226 0.2401599
    ## [1] "stddev est from aggregate mean"
    ## [1] 0.3367812

<img src="BiasEsts_files/figure-gfm/run-45.png" width="768" /><img src="BiasEsts_files/figure-gfm/run-46.png" width="768" />

``` r
parallel::stopCluster(cl)
```
