Bias Ests
================

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
library("cdata")
library("ggplot2")

set.seed(32535)

naive_sd_fun <- function(x) {
  # also equals (sum(x)/length(x))*(1-sum(x)/length(x))
  sqrt(mean((mean(x)-x)^2))
}

Bessel_sd_fun <- function(x) {
  sd(x)
}
  
sd_attinuation_binomial <- function(k, n, sd_fun = naive_sd_fun) {
  obs <- 0:n
  probs <- dbinom(obs, size = n, prob = k/n)
  sds <- vapply(obs,
                function(ki) {
                  sd_fun(c(rep(1, ki), rep(0, n-ki)))
                }, numeric(1))
  observed <- sum(probs*sds)
  sd_target <- sd_fun(c(rep(1, k), rep(0, n-k)))
  attenuation <- observed/sd_target
  attenuation
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

mk_scale_adjustment_table <- function(n, sd_fun = naive_sd_fun) {
  v <- vapply(0:n,
         function(k) {
           1/sd_attinuation_binomial(k, n, sd_fun)
         }, numeric(1))
  # can't correct 0 or all cases- just put a nice value there.
  v[is.na(v) | is.nan(v) | is.infinite(v) | (v<=0)] <- 0
  v
}



tab <- mk_scale_adjustment_table(10)
adjs <- data.frame(p = seq(0, 1, by = 0.01))
adjs$self_scaled <- vapply(
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
  nameForNewValueColumn = "scale", columnsToTakeFrom = c("self_scaled", "unscaled", "Bessel_scaled"))
adjsp <- adjsp[!is.na(adjsp$scale), , drop = FALSE]
adjsp$method <- reorder(factor(adjsp$method), -adjsp$scale)


ggplot(data = adjsp, mapping = aes(x = p, y = scale, color = method)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype=2) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("scaled estimates as a function of unknown true probability")
```

![](BiasEsts_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
adjs[adjs$p==0.5, , drop = FALSE]
```

    ##      p self_scaled Bessel_scaled  unscaled
    ## 51 0.5    1.014782     0.9959094 0.9448026

``` r
adjs2 <- adjs
for(col in c("self_scaled", "unscaled", "Bessel_scaled")) {
  adjs2[[col]] <- adjs2[[col]]/adjs2[[col]][adjs2$p==0.5]
}
adjs2[adjs2$p==0.5, , drop = FALSE]
```

    ##      p self_scaled Bessel_scaled unscaled
    ## 51 0.5           1             1        1

``` r
adjsp2 <- unpivot_to_blocks(
  adjs2, 
  nameForNewKeyColumn = "method", 
  nameForNewValueColumn = "scale", columnsToTakeFrom = c("self_scaled", "unscaled", "Bessel_scaled"))
adjsp2 <- adjsp2[!is.na(adjsp2$scale), , drop = FALSE]
adjsp2$method <- reorder(factor(adjsp2$method), -adjsp2$scale)

ggplot(data = adjsp2, mapping = aes(x = p, y = scale, 
                                    color = method,
                                    linetype = method)) +
  geom_line(size=2, alpha=0.8) +
  geom_hline(yintercept = 1, color = "red", linetype=2) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("scaled estimates as a function of unknown true probability",
          subtitle = "rescaled at p=0.5")
```

![](BiasEsts_files/figure-markdown_github/unnamed-chunk-1-2.png)

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

su <- summary1(
  universe, 
  mk_scale_adjustment_table(length(universe), naive_sd_fun))
print(su)
```

    ##        mean       var        sd naive_var  naive_sd    adj_sd
    ## 1 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812 0.3596276

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
mk_f <- function(universe, samp_size, summary1) {
  force(universe)
  force(samp_size)
  force(summary1)
  scale_adjustment_table <- 
    mk_scale_adjustment_table(samp_size, naive_sd_fun)
  f <- function(i) {
    sample <- universe[sample.int(length(universe), 
                                  samp_size, 
                                  replace = TRUE)]
    summary1(sample, scale_adjustment_table)
  }
}

f <- mk_f(universe, 5, summary1)


cl <- parallel::makeCluster(parallel::detectCores())
res <- parallel::parLapply(cl, 1:100000, f)
res <- do.call(rbind, res)

as.data.frame(lapply(res, mean))
```

    ##       mean      var        sd naive_var  naive_sd    adj_sd
    ## 1 0.869606 0.113248 0.2373506 0.0905984 0.2122928 0.2786906
