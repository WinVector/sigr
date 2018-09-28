Bias Ests
================

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
set.seed(32535)

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
summary1 <- function(x) {
  naive_var <- mean((mean(x)-x)^2)
  data.frame(mean = mean(x),
             var = var(x),
             sd = sd(x),
             naive_var = naive_var,
             naive_sd = sqrt(naive_var))
}

su <- summary1(universe)
print(su)
```

    ##        mean       var        sd naive_var  naive_sd
    ## 1 0.8695652 0.1185771 0.3443502 0.1134216 0.3367812

``` r
n <- length(universe)
adj_var <- (n/(n-1))*su$naive_var
print(adj_var)
```

    ## [1] 0.1185771

``` r
adj_sd <- sqrt(adj_var)
print(adj_sd)
```

    ## [1] 0.3443502

``` r
mk_f <- function(universe, summary1) {
  force(universe)
  force(summary1)
  f <- function(i) {
    sample <- universe[sample.int(length(universe), 
                                  5, 
                                  replace = TRUE)]
    summary1(sample)
  }
}

f <- mk_f(universe, summary1)


cl <- parallel::makeCluster(parallel::detectCores())
res <- parallel::parLapply(cl, 1:1000000, f)
res <- do.call(rbind, res)

as.data.frame(lapply(res, mean))
```

    ##        mean       var        sd naive_var  naive_sd
    ## 1 0.8696168 0.1133575 0.2376197  0.090686 0.2125335
