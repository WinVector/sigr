
<!-- README.md is generated from README.Rmd. Please edit that file -->
sigr
====

Concise formatting of significances in R (GPL3 license).

![](https://github.com/WinVector/sigr/raw/master/tools/sigr.png)

Please see [Adding polished significance summaries to papers using R](http://www.win-vector.com/blog/2016/10/adding-polished-significance-summaries-to-papers-using-r/) for some discussion.

See also:

-   ["The prevalence of statistical reporting errors in psychology (1985–2013)", Nuijten, M.B., Hartgerink, C.H.J., van Assen, M.A.L.M. et al., Behav Res (2015), doi:10.3758/s13428-015-0664-2](http://link.springer.com/article/10.3758%2Fs13428-015-0664-2)
-   [Reporting Statistics in APA Style](http://my.ilstu.edu/~jhkahn/apastats.html)
-   [Reporting Results of Common Statistical Tests in APA Format](https://depts.washington.edu/psych/files/writing_center/stats.pdf)
-   [Adding polished significance summaries to papers using R](http://www.win-vector.com/blog/2016/10/adding-polished-significance-summaries-to-papers-using-r/)
-   [Publication Manual of the American Psychological Association, Sixth Edition](http://www.apastyle.org/manual/)
-   [Proofing statistics in papers](http://www.win-vector.com/blog/2016/10/proofing-statistics-in-papers/)
-   [apa](https://CRAN.R-project.org/package=apa)
-   [bootstrap](https://CRAN.R-project.org/package=bootstrap)
-   [broom](https://cran.r-project.org/package=broom)
-   [ralfer/apa\_format\_and\_misc](https://github.com/ralfer/apa_format_and_misc)
-   [pwr](https://CRAN.R-project.org/package=pwr)
-   ["Why Most Published Research Findings Are False", John P. A. Ioannidis PLOS Medicine, August 30, 2005](https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020124)
-   ["The garden of forking paths", Andrew Gelman and Eric Loken, 14 Nov 2013](http://www.stat.columbia.edu/~gelman/research/unpublished/p_hacking.pdf)

Used to render the [support material](http://www.win-vector.com/blog/2016/10/proofing-statistics-in-papers/) for [Proofing statistics in papers](http://www.win-vector.com/blog/2016/10/proofing-statistics-in-papers/).

------------------------------------------------------------------------

*sigr* is a small package that concentrates on computing summary statistics and reporting in an appropriate format.

For example here is formatting the quality of a logistic regression.

``` r
d <- data.frame(x=c(1,2,3,4,5,6,7,7),
      y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
model <- glm(y~x,data=d,family=binomial)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x, family = binomial, data = d)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.37180  -1.09714  -0.00811   1.08024   1.42939  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.7455     1.6672  -0.447    0.655
    ## x             0.1702     0.3429   0.496    0.620
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11.090  on 7  degrees of freedom
    ## Residual deviance: 10.837  on 6  degrees of freedom
    ## AIC: 14.837
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library("sigr")
```

``` r
cat(render(wrapChiSqTest(model),
           pLargeCutoff=1, format='markdown'))
```

**Chi-Square Test** summary: *pseudo-<i>R<sup>2</sup></i>*=0.02282 (<i>χ<sup>2</sup></i>(1,*N*=8)=0.2531, *p*=0.6149).
