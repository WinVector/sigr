# sigr
Concise formatting of significances in R (GPL3 license).

To install in R use: <code>devtools::install_github('WinVector/sigr')</code>. Please see [Adding polished significance summaries to papers using R](http://www.win-vector.com/blog/2016/10/adding-polished-significance-summaries-to-papers-using-r/) for some discussion.

See also:

  * [“The prevalence of statistical reporting errors in psychology (1985–2013)”, Nuijten, M.B., Hartgerink, C.H.J., van Assen, M.A.L.M. et al., Behav Res (2015), doi:10.3758/s13428-015-0664-2](http://link.springer.com/article/10.3758%2Fs13428-015-0664-2)
  * [Reporting Statistics in APA Style](http://my.ilstu.edu/~jhkahn/apastats.html)
  * [Reporting Results of Common Statistical Tests in APA Format](https://depts.washington.edu/psych/files/writing_center/stats.pdf)
  * [Adding polished significance summaries to papers using R](http://www.win-vector.com/blog/2016/10/adding-polished-significance-summaries-to-papers-using-r/)
  * [Publication Manual of the American Psychological Association, Sixth Edition](http://www.apastyle.org/manual/)
  * [Proofing statistics in papers](http://www.win-vector.com/blog/2016/10/proofing-statistics-in-papers/)
  * [broom](https://cran.r-project.org/package=broom)
  * [ralfer/apa_format_and_misc](https://github.com/ralfer/apa_format_and_misc)
  * [boost](https://cran.r-project.org/package=boost)

Used to render the [support material](http://www.win-vector.com/blog/2016/10/proofing-statistics-in-papers/) for [Proofing statistics in papers](http://www.win-vector.com/blog/2016/10/proofing-statistics-in-papers/).

-----------


Is this the same as _broom_?  Not quite, _broom_ is
a very powerful package that
organizes a number of results into data frames for
further use.  It encapsulates the very good idea of 
regularizing data for re-use. So _broom_ concentrates
on reporting what is there in a machine usable format.

_sigr_ is a much smaller package that concentrates on 
final step reporting to audiences.  So it may calculate
items that are not present and return formatted results.

For example here is _broom::glance_ and _sigr::formatChiSqTest_ both
working on the same model.

```r
d <- data.frame(x=c(1,2,3,4,5,6,7,7),
      y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
model <- glm(y~x,data=d,family=binomial)
summary(model)
```

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
```

```r
broom::glance(model)
```

```
##   null.deviance df.null    logLik      AIC      BIC deviance df.residual
## 1      11.09035       7 -5.418632 14.83726 14.99615 10.83726           6
```

```r
cat(sigr::formatChiSqTest(model,pLargeCutoff=1)$formatStr)
```

**Chi-Square Test** summary: *pseudo-<i>R^2^</i>*=0.023 (<i>&chi;^2^</i>(1,*N*=8)=0.25, *p*=0.615).

Notice _broom::glance_ does not derive the p-value and returns
a data.frame (an object needing further formatting), while _sigr::formatChiSqTest_ returns markdown text with the derived p-value.

