

## Test environments

### OSX

    R CMD check --as-cran sigr_1.0.4.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.0.4’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    433#> * using R Under development (unstable) (2018-12-26 r75909)
    434#> * using platform: x86_64-w64-mingw32 (64-bit)
    435#> * using session charset: ISO8859-1
    436#> * using option '--as-cran'
    437#> * checking for file 'sigr/DESCRIPTION' ... OK
    438#> * checking extension type ... Package
    439#> * this is package 'sigr' version '1.0.4'
    440#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    441#> Maintainer: 'John Mount '
    495#> Status: OK


## Downstream dependencies

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes

Zumel is not a mis-spelling.
