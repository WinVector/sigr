

## Test environments

### OSX

    R CMD check --as-cran sigr_1.0.3.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.0.3’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
    

### Windows

    rhub::check_for_cran()
    447#> * using R Under development (unstable) (2018-09-27 r75377)
    448#> * using platform: x86_64-w64-mingw32 (64-bit)
    449#> * using session charset: ISO8859-1
    450#> * using option '--as-cran'
    451#> * checking for file 'sigr/DESCRIPTION' ... OK
    452#> * checking extension type ... Package
    453#> * this is package 'sigr' version '1.0.3'
    454#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    455#> Maintainer: 'John Mount '
    509#> Status: OK

## Downstream dependencies

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes

Zumel is not a mis-spelling.
