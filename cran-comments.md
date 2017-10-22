
## Test environments

 * OSX
 * using R version 3.4.2 (2017-09-28)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)

 * Windows
 * using R Under development (unstable) (2017-09-12 r73242)
 * using platform: x86_64-w64-mingw32 (64-bit)

## R CMD check --as-cran sigr_0.2.2.tar.gz 

 * using session charset: UTF-8
 * using option ‘--as-cran’
 * checking for file ‘sigr/DESCRIPTION’ ... OK
 * checking extension type ... Package
 * this is package ‘sigr’ version ‘0.2.2’


Note_to_CRAN_maintainers
Maintainer: ‘John Mount <jmount@win-vector.com>’

Status: OK

## Downstream dependencies

Checked all of:
  
    devtools::revdep('sigr')
    [1] "WVPlots"

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
