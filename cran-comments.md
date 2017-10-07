
## Test environments

 * using R version 3.4.1 (2017-06-30)
 * using platform: x86_64-apple-darwin15.6.0 (64-bit)
 * win-builder (devel and release) 

## R CMD check --as-cran sigr_0.2.0.tar.gz 

* using R version 3.4.1 (2017-06-30)
* using platform: x86_64-apple-darwin15.6.0 (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘sigr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sigr’ version ‘0.2.0’

One NOTE:

Note_to_CRAN_maintainers
Maintainer: ‘John Mount <jmount@win-vector.com>’

No WARNINGs or ERRORs.

Status: OK

## Downstream dependencies

Checked all of:
  
    devtools::revdep('sigr')
    [1] "WVPlots"

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
