
## Test environments

 * local OS X install x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3
 * win-builder (devel and release) 

## R CMD check --as-cran sigr_0.1.5.tar.gz 

* using R version 3.3.3 (2017-03-06)
* using platform: x86_64-apple-darwin13.4.0 (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘sigr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sigr’ version ‘0.1.5’

One NOTE:

Note_to_CRAN_maintainers
Maintainer: ‘John Mount <jmount@win-vector.com>’

No WARNINGs or ERRORs.

Status: OK

## Downstream dependencies

checked:
  
    devtools::revdep('sigr')
    [1] "WVPlots"
