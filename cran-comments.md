
## Test environments

 * local OS X install x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3
 * win-builder (devel and release) 

## R CMD check --as-cran sigr_0.1.6.tar.gz 

* using log directory ‘/Users/johnmount/Documents/work/sigr.Rcheck’
* using R version 3.4.0 (2017-04-21)
* using platform: x86_64-apple-darwin15.6.0 (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘sigr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sigr’ version ‘0.1.6’


One NOTE:

Note_to_CRAN_maintainers
Maintainer: ‘John Mount <jmount@win-vector.com>’

No WARNINGs or ERRORs.

Status: OK

## Downstream dependencies

checked:
  
    devtools::revdep('sigr')
    [1] "WVPlots"
