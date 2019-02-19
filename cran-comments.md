

## Test environments

### OSX

    R CMD check --as-cran sigr_1.0.5.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.0.5’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    devtools::build_win()
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.0.5'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK
    
    * using R Under development (unstable) (2019-02-17 r76112)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.0.5'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Status: OK


## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
