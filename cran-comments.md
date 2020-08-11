

## Test environments

### OSX

    R CMD check --as-cran sigr_1.0.8.tar.gz
    * using R version 4.0.0 (2020-04-24)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.0.8’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...    
    Status: OK

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2020-08-10 r79000)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.0.8'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK

## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
