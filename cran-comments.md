

## Test environments

### OSX

    R CMD check --as-cran sigr_1.0.7.tar.gz 
    * using R version 4.0.0 (2020-04-24)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.0.7’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    devtools::check_win_devel()


## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
