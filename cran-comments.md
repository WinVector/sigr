

## Test environments

### OSX

    R CMD check --as-cran sigr_1.1.0.tar.gz
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.1.0’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    * checking for future file timestamps ... NOTE
    unable to verify current time
    ...
    Status: 1 NOTE
    Time check problem is a problem with check environment or remote service, not the package.

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2020-09-09 r79174)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.1.0'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK
 
## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
