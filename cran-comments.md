

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

    rhub::check_for_cran()
    442#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    443#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    444#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    445#> setting R_REMOTES_STANDALONE to true
    446#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    447#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    448#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    449#> * using log directory 'C:/Users/USERaTOyHIbqnv/sigr.Rcheck'
    450#> * using R Under development (unstable) (2020-07-05 r78784)
    451#> * using platform: x86_64-w64-mingw32 (64-bit)
    452#> * using session charset: ISO8859-1
    453#> * using option '--as-cran'
    454#> * checking for file 'sigr/DESCRIPTION' ... OK
    455#> * checking extension type ... Package
    456#> * this is package 'sigr' version '1.0.7'
    457#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    458#> Maintainer: 'John Mount '
    470#> * checking for future file timestamps ... NOTE
    471#> unable to verify current time
    515#> Status: 1 NOTE
    "unable to verify current time" is a property of testing environment, not the package.

## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
