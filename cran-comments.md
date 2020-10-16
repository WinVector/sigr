

## Test environments

### OSX

    R CMD check --as-cran sigr_1.1.3.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/sigr.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.1.3’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    devtools::check_win_devel()
    * using log directory 'd:/RCompile/CRANguest/R-devel/sigr.Rcheck'
    * using R Under development (unstable) (2020-10-15 r79342)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.1.3'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK


    rhub::check_for_cran()
    587#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    588#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    589#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    590#> setting R_REMOTES_STANDALONE to true
    591#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    592#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    593#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    594#> * using log directory 'C:/Users/USERATuZHaQhzS/sigr.Rcheck'
    595#> * using R Under development (unstable) (2020-10-09 r79317)
    596#> * using platform: x86_64-w64-mingw32 (64-bit)
    597#> * using session charset: ISO8859-1
    598#> * using option '--as-cran'
    599#> * checking for file 'sigr/DESCRIPTION' ... OK
    600#> * checking extension type ... Package
    601#> * this is package 'sigr' version '1.1.3'
    602#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    603#> Maintainer: 'John Mount '
    659#> Status: OK
 
## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
