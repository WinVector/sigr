

## Test environments

### OSX

    R CMD check --as-cran sigr_1.1.2.tar.gz
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.1.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    * checking for future file timestamps ... NOTE
    unable to verify current time
    ...
    Status: 1 NOTE
    Time check problem is a problem with check environment or remote service, not the package.

### Windows

    rhub::check_for_cran()
    777#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    778#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    779#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    780#> setting R_REMOTES_STANDALONE to true
    781#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    782#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    783#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    784#> * using log directory 'C:/Users/USERKqSNkUWBlj/sigr.Rcheck'
    785#> * using R Under development (unstable) (2020-09-16 r79221)
    786#> * using platform: x86_64-w64-mingw32 (64-bit)
    787#> * using session charset: ISO8859-1
    788#> * using option '--as-cran'
    789#> * checking for file 'sigr/DESCRIPTION' ... OK
    790#> * checking extension type ... Package
    791#> * this is package 'sigr' version '1.1.2'
    792#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    793#> Maintainer: 'John Mount '
    849#> Status: OK
 
## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
