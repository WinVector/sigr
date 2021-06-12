

## Test environments

### OSX

    R CMD check --as-cran sigr_1.1.4.tar.gz 
    ...

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2021-06-11 r80486)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.1.4'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK

### Linux

    rhub::check_for_cran()
    1432#> About to run xvfb-run R CMD check --as-cran sigr_1.1.4.tar.gz
    1438#> * using R version 4.1.0 (2021-05-18)
    1439#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1445#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1446#> Maintainer: ‘John Mount ’
    1503#> Status: OK

    rhub::check_for_cran()
    1243#> About to run xvfb-run R CMD check --as-cran sigr_1.1.4.tar.gz
    1247#> * using R Under development (unstable) (2021-06-10 r80480)
    1248#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1253#> * this is package ‘sigr’ version ‘1.1.4’
    1254#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1255#> Maintainer: ‘John Mount ’
    1312#> Status: OK
 
## Downstream dependencies

    Checked all reverse dependencies: https://github.com/WinVector/sigr/blob/master/extras/check_reverse_dependencies.md

Zumel is not a mis-spelling.
