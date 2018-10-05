

## Test environments

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-10-04 r75399)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'sigr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'sigr' version '1.0.2'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

    
### Linux

    rhub::check_for_cran()
    #> * using R version 3.4.4 (2018-03-15)
    #> * using platform: x86_64-pc-linux-gnu (64-bit)
    #> * using session charset: UTF-8
    #> * using option ‘--as-cran’
    #> * checking for file ‘sigr/DESCRIPTION’ ... OK
    #> * checking extension type ... Package
    #> * this is package ‘sigr’ version ‘1.0.2’
    #> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    #> Maintainer: ‘John Mount ’
    #> Status: OK
    
    rhub::check_for_cran()
    #> * using R Under development (unstable) (2018-09-27 r75377)
    #> * using platform: x86_64-w64-mingw32 (64-bit)
    #> * using session charset: ISO8859-1
    #> * using option '--as-cran'
    #> * checking for file 'sigr/DESCRIPTION' ... OK
    #> * checking extension type ... Package
    #> * this is package 'sigr' version '1.0.2'
    #> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    #> Maintainer: 'John Mount '
    #> * checking top-level files ... WARNING
    #> Conversion of 'README.md' failed:
    #> pandoc.exe: Could not fetch https://github.com/WinVector/sigr/raw/master/tools/sigr.png
    #> no store
    #> Status: 1 WARNING
    Warning is suprious, URL exists.
    
## Downstream dependencies

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
    
