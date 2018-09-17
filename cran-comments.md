

## Test environments

### OSX

    R CMD check --as-cran sigr_1.0.0.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘sigr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘sigr’ version ‘1.0.0’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Status: OK

### Windows
    devtools::build_win()
 

## Downstream dependencies

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
