# broom

Version: 0.5.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    Joining, by = "class"
    Joining, by = "class"
    Loading required package: tidyr
    
    Attaching package: 'rsample'
    
    The following object is masked from 'package:tidyr':
    
        fill
    
    The following object is masked from 'package:broom':
    
        tidy
    
    Quitting from lines 66-76 (bootstrapping.Rmd) 
    Error: processing vignette 'bootstrapping.Rmd' failed with diagnostics:
    Evaluation error: no applicable method for 'tidy' applied to an object of class "nls".
    Execution halted
    ```

# MachineShop

Version: 0.2.0

## Newly broken

*   checking whether package ‘MachineShop’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/rsample/revdep/checks.noindex/MachineShop/new/MachineShop.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MachineShop’ ...
** package ‘MachineShop’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘prepper’ is not exported by 'namespace:rsample'
ERROR: lazy loading failed for package ‘MachineShop’
* removing ‘/Users/max/github/rsample/revdep/checks.noindex/MachineShop/new/MachineShop.Rcheck/MachineShop’

```
### CRAN

```
* installing *source* package ‘MachineShop’ ...
** package ‘MachineShop’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for ‘append’ in package ‘MachineShop’
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded
* DONE (MachineShop)

```
# recipes

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppRoll’
      All declared Imports should be used.
    ```

# tidymodels

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘ggplot2’ ‘infer’ ‘pillar’ ‘recipes’ ‘rsample’
      ‘tidyposterior’ ‘tidypredict’ ‘tidytext’ ‘yardstick’
      All declared Imports should be used.
    ```

