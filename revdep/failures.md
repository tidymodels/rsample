# did2s

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/did2s
* Date/Publication: 2021-09-27 08:50:07 UTC
* Number of recursive dependencies: 160

Run `revdep_details(, "did2s")` for more info

</details>

## In both

*   checking whether package ‘did2s’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/juliasilge/Work/rstudio/rsample/revdep/checks.noindex/did2s/new/did2s.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘did2s’ ...
** package ‘did2s’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/RcppArmadillo/include' -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/Rcpp/include' -I/opt/R/arm64/include   -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/RcppArmadillo/include' -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/Rcpp/include' -I/opt/R/arm64/include   -fPIC  -falign-functions=64 -Wall -g -O2  -c code.cpp -o code.o
clang++ -arch arm64 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o did2s.so RcppExports.o code.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/R/arm64/gfortran/lib/gcc/aarch64-apple-darwin20.2.0/11.0.0 -L/opt/R/arm64/gfortran/lib -lgfortran -lemutls_w -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/opt/R/arm64/gfortran/lib/gcc/aarch64-apple-darwin20.2.0/11.0.0'
ld: warning: directory not found for option '-L/opt/R/arm64/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [did2s.so] Error 1
ERROR: compilation failed for package ‘did2s’
* removing ‘/Users/juliasilge/Work/rstudio/rsample/revdep/checks.noindex/did2s/new/did2s.Rcheck/did2s’


```
### CRAN

```
* installing *source* package ‘did2s’ ...
** package ‘did2s’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/RcppArmadillo/include' -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/Rcpp/include' -I/opt/R/arm64/include   -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/RcppArmadillo/include' -I'/Users/juliasilge/Work/rstudio/rsample/revdep/library.noindex/did2s/Rcpp/include' -I/opt/R/arm64/include   -fPIC  -falign-functions=64 -Wall -g -O2  -c code.cpp -o code.o
clang++ -arch arm64 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o did2s.so RcppExports.o code.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/R/arm64/gfortran/lib/gcc/aarch64-apple-darwin20.2.0/11.0.0 -L/opt/R/arm64/gfortran/lib -lgfortran -lemutls_w -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/opt/R/arm64/gfortran/lib/gcc/aarch64-apple-darwin20.2.0/11.0.0'
ld: warning: directory not found for option '-L/opt/R/arm64/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [did2s.so] Error 1
ERROR: compilation failed for package ‘did2s’
* removing ‘/Users/juliasilge/Work/rstudio/rsample/revdep/checks.noindex/did2s/old/did2s.Rcheck/did2s’


```
