
```r
library(broom)
library(dplyr)
library(purrr)
library(tibble)

lm_est <- function(split, ...) {
  lm(mpg ~ disp + hp, data = analysis(split)) %>%
    tidy()
}

set.seed(52156)
car_rs <-
  bootstraps(mtcars, 5000, apparent = TRUE) %>%
  mutate(results = map(splits, lm_est))

int_pctl(car_rs, results)
```

```
## # A tibble: 3 x 6
##   term         .lower .estimate   .upper .alpha .method   
##   <chr>         <dbl>     <dbl>    <dbl>  <dbl> <chr>     
## 1 (Intercept) 27.7      30.8    33.6       0.05 percentile
## 2 disp        -0.0439   -0.0295 -0.0136    0.05 percentile
## 3 hp          -0.0599   -0.0272 -0.00741   0.05 percentile
```

```r
int_t(car_rs, results)
```

```
## # A tibble: 3 x 6
##   term         .lower .estimate    .upper .alpha .method  
##   <chr>         <dbl>     <dbl>     <dbl>  <dbl> <chr>    
## 1 (Intercept) 28.0      30.8    34.3        0.05 student-t
## 2 disp        -0.0464   -0.0295 -0.0172     0.05 student-t
## 3 hp          -0.0458   -0.0272 -0.000643   0.05 student-t
```

```r
int_bca(car_rs, results, .fn = lm_est)
```

```
## # A tibble: 3 x 6
##   term         .lower .estimate   .upper .alpha .method
##   <chr>         <dbl>     <dbl>    <dbl>  <dbl> <chr>  
## 1 (Intercept) 27.8      30.8    33.7       0.05 BCa    
## 2 disp        -0.0455   -0.0295 -0.0148    0.05 BCa    
## 3 hp          -0.0561   -0.0272 -0.00502   0.05 BCa
```

```r
# putting results into a tidy format
rank_corr <- function(split) {
  dat <- analysis(split)
  tibble(
    term = "corr",
    estimate = cor(dat$Sepal.Length, dat$Sepal.Width, method = "spearman"),
    # don't know the analytical std.err so no t-intervals
    std.err = NA_real_
  )
}

set.seed(69325)
bootstraps(iris, 5000, apparent = TRUE) %>%
  mutate(correlations = map(splits, rank_corr)) %>%
  int_pctl(correlations)
```

```
## # A tibble: 1 x 6
##   term  .lower .estimate  .upper .alpha .method   
##   <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>     
## 1 corr  -0.309    -0.165 -0.0142   0.05 percentile
```
