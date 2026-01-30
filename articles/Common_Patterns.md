# Common Resampling Patterns

The rsample package provides a number of resampling methods which are
broadly applicable to a wide variety of modeling applications. This
vignette walks through the most popular methods in the package, with
brief descriptions of how they can be applied. For a more in-depth
overview of resampling, check out the matching chapters in [Tidy
Modeling with R](https://www.tmwr.org/resampling.html) and [Feature
Engineering and Selection](http://www.feat.engineering/resampling.md).

Let’s go ahead and load rsample now:

``` r
library(rsample)
```

We’ll also load in a few data sets from the modeldata package. First,
the Ames housing data, containing the sale prices of homes in Ames,
Iowa:

``` r
data(ames, package = "modeldata")
head(ames, 2)
#> # A tibble: 2 × 74
#>   MS_SubClass    MS_Zoning Lot_Frontage Lot_Area Street Alley Lot_Shape
#>   <fct>          <fct>            <dbl>    <int> <fct>  <fct> <fct>    
#> 1 One_Story_194… Resident…          141    31770 Pave   No_A… Slightly…
#> 2 One_Story_194… Resident…           80    11622 Pave   No_A… Regular  
#> # ℹ 67 more variables: Land_Contour <fct>, Utilities <fct>,
#> #   Lot_Config <fct>, Land_Slope <fct>, Neighborhood <fct>,
#> #   Condition_1 <fct>, Condition_2 <fct>, Bldg_Type <fct>,
#> #   House_Style <fct>, Overall_Cond <fct>, Year_Built <int>,
#> #   Year_Remod_Add <int>, Roof_Style <fct>, Roof_Matl <fct>,
#> #   Exterior_1st <fct>, Exterior_2nd <fct>, Mas_Vnr_Type <fct>,
#> #   Mas_Vnr_Area <dbl>, Exter_Cond <fct>, Foundation <fct>, …
```

Secondly, data on Chicago transit ridership numbers:

``` r
data(Chicago, package = "modeldata")
head(Chicago, 2)
#> # A tibble: 2 × 50
#>   ridership Austin Quincy_Wells Belmont Archer_35th Oak_Park Western
#>       <dbl>  <dbl>        <dbl>   <dbl>       <dbl>    <dbl>   <dbl>
#> 1      15.7   1.46         8.37    4.60        2.01     1.42    3.32
#> 2      15.8   1.50         8.35    4.72        2.09     1.43    3.34
#> # ℹ 43 more variables: Clark_Lake <dbl>, Clinton <dbl>,
#> #   Merchandise_Mart <dbl>, Irving_Park <dbl>, Washington_Wells <dbl>,
#> #   Harlem <dbl>, Monroe <dbl>, Polk <dbl>, Ashland <dbl>,
#> #   Kedzie <dbl>, Addison <dbl>, Jefferson_Park <dbl>, Montrose <dbl>,
#> #   California <dbl>, temp_min <dbl>, temp <dbl>, temp_max <dbl>,
#> #   temp_change <dbl>, dew <dbl>, humidity <dbl>, pressure <dbl>,
#> #   pressure_change <dbl>, wind <dbl>, wind_max <dbl>, gust <dbl>, …
```

In addition to these data sets from the modeldata package, we’ll also
make use of the Orange data set in base R, containing repeated
measurements of 5 orange trees over time:

``` r
head(Orange, 2)
#>   Tree age circumference
#> 1    1 118            30
#> 2    1 484            58
```

And last but not least, we’ll set a seed so our results are
reproducible:

``` r
set.seed(123)
```

## Random Resampling

By far and away, the most common use for rsample is to generate simple
random resamples of your data. The rsample package includes a number of
functions specifically for this purpose.

### Initial Splits

To split your data into two sets – often referred to as the “training”
and “testing” sets – rsample provides the
[`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.md)
function:

``` r
initial_split(ames)
#> <Training/Testing/Total>
#> <2197/733/2930>
```

The output of this is [an rsplit
object](https://rsample.tidymodels.org/articles/rsample.md) with each
observation assigned to one of the two sets. You can control the
proportion of data assigned to the “training” set through the `prop`
argument:

``` r
initial_split(ames, prop = 0.8)
#> <Training/Testing/Total>
#> <2344/586/2930>
```

To get the actual data assigned to either set, use the
[`training()`](https://rsample.tidymodels.org/reference/initial_split.md)
and
[`testing()`](https://rsample.tidymodels.org/reference/initial_split.md)
functions:

``` r
resample <- initial_split(ames, prop = 0.6)

head(training(resample), 2)
#> # A tibble: 2 × 74
#>   MS_SubClass    MS_Zoning Lot_Frontage Lot_Area Street Alley Lot_Shape
#>   <fct>          <fct>            <dbl>    <int> <fct>  <fct> <fct>    
#> 1 One_Story_194… Resident…          110    14333 Pave   No_A… Regular  
#> 2 One_Story_194… Resident…           65     8450 Pave   No_A… Regular  
#> # ℹ 67 more variables: Land_Contour <fct>, Utilities <fct>,
#> #   Lot_Config <fct>, Land_Slope <fct>, Neighborhood <fct>,
#> #   Condition_1 <fct>, Condition_2 <fct>, Bldg_Type <fct>,
#> #   House_Style <fct>, Overall_Cond <fct>, Year_Built <int>,
#> #   Year_Remod_Add <int>, Roof_Style <fct>, Roof_Matl <fct>,
#> #   Exterior_1st <fct>, Exterior_2nd <fct>, Mas_Vnr_Type <fct>,
#> #   Mas_Vnr_Area <dbl>, Exter_Cond <fct>, Foundation <fct>, …
head(testing(resample), 2)
#> # A tibble: 2 × 74
#>   MS_SubClass    MS_Zoning Lot_Frontage Lot_Area Street Alley Lot_Shape
#>   <fct>          <fct>            <dbl>    <int> <fct>  <fct> <fct>    
#> 1 One_Story_194… Resident…          141    31770 Pave   No_A… Slightly…
#> 2 One_Story_194… Resident…           80    11622 Pave   No_A… Regular  
#> # ℹ 67 more variables: Land_Contour <fct>, Utilities <fct>,
#> #   Lot_Config <fct>, Land_Slope <fct>, Neighborhood <fct>,
#> #   Condition_1 <fct>, Condition_2 <fct>, Bldg_Type <fct>,
#> #   House_Style <fct>, Overall_Cond <fct>, Year_Built <int>,
#> #   Year_Remod_Add <int>, Roof_Style <fct>, Roof_Matl <fct>,
#> #   Exterior_1st <fct>, Exterior_2nd <fct>, Mas_Vnr_Type <fct>,
#> #   Mas_Vnr_Area <dbl>, Exter_Cond <fct>, Foundation <fct>, …
```

### V-Fold Cross-Validation

You should only evaluate models against your test set once, when you’ve
completely finished tuning and training your models. To estimate
performance of model candidates, you typically split your training data
into one part used for model fitting and one part used for measuring
performance. To distinguish those set from training and test set, we
refer to them as analysis and assessment set, respectively. Typically,
you split your training data into analysis and assessment sets multiple
times to get stable estimates of model performance.

Perhaps the most common cross-validation method is [V-fold
cross-validation](https://www.tmwr.org/resampling.html#cv). Also known
as “k-fold cross-validation”, this method creates V resamples by
splitting your data into V groups (also known as “folds”) of roughly
equal size. The analysis set of each resample is made up of V-1 folds,
with the remaining fold being used as the assessment set. This way, each
observation in your data is used in exactly one assessment set.

To use V-fold cross-validation in rsample, use the
[`vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.md)
function:

``` r
vfold_cv(ames, v = 2)
#> #  2-fold cross-validation 
#> # A tibble: 2 × 2
#>   splits              id   
#>   <list>              <chr>
#> 1 <split [1465/1465]> Fold1
#> 2 <split [1465/1465]> Fold2
```

One downside to V-fold cross validation is that it tends to produce
“noisy”, or high-variance, estimates [when compared to other resampling
methods](https://bookdown.org/max/FES/resampling.html#resample-var-bias).
To try and reduce that variance, it’s often helpful to perform what’s
known as [repeated
cross-validation](https://www.tmwr.org/resampling.html#repeated-cross-validation),
effectively running the V-fold resampling procedure multiple times for
your data. To perform repeated V-fold cross-validation in rsample, you
can use the repeats argument inside of
[`vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.md):

``` r
vfold_cv(ames, v = 2, repeats = 2)
#> #  2-fold cross-validation repeated 2 times 
#> # A tibble: 4 × 3
#>   splits              id      id2  
#>   <list>              <chr>   <chr>
#> 1 <split [1465/1465]> Repeat1 Fold1
#> 2 <split [1465/1465]> Repeat1 Fold2
#> 3 <split [1465/1465]> Repeat2 Fold1
#> 4 <split [1465/1465]> Repeat2 Fold2
```

### Monte-Carlo Cross-Validation

An alternative to V-fold cross-validation is Monte-Carlo
cross-validation. Where V-fold assigns each observation in your data to
one (and exactly one) assessment set, Monte-Carlo cross-validation takes
a random subset of your data for each assessment set, meaning each
observation can be used in 0, 1, or many assessment sets. The analysis
set is then made up of all the observations that weren’t selected.
Because each assessment set is sampled independently, you can repeat
this as many times as you want.

To use Monte-Carlo cross-validation in rsample, use the
[`mc_cv()`](https://rsample.tidymodels.org/reference/mc_cv.md) function:

``` r
mc_cv(ames, prop = 0.8, times = 2)
#> # Monte Carlo cross-validation (0.8/0.2) with 2 resamples 
#> # A tibble: 2 × 2
#>   splits             id       
#>   <list>             <chr>    
#> 1 <split [2344/586]> Resample1
#> 2 <split [2344/586]> Resample2
```

Similar to
[`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.md),
you can control the proportion of your data assigned to the analysis
fold using `prop`. You can also control the number of resamples you
create using the `times` argument.

Monte-Carlo cross-validation tends to produce more biased estimates than
V-fold. As such, when computationally feasible we typically recommend
using [five or so repeats of 10-fold
cross-validation](https://bookdown.org/max/FES/resampling.html#resample-var-bias)
for model assessment.

### Bootstrap Resampling

The last primary technique in rsample for creating resamples from the
training data is bootstrap resampling. A “bootstrap sample” is a sample
of your data set, the same size as your data set, taken with replacement
so that a single observation might be sampled multiple times. The
assessment set is then made up of all the observations that weren’t
selected for the analysis set. Generally, bootstrap resampling produces
pessimistic estimates of model accuracy.

You can create bootstrap resamples in rsample using the
[`bootstraps()`](https://rsample.tidymodels.org/reference/bootstraps.md)
function. While you can’t control the proportion of data in each set –
the assessment set of a bootstrap resample is always the same size as
the training data – the function otherwise works exactly like
[`mc_cv()`](https://rsample.tidymodels.org/reference/mc_cv.md):

``` r
bootstraps(ames, times = 2)
#> # Bootstrap sampling 
#> # A tibble: 2 × 2
#>   splits              id        
#>   <list>              <chr>     
#> 1 <split [2930/1072]> Bootstrap1
#> 2 <split [2930/1085]> Bootstrap2
```

### Validation Set

If your data is vast enough for a reliable performance estimate from
just one assessment set, you can do a three-way split of your data into
a training, validation and test set right at the start. (The validation
set has the role of the single assessment set.) Instead of using
[`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.md)
to create a binary split, you can use
[`initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md)
to create that three-way split:

``` r
three_way_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
three_way_split
#> <Training/Validation/Testing/Total>
#> <1758/586/586/2930>
```

The `prop` argument here has two elements, specifying the proportion of
the data assigned to the training and the validation set.

To create an `rset` object for tuning,
[`validation_set()`](https://rsample.tidymodels.org/reference/validation_set.md)
bundles together the training and validation set, read for use with the
[tune](https://tune.tidymodels.org/)) package.

``` r
validation_set(three_way_split)
#> # A tibble: 1 × 2
#>   splits             id        
#>   <list>             <chr>     
#> 1 <split [1758/586]> validation
```

## Stratified Resampling

If your data is heavily imbalanced (that is, if the distribution of an
important continuous variable is skewed, or some classes of a
categorical variable are much more common than others), simple random
resampling may accidentally skew your data even further by allocating
more “rare” observations disproportionately into the analysis or
assessment fold. In these situations, it can be useful to instead use
[stratified
resampling](https://www.tmwr.org/splitting.html#splitting-methods) to
ensure the analysis and assessment folds have a similar distribution as
your overall data.

All of the functions discussed so far support stratified resampling
through their `strata` argument. This argument takes a single column
identifier and uses it to stratify the resampling procedure:

``` r
vfold_cv(ames, v = 2, strata = Sale_Price)
#> #  2-fold cross-validation using stratification 
#> # A tibble: 2 × 2
#>   splits              id   
#>   <list>              <chr>
#> 1 <split [1464/1466]> Fold1
#> 2 <split [1466/1464]> Fold2
```

By default, rsample will cut continuous variables into four bins, and
ensure that each bin is proportionally represented in each set. If
desired, this behavior can be changed using the `breaks` argument:

``` r
vfold_cv(ames, v = 2, strata = Sale_Price, breaks = 100)
#> #  2-fold cross-validation using stratification 
#> # A tibble: 2 × 2
#>   splits              id   
#>   <list>              <chr>
#> 1 <split [1439/1491]> Fold1
#> 2 <split [1491/1439]> Fold2
```

## Grouped Resampling

Often, some observations in your data will be “more related” to each
other than would be probable under random chance, for instance because
they represent repeated measurements of the same subject or were all
collected at a single location. In these situations, you often want to
assign all related observations to either the analysis or assessment
fold as a group, to avoid having assessment data that’s closely related
to the data used to fit a model.

All of the functions discussed so far have a “grouped resampling”
variation to handle these situations. These functions all start with the
`group_` prefix, and use the argument `group` to specify which column
should be used to group observations. Other than respecting these
groups, these functions all work like their ungrouped variants:

``` r
resample <- group_initial_split(Orange, group = Tree)

unique(training(resample)$Tree)
#> [1] 1 2 3 4
#> Levels: 3 < 1 < 5 < 2 < 4
unique(testing(resample)$Tree)
#> [1] 5
#> Levels: 3 < 1 < 5 < 2 < 4
```

It’s important to note that, while functions like
[`group_mc_cv()`](https://rsample.tidymodels.org/reference/group_mc_cv.md)
still let you specify what proportion of your data should be in the
analysis set (and
[`group_bootstraps()`](https://rsample.tidymodels.org/reference/group_bootstraps.md)
still attempts to create analysis sets the same size as your original
data), rsample won’t “split” groups in order to exactly meet that
proportion. These functions start out by assigning one group at random
to each set (or, for
[`group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.md),
to each fold) and then assign each of the remaining groups, in a random
order, to whichever set brings the relative sizes of each set closest to
the target proportion. That means that resamples are randomized, and you
can safely use repeated cross-validation just as you would with
ungrouped resampling, but also means you can wind up with very
differently sized analysis and assessment sets than anticipated if your
groups are unbalanced:

``` r
set.seed(1)
group_bootstraps(ames, Neighborhood, times = 2)
#> # Group bootstrap sampling 
#> # A tibble: 2 × 2
#>   splits             id        
#>   <list>             <chr>     
#> 1 <split [2939/907]> Bootstrap1
#> 2 <split [2958/635]> Bootstrap2
```

While most of the grouped resampling functions are always focused on
balancing the proportion of data in the analysis set, by default
[`group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.md)
will attempt to balance the number of groups assigned to each fold. If
instead you’d like to balance the number of observations in each fold
(meaning your assessment sets will be of similar sizes, but smaller
groups will be more likely to be assigned to the same folds than would
happen under random chance), you can use the argument
`balance = "observations"`:

``` r
group_vfold_cv(ames, Neighborhood, balance = "observations", v = 2)
#> # Group 2-fold cross-validation 
#> # A tibble: 2 × 2
#>   splits              id       
#>   <list>              <chr>    
#> 1 <split [1475/1455]> Resample1
#> 2 <split [1455/1475]> Resample2
```

If you’re working with spatial data, your observations will often be
more related to their neighbors than to the rest of the data set; as
[Tobler’s first law of
geography](https://en.wikipedia.org/wiki/Tobler%27s_first_law_of_geography)
puts it, “everything is related to everything else, but near things are
more related than distant things.” However, you often won’t have a
pre-defined “location” variable that you can use to group related
observations. The [spatialsample](https://spatialsample.tidymodels.org/)
package provides functions for spatial cross-validation using rsample
syntax and classes, and is often useful for these situations.

## Time-Based Resampling

When working with time-based data, it usually doesn’t make sense to
randomly resample your data: random resampling will likely result in
your analysis set having observations from later than your assessment
set, which isn’t a realistic way to assess model performance.

As such, rsample provides a few different functions to make sure that
all data in your assessment sets are after that in the analysis set.

First off, two variants on
[`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.md)
and
[`initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md),
[`initial_time_split()`](https://rsample.tidymodels.org/reference/initial_split.md)
and
[`initial_validation_time_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md),
will assign the *first* rows of your data to the training set (with the
number of rows assigned determined by `prop`):

``` r
initial_time_split(Chicago)
#> <Training/Testing/Total>
#> <4273/1425/5698>

initial_validation_time_split(Chicago)
#> <Training/Validation/Testing/Total>
#> <3418/1140/1140/5698>
```

There are also several functions in rsample to help you construct
multiple analysis and assessment sets from time-based data. For
instance, the
[`sliding_window()`](https://rsample.tidymodels.org/reference/slide-resampling.md)
will create “windows” of your data, moving down through the rows of the
data frame:

``` r
sliding_window(Chicago) |>
  head(2)
#> # A tibble: 2 × 2
#>   splits        id       
#>   <list>        <chr>    
#> 1 <split [1/1]> Slice0001
#> 2 <split [1/1]> Slice0002
```

If you want to create sliding windows of your data based on a specific
variable, you can use the
[`sliding_index()`](https://rsample.tidymodels.org/reference/slide-resampling.md)
function:

``` r
sliding_index(Chicago, date) |>
  head(2)
#> # A tibble: 2 × 2
#>   splits        id       
#>   <list>        <chr>    
#> 1 <split [1/1]> Slice0001
#> 2 <split [1/1]> Slice0002
```

And if you want to set the size of windows based on units of time, for
instance to have each window contain a year of data, you can use
[`sliding_period()`](https://rsample.tidymodels.org/reference/slide-resampling.md):

``` r
sliding_period(Chicago, date, "year") |>
  head(2)
#> # A tibble: 2 × 2
#>   splits            id     
#>   <list>            <chr>  
#> 1 <split [344/365]> Slice01
#> 2 <split [365/365]> Slice02
```

All of these functions produce analysis sets of the same size, with the
start and end of the analysis set “sliding” down your data frame. If
you’d rather have your analysis set get progressively larger, so that
you’re predicting new data based upon a growing set of older
observations, you can use the
[`sliding_window()`](https://rsample.tidymodels.org/reference/slide-resampling.md)
function with `lookback = -Inf`:

``` r
sliding_window(Chicago, lookback = Inf) |>
  head(2)
#> # A tibble: 2 × 2
#>   splits        id       
#>   <list>        <chr>    
#> 1 <split [1/1]> Slice0001
#> 2 <split [2/1]> Slice0002
```

This is commonly referred to as “evaluation on a rolling forecasting
origin”, or more colloquially, “rolling origin cross-validation”.

Note that all of these time-based resampling functions are
deterministic: unlike the rest of the package, running these functions
repeatedly under different random seeds will always return the same
results.
