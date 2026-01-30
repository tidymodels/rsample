# Internal calibration split of the analysis set for fitting a post-processor

Internal calibration split of the analysis set for fitting a
post-processor

## Usage

``` r
internal_calibration_split(x, ...)

# Default S3 method
internal_calibration_split(x, ...)

# S3 method for class 'mc_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'group_mc_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'vfold_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'group_vfold_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'boot_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'group_boot_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'val_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'group_val_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'time_val_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'clustering_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'apparent_split'
internal_calibration_split(x, ...)

# S3 method for class 'sliding_window_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'sliding_index_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'sliding_period_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'initial_time_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'initial_validation_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'group_initial_validation_split'
internal_calibration_split(x, split_args, ...)

# S3 method for class 'initial_validation_time_split'
internal_calibration_split(x, split_args, ...)

calibration(x, ...)

# Default S3 method
calibration(x, ...)

# S3 method for class 'internal_calibration_split'
calibration(x, ...)

# S3 method for class 'internal_calibration_split'
assessment(x, ...)

# S3 method for class 'internal_calibration_split'
print(x, ...)
```

## Arguments

- x:

  An `rsplit` object.

- ...:

  Not currently used.

- split_args:

  A list of arguments to be used for the internal calibration split.

## Value

An `rsplit` object.

## Details

`rsplit` objects live most commonly inside of an `rset` object. The
`split_args` argument can be the output of
[`.get_split_args()`](https://rsample.tidymodels.org/reference/dot-get_split_args.md)
on that corresponding `rset` object, even if some of the arguments used
to create the `rset` object are not needed for the internal calibration
split.

- For `mc_split` and `group_mc_split` objects,
  `internal_calibration_split()` will ignore `split_args$times`.

- For `vfold_split` and `group_vfold_split` objects, it will ignore
  `split_args$times` and `split_args$repeats`. `split_args$v` will be
  used to set `split_args$prop` to `1 - 1/v` if `prop` is not already
  set and otherwise ignored. The method for `group_vfold_split` will
  always use `split_args$balance = NULL`.

- For `boot_split` and `group_boot_split` objects, it will ignore
  `split_args$times`.

- For `val_split`, `group_val_split`, and `time_val_split` objects, it
  will interpret a length-2 `split_args$prop` as a ratio between the
  training and validation sets and split into inner analysis and
  calibration set in the same ratio. If `split_args$prop` is a single
  value, it will be used as the proportion of the inner analysis set.

- For `clustering_split` objects, it will ignore `split_args$repeats`.
