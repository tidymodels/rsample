# Constructor for new rset objects

Constructor for new rset objects

## Usage

``` r
new_rset(splits, ids, attrib = NULL, subclass = character())
```

## Arguments

- splits:

  A list column of `rsplits` or a tibble with a single column called
  "splits" with a list column of `rsplits`.

- ids:

  A character vector or a tibble with one or more columns that begin
  with "id".

- attrib:

  An optional named list of attributes to add to the object.

- subclass:

  A character vector of subclasses to add.

## Value

An `rset` object.

## Details

Once the new `rset` is constructed, an additional attribute called
"fingerprint" is added that is a hash of the `rset`. This can be used to
make sure other objects have the exact same resamples.
