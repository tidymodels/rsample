# Compatibility with dplyr

This page lays out the compatibility between rsample and dplyr. The
`rset` objects from rsample are a specific subclass of tibbles, hence
standard dplyr operations like joins as well row or column modifications
work. However, whether the operation returns an rset or a tibble depends
on the details of the operation.

The overarching principle is that any operation which leaves the
specific characteristics of an rset intact will return an rset. If an
operation modifies any of the following characteristics, the result will
be a `tibble` rather than an `rset`:

- Rows: The number of rows needs to remain unchanged to retain the rset
  property. For example, you can't have a 10-fold CV object without 10
  rows. The order of the rows can be changed though and the object
  remains an rset.

- Columns: The `splits` column and the `id` column(s) are required for
  an rset and need to remain untouched. They cannot be dropped, renamed,
  or modified if the result should remain an rset.

### Joins

The following affect all of the dplyr joins, such as
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
and
[`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).

The resulting object is an `rset` if the number of rows is unaffected.
Rows can be reordered but not added or removed, otherwise the resulting
object is a `tibble`.

|                   |                               |                    |
|-------------------|-------------------------------|--------------------|
| operation         | same rows, possibly reordered | add or remove rows |
| `join(rset, tbl)` | `rset`                        | `tibble`           |

### Row Operations

The resulting object is an `rset` if the number of rows is unaffected.
Rows can be reordered but not added or removed, otherwise the resulting
object is a `tibble`.

|                 |                               |                    |
|-----------------|-------------------------------|--------------------|
| operation       | same rows, possibly reordered | add or remove rows |
| `rset[ind,]`    | `rset`                        | `tibble`           |
| `slice(rset)`   | `rset`                        | `tibble`           |
| `filter(rset)`  | `rset`                        | `tibble`           |
| `arrange(rset)` | `rset`                        | `tibble`           |

### Column Operations

The resulting object is an `rset` if the required `splits` and `id`
columns remain unaltered. Otherwise the resulting object is a `tibble`.

|                |                            |                                                |
|----------------|----------------------------|------------------------------------------------|
| operation      | required columns unaltered | required columns removed, renamed, or modified |
| `rset[,ind]`   | `rset`                     | `tibble`                                       |
| `select(rset)` | `rset`                     | `tibble`                                       |
| `rename(rset)` | `rset`                     | `tibble`                                       |
| `mutate(rset)` | `rset`                     | `tibble`                                       |
