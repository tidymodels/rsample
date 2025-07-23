# informative error messages

    Code
      inner_split(NULL)
    Condition
      Error in `inner_split()`:
      ! No method for objects of class: NULL.

---

    Code
      inner_split(m_rset)
    Condition
      Error in `inner_split()`:
      ! No method for objects of classes: manual_rset, rset, tbl_df, tbl, and data.frame.

# sliding_window_split needs at least 2 observations

    Code
      inner_split(r_split, split_args)
    Condition
      Error in `inner_split()`:
      ! This set cannot be split into an analysis and a calibration set as there is only one row.

# sliding_window_split with incomplete sets

    Code
      inner_split(r_split, split_args)
    Condition
      Error in `inner_split()`:
      ! No calibration split possible.

# sliding_index_split needs at least 2 observations

    Code
      inner_split(r_split, split_args)
    Condition
      Error in `inner_split()`:
      ! This set cannot be split into an analysis and a calibration set as there is only one row.

# sliding_index_split with incomplete sets

    Code
      inner_split(r_split, split_args)
    Condition
      Error in `inner_split()`:
      ! No calibration split possible.

