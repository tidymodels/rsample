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

# mc_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_mc_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# vfold_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_vfold_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# boot_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_boot_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# val_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_val_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# time_val_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# clustering_split can create mock split

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# sliding_window_split needs at least 2 observations

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      This set cannot be split into an analysis and a calibration set as there is only one row; creating an empty calibration set.

# sliding_window_split with incomplete sets

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# sliding_index_split needs at least 2 observations

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      This set cannot be split into an analysis and a calibration set as there is only one row; creating an empty calibration set.

# sliding_index_split with incomplete sets

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# sliding_period_split needs at least 2 observations

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      This set cannot be split into an analysis and a calibration set as there is only one row; creating an empty calibration set.

# sliding_period_split needs observations in at least 2 periods

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

---

    Code
      isplit <- inner_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# initial_time_split can create mock split

    Code
      isplit <- inner_split(split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# initial_validation_split can create mock split

    Code
      isplit <- inner_split(initial_vsplit, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_initial_validation_split can create mock split

    Code
      isplit <- inner_split(initial_vsplit, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# initial_validation_time_split can create mock split

    Code
      isplit <- inner_split(initial_vsplit, split_args)
    Condition
      Warning:
      This set cannot be split into a training and a calibration set as there is only one row; creating an empty calibration set.

# assessment() fails

    Code
      assessment(inner_split)
    Condition
      Error in `assessment()`:
      ! Inner splits are designed to only return analysis and calibration sets.

# print()

    Code
      print(inner_split)
    Output
      <Analysis/Calibration/Total>
      <32/0/32>

