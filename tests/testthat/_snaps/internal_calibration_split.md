# informative error messages

    Code
      internal_calibration_split(NULL)
    Condition
      Error in `internal_calibration_split()`:
      ! No method for objects of class: NULL.

---

    Code
      internal_calibration_split(m_rset)
    Condition
      Error in `internal_calibration_split()`:
      ! No method for objects of classes: manual_rset, rset, tbl_df, tbl, and data.frame.

# mc_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_mc_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# vfold_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_vfold_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# boot_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_boot_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# val_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_val_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# time_val_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# clustering_split can create mock split

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# sliding_window_split needs at least 2 observations

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      This set cannot be split into an analysis and a calibration set as there is only one row; creating an empty calibration set.

# sliding_window_split with incomplete sets

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# sliding_index_split needs at least 2 observations

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      This set cannot be split into an analysis and a calibration set as there is only one row; creating an empty calibration set.

# sliding_index_split with incomplete sets

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# sliding_period_split needs at least 2 observations

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      This set cannot be split into an analysis and a calibration set as there is only one row; creating an empty calibration set.

# sliding_period_split needs observations in at least 2 periods

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

---

    Code
      split_cal <- internal_calibration_split(r_split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# initial_time_split can create mock split

    Code
      split_cal <- internal_calibration_split(split, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# initial_validation_split can create mock split

    Code
      split_cal <- internal_calibration_split(initial_vsplit, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# group_initial_validation_split can create mock split

    Code
      split_cal <- internal_calibration_split(initial_vsplit, split_args)
    Condition
      Warning:
      Cannot create calibration split; creating an empty calibration set.

# initial_validation_time_split can create mock split

    Code
      split_cal <- internal_calibration_split(initial_vsplit, split_args)
    Condition
      Warning:
      This set cannot be split into a training and a calibration set as there is only one row; creating an empty calibration set.

# assessment() fails

    Code
      assessment(internal_calibration_split)
    Condition
      Error in `assessment()`:
      ! Internal calibration splits are designed to only return analysis and calibration sets.

# print()

    Code
      print(internal_calibration_split)
    Output
      <Analysis/Calibration/Total>
      <32/0/32>

