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

