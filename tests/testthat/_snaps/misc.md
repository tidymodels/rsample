# reverse_splits is working

    Code
      reverse_splits(1)
    Condition
      Error in `reverse_splits()`:
      ! `x` must be either an `rsplit` or an `rset` object

---

    Code
      reverse_splits(permutes)
    Condition
      Error in `reverse_splits()`:
      ! Permutations cannot have their splits reversed

---

    Code
      reverse_splits(permutes$splits[[1]])
    Condition
      Error in `reverse_splits()`:
      ! Permutations cannot have their splits reversed

