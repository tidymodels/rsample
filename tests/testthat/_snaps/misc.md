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

---

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

# reshuffle_rset is working

    Cannot reshuffle this rset (`attr(rset, 'strata')` is `TRUE`, not a column identifier)

---

    `manual_rset` objects cannot be reshuffled

---

    `rset` must be an rset object

