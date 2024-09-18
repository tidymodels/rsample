# improper argument

    Code
      make_splits("potato")
    Condition
      Error in `make_splits()`:
      ! No method for objects of class: character

# get_rsplit()

    Code
      get_rsplit(val, 3)
    Condition
      Error in `get_rsplit()`:
      ! `index` must be a length-1 integer between 1 and 1.
      * A value of 3 was provided.

---

    Code
      get_rsplit(val, c(1, 2))
    Condition
      Error in `get_rsplit()`:
      ! `index` must be a length-1 integer between 1 and 1.
      * Index was of length 2.

---

    Code
      get_rsplit(val, 1.5)
    Condition
      Error in `get_rsplit()`:
      ! `index` must be a length-1 integer between 1 and 1.
      * A value of 1.5 was provided.

---

    Code
      get_rsplit(warpbreaks, 1)
    Condition
      Error in `get_rsplit()`:
      ! No method for objects of class: data.frame

