# vec_rbind() returns a bare tibble

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `inner_resamples` -> `inner_resamples...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `inner_resamples` -> `inner_resamples...6`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

# vec_cbind() returns a bare tibble

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `inner_resamples` -> `inner_resamples...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `inner_resamples` -> `inner_resamples...6`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `inner_resamples` -> `inner_resamples...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `inner_resamples` -> `inner_resamples...6`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `inner_resamples` -> `inner_resamples...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `inner_resamples` -> `inner_resamples...6`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `inner_resamples` -> `inner_resamples...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `inner_resamples` -> `inner_resamples...6`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `inner_resamples` -> `inner_resamples...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `inner_resamples` -> `inner_resamples...6`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `id2` -> `id2...3`
      * `splits` -> `splits...4`
      * `id` -> `id...5`
      * `id2` -> `id2...6`

---

    Code
      expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

---

    Code
      expect_s3_class_bare_tibble(vec_cbind(x, x))
    Message
      New names:
      * `splits` -> `splits...1`
      * `id` -> `id...2`
      * `splits` -> `splits...3`
      * `id` -> `id...4`

