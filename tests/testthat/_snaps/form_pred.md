# dots

    Code
      form_pred(y ~ .)
    Condition
      Error in `terms.formula()`:
      ! '.' in formula and no 'data' argument

---

    Code
      form_pred(terms(y ~ .))
    Condition
      Error in `terms.formula()`:
      ! '.' in formula and no 'data' argument

---

    Code
      form_pred(y ~ (.)^2)
    Condition
      Error in `terms.formula()`:
      ! '.' in formula and no 'data' argument

---

    Code
      form_pred(terms(y ~ (.)^2))
    Condition
      Error in `terms.formula()`:
      ! '.' in formula and no 'data' argument

