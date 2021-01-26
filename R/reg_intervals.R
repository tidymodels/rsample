#' A convenience function for confidence intervals with linear-ish parametric models
#'
#' @param formula An R model formula with one outcome and at least one predictor.
#' @param data A data frame.
#' @param model_fn The model to fit. Allowable values are "lm", "glm",
#'  "survreg", and "coxph". The latter two require that the `survival` package
#'  be installed.
#' @param type The type of bootstrap confidence interval. Values of "student-t" and
#' "percentile" are allowed.
#' @param times A single integer for the number of bootstrap samples. If left
#' NULL, 1,001 are used for t-intervals and 2,001 for percentile intervals.
#' @param alpha Level of significance.
#' @param filter A logical expression used to remove rows from the final result, or `NULL` to keep all rows.
#' @param keep_reps Should the individual parameter estimates for each bootstrap
#' sample be retained?
#' @param ... Options to pass to the model function (such as `family` for `glm()`).
#' @return A tibble with columns "term", ".lower", ".estimate", ".upper",
#' ".alpha", and ".method". If `keep_reps = TRUE`, an additional list column
#' called ".replicates" is also returned.
#' @export
#' @seealso [int_pctl()], [int_t()]
#' @references
#' Davison, A., & Hinkley, D. (1997). _Bootstrap Methods and their
#'  Application_. Cambridge: Cambridge University Press.
#'  doi:10.1017/CBO9780511802843
#'
#' _Bootstrap Confidence Intervals_,
#' \url{https://rsample.tidymodels.org/articles/Applications/Intervals.html}
#' @examples
#' \donttest{
#' set.seed(1)
#' reg_intervals(mpg ~ I(1/sqrt(disp)), data = mtcars)
#'
#' set.seed(1)
#' reg_intervals(mpg ~ I(1/sqrt(disp)), data = mtcars, keep_reps = TRUE)
#' }
reg_intervals <-
  function(formula, data, model_fn = "lm", type = "student-t", times = NULL,
           alpha = 0.05, filter = term != "(Intercept)", keep_reps = FALSE, ...) {
    model_fn <- match.arg(model_fn, c("lm", "glm", "survreg", "coxph"))
    type <- match.arg(type, c("student-t", "percentile"))

    filter <- rlang::enexpr(filter)

    if (is.null(times)) {
      if (type == "student-t") {
        times <- 1001
      } else {
        times <- 2001
      }
    } else {
      times <- times[1]
      if (!is.numeric(times)) {
        rlang::abort("'times' should be a single integer.")
      }
    }

    if (length(alpha) != 1 || !is.numeric(alpha)) {
      abort("`alpha` must be a single numeric value.")
    }

    if (model_fn %in% c("survreg", "coxph")) {
      pkg <- "survival"
      rlang::check_installed("survival")
    } else {
      pkg <- NULL
    }
    fn_call <- rlang::call2(model_fn, formula = formula,
                            data = rlang::expr(data), .ns = pkg, ...)

    bt <- rsample::bootstraps(data, times = times, apparent = type %in% c("student-t"))
    bt <-
      dplyr::mutate(bt,
                    models =
                      purrr::map(splits,
                                 ~ model_results(rsample::analysis(.x), fn_call, filter)
                      )
      )
    if (type == "student-t") {
      res <- int_t(bt, models, alpha = alpha)
    } else {
      res <- int_pctl(bt, models, alpha = alpha)
    }

    if (keep_reps) {
      bt <- bt[bt$id != "Apparent",]
      reps <- purrr::map_dfr(bt$models, I)
      reps <- dplyr::group_nest(reps, term, .key = ".replicates")
      res <- dplyr::full_join(res, reps, by = "term")
    }

    res
  }

# TODO add handler for survival models to catch warnings? That seems to be the
# only way to know about convergence.
model_results <- function(data, cl, flt) {
  mod <- broom::tidy(rlang::eval_tidy(cl, data))
  mod <- mod[, c("term", "estimate", "std.error")]
  if (is.language(flt)) {
    mod <- dplyr::filter(mod, !!flt)
  }
  mod
}
