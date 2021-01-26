#' @importFrom utils globalVariables
#' @importFrom purrr map map_df map2 map_dfr map_dbl pluck map_lgl
#' @importFrom tibble tibble is_tibble as_tibble obj_sum type_sum
#' @importFrom tidyr gather unnest
#' @importFrom dplyr select %>% bind_cols bind_rows arrange_ arrange full_join
#' @importFrom dplyr mutate last ungroup group_by inner_join summarize do case_when
#' @importFrom rlang !! is_call is_string enquo quos exec is_list abort warn
#' @importFrom methods formalArgs
#' @importFrom stats quantile setNames qnorm pnorm
#' @importFrom tidyselect vars_select one_of
#' @importFrom furrr future_map_dfr
#' @importFrom tidyr gather



# ------------------------------------------------------------------------------

utils::globalVariables(
  c("model", "splits", "statistic", "Data", "Row", "id", ".", ".estimate",
    ".lower", ".upper", "Z0", "Za", "Zl", "Zu", "a", "cubed", "estimate",
    "orig", "p0", "squared", "term", "theta_0", "loo", "n", "..index", "models"
    )
  )
