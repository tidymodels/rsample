#' Tidy resampling object
#'
#' The `tidy` function from the `broom` package can be used on `rset` and `rsplit` objects to generate tibbles with which rows are in the analysis and assessment sets. 
#' @param x A `rset` or `rsplit` object
#' @param unique_ind Should unique row identifiers be returned? For example, if `FALSE` then bootstrapping results will include multiple rows in the sample for the same row in the original data. 
#' @param ... Not currently used. 
#' @return A tibble with columns `Row` and `Data`. The latter has possible values "Analysis" or "Assessment". For `rset` inputs, identification columns are also returned but their names and values depend on the type of resampling. `vfold_cv` contains a column "Fold" and, if repeats are used, another called "Repeats". `bootstraps` and `mc_cv` use the column "Resample". 
#' @examples 
#' set.seed(4121)
#' cv <-   tidy(vfold_cv(mtcars, v = 5))
#' rcv <-  tidy(vfold_cv(mtcars, v = 5, repeats = 2))
#' mccv <- tidy(mc_cv(mtcars, times = 10))
#' bt <-   tidy(bootstraps(mtcars, time = 5))
#' 
#' library(ggplot2)
#' theme_set(theme_bw())
#' 
#' ggplot(cv, aes(x = Fold, y = Row, fill = Data)) + 
#'   geom_tile() 
#' ggplot(rcv, aes(x = Fold, y = Row, fill = Data)) + 
#'   geom_tile() + facet_wrap(~Repeat)   
#' ggplot(mccv, aes(x = Resample, y = Row, fill = Data)) + 
#'   geom_tile()       
#' ggplot(bt, aes(x = Resample, y = Row, fill = Data)) + 
#'   geom_tile()
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
tidy.rsplit <- function(x, unique_ind = TRUE, ...) {
  if(unique_ind) x$in_id <- unique(x$in_id)
  tibble(Row = c(x$in_id, x$out_id),
         Data = rep(c("Analysis", "Assessment"),
                    c(length(x$in_id), length(x$out_id))))
}

#' @rdname tidy.rsplit
#' @export
#' @inheritParams tidy.rsplit
tidy.rset <- function(x, ...)  {
  stacked <- along(x, tidy, ...)
  for(i in seq(along = stacked))
    stacked[[i]]$Resample <- splits(x, "id")[i]
  stacked <- dplyr::bind_rows(stacked)
  stacked
}  
#' @rdname tidy.rsplit
#' @export
#' @inheritParams tidy.rsplit
tidy.vfold_cv <- function(x, ...)  {
  stacked <- along(x, tidy, ...)
  for(i in seq(along = stacked)) {
    if(x$repeats > 1) {
      stacked[[i]]$Repeat <- splits(x, "id")[i]
      stacked[[i]]$Fold <- splits(x, "id2")[i]
    } else stacked[[i]]$Fold <- splits(x, "id")[i]
  }
  stacked <- dplyr::bind_rows(stacked)
  stacked
}  
