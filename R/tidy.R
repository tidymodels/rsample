#' Tidy resampling object
#'
#' The \code{tidy} function from the  \pkg{broom} package can be used on  \code{rset} and  \code{rsplit} objects to generate tibbles with which rows are in the analysis and assessment sets. 
#' @param x A  \code{rset} or  \code{rsplit} object
#' @param unique_ind Should unique row identifiers be returned? For example, if  \code{FALSE} then bootstrapping results will include multiple rows in the sample for the same row in the original data. 
#' @param ... Not currently used. 
#' @return A tibble with columns  \code{Row} and  \code{Data}. The latter has possible values "Analysis" or "Assessment". For  \code{rset} inputs, identification columns are also returned but their names and values depend on the type of resampling.  \code{vfold_cv} contains a column "Fold" and, if repeats are used, another called "Repeats".  \code{bootstraps} and  \code{mc_cv} use the column "Resample". 
#' @examples 
#' library(ggplot2)
#' theme_set(theme_bw())
#' 
#' set.seed(4121)
#' cv <-   tidy(vfold_cv(mtcars, v = 5))
#' ggplot(cv, aes(x = Fold, y = Row, fill = Data)) + 
#'   geom_tile() + scale_fill_brewer("Paired")
#'   
#' set.seed(4121)
#' rcv <-  tidy(vfold_cv(mtcars, v = 5, repeats = 2))
#' ggplot(rcv, aes(x = Fold, y = Row, fill = Data)) + 
#'   geom_tile() + facet_wrap(~Repeat) + scale_fill_brewer("Paired")
#'   
#' set.seed(4121)
#' mccv <- tidy(mc_cv(mtcars, times = 5))
#' ggplot(mccv, aes(x = Resample, y = Row, fill = Data)) + 
#'   geom_tile() + scale_fill_brewer("Paired") 
#'   
#' set.seed(4121)
#' bt <-   tidy(bootstraps(mtcars, time = 5))
#' ggplot(bt, aes(x = Resample, y = Row, fill = Data)) + 
#'   geom_tile() + scale_fill_brewer("Paired")
#'   
#' dat <- data.frame(day = 1:30)
#' # Resample by week instead of day
#' ts_cv <- rolling_origin(dat, initial = 7, assess = 7, skip = 6, cumulative = FALSE)
#' ts_cv <- tidy(ts_cv)
#' ggplot(ts_cv, aes(x = Resample, y = factor(Row), fill = Data)) +
#'   geom_tile() + scale_fill_brewer("Paired")
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
  stacked <- purrr::map(x$splits, tidy)
  for(i in seq(along = stacked))
    stacked[[i]]$Resample <- x$id[i]
  stacked <- dplyr::bind_rows(stacked)
  stacked
}  
#' @rdname tidy.rsplit
#' @export
#' @inheritParams tidy.rsplit
tidy.vfold_cv <- function(x, ...)  {
  stacked <- purrr::map(x$splits, tidy)
  for(i in seq(along = stacked)) {
    if(attr(x, "repeats") > 1) {
      stacked[[i]]$Repeat <- x$id[i]
      stacked[[i]]$Fold <- x$id2[i]
    } else stacked[[i]]$Fold <- x$id[i]
  }
  stacked <- dplyr::bind_rows(stacked)
  stacked
}  
