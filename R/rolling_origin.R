#' Rolling Origin Forecast Resampling
#'
#' This resampling method is useful when the data set has a strong time component. The resamples are not random and contain data points that are consecutive values. The function assumes that the original data set are sorted in time order. 
#' 
#' @details
#' The main options, \code{initial} and \code{assess}, control the number of data points from the original data that are in the analysis and assessment set, respectively. When \code{cumulative = TRUE}, the analysis set will grow as resampling continues while the assessment set size will always remain static. 
#' \code{skip} enables the function to not use every data point in the resamples. When \code{skip = 0}, the resampling data sets will increment by one position. Suppose that the rows of a data set are consecutive days. Using \code{skip = 6} will make the analysis data set operate on \emph{weeks} instead of days. The assessment set size is not affected by this option. 
#' @inheritParams vfold_cv
#' @param initial The number of samples used for analysis/modeling in the initial resample. 
#' @param assess The number of samples used for each assessment resample.
#' @param cumulative A logical. Should the analysis resample grow beyond the size specified by \code{initial} at each resample?.
#' @param skip A integer indicating how many (if any) resamples to skip to thin the total amount of data points in the analysis resample. 
#' @export
#' @return  An tibble with classes \class{rolling_origin}, \class{rset}, \class{tbl_df}, \class{tbl}, and \class{data.frame}. The results include a column for the data split objects and a column called \code{id} that has a character string with the resample identifier.
#' @examples
#' set.seed(1131)
#' ex_data <- data.frame(row = 1:20, some_var = rnorm(20))
#' dim(rolling_origin(ex_data))
#' dim(rolling_origin(ex_data, skip = 2))
#' dim(rolling_origin(ex_data, skip = 2, cumulative = FALSE))
#' @export
rolling_origin <- function(data, initial = 5, assess = 1, cumulative = TRUE, skip = 0, ...) {
  n <- nrow(data)
  
  if (n <= initial + assess)
    stop("There should be at least ",
         initial + assess,
         " nrows in `data`",
         call. = FALSE)
  
  stops <- seq(initial, (n - assess), by = skip + 1)
  starts <- if (!cumulative)
    stops - initial + 1
  else
    starts <- rep(1, length(stops))
  
  in_ind <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  out_ind <-
    mapply(seq, stops + 1, stops + assess, SIMPLIFY = FALSE)
  indices <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "rof_split")
  split_objs <- tibble::tibble(splits = split_objs,
                               id = names0(length(split_objs), "Slice"))
  
  attr(split_objs, "initial") <- initial
  attr(split_objs, "assess") <- assess
  attr(split_objs, "cumulative") <- cumulative
  attr(split_objs, "skip") <- skip
  
  class(split_objs) <- c("rolling_origin", "rset", class(split_objs))
  split_objs
}

#' @export
print.rolling_origin <- function(x, ...) {
  details <- attributes(x)
  cat("# Rolling origin forecast resampling\n")
  class(x) <- class(x)[!(class(x) %in% c("rolling_origin", "rset"))]
  print(x)
}