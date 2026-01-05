#' Categorize a Numeric Variable into Custom Ranges
#'
#' `category` creates a new categorical variable by splitting a numeric column
#' into specified ranges. You can provide custom labels for each range, or it will
#' default to the range values themselves.
#'
#' @param data A data.frame or tibble containing the numeric variable.
#' @param var The numeric variable to categorize (unquoted).
#' @param level A character vector of numeric ranges, e.g., c("18-25", "26-35").
#' @param new_var Optional. Name of the new categorical variable. Defaults to "<var>_group".
#' @param labels Optional. Custom labels for each category. Length must match `level`.
#' @importFrom purrr map2
#' @importFrom rlang enquo !!
#' @importFrom rlang sym expr
#' @importFrom dplyr pull mutate
#'
#' @return A data.frame or tibble with a new categorical variable.
#' @export
#'
#' @examples
#' df <- data.frame(age = c(20, 28, 40, 55, 34, 10, 24, 55))
#'
#' # Categorize without custom labels
#' category(df, var = age, level = c("10-25", "26-35", "36-50"))
#'
#' # Categorize with custom labels
#' category(df, var = age, level = c("10-25", "26-35", "36-50"),
#'          labels = c("young", "adult", "old"))
#'
category <- function(data,
                     var,        # unquoted numeric column
                     level,      # numeric ranges as character: "18-25", "26-35"
                     new_var = NULL,
                     labels = NULL) {  # optional labels

  # check for missing level
  if (missing(level)) stop("Please provide 'level' as numeric ranges, e.g., c('18-25','26-35')")

  if (!is.numeric(dplyr::pull(data, {{var}}))) {
    stop("'var' must be a numeric column")
  }

  var_enq <- enquo(var)

  # default new_var name = varname_group
  if (is.null(new_var)) {
    new_var <- paste0(as_label(var_enq), "_group")
  }

  new_var_sym <- sym(new_var)

  # normalize dash
  level_clean <- gsub("\u2013", "-", level)

  # extract numeric lower and upper limits
  limits <- strsplit(level_clean, "-")
  limits <- do.call(rbind, limits)

  # convert to numeric
  limits <- apply(limits, 2, function(x) as.numeric(x))
  if (any(is.na(limits))) stop("All ranges in 'level' must be numeric, e.g., '18-25'")

  # check labels
  if (!is.null(labels)) {
    if (length(labels) != nrow(limits)) stop("Length of 'labels' must match number of ranges in 'level'")
  } else {
    labels <- level  # default labels = ranges
  }

  # dynamically build case_when expressions
  case_expr <- map2(
    limits[,1], limits[,2],
    ~ expr(!!var_enq >= !!.x & !!var_enq <= !!.y ~ !!labels[which(limits[,1] == .x & limits[,2] == .y)])
  )

  # add catch-all for values outside ranges
  case_expr <- append(case_expr, list(expr(TRUE ~ NA_character_)))

  # create new column
  data %>%
    mutate(
      !!new_var_sym := case_when(!!!case_expr)
    )
}




