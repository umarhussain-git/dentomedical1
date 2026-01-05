#' Recode values in a data frame using a lookup table
#'
#' This function replaces values in a data frame according to a named lookup vector.
#' All columns are converted to character, and any value matching a name in `lookup`
#' will be replaced by its corresponding value.
#'
#' @param data A data frame whose values you want to recode.
#' @param lookup A named character vector where names are original values and elements are the new values.
#' @import dplyr
#'
#' @return A data frame with the same structure as `data`, with values recoded according to `lookup`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   gender = c("male", "F", "male", "female"),
#'   status = c("single", "Married", "oo", "M"),
#'   stringsAsFactors = FALSE
#' )
#'
#' lookup <- c(
#'   "male" = "Male",
#'   "M" = "Married",
#'   "oo" = "Widow",
#'   "female" = "Female",
#'   "F" = "Female"
#' )
#'
#' df_recode <- recode_data(df, lookup)
#' print(df_recode)
#'
recode_data <- function(data, lookup) {
  data %>%
    mutate(across(everything(), ~{
      x <- as.character(.)
      recoded <- ifelse(x %in% names(lookup), lookup[x], x)
      recoded
    }))
}
