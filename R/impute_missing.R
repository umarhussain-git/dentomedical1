#' Impute Missing Values in a Data Frame
#'
#' This function imputes missing values in a data frame.
#' For categorical variables (factor or character), missing values are replaced with the mode.
#' For numeric variables, missing values can be imputed using the mean, median, or regression-based imputation.
#' If no method is specified for numeric columns, missing values are left as NA.
#'
#' @param data A data frame containing numeric and/or categorical variables with missing values.
#' @param method A character string specifying the imputation method for numeric columns.
#'               Options are \code{"mean"}, \code{"median"}, \code{"regression"}, or \code{NULL} (default: \code{NULL}).
#'
#' @return A data frame with missing values imputed according to the specified method.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' # Impute numeric columns using regression and categorical with mode
#' impute_missing(starwars, method = "regression")
#'
#' # Impute numeric columns using mean
#' impute_missing(starwars, method = "mean")
#'
#' # Impute numeric columns using median
#' impute_missing(starwars, method = "median")
impute_missing <- function(data, method = NULL) {

  data_imputed <- data

  for (col in names(data)) {

    # Skip if no missing values
    if (!any(is.na(data[[col]]))) next

    # Mode calculation inside function
    mode_calc <- function(x) {
      ux <- unique(x[!is.na(x)])
      ux[which.max(tabulate(match(x, ux)))]
    }

    # Categorical: always mode
    if (is.factor(data[[col]]) | is.character(data[[col]])) {
      data_imputed[[col]][is.na(data[[col]])] <- mode_calc(data[[col]])
    }

    # Numeric columns
    if (is.numeric(data[[col]])) {
      if (is.null(method)) {
        next  # leave as NA
      } else if (method == "mean") {
        data_imputed[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      } else if (method == "median") {
        data_imputed[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      } else if (method == "regression") {
        # Use other numeric columns as predictors
        predictors <- data[, sapply(data, is.numeric) & names(data) != col, drop = FALSE]
        if (ncol(predictors) == 0) {
          warning(paste("No numeric predictors available for regression imputation of", col))
        } else {
          # Combine target and predictors
          df_model <- cbind(target = data[[col]], predictors)
          complete_cases <- complete.cases(df_model)
          if (sum(complete_cases) > 0) {
            model <- lm(target ~ ., data = df_model[complete_cases, , drop = FALSE])
            missing_idx <- which(is.na(data[[col]]))
            predicted <- predict(model, newdata = df_model[missing_idx, , drop = FALSE])

            # Cap predictions to observed range
            observed_range <- range(data[[col]], na.rm = TRUE)
            predicted <- pmax(pmin(predicted, observed_range[2]), observed_range[1])

            data_imputed[[col]][missing_idx] <- predicted
          } else {
            warning(paste("Not enough data to impute", col, "using regression."))
          }
        }

        # Fallback to median for any remaining NAs
        if (any(is.na(data_imputed[[col]]))) {
          data_imputed[[col]][is.na(data_imputed[[col]])] <- median(data[[col]], na.rm = TRUE)
        }

      } else {
        stop("Invalid method. Choose 'mean', 'median', 'regression', or NULL")
      }
    }
  }

  return(data_imputed)
}
