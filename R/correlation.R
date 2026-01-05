#' Summarize Correlations Between a Reference Variable and Others
#'
#' Computes correlations between a reference variable and one or more comparison variables.
#' For Pearson correlations, 95% confidence intervals are also calculated.
#' Can optionally stratify by a grouping variable. Returns a formatted `flextable`
#' and optionally prints a narrative summary describing weak, moderate, and strong correlations.
#'
#' @param data A data frame containing the variables of interest.
#' @param ref_var The reference variable (numeric) for correlation calculations.
#' @param compare_vars A character vector of variables to correlate with the reference variable.
#' @param by Optional grouping variable. If provided, correlations are calculated within each group.
#' @param method Correlation method. Options: "pearson", "spearman", or "kendall" (default: "pearson").
#' @param digits Number of decimal places to round correlation coefficients and CIs (default: 3).
#' @param report Logical. If TRUE, prints a narrative summary of correlations (default: TRUE).
#'
#' @importFrom stats cor.test
#' @return A `flextable` object showing correlations, 95% CI (Pearson only), p-values, and interpretation.
#'
#' @export
#'
#' @import dplyr
#' @import flextable
#' @import broom
#' @importFrom purrr walk
#'
#' @examples
#' # Example 1: Correlations across entire dataset
#' sum_cor(
#'   data = iris,
#'   ref_var = "Sepal.Length",
#'   compare_vars = c("Petal.Length", "Petal.Width", "Sepal.Width"),
#'   method = "pearson",
#'   digits = 2,
#'   report = TRUE
#' )
#'
#' # Example 2: Correlations by Species
#' sum_cor(
#'   data = iris,
#'   ref_var = "Sepal.Length",
#'   by = "Species",
#'   compare_vars = c("Petal.Length", "Petal.Width", "Sepal.Width"),
#'   method = "pearson",
#'   digits = 2,
#'   report = TRUE
#' )
sum_cor <- function(data, ref_var, compare_vars, by = NULL, method = "pearson", digits = 3, report = TRUE) {

  # Check variables
  if (!all(c(ref_var, compare_vars, by) %in% names(data))) stop("Some variables not found in data")

  # Function to compute correlation, CI (Pearson only), p-value, interpretation
  cor_ci <- function(x, y, method) {
    test <- suppressWarnings(cor.test(x, y, method = method))  # suppress tie warnings
    r <- test$estimate

    if(method == "pearson") {
      lower <- test$conf.int[1]
      upper <- test$conf.int[2]
    } else {
      lower <- NA
      upper <- NA
    }

    tibble(
      Correlation = r,
      Lower = lower,
      Upper = upper,
      p = test$p.value,
      Interpretation = case_when(
        abs(r) < 0.3 ~ "weak",
        abs(r) < 0.6 ~ "moderate",
        TRUE ~ "strong"
      )
    )
  }

  # Compute correlations
  if(!is.null(by)) {
    if(by == "Species") data[[by]] <- factor(data[[by]], levels = c("setosa", "versicolor", "virginica"))

    cor_list <- lapply(compare_vars, function(var2) {
      data %>%
        group_by(across(all_of(by))) %>%
        summarise(cor_ci(.data[[ref_var]], .data[[var2]], method), .groups = "drop") %>%
        mutate(Variable = var2) %>%
        select(all_of(by), Variable, Correlation, Lower, Upper, p, Interpretation)
    })

    cor_results <- bind_rows(cor_list) %>%
      arrange(.data[[by]], desc(abs(Correlation))) %>%
      mutate(
        across(c(Correlation, Lower, Upper), ~round(.x, digits)),
        p = ifelse(p < 0.001, "<0.001", round(p, digits))
      )

  } else {
    cor_results <- lapply(compare_vars, function(var2) {
      cor_ci(data[[ref_var]], data[[var2]], method) %>%
        mutate(Variable = var2) %>%
        select(Variable, Correlation, Lower, Upper, p, Interpretation)
    }) %>%
      bind_rows() %>%
      arrange(desc(abs(Correlation))) %>%
      mutate(
        across(c(Correlation, Lower, Upper), ~round(.x, digits)),
        p = ifelse(p < 0.001, "<0.001", round(p, digits))
      )
  }

  # Flextable
  ft <- flextable(cor_results) %>%
    add_header_lines(values = paste("Reference Variable:", ref_var)) %>%
    set_header_labels(
      Variable = "Comparison Variable",
      Correlation = "Correlation",
      Lower = "95% CI Lower",
      Upper = "95% CI Upper",
      p = "p-value",
      Interpretation = "Strength"
    ) %>%
    {if(!is.null(by)) merge_v(., j = by) else .} %>%
    footnote(i = 1, j = 1, value = as_paragraph(paste("Test:", method)), ref_symbols = "a") %>%
    colformat_double(j = c("Correlation", "Lower", "Upper"), digits = digits)

  # Narrative report
  if(report) {
    if(!is.null(by)) {
      cor_results %>%
        group_by(across(all_of(by))) %>%
        group_split() %>%
        walk(function(df_group) {
          group_name <- df_group[[by]][1]
          text <- paste0("For ", group_name, ": ")
          for(strength in c("strong","moderate","weak")) {
            df_strength <- df_group %>% filter(Interpretation==strength)
            if(nrow(df_strength) > 0){
              vars_text <- paste0(df_strength$Variable,
                                  " (r = ", df_strength$Correlation,
                                  if(!all(is.na(df_strength$Lower))) paste0(", 95% CI = ", df_strength$Lower, ",", df_strength$Upper) else "",
                                  ", p = ", df_strength$p, ")", collapse = " and ")
              text <- paste0(text, toupper(substr(strength,1,1)), substr(strength,2,nchar(strength)),
                             " correlation was found with ", vars_text, ". ")
            }
          }
          cat(text, "\n\n")
        })
    } else {
      text <- ""
      for(strength in c("strong","moderate","weak")) {
        df_strength <- cor_results %>% filter(Interpretation==strength)
        if(nrow(df_strength) > 0){
          vars_text <- paste0(df_strength$Variable,
                              " (r = ", df_strength$Correlation,
                              if(!all(is.na(df_strength$Lower))) paste0(", 95% CI = ", df_strength$Lower, ",", df_strength$Upper) else "",
                              ", p = ", df_strength$p, ")", collapse = " and ")
          text <- paste0(text, toupper(substr(strength,1,1)), substr(strength,2,nchar(strength)),
                         " correlation was found with ", vars_text, ". ")
        }
      }
      cat(text, "\n\n")
    }
  }

  return(ft)
}
