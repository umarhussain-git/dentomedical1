#' Binary Logistic Regression Table with Univariable and Multivariable Analysis
#'
#' Fits univariable and multivariable logistic regression models for a binary outcome,
#' summarizing odds ratios (ORs), 95% confidence intervals, and p-values.
#' Factor predictors include reference levels in the table. Returns a formatted `flextable`
#' and optionally provides an automatic textual interpretation of results.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome Name of the binary outcome variable (character).
#' @param predictors Character vector of predictor variable names.
#' @param report Logical; if TRUE, prints an automatic textual interpretation of multivariable results (default: FALSE).
#'
#' @return A `flextable` summarizing univariable and multivariable logistic regression results, including ORs, 95% CI, and p-values.
#'
#' @export
#'
#' @import dplyr
#' @import tibble
#' @import broom
#' @import flextable
#' @importFrom stats lm as.formula predict binomial glm confint
#' @importFrom broom tidy
#' @importFrom flextable flextable autofit bold add_footer_lines
#'
#' @examples logreg(data=medical_data(), outcome="case" ,
#'    predictors= c("age" ,  "parity" ,    "induced" ), report = TRUE)
#' @name logreg
utils::globalVariables(c(
  "term", "estimate", "conf.low", "conf.high", "p.value",
  "Univariable OR (95% CI)", "P-value (Univariable)",
  "Multivariable OR (95% CI)", "P-value (Multivariable)"
))
logreg <- function(data, outcome, predictors, report = FALSE) {

  # Convert character predictors to factor
  for (var in predictors) {
    if (is.character(data[[var]])) data[[var]] <- as.factor(data[[var]])
  }

  # Helper to format p-values
  format_pval <- function(p) {
    ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.2f", p)))
  }

  #-------------------- Fit logistic model --------------------#
  fit_glm <- function(formula) {
    glm(formula, data = data, family = binomial) %>%
      broom::tidy(conf.int = TRUE, exponentiate = TRUE)
  }

  add_reference <- function(var, fit_tidy) {
    if (is.factor(data[[var]])) {
      ref_level <- levels(data[[var]])[1]
      ref_row <- tibble(
        term = paste0(var, "_", ref_level),
        estimate = NA,
        conf.low = NA,
        conf.high = NA,
        p.value = NA
      )
      fit_tidy <- fit_tidy %>%
        mutate(term = paste0(var, "_", sub(paste0("^", var), "", term)))
      fit_tidy <- bind_rows(ref_row, fit_tidy)
    } else {
      fit_tidy <- fit_tidy %>% mutate(term = var)
    }
    fit_tidy
  }

  #-------------------- Univariable --------------------#
  uni_list <- lapply(predictors, function(var) {
    fit <- fit_glm(as.formula(paste(outcome, "~", var))) %>% filter(term != "(Intercept)")
    add_reference(var, fit)
  })

  uni_results_numeric <- bind_rows(uni_list)

  uni_results <- uni_results_numeric %>%
    mutate(
      `Univariable OR (95% CI)` = ifelse(is.na(estimate), "Reference",
                                         paste0(round(estimate,2), " (", round(conf.low,2), "-", round(conf.high,2), ")")),
      `P-value (Univariate)` = format_pval(p.value)
    ) %>%
    select(term, `Univariable OR (95% CI)`, `P-value (Univariate)`)

  #-------------------- Multivariable --------------------#
  formula_multi <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  multi_fit <- fit_glm(formula_multi) %>% filter(term != "(Intercept)")

  multi_list <- lapply(predictors, function(var) {
    fit <- multi_fit %>% filter(grepl(var, term))
    add_reference(var, fit)
  })

  multi_results_numeric <- bind_rows(multi_list)

  multi_results <- multi_results_numeric %>%
    mutate(
      `Multivariate OR (95% CI)` = ifelse(is.na(estimate), "Reference",
                                          paste0(round(estimate,2), " (", round(conf.low,2), "-", round(conf.high,2), ")")),
      `P-value (Multivariate)` = format_pval(p.value)
    ) %>%
    select(term, `Multivariate OR (95% CI)`, `P-value (Multivariate)`)

  #-------------------- Combine --------------------#
  combined <- full_join(uni_results, multi_results, by = "term") %>%
    rename(Predictor = term)

  #-------------------- Flextable --------------------#
  ft <- flextable(combined) %>%
    set_header_labels(
      Predictor = "Predictor",
      `Univariable OR (95% CI)` = "Univariable\nOR (95% CI)",
      `P-value (Univariate)` = "Univariable\np",
      `Multivariate OR (95% CI)` = "Multivariable\nOR (95% CI)",
      `P-value (Multivariate)` = "Multivariable\np"
    ) %>%
    bold(part = "header") %>%
    autofit() %>%
    add_footer_lines(values = "OR, Odds ratio; Binary logistic regression")

  #-------------------- Automatic Interpretation --------------------#

  if(report) {
    # Remove reference levels from interpretation
    interp_lines <- multi_results_numeric %>%
      filter(!is.na(estimate)) %>%  # omit reference rows
      mutate(
        Predictor_clean = gsub(".*_", "", term),  # remove var_ prefix for factors
        direction = case_when(
          estimate > 1 & p.value < 0.05 ~ "significant increase",
          estimate < 1 & p.value < 0.05 ~ "significant decrease",
          TRUE ~ "non-significant increase"
        ),
        sentence = paste0(
          Predictor_clean, " showed a ", direction,
          " in odds of ", outcome,
          " (OR ", round(estimate,2),
          ", 95% CI ", round(conf.low,2), "-", round(conf.high,2),
          "; p=", format_pval(p.value), ")."
        )
      )

    interpretation <- paste(interp_lines$sentence, collapse = " ")
    cat("\n--- Automatic Interpretation ---\n")
    cat(interpretation, "\n")
  }


  return(ft)
}



