#' Linear Regression Table with Univariable and Multivariable Analysis
#'
#' Fits univariable and multivariable linear regression models for a continuous outcome,
#' summarizing beta coefficients, 95% confidence intervals, and p-values.
#' Factor predictors include reference levels in the table. Returns a formatted `flextable`
#' and optionally provides an automatic textual interpretation of results.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome Name of the continuous outcome variable (character).
#' @param predictors Character vector of predictor variable names.
#' @param report Logical; if TRUE, prints an automatic textual interpretation of multivariable results (default: TRUE).
#'
#' @importFrom stats lm as.formula predict binomial glm confint
#' @importFrom broom tidy
#' @importFrom flextable flextable autofit bold add_footer_lines
#' @return
#' If `report = FALSE`, returns a `flextable` summarizing univariable and multivariable beta coefficients, 95% CI, and p-values.
#' If `report = TRUE`, returns a list with `table` (the flextable) and `interpretation` (textual summary of multivariable results).
#'
#' @export
#'
#' @import dplyr
#' @import tibble
#' @import broom
#' @import flextable
#'
#' @examples
#' # Apply linear regression on iris dataset
#' linreg(
#'   data = iris,
#'   outcome = "Sepal.Length",
#'   predictors = c("Sepal.Width", "Petal.Length", "Species"),
#'   report = TRUE
#' )
linreg <- function(data, outcome, predictors, report = TRUE) {
  data <- as_tibble(data)

  # Format p-values
  format_pval <- function(p) {
    ifelse(p < 0.001, "<0.001", sprintf("%.2f", p))
  }

  #-------------------- Univariable --------------------#
  uni_list <- list()
  for (var in predictors) {
    model_uni <- lm(as.formula(paste(outcome, "~", var)), data = data)
    t <- broom::tidy(model_uni, conf.int = TRUE)

    if (is.numeric(data[[var]])) {
      uni_list[[var]] <- tibble(
        Predictor = var,
        `Univariable beta (95% CI)` = paste0(round(t$estimate[-1],2),
                                             " (", round(t$conf.low[-1],2),
                                             ", ", round(t$conf.high[-1],2), ")"),
        `Univariable p` = format_pval(t$p.value[-1])
      )
    } else {
      levels <- levels(as.factor(data[[var]]))
      ref <- levels[1]
      ref_row <- tibble(
        Predictor = ref,
        `Univariable beta (95% CI)` = "Reference",
        `Univariable p` = ""
      )

      other_rows <- t %>%
        filter(row_number() != 1) %>%
        mutate(Predictor = gsub(var, "", term),
               `Univariable beta (95% CI)` = paste0(round(estimate,2),
                                                    " (", round(conf.low,2),
                                                    ", ", round(conf.high,2), ")"),
               `Univariable p` = format_pval(p.value)) %>%
        select(Predictor, `Univariable beta (95% CI)`, `Univariable p`)

      uni_list[[var]] <- bind_rows(ref_row, other_rows)
    }
  }
  uni_results <- bind_rows(uni_list)

  #-------------------- Multivariable --------------------#
  model_multi <- lm(as.formula(paste(outcome, "~", paste(predictors, collapse=" + "))),
                    data = data)
  t_multi <- broom::tidy(model_multi, conf.int = TRUE)

  multi_list <- list()
  for (var in predictors) {
    if (is.numeric(data[[var]])) {
      row <- t_multi %>% filter(term == var)
      multi_list[[var]] <- tibble(
        Predictor = var,
        `Multivariable beta (95% CI)` = paste0(round(row$estimate,2),
                                               " (", round(row$conf.low,2),
                                               ", ", round(row$conf.high,2), ")"),
        `Multivariable p` = format_pval(row$p.value)
      )
    } else {
      levels <- levels(as.factor(data[[var]]))
      ref <- levels[1]
      ref_row <- tibble(
        Predictor = ref,
        `Multivariable beta (95% CI)` = "Reference",
        `Multivariable p` = ""
      )

      other_rows <- t_multi %>%
        filter(grepl(var, term)) %>%
        mutate(Predictor = gsub(var, "", term),
               `Multivariable beta (95% CI)` = paste0(round(estimate,2),
                                                      " (", round(conf.low,2),
                                                      ", ", round(conf.high,2), ")"),
               `Multivariable p` = format_pval(p.value)) %>%
        select(Predictor, `Multivariable beta (95% CI)`, `Multivariable p`)

      multi_list[[var]] <- bind_rows(ref_row, other_rows)
    }
  }
  multi_results <- bind_rows(multi_list)

  #-------------------- Merge and final table --------------------#
  final_tbl <- uni_results %>%
    left_join(multi_results, by = "Predictor")

  # Rename columns with two lines
  colnames(final_tbl) <- c(
    "Predictor",
    "Univariable\nBeta (95% CI)",
    "Univariable\np",
    "Multivariable\nBeta (95% CI)",
    "Multivariable\np"
  )

  # Footer text
  footer_txt <- paste0(
    "Multivariable model: R^2 = ", round(summary(model_multi)$r.squared,3),
    "; Adjusted R^2 = ", round(summary(model_multi)$adj.r.squared,3)
  )

  ft <- flextable::flextable(final_tbl) %>%
    flextable::autofit() %>%
    flextable::bold(part="header") %>%
    flextable::add_footer_lines(footer_txt)

  #-------------------- Automatic Interpretation --------------------#
  if (report) {
    tidy_res <- tidy(model_multi, conf.int = TRUE) %>%
      filter(term != "(Intercept)")

    sig <- tidy_res %>% filter(p.value < 0.05)
    nonsig <- tidy_res %>% filter(p.value >= 0.05)

    sig_text <- if (nrow(sig) > 0) {
      paste(apply(sig, 1, function(x) {
        direction <- ifelse(as.numeric(x["estimate"]) > 0, "increase", "decrease")
        stat_sig <- ifelse(as.numeric(x["p.value"]) < 0.05, "statistically significant ", "")
        paste0(
          "Each one-unit increase in ", x["term"], " was associated with a ",
          stat_sig, direction, " of ", abs(round(as.numeric(x["estimate"]),2)),
          " units in ", outcome, " (95% CI ", round(as.numeric(x["conf.low"]),2),
          " to ", round(as.numeric(x["conf.high"]),2),
          ", p = ", format_pval(as.numeric(x["p.value"])), ")."
        )
      }), collapse = " ")
    } else { "No predictors were significant." }

    nonsig_text <- if (nrow(nonsig) > 0) {
      paste(paste(nonsig$term, collapse = ", "), "were non-significant predictors.")
    } else { "" }

    interpretation <- paste0(
      "The multivariable model explained ", round(summary(model_multi)$r.squared*100,1), "% of variance in ", outcome,
      " (Adjusted R^2 = ", round(summary(model_multi)$adj.r.squared,3), "). ",
      sig_text, " ", nonsig_text
    )

    return(list(table = ft, interpretation = interpretation))
  } else {
    return(ft)
  }
}

