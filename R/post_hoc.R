#' Summarize Variables with Post-hoc Tests: Mutiple comparisons
#'
#' Produces a summary table of numeric or categorical variables grouped by a factor,
#' optionally performing global tests (ANOVA or Kruskal-Wallis) and post-hoc comparisons
#' (Tukey or Dunn test). Numeric variables can be summarized using mean (SD) or median (IQR).
#' Returns a `flextable` suitable for reporting.
#'
#' @param data A data frame containing the variables to summarize.
#' @param by The grouping variable for comparisons (factor or character).
#' @param variables A character vector of variable names to summarize. If NULL, all variables except `by` are used.
#' @param digits Number of decimal places for numeric summaries (default: 2).
#' @param format_lt Character string to display very small p-values, e.g., "<0.001" (default: "<0.001").
#' @param na.rm Logical. If TRUE, removes missing values before computations (default: TRUE).
#' @param statistic Summary statistic for numeric variables. Options: "auto" (default), "mean_sd", "med_iqr".
#' @param test_type Global test type. Options: "auto" (default), "anova", "kruskal".
#'
#' @importFrom stats complete.cases shapiro.test TukeyHSD na.omit
#' @return A `flextable` containing the summary table, global p-values, and post-hoc results.
#'
#' @export
#'
#' @import FSA
#' @import dplyr
#' @import tidyr
#' @import flextable
#' @import tibble
#'
#' @examples
#' sum_posthoc(
#'   data = iris,
#'   by = "Species",
#'   variables = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
#'   digits = 2,
#'   statistic = "auto",
#'   test_type = "auto"
#' )
sum_posthoc <- function(data,
                        by,
                        variables = NULL,
                        digits = 2,
                        format_lt = "<0.001",
                        na.rm = TRUE,
                        statistic = "auto",    # "auto", "mean_sd", "med_iqr"
                        test_type = "auto") {  # "auto", "anova", "kruskal"

  data <- as_tibble(data)
  if (is.null(variables)) variables <- names(data)[names(data) != by]

  summary_list <- list()
  posthoc_list <- list()
  used_tests <- c()

  grp_levels <- levels(as.factor(data[[by]]))

  for (varname in variables) {
    var <- data[[varname]]
    grp <- data[[by]]

    if (na.rm) {
      idx <- complete.cases(var, grp)
      var <- var[idx]
      grp <- grp[idx]
    }

    # ----- DETERMINE TEST TYPE -----
    test_type_var <- test_type
    if (test_type == "auto") {
      if (is.numeric(var) && length(unique(grp)) > 1) {
        normal_check <- sapply(unique(grp), function(g) {
          if(length(var[grp == g]) >= 3) shapiro.test(var[grp == g])$p.value > 0.05 else TRUE
        })
        test_type_var <- ifelse(all(normal_check), "anova", "kruskal")
      } else {
        test_type_var <- NA
      }
    }

    # ----- DETERMINE STATISTIC TYPE -----
    stat_label <- statistic
    if (statistic == "auto") {
      stat_label <- ifelse(test_type_var == "anova", "Mean (SD)",
                           ifelse(test_type_var == "kruskal", "Median (IQR)", NA))
    }

    # Add stars to variable name
    var_label <- varname
    if (!is.na(test_type_var)) {
      var_label <- ifelse(test_type_var == "anova", paste0(varname, "*"), paste0(varname, "**"))
    }

    # ----- MAIN SUMMARY -----
    if (is.numeric(var)) {
      summary_tbl <- data %>%
        group_by(!!sym(by)) %>%
        summarise(
          val = if (stat_label=="Mean (SD)") {
            paste0(round(mean(!!sym(varname)), digits),
                   " (", round(sd(!!sym(varname)), digits), ")")
          } else {
            paste0(round(median(!!sym(varname)), digits),
                   " (", round(quantile(!!sym(varname), 0.25), digits),
                   ", ", round(quantile(!!sym(varname), 0.75), digits), ")")
          },
          .groups = "drop"
        ) %>%
        mutate(Variable = var_label,
               Characteristic = stat_label) %>%
        pivot_wider(names_from = !!sym(by), values_from = val)
    } else {
      summary_tbl <- data %>%
        group_by(!!sym(by), !!sym(varname)) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(!!sym(by)) %>%
        mutate(pct = round(n/sum(n)*100)) %>%
        ungroup() %>%
        mutate(
          Variable = var_label,
          Characteristic = as.character(!!sym(varname)),
          val = paste0(n, " (", pct, "%)")
        ) %>%
        pivot_wider(names_from = !!sym(by), values_from = val)
    }

    # ----- GLOBAL TEST -----
    pval_main <- NA
    if (!is.na(test_type_var)) {
      if (test_type_var == "anova") {
        aov_res <- aov(var ~ grp)
        pval_main <- summary(aov_res)[[1]]$`Pr(>F)`[1]
        used_tests <- c(used_tests, "ANOVA + Tukey")
      } else if (test_type_var == "kruskal") {
        pval_main <- kruskal.test(var ~ grp)$p.value
        used_tests <- c(used_tests, "Kruskal-Wallis + Dunn")
      }
    }
    summary_tbl <- summary_tbl %>% mutate(`p-value` = format.pval(pval_main, digits = 3, eps = 0.001))

    summary_list[[varname]] <- summary_tbl

    # ----- POST-HOC -----
    posthoc_tbl <- NULL
    if (is.numeric(var) && length(unique(grp)) > 2 && !is.na(test_type_var)) {
      if (test_type_var == "anova") {
        tukey <- TukeyHSD(aov(var ~ grp))
        tukey_res <- as.data.frame(tukey[[1]])
        tukey_res <- tibble::rownames_to_column(tukey_res, "Comparison") %>%
          mutate(value = paste0(round(diff, digits), " (",
                                ifelse(`p adj` < 10^-digits, format_lt, signif(`p adj`, digits)), ")"))
        tukey_res$Comparison <- gsub(" - ", "-", tukey_res$Comparison)
        posthoc_tbl <- tukey_res %>% select(Comparison, value)
      } else {
        dunn <- dunnTest(var ~ grp)
        dunn_res <- dunn$res %>%
          mutate(value = ifelse(P.adj < 10^-digits, format_lt, signif(P.adj, digits)))
        posthoc_tbl <- dunn_res %>% select(Comparison, value)
      }
      posthoc_tbl <- posthoc_tbl %>% mutate(Variable = var_label) %>%
        select(Variable, Comparison, value) %>%
        pivot_wider(names_from = Comparison, values_from = value)
      posthoc_list[[varname]] <- posthoc_tbl
    }
  }

  # ----- COMBINE -----
  summary_df <- bind_rows(summary_list)
  posthoc_df <- bind_rows(posthoc_list)
  combined_df <- summary_df %>% left_join(posthoc_df, by = "Variable")

  # ----- FLEXTABLE -----
  ft_combined <- flextable(combined_df) %>%
    autofit() %>%
    bold(part = "header") %>%
    add_footer_lines(paste0("Statistic: ", ifelse(statistic=="auto","Mean (SD) for ANOVA, Median (IQR) for Kruskal-Wallis", statistic))) %>%
    add_footer_lines(paste0("Tests used: ", paste(unique(used_tests), collapse = ", "))) %>%
    add_footer_lines("* ANOVA, ** Kruskal-Wallis") %>%
    add_footer_lines("Post-hoc: mean difference (p-value) for pairwise comparisons")

  return(ft_combined)
}
