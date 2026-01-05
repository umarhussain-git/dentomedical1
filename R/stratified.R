#' Summarize Variables with Optional Stratification and Statistical Tests
#'
#' Produces summary tables for numeric and categorical variables in a dataset,
#' optionally stratified by a grouping variable. Numeric variables are summarized
#' with mean (SD) or median (IQR), and categorical variables with counts and percentages.
#' Appropriate statistical tests (t-test, Wilcoxon, ANOVA, Kruskal-Wallis, Chi-square, or Fisher's Exact)
#' are performed depending on the variable type, number of groups, and user-specified options.
#'
#' @param data A data frame containing the variables to summarize.
#' @param by The main grouping variable for comparison.
#' @param strata Optional stratification variable. Summaries are produced within each stratum.
#' @param statistic Statistic to report for numeric variables: `"mean_sd"` (default) or `"med_iqr"`.
#' @param test_type Statistical test to use. Options: `"auto"` (default, selects appropriate test),
#'                  `"t.test"`, `"wilcox"`, `"anova"`, `"kruskal"`, `"fisher"`, or `"chisq"`.
#' @param paired Logical. If TRUE, paired t-tests are used when applicable (default: FALSE).
#'
#' @return A `flextable` object containing the summary table with optional p-values,
#'         counts, percentages, and numeric summaries. Footnotes describe the statistics used and tests performed.
#'
#' @export
#'
#' @examples
#' # Example : Summary of CO2 dataset by Type, stratified by Treatment
#' sum_stat_p_strata(data = CO2, by = "Type", strata = "Treatment")
#'
sum_stat_p_strata <- function(data, by, strata = NULL,
                              statistic = "mean_sd",
                              test_type = "auto",
                              paired = FALSE) {

  data <- as_tibble(data)

  stopifnot(by %in% names(data))
  if (!is.null(strata)) stopifnot(strata %in% names(data))

  # ---- internal sum_stat_p function ----
  sum_stat_p <- function(data, by, statistic = "mean_sd", test_type = "auto", paired = FALSE) {
    data <- as_tibble(data)
    is_cat <- function(x) is.factor(x) || is.character(x)

    summary_list <- list()
    test_info <- c()

    for (colname in names(data)) {
      if (colname == by) next
      var <- data[[colname]]
      pval <- NA
      test_used <- ""

      try({
        if (is_cat(var)) {
          tbl <- table(data[[colname]], data[[by]])
          if (test_type == "fisher" || (test_type == "auto" && any(tbl < 5))) {
            pval <- fisher.test(tbl)$p.value
            test_used <- "Fisher's Exact"
          } else {
            pval <- chisq.test(tbl)$p.value
            test_used <- "Chi-square"
          }
        } else {
          g <- unique(data[[by]])
          if (length(g) == 2) {
            if (paired) {
              pval <- t.test(var ~ data[[by]], paired = TRUE)$p.value
              test_used <- "Paired t-test"
            } else if (test_type == "wilcox") {
              pval <- wilcox.test(var ~ data[[by]])$p.value
              test_used <- "Wilcoxon Rank-Sum"
            } else if (test_type == "t.test" || test_type == "auto") {
              pval <- t.test(var ~ data[[by]])$p.value
              test_used <- "Student's t-test"
            }
          } else {
            if (test_type == "kruskal" || test_type == "auto") {
              pval <- kruskal.test(var ~ data[[by]])$p.value
              test_used <- "Kruskal-Wallis"
            } else if (test_type == "anova") {
              pval <- summary(aov(var ~ data[[by]]))[[1]][["Pr(>F)"]][1]
              test_used <- "ANOVA"
            }
          }
        }
      }, silent = TRUE)

      test_info <- c(test_info, paste(colname, ":", test_used))

      # ---- format p-value ----
      pval_fmt <- ifelse(is.na(pval), "",
                         ifelse(pval < 0.001, "<0.001", sprintf("%.2f", pval)))

      # ----- SUMMARY -----
      if (is_cat(var)) {
        tbl <- data %>%
          group_by(!!sym(colname), !!sym(by)) %>%
          summarise(n = n(), .groups = "drop") %>%
          complete(!!sym(colname), !!sym(by), fill = list(n = 0)) %>%
          group_by(!!sym(by)) %>%
          mutate(pct = round(n / sum(n) * 100)) %>%
          ungroup() %>%
          mutate(
            Variable = colname,
            Characteristic = as.character(!!sym(colname)),
            label = paste0(n, " (", pct, "%)")
          ) %>%
          select(Variable, Characteristic, !!sym(by), label) %>%
          pivot_wider(names_from = !!sym(by), values_from = label) %>%
          mutate(`p-value` = if_else(row_number() == 1, pval_fmt, ""))

      } else {
        tbl <- data %>%
          group_by(!!sym(by)) %>%
          summarise(
            val = case_when(
              statistic == "mean_sd" ~ paste0(
                round(mean(!!sym(colname), na.rm = TRUE), 2),
                " (",
                round(sd(!!sym(colname), na.rm = TRUE), 2),
                ")"
              ),
              TRUE ~ paste0(
                round(median(!!sym(colname), na.rm = TRUE), 2),
                " (",
                round(quantile(!!sym(colname), 0.25, na.rm = TRUE), 2),
                ", ",
                round(quantile(!!sym(colname), 0.75, na.rm = TRUE), 2),
                ")"
              )
            ),
            .groups = "drop"
          ) %>%
          mutate(Variable = colname) %>%
          pivot_wider(names_from = !!sym(by), values_from = val) %>%
          mutate(
            Characteristic = if_else(statistic == "mean_sd", "Mean (SD)", "Median (IQR)"),
            `p-value` = pval_fmt
          ) %>%
          select(Variable, Characteristic, everything())
      }

      summary_list[[colname]] <- tbl
    }

    summary_df <- bind_rows(summary_list) %>%
      group_by(Variable) %>%
      mutate(Variable = if_else(row_number() == 1, Variable, "")) %>%
      ungroup()

    stat_text <- case_when(
      statistic == "mean_sd" ~ "1 n (%); Mean (SD)",
      statistic == "med_iqr" ~ "1 n (%); Median (IQR)",
      TRUE ~ "1 n (%)"
    )

    test_text <- paste("Tests used:", paste(unique(test_info), collapse = "; "))

    list(dataset = summary_df, stat_text = stat_text, test_text = test_text)
  }

  # ---- NO STRATIFICATION ----
  if (is.null(strata)) {
    ft <- sum_stat_p(data = data, by = by, statistic = statistic, test_type = test_type, paired = paired)
    flextable(ft$dataset) %>%
      autofit() %>%
      add_footer_lines(ft$stat_text) %>%
      add_footer_lines(ft$test_text)
  } else {
    # ---- STRATIFIED ----
    strata_vals <- unique(na.omit(data[[strata]]))

    all_blocks <- lapply(strata_vals, function(s) {
      df_s <- data %>% filter(.data[[strata]] == s) %>% select(-all_of(strata))
      if (nrow(df_s) == 0) return(NULL)
      ft <- sum_stat_p(df_s, by = by, statistic = statistic, test_type = test_type, paired = paired)
      ft$dataset %>% mutate(strata_val = as.character(s))
    })

    final_df <- bind_rows(all_blocks) %>%
      group_by(strata_val, Variable) %>%
      mutate(
        Variable = if_else(row_number() == 1, Variable, ""),
        `p-value` = if_else(row_number() == 1, `p-value`, "")
      ) %>%
      ungroup() %>%
      group_by(strata_val) %>%
      mutate(strata_display = if_else(row_number() == 1, strata_val, "")) %>%
      ungroup() %>%
      select(strata_display, everything(), -strata_val) %>%
      rename(!!strata := strata_display)

    # Show footer: only summary text + tests
    ft_all <- flextable(final_df) %>%
      autofit()

    # Add footers from internal sum_stat_p of first stratum
    first_stratum <- data %>% filter(.data[[strata]] == strata_vals[1]) %>% select(-all_of(strata))
    ft_first <- sum_stat_p(first_stratum, by = by, statistic = statistic, test_type = test_type, paired = paired)

    ft_all <- ft_all %>% add_footer_lines(ft_first$stat_text) %>% add_footer_lines(ft_first$test_text)

    ft_all
  }
}
