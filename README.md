Here is a **clean, CRAN-style GitHub README.md** tailored specifically for your **dentomedical (v0.2.0)** package and its current function set. You can copy-paste this directly into **README.md** on GitHub.

---

# dentomedical

**dentomedical** is an R package designed for **medical, dental, and clinical research**, providing simple, reproducible, and publication-ready statistical summaries and regression tables.
The package emphasizes **clean output**, **flexibility**, and **journal-ready tables** using `flextable`.

---

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("umarhussain-git/dentomedical1")
```

Load the package:

```r
library(dentomedical)
```

---

## 🎯 Package Scope

The **dentomedical** package is intended for:

* Medical and dental research
* Clinical and epidemiological studies
* Diagnostic accuracy analysis
* Regression modeling
* Descriptive and inferential statistics
* Reproducible, publication-ready tables

All major outputs are formatted using **flextable**, making them suitable for **Word and PDF manuscripts**.

---

## 📘 Main Functions

### 📊 Descriptive Statistics

| Function              | Description                                        |
| --------------------- | -------------------------------------------------- |
| `sum_stat()`          | Summarize continuous and categorical variables     |
| `sum_stat_p()`        | Summaries with group-wise comparisons and p-values |
| `sum_stat_p_strata()` | Stratified summaries with statistical testing      |
| `sum_norm()`          | Normality test summary for numeric variables       |
| `sum_posthoc()`       | Post-hoc multiple comparison summaries             |

---

### 🔗 Correlation Analysis

| Function    | Description                                                                    |
| ----------- | ------------------------------------------------------------------------------ |
| `sum_cor()` | Correlations with confidence intervals, p-values, and narrative interpretation |

---

### 📈 Regression Models

| Function   | Description                                                  |
| ---------- | ------------------------------------------------------------ |
| `linreg()` | Linear regression with univariable and multivariable results |
| `logreg()` | Binary logistic regression with ORs and confidence intervals |

---

### 🧪 Diagnostic Accuracy

| Function          | Description                                                              |
| ----------------- | ------------------------------------------------------------------------ |
| `diag_accuracy()` | Sensitivity, specificity, PPV, NPV, and accuracy with optional 2x2 table |

---

### 🧩 Data Management Utilities

| Function           | Description                                     |
| ------------------ | ----------------------------------------------- |
| `category()`       | Categorize numeric variables into custom ranges |
| `recode_data()`    | Recode values using a lookup table              |
| `impute_missing()` | Impute missing values (mean, median, mode)      |
| `medical_data()`   | Load example infertility dataset                |

---

## ✨ Examples

### Summarize Variables with P-values

```r
# Summary of iris dataset by species
sum_stat_p(iris, by = "Species", statistic = "mean_sd", test_type = "auto")

# Summary of CO2 dataset by Type with paired t-test
sum_stat_p(CO2, by = "Type", statistic = "mean_sd", test_type = "t.test", paired = TRUE)

# Summary using median and IQR
sum_stat_p(iris, by = "Species", statistic = "med_iqr", test_type = "kruskal")
```

---

### Stratified Summary

```r
Library(gtsummary) # to import trial data
sum_stat_p_strata(
  data = trial,
  by = "trt",
  strata = "stage",
  statistic = "med_iqr"
)
```

---

### Correlation Analysis

```r
# Example 1: Correlations across entire dataset
sum_cor(
  data = iris,
  ref_var = "Sepal.Length",
  compare_vars = c("Petal.Length", "Petal.Width", "Sepal.Width"),
  method = "pearson",
  digits = 2,
  report = TRUE
)

# Example 2: Correlations by Species
sum_cor(
  data = iris,
  ref_var = "Sepal.Length",
  by = "Species",
  compare_vars = c("Petal.Length", "Petal.Width", "Sepal.Width"),
  method = "pearson",
  digits = 2,
  report = TRUE
)
```

---

### Logistic Regression

```r
library(gtsummary) # to import trial data
logreg(
  data = trial,
  outcome = "death",
  predictors = c("age", "marker", "stage")
)
```

---

### Diagnostic Accuracy

```r
diagnostic_data <- data.frame(
  test = c("positive","negative","positive","
  negative","positive","negative","positive","negative"),
  goldstandard = c("positive","positive","negative",
  "negative","positive","negative","positive","negative")
)
diag_accuracy(diagnostic_data, test_col = "test",
gold_col = "goldstandard",
descriptive = FALSE)
```

---

## 📄 Output Format

* All summary and regression functions return **`flextable` objects**
* Easily export with rmarkdown to:

  * Microsoft Word
  * PDF
  * HTML reports

---

## 🔍 Reproducibility & Standards

* CRAN-compatible code
* ASCII-safe source files
* Explicit imports and namespaces
* Designed for transparent and reproducible research

---

## 👨‍⚕️ Author

**Umar Hussain**


**Nikoloas Pandis**

Orthodontics | Clinical Research | Biostatistics


GitHub: [https://github.com/umarhussain-git](https://github.com/umarhussain-git)

---

## 📜 License

This package is licensed under the **MIT License**.

---


