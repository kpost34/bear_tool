# Bio-Efficacy Automated Reporting (BEAR) Tool

**An automated R-based analytical engine for high-throughput dose-response modeling and potency estimation.**

The BEAR Tool was engineered to replace manual, inconsistent curve-fitting with a defensive analytical pipeline. It utilizes three sequential validation gates to ensure biological conclusions are only drawn from statistically sound data.

---

## Analysis Gallery
These reports demonstrate BEAR's adaptive logic across varying data quality scenarios:

* [Standard Sigmoidal Fit (Success)](https://kpost34.github.io/bear_pipeline_analysis/) – Full potency estimation and model selection.
* [Biological Audit Failure (Chaos)](https://kpost34.github.io/bear_pipeline_analysis/chaos_report.html) – Automated "kill-switch" and diagnostic-only mode.
* [No Detectable Response (Null)](https://kpost34.github.io/bear_pipeline_analysis/null_report.html) – Intercept-only fallback for high-noise datasets.

---

## The Three-Gate Pipeline

### 1. Structural Validation
* **Automated Pre-processing:** Validates column mapping, data types, and mapping of Dose vs. Response variables.
* **Fail-Fast Logic:** Halts the pipeline immediately if the data structure is non-compliant, preventing downstream execution errors and ensuring data integrity.

### 2. Biological Audit
* **Signal-to-Noise Ratio (SNR):** Quantifies signal strength; automatically flags exploratory results if SNR < 3 to warn of $ED_{50}$ instability.
* **Monotonicity Check:** Detects non-monotonic data. If net displacement is insufficient relative to variance, BEAR triggers a biological kill-switch, diverting the report to a diagnostic-only mode.
* **Integrity Checks:** Implements automated outlier detection (Tukey’s 1.5*IQR Rule) and terminal plateau assessments to qualify the reliability of upper-limit estimations.

### 3. Model Selection Hierarchy
* **Sigmoidal Tier:** Evaluates Brain-Cousens (Hormetic) and Log-Logistic (3-parameter and 4-parameter) models. Winners are selected via a 2-unit AIC threshold to prioritize parsimony.
* **Linear Fallback:** If sigmoidal fits lack a significant slope ($p > 0.05$), the engine automatically attempts a linear trend.
* **Null Response:** If no trend is detected, BEAR defaults to an intercept-only model, preventing the reporting of erroneous or forced potency estimates.

---

## Automated Reporting and Data Archiving
BEAR generates a comprehensive Quarto-based HTML report and archives all analytical states for full reproducibility:

* **Visual Audit Plots:** Generates diagnostic plots comparing replicates vs. means, utilizing conditional color palettes to highlight failed audits.
* **Potency Estimates:** Automated $ED_{50}$ calculation with standard errors and confidence intervals.
* **Multi-Format Exports:** 
    1. **Model Objects:** Saves serialized .rds objects for future re-analysis.
    2. **Summary Tables:** Exports CSVs containing the winning model name, selection rationale, AIC, and $ED_{50}$ statistics.
    3. **Publication-Ready Plots:** Saves Audit and Final Curve images in both PNG and PDF formats, conditional on pipeline progress.

---

## Tech Stack
* **R:** drc (Bio-statistical modeling), tidyverse (Data engineering), here (Project-level workflow).
* **Quarto:** Dynamic HTML rendering for reproducible research.

---

#### Project Creator: Keith Post
* [GitHub Profile](https://github.com/kpost34) 
* [LinkedIn Profile](https://www.linkedin.com/in/keith-post/)
* [Email](mailto:keithpost.kp@gmail.com)