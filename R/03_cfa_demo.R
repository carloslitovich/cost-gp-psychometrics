# ============================================================
# COST-GP Psychometric Validation
# GitHub Demonstration
# 03_cfa_demo.R
#
# Purpose:
# Compare a misspecified one-factor CFA model with the
# correctly specified literature-based two-factor model
# using synthetic ordinal COST-GP data.
# ============================================================

library(dplyr)
library(lavaan)

# ------------------------------------------------------------
# 1. Load synthetic CFA dataset
# ------------------------------------------------------------

cfa_data <- read.csv(
  "synthetic_data/cost_gp_cfa_synthetic.csv"
)

# ------------------------------------------------------------
# 2. Prepare ordinal COST-GP items
# ------------------------------------------------------------

cfa_items <- cfa_data |>
  dplyr::select(
    COST_FT1,
    COST_FT2,
    COST_FT3,
    COST_FT4,
    COST_FT5,
    COST_FT6,
    COST_FT7,
    COST_FT8,
    COST_FT9,
    COST_FT10,
    COST_FT11
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ordered
    )
  )

str(cfa_items)

# ------------------------------------------------------------
# 3. Fit one-factor CFA
# ------------------------------------------------------------

model_1factor <- '

  FinancialWellBeing =~
    COST_FT1 +
    COST_FT2 +
    COST_FT3 +
    COST_FT4 +
    COST_FT5 +
    COST_FT6 +
    COST_FT7 +
    COST_FT8 +
    COST_FT9 +
    COST_FT10 +
    COST_FT11

'

fit_1factor <- lavaan::cfa(
  model = model_1factor,
  data = cfa_items,
  ordered = names(cfa_items),
  estimator = "WLSMV"
)

# ------------------------------------------------------------
# 4. Fit literature-based two-factor CFA
# ------------------------------------------------------------

model_2factor <- '

  GeneralFinancialSituation =~
    COST_FT1 +
    COST_FT6 +
    COST_FT7 +
    COST_FT11

  MedicalCareFinancialImpact =~
    COST_FT2 +
    COST_FT3 +
    COST_FT4 +
    COST_FT5 +
    COST_FT9 +
    COST_FT10

  # FT8 cross-loads on both factors
  GeneralFinancialSituation =~ COST_FT8
  MedicalCareFinancialImpact =~ COST_FT8

'

fit_2factor <- lavaan::cfa(
  model = model_2factor,
  data = cfa_items,
  ordered = names(cfa_items),
  estimator = "WLSMV"
)

# ------------------------------------------------------------
# 5. Compare model fit
# ------------------------------------------------------------

fit_indices_1factor <- lavaan::fitMeasures(
  fit_1factor,
  c(
    "cfi.scaled",
    "tli.scaled",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",
    "srmr"
  )
)

fit_indices_2factor <- lavaan::fitMeasures(
  fit_2factor,
  c(
    "cfi.scaled",
    "tli.scaled",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",
    "srmr"
  )
)

fit_indices_1factor

fit_indices_2factor

model_comparison <- tibble::tibble(
  Model = c(
    "One-factor",
    "Two-factor"
  ),
  CFI = c(
    fit_indices_1factor["cfi.scaled"],
    fit_indices_2factor["cfi.scaled"]
  ),
  TLI = c(
    fit_indices_1factor["tli.scaled"],
    fit_indices_2factor["tli.scaled"]
  ),
  RMSEA = c(
    fit_indices_1factor["rmsea.scaled"],
    fit_indices_2factor["rmsea.scaled"]
  ),
  SRMR = c(
    fit_indices_1factor["srmr"],
    fit_indices_2factor["srmr"]
  )
)

model_comparison

# ------------------------------------------------------------
# 6. Extract standardized factor loadings
# ------------------------------------------------------------

loadings_2factor <- lavaan::standardizedSolution(
  fit_2factor
) |>
  dplyr::filter(op == "=~") |>
  dplyr::select(
    Factor = lhs,
    Item = rhs,
    Loading = est.std
  )

loadings_2factor

factor_correlation <- lavaan::standardizedSolution(
  fit_2factor
) |>
  dplyr::filter(
    op == "~~",
    lhs == "GeneralFinancialSituation",
    rhs == "MedicalCareFinancialImpact"
  )

factor_correlation

# ------------------------------------------------------------
# 7. Save CFA demonstration outputs
# ------------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE
)

readr::write_csv(
  model_comparison,
  "tables/cfa_model_comparison.csv"
)

readr::write_csv(
  loadings_2factor,
  "tables/cfa_twofactor_loadings.csv"
)

readr::write_csv(
  factor_correlation,
  "tables/cfa_factor_correlation.csv"
)

list.files("tables")