# ============================================================
# COST-GP Psychometric Validation
# GitHub Demonstration
# 04_internal_consistency.R
#
# Purpose:
# Evaluate internal consistency reliability using synthetic
# ordinal COST-GP data.
#
# Analyses include:
#   1. Ordinal alpha for the full 11-item scale
#   2. Alpha-if-item-deleted
#   3. Model-based composite omega for the two-factor model
#
# All data are synthetic and are not derived from real
# study participants.
# ============================================================

library(dplyr)
library(psych)
library(lavaan)
library(semTools)

# ------------------------------------------------------------
# 1. Load synthetic CFA dataset
# ------------------------------------------------------------

reliability_data <- read.csv(
  "synthetic_data/cost_gp_cfa_synthetic.csv"
)

cost_items <- reliability_data |>
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
  )

dim(cost_items)

# ------------------------------------------------------------
# 2. Calculate polychoric correlation matrix
# ------------------------------------------------------------

cost_poly <- psych::polychoric(
  cost_items
)

dim(cost_poly$rho)

round(
  cost_poly$rho,
  2
)

# ------------------------------------------------------------
# 3. Calculate ordinal alpha
# ------------------------------------------------------------

cost_alpha_ordinal <- psych::alpha(
  cost_poly$rho,
  n.obs = nrow(cost_items),
  check.keys = FALSE
)

cost_alpha_ordinal$total

# ------------------------------------------------------------
# 4. Alpha if item deleted
# ------------------------------------------------------------

cost_alpha_ordinal$alpha.drop

# ------------------------------------------------------------
# 5. Fit two-factor CFA for model-based reliability
# ------------------------------------------------------------

cost_items_ordinal <- cost_items |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ordered
    )
  )

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
  data = cost_items_ordinal,
  ordered = names(cost_items_ordinal),
  estimator = "WLSMV"
)

# ------------------------------------------------------------
# 6. Calculate factor-specific composite omega
# ------------------------------------------------------------

composite_omega <- semTools::compRelSEM(
  fit_2factor,
  ord.scale = TRUE
)

composite_omega

# ------------------------------------------------------------
# 7. Create reliability summary table
# ------------------------------------------------------------

reliability_summary <- tibble::tibble(
  Score_Composite = c(
    "Full 11-item COST-GP",
    "General Financial Situation",
    "Medical Care-Related Financial Impact"
  ),
  Reliability_Estimate = c(
    "Ordinal alpha",
    "Composite omega",
    "Composite omega"
  ),
  Estimate = c(
    cost_alpha_ordinal$total$raw_alpha,
    composite_omega$GeneralFinancialSituation,
    composite_omega$MedicalCareFinancialImpact
  )
)

reliability_summary

# ------------------------------------------------------------
# 8. Create alpha-if-item-deleted table
# ------------------------------------------------------------

alpha_if_deleted <- cost_alpha_ordinal$alpha.drop |>
  as.data.frame() |>
  tibble::rownames_to_column("Item") |>
  dplyr::select(
    Item,
    Alpha_If_Deleted = raw_alpha
  )

alpha_if_deleted

# ------------------------------------------------------------
# 9. Save reliability outputs
# ------------------------------------------------------------

readr::write_csv(
  reliability_summary,
  "tables/internal_consistency_summary.csv"
)

readr::write_csv(
  alpha_if_deleted,
  "tables/alpha_if_item_deleted.csv"
)

file.exists(
  "tables/internal_consistency_summary.csv"
)

file.exists(
  "tables/alpha_if_item_deleted.csv"
)