# ============================================================
# COST-GP Psychometric Validation
# GitHub Demonstration
# 02_generate_cfa_data.R
#
# Purpose:
# Generate synthetic ordinal COST-GP item responses from a
# known two-factor latent-variable model.
#
# The factor structure follows a literature-reported COST
# item grouping:
#
# Factor 1: General Financial Situation
#   FT1, FT6, FT7, FT11
#
# Factor 2: Medical Care-Related Financial Impact
#   FT2, FT3, FT4, FT5, FT9, FT10
#
# FT8 cross-loads on both factors.
#
# These are entirely synthetic data and are not derived from
# real study participants or real parameter estimates.
# ============================================================

library(MASS)
library(dplyr)

# ------------------------------------------------------------
# 1. Reproducibility
# ------------------------------------------------------------

set.seed(2026)

# ------------------------------------------------------------
# 2. Define synthetic sample size
# ------------------------------------------------------------

n_cfa <- 1000

# ------------------------------------------------------------
# 3. Simulate two correlated latent factors
# ------------------------------------------------------------

factor_correlation <- 0.35

latent_covariance <- matrix(
  c(
    1.00, factor_correlation,
    factor_correlation, 1.00
  ),
  nrow = 2,
  byrow = TRUE
)

latent_scores <- MASS::mvrnorm(
  n = n_cfa,
  mu = c(0, 0),
  Sigma = latent_covariance
)

colnames(latent_scores) <- c(
  "GeneralFinancialSituation",
  "MedicalCareFinancialImpact"
)

latent_data <- as.data.frame(latent_scores)

# ------------------------------------------------------------
# 4. Define population factor loadings (generic artificial strong loadings)
# ------------------------------------------------------------

lambda_f1 <- c(
  FT1  = 0.80,
  FT6  = 0.82,
  FT7  = 0.78,
  FT11 = 0.84
)

lambda_f2 <- c(
  FT2  = 0.78,
  FT3  = 0.84,
  FT4  = 0.76,
  FT5  = 0.80,
  FT9  = 0.77,
  FT10 = 0.83
)

# FT8 cross-loads on both factors
lambda_ft8_f1 <- 0.40
lambda_ft8_f2 <- 0.60

# ------------------------------------------------------------
# 5. Generate continuous item responses
# ------------------------------------------------------------

generate_single_factor_item <- function(
    factor_score,
    loading
) {
  
  residual_sd <- sqrt(1 - loading^2)
  
  loading * factor_score +
    rnorm(
      length(factor_score),
      mean = 0,
      sd = residual_sd
    )
}
#Factor 1 items
continuous_items <- data.frame(
  COST_FT1 = generate_single_factor_item(
    latent_data$GeneralFinancialSituation,
    lambda_f1["FT1"]
  ),
  
  COST_FT6 = generate_single_factor_item(
    latent_data$GeneralFinancialSituation,
    lambda_f1["FT6"]
  ),
  
  COST_FT7 = generate_single_factor_item(
    latent_data$GeneralFinancialSituation,
    lambda_f1["FT7"]
  ),
  
  COST_FT11 = generate_single_factor_item(
    latent_data$GeneralFinancialSituation,
    lambda_f1["FT11"]
  )
)

#Factor 2 items
continuous_items$COST_FT2 <- generate_single_factor_item(
  latent_data$MedicalCareFinancialImpact,
  lambda_f2["FT2"]
)

continuous_items$COST_FT3 <- generate_single_factor_item(
  latent_data$MedicalCareFinancialImpact,
  lambda_f2["FT3"]
)

continuous_items$COST_FT4 <- generate_single_factor_item(
  latent_data$MedicalCareFinancialImpact,
  lambda_f2["FT4"]
)

continuous_items$COST_FT5 <- generate_single_factor_item(
  latent_data$MedicalCareFinancialImpact,
  lambda_f2["FT5"]
)

continuous_items$COST_FT9 <- generate_single_factor_item(
  latent_data$MedicalCareFinancialImpact,
  lambda_f2["FT9"]
)

continuous_items$COST_FT10 <- generate_single_factor_item(
  latent_data$MedicalCareFinancialImpact,
  lambda_f2["FT10"]
)

# ------------------------------------------------------------
# 6. Generate cross-loading FT8
# ------------------------------------------------------------

ft8_common_variance <-
  lambda_ft8_f1^2 +
  lambda_ft8_f2^2 +
  2 *
  lambda_ft8_f1 *
  lambda_ft8_f2 *
  factor_correlation

ft8_residual_sd <- sqrt(
  1 - ft8_common_variance
)

continuous_items$COST_FT8 <-
  lambda_ft8_f1 *
  latent_data$GeneralFinancialSituation +
  lambda_ft8_f2 *
  latent_data$MedicalCareFinancialImpact +
  rnorm(
    n_cfa,
    mean = 0,
    sd = ft8_residual_sd
  )

#Putting variables in COST order
continuous_items <- continuous_items |>
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

# ------------------------------------------------------------
# 7. Convert continuous responses into ordinal 0-4 responses
# ------------------------------------------------------------

ordinal_cutpoints <- c(
  -Inf,
  -1.0,
  -0.35,
  0.35,
  1.0,
  Inf
)

convert_to_ordinal <- function(x) {
  as.integer(
    cut(
      x,
      breaks = ordinal_cutpoints,
      labels = 0:4,
      ordered_result = TRUE
    )
  ) - 1
}

cfa_synthetic_data <- continuous_items |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      convert_to_ordinal
    )
  )

cfa_synthetic_data <- cfa_synthetic_data |>
  dplyr::mutate(
    ID = 1:n_cfa,
    .before = 1
  )

# ------------------------------------------------------------
# 8. QC checks
# ------------------------------------------------------------

dim(cfa_synthetic_data)

summary(
  cfa_synthetic_data |>
    dplyr::select(COST_FT1:COST_FT11)
)

sapply(
  cfa_synthetic_data |>
    dplyr::select(COST_FT1:COST_FT11),
  unique
)

write.csv(
  cfa_synthetic_data,
  "synthetic_data/cost_gp_cfa_synthetic.csv",
  row.names = FALSE
)

file.exists(
  "synthetic_data/cost_gp_cfa_synthetic.csv"
)