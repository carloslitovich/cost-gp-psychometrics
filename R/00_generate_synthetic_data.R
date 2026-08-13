# ============================================================
# COST-GP Psychometric Validation
# GitHub Demonstration
# 00_generate_synthetic_data.R
#
# Purpose:
# Generate a synthetic COST-GP dataset for demonstrating
# scoring, missing-data handling, and psychometric analyses
# without using real participant data.
# ============================================================

library(dplyr)
set.seed(2026)
sample(0:4, 10, replace = TRUE)
n_synthetic <- 500
synthetic_data <- tibble(
  ID = 1:n_synthetic
)

synthetic_data <- synthetic_data |>
  mutate(
    COST_FT1  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT2  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT3  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT4  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT5  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT6  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT7  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT8  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT9  = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT10 = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT11 = sample(0:4, n_synthetic, replace = TRUE),
    COST_FT12 = sample(0:4, n_synthetic, replace = TRUE)
  )
summary(
  synthetic_data |>
    select(COST_FT1:COST_FT12)
)

# ------------------------------------------------------------
# Add deliberate missingness to demonstrate FACIT scoring rules
# ------------------------------------------------------------

# Participant 1: 11/11 scored items answered
# No change needed

# Participant 2: 10/11 answered
synthetic_data$COST_FT3[2] <- NA

# Participant 3: 8/11 answered
synthetic_data$COST_FT2[3] <- NA
synthetic_data$COST_FT5[3] <- NA
synthetic_data$COST_FT9[3] <- NA

# Participant 4: 6/11 answered
synthetic_data$COST_FT2[4]  <- NA
synthetic_data$COST_FT3[4]  <- NA
synthetic_data$COST_FT4[4]  <- NA
synthetic_data$COST_FT8[4]  <- NA
synthetic_data$COST_FT10[4] <- NA

# Participant 5: 5/11 answered
# This participant should NOT receive a COST total score
synthetic_data$COST_FT2[5]  <- NA
synthetic_data$COST_FT3[5]  <- NA
synthetic_data$COST_FT4[5]  <- NA
synthetic_data$COST_FT5[5]  <- NA
synthetic_data$COST_FT8[5]  <- NA
synthetic_data$COST_FT9[5]  <- NA

synthetic_data |>
  select(
    ID,
    COST_FT1:COST_FT11
  ) |>
  slice(1:5)

write.csv(
  synthetic_data,
  "synthetic_data/cost_gp_synthetic.csv",
  row.names = FALSE
)

#verify file
file.exists("synthetic_data/cost_gp_synthetic.csv")

