# ============================================================
# COST-GP Psychometric Validation
# GitHub Demonstration
# 01_scale_scoring.R
#
# Purpose:
# Score synthetic COST-GP data using FACIT-COST
# Version 2 scoring rules.
# ============================================================

synthetic_data <- read.csv(
  "synthetic_data/cost_gp_synthetic.csv"
)

cost_scored_items <- paste0("COST_FT", 1:11)

cost_reverse_items <- c(
  "COST_FT2",
  "COST_FT3",
  "COST_FT4",
  "COST_FT5",
  "COST_FT8",
  "COST_FT9",
  "COST_FT10"
)

cost_nonreverse_items <- c(
  "COST_FT1",
  "COST_FT6",
  "COST_FT7",
  "COST_FT11"
)

scored_data <- synthetic_data

scored_data <- scored_data |>
  dplyr::mutate(
    COST_FT2_S  = 4 - COST_FT2,
    COST_FT3_S  = 4 - COST_FT3,
    COST_FT4_S  = 4 - COST_FT4,
    COST_FT5_S  = 4 - COST_FT5,
    COST_FT8_S  = 4 - COST_FT8,
    COST_FT9_S  = 4 - COST_FT9,
    COST_FT10_S = 4 - COST_FT10
  )

scored_data <- scored_data |>
  dplyr::mutate(
    COST_FT1_S  = COST_FT1,
    COST_FT6_S  = COST_FT6,
    COST_FT7_S  = COST_FT7,
    COST_FT11_S = COST_FT11
  )

#Counting how many scored items each participant answered:
cost_scored_vars <- paste0("COST_FT", 1:11, "_S")

scored_data <- scored_data |>
  dplyr::mutate(
    COST_N_ITEMS_ANSWERED = rowSums(
      !is.na(
        dplyr::across(
          dplyr::all_of(cost_scored_items)
        )
      )
    )
  )

#Item sum
scored_data <- scored_data |>
  dplyr::mutate(
    COST_ITEM_SUM = rowSums(
      dplyr::across(
        dplyr::all_of(cost_scored_vars)
      ),
      na.rm = TRUE
    )
  )

#FACIT proprating rule
scored_data <- scored_data |>
  dplyr::mutate(
    COST_GP_TOTAL = dplyr::if_else(
      COST_N_ITEMS_ANSWERED > 5,
      COST_ITEM_SUM * 11 / COST_N_ITEMS_ANSWERED,
      NA_real_
    )
  )

#flags
scored_data <- scored_data |>
  dplyr::mutate(
    COST_PRORATED =
      COST_N_ITEMS_ANSWERED >= 6 &
      COST_N_ITEMS_ANSWERED < 11,
    
    COST_MISSING =
      is.na(COST_GP_TOTAL)
  )

#Case verification
scored_data |>
  dplyr::select(
    ID,
    COST_N_ITEMS_ANSWERED,
    COST_ITEM_SUM,
    COST_GP_TOTAL,
    COST_PRORATED,
    COST_MISSING
  ) |>
  dplyr::slice(1:5)


# ------------------------------------------------------------
# Validate scoring rules
# ------------------------------------------------------------

scored_data |>
  dplyr::select(
    ID,
    COST_N_ITEMS_ANSWERED,
    COST_ITEM_SUM,
    COST_GP_TOTAL,
    COST_PRORATED,
    COST_MISSING
  ) |>
  dplyr::slice(1:5)

# Confirm total scores remain within theoretical range
range(scored_data$COST_GP_TOTAL, na.rm = TRUE)

# Count prorated scores
table(scored_data$COST_PRORATED)

# Count missing total scores
table(scored_data$COST_MISSING)

write.csv(
  scored_data,
  "synthetic_data/cost_gp_synthetic_scored.csv",
  row.names = FALSE
)