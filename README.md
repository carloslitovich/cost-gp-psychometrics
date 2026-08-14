# COST-GP Psychometric Validation in R

This repository demonstrates an R-based workflow for scoring and evaluating
the psychometric properties of the Comprehensive Score for Financial Toxicity
(COST).

## Purpose

The repository was developed as a reproducible demonstration of psychometric
analysis in R. All data included in this repository are synthetic and do not
contain real participant information.

## Current Workflow

### 1. Synthetic Data Generation

`R/00_generate_synthetic_data.R`

Generates synthetic COST item responses and creates predefined missing-data
cases to test the scoring algorithm.

### 2. COST Scoring

`R/01_scale_scoring.R`

Implements COST Version 2 scoring procedures, including:

- Scoring of FT1-FT11
- Reverse scoring of applicable items
- Exclusion of FT12 from the total score
- Assessment of item completion
- Prorating when more than 50% of scored items are answered
- Missing total scores when 50% or fewer items are answered
- Validation of scoring boundary conditions

Higher COST scores indicate better financial well-being.

## Data

The `synthetic_data/` directory contains simulated data generated specifically
for this repository. These data are not derived from study participants.

## Software

Analyses are conducted in R using reproducible R scripts.

## Confirmatory Factor Analysis Demonstration

Synthetic ordinal COST-GP data were generated from a known two-factor latent structure based on a literature-reported COST item grouping.

The simulated factors were:

- General Financial Situation: FT1, FT6, FT7, FT11
- Medical Care-Related Financial Impact: FT2, FT3, FT4, FT5, FT9, FT10
- FT8 cross-loaded on both factors

The two latent factors were simulated as moderately correlated.

A one-factor CFA and the correctly specified two-factor CFA were fit using WLSMV estimation for ordinal indicators.

The one-factor model demonstrated poor fit, whereas the two-factor model recovered the known synthetic structure and demonstrated excellent global fit. This module illustrates how confirmatory factor analysis can be used to evaluate competing latent-variable structures in ordinal patient-reported outcome data.

All data are synthetic and are not derived from real participants.

## Internal Consistency Reliability

Internal consistency reliability was evaluated using the synthetic ordinal COST-GP data.

Ordinal alpha was calculated for the full 11-item scale using the polychoric correlation matrix. Alpha-if-item-deleted statistics were also examined to assess whether removal of individual items improved internal consistency.

Model-based composite omega was estimated separately for the General Financial Situation and Medical Care-Related Financial Impact composites using the two-factor CFA model.

The synthetic data demonstrated high internal consistency for the full 11-item scale and high composite reliability for both factors. Removal of individual items did not improve ordinal alpha.

Importantly, the synthetic data were generated from a known two-factor structure despite demonstrating high full-scale internal consistency. This illustrates that high internal consistency does not, by itself, establish unidimensionality.

All results are based entirely on synthetic data and do not represent findings from real study participants.