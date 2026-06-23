# =============================================================================
# 00_master.r — Complete reproducible analysis pipeline
# =============================================================================
# Run this script from the project root directory (D:/ClaudeBox/1006-stat_cc/).
#
# Architecture:
#   1.x  Data preparation
#   2.x  Track 1 — Association analysis (full cohort, no split)
#   3.x  Track 1 — Baseline tables
#   4.x  Track 2 — Predictive modeling (train/test + external validation)
#   5.x  Track 2 — Model interpretation
#
# Usage:
#   setwd("D:/ClaudeBox/1006-stat_cc/")
#   source("1006-code/00_master.r")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1.x DATA PREPARATION
# =============================================================================

cat("\n===== 1.1: Vital signs processing (predictors.r) =====\n")
source("1006-code/1.1 predictors.r")

cat("\n===== 1.2: Outcome processing (outcome.r) =====\n")
source("1006-code/1.2 outcome.r")

cat("\n===== 1.3: Feature assembly (features.r) =====\n")
source("1006-code/1.3 features.r")

# =============================================================================
# 2.x TRACK 1: Association analysis (full cohort — descriptive/confirmatory)
# =============================================================================

cat("\n===== 2.1: CPP logistic regression (logCPP.r) =====\n")
source("1006-code/2.1 logCPP.r")

cat("\n===== 2.2: CPP cutoff analysis (cutoff.r) =====\n")
source("1006-code/2.2 cutoff.r")

cat("\n===== 2.3: RCS analysis (rcs.r) =====\n")
source("1006-code/2.3 rcs.r")

# =============================================================================
# 3.x TRACK 1: Baseline characteristics
# =============================================================================

cat("\n===== 3.1: Baseline tables (baseline.r) =====\n")
source("1006-code/3.1 baseline.r")

# =============================================================================
# 4.x TRACK 2: Predictive modeling (train/test split + external validation)
# =============================================================================

cat("\n===== 4.1: Benchmark models (benchCPP.r) =====\n")
source("1006-code/4.1 benchCPP.r")

cat("\n===== 4.2: External validation (valCPP.r) =====\n")
source("1006-code/4.2 valCPP.r")

# =============================================================================
# 5.x TRACK 2: Model interpretation
# =============================================================================

cat("\n===== 5.1: Model inspection (model_inspect.r) =====\n")
source("1006-code/5.1 model_inspect.r")

cat("\n===== 5.2: SHAP analysis (model_shap.r) =====\n")
source("1006-code/5.2 model_shap.r")

cat("\n===== 5.3: Feature effects (model_pdp.r) =====\n")
source("1006-code/5.3 model_pdp.r")

cat("\n===== PIPELINE COMPLETE =====\n")
print(sessionInfo())
# =============================================================================
