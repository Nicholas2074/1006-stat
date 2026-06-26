# =============================================================================
# 00_master.r — Complete reproducible analysis pipeline
# =============================================================================
# Run this script from the project root directory.
#
# Architecture:
#   1.x  Data preparation
#   2.x  Track 1 — Association analysis (full cohort, no split)
#   3.x  Track 1 — Baseline tables
#   4.x  Track 2 — Predictive modeling (train/test + external validation)
#   5.x  Track 2 — Model interpretation
#
# Usage:
#   Rscript 1006-code/00_master.r              Run full pipeline
#   Rscript 1006-code/00_master.r --check      Dry-run: syntax check only
# =============================================================================

# ---- Parse command-line arguments ----
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--check" %in% args

# ---- Automatically set working directory to project root ----
# Detects the project root: the directory containing this script's parent folder
if (interactive()) {
    project_root <- normalizePath(dirname(dirname(sys.frame(1)$ofile)))
} else {
    project_root <- getwd()
}
setwd(project_root)
cat(sprintf("Project root: %s\n", project_root))
cat(sprintf("Mode: %s\n\n", if (dry_run) "DRY-RUN (--check)" else "RUN"))

# Verify we're in the right place
stopifnot(dir.exists("1006-code"))
stopifnot(dir.exists("1006-oridata"))
cat("Project structure verified.\n\n")

# =============================================================================
# Required packages (conda-managed)
# =============================================================================
# Install via conda before first run:
#   conda install -c conda-forge r-tidyverse r-mlr3verse r-mlr3extralearners \
#     r-kernelshap r-shapviz r-forestploter r-cutoff r-rms r-rcssci r-missforest \
#     r-doparallel r-ranger r-xgboost r-lightgbm r-catboost r-comparegroups \
#     r-e1071 r-kknn r-nnet -y

# =============================================================================
# Required packages (cran/github-managed)
# =============================================================================
# install.packages("mlr3verse")
# install.packages("cutoff")
# install.packages("rcssci")

# install.packages(pak)

# development version
# pak::pak("mlr-org/mlr3extralearners")

# install.packages(c("e1071", "kknn", "xgboost", "lightgbm"))

library(pak)
library(xgboost)
library(lightgbm)
library(tidyverse)
library(mlr3verse)
library(mlr3extralearners)
library(kernelshap)
library(shapviz)
library(forestploter)
library(cutoff)
library(rms)
library(rcssci)
library(missForest)
library(doParallel)
library(compareGroups)

# =============================================================================
# DRY-RUN mode: parse all scripts, then stop
# =============================================================================
if (dry_run) {
    scripts <- sort(list.files("1006-code", pattern = "\\.r$"))
    cat(sprintf("\nChecking %d scripts...\n", length(scripts)))
    ok <- TRUE
    for (s in scripts) {
        path <- file.path("1006-code", s)
        res <- tryCatch({ parse(file = path); TRUE }, error = function(e) e$message)
        if (isTRUE(res)) {
            cat(sprintf("  OK    %s\n", s))
        } else {
            cat(sprintf("  FAIL  %s  →  %s\n", s, res))
            ok <- FALSE
        }
    }
    if (!ok) stop("Syntax errors found.")
    cat("\n========================================\n")
    cat("  DRY-RUN PASSED — ready to run.\n")
    cat("  Execute:  Rscript 1006-code/00_master.r\n")
    cat("========================================\n")
    quit(save = "no", status = 0)
}

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

# TODO: Enable after HiRID per-patient features are assembled
# cat("\n===== 4.2: External validation (valCPP.r) =====\n")
# source("1006-code/4.2 valCPP.r")

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
