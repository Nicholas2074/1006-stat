# //SECTION - SHAP analysis
#
# Computes SHAP values for the best model from 4.1 benchCPP.r.
# Uses kernelshap (model-agnostic, works for all learner types).
# Samples 500 rows from the preprocessed training data.

library(tidyverse)
library(mlr3verse)
library(kernelshap)
library(shapviz)

cat("========================================\n")
cat("  SHAP Analysis: Best CPP Predictor\n")
cat("========================================\n\n")

# ---- load best model and data ----
load("benchCPP_results.RData")
load("ih2.RData")
load("phase2_split.RData")

cat(sprintf("Best model: %s\n", finalModelLC$id))

# ---- rebuild merged feature set (same pipeline as 4.1 benchCPP.r) ----
dfMor_full <- rbind(dfMor_train_imp, dfMor_test_imp)
exclude_patient <- c("icuid", "cppMean", "hospMortality")
patient_cols <- setdiff(names(dfMor_full), exclude_patient)
exclude_interval <- c("icp", "cpp", "mab")
interval_cols <- setdiff(names(ih2), c(exclude_interval, "interval"))
ih_all <- merge(ih2[, interval_cols], dfMor_full[, patient_cols], by = "icuid", all = FALSE)

feature_cols_full <- setdiff(names(ih_all), c("icuid", "outCPP"))

# Use training data for SHAP background
train_icuids <- dfMor_train_imp$icuid
ih_train <- ih_all[ih_all$icuid %in% train_icuids, ]

# ---- prepare SHAP data ----
# Sample background (reference) rows and explanation rows
set.seed(42)
n_bg <- min(200, nrow(ih_train))
n_shap <- min(500, nrow(ih_train))

idx_bg <- sample(nrow(ih_train), n_bg)
idx_shap <- sample(setdiff(seq_len(nrow(ih_train)), idx_bg), n_shap)

X_bg <- ih_train[idx_bg, feature_cols_full, drop = FALSE]
X_explain <- ih_train[idx_shap, feature_cols_full, drop = FALSE]

cat(sprintf("SHAP: %d background rows, %d explanation rows, %d features\n",
    nrow(X_bg), nrow(X_explain), ncol(X_explain)))

# ---- compute SHAP ----
# kernelshap needs a prediction function that returns a numeric vector.
# finalModelLC is a GraphLearner; its predict method returns class probabilities.
pred_fun <- function(model, newdata) {
    pred <- model$predict_newdata(newdata)
    pred$prob[, "1"]  # probability of positive class (outCPP = 1)
}

cat("Computing kernelshap (this may take a few minutes)...\n")
shap <- kernelshap(
    object = finalModelLC,
    X = X_explain,
    bg_X = X_bg,
    pred_fun = pred_fun
)

cat("SHAP computation complete.\n")

# ---- visualize ----
viz <- shapviz(shap, which_class = 1)

cat("\n--- SHAP Importance (mean |SHAP|) ---\n")
print(sort(colMeans(abs(shap$S)), decreasing = TRUE))

p1 <- sv_importance(viz, kind = "beeswarm", show_numbers = TRUE)
print(p1)

p2 <- sv_importance(viz, kind = "bar", show_numbers = TRUE)
print(p2)

# Dependence plot for top 3 features
top_features <- names(sort(colMeans(abs(shap$S)), decreasing = TRUE))[1:min(3, ncol(shap$S))]
for (f in top_features) {
    p <- sv_dependence(viz, v = f)
    print(p)
}

cat("\n5.2 model_shap.r: DONE\n")
# //!SECTION
