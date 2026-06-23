# //SECTION - model inspection
#
# Interprets the best model from 4.1 benchCPP.r.
# Reports model type, hyperparameters, and feature importance.

library(tidyverse)
library(mlr3verse)

# ---- load best model ----
load("benchCPP_results.RData")

cat("========================================\n")
cat("  Model Inspection: Best CPP Predictor\n")
cat("========================================\n\n")

# ---- model identity ----
cat(sprintf("Model ID:   %s\n", finalModelLC$id))
cat(sprintf("Model type: %s\n", class(finalModelLC)[1]))

# Extract inner learner from GraphLearner
ppo_names <- names(finalModelLC$graph$pipeops)
learner_name <- ppo_names[grepl("classif\\.", ppo_names)]
if (length(learner_name) > 0) {
    inner_learner <- finalModelLC$graph$pipeops[[learner_name]]$learner
    cat(sprintf("Inner learner: %s (%s)\n", inner_learner$id, class(inner_learner)[1]))
} else {
    inner_learner <- finalModelLC
}

# ---- hyperparameters ----
cat("\n--- Hyperparameters ---\n")
print(inner_learner$param_set$values)

# ---- test set performance recap ----
cat("\n--- Test Set Performance ---\n")
print(test_measures)

# ---- built-in feature importance ----
cat("\n--- Feature Importance ---\n")

model_type <- class(inner_learner)[1]

if (grepl("ranger", model_type)) {
    # Random Forest
    imp <- inner_learner$model$variable.importance
    imp <- sort(imp, decreasing = TRUE)
    cat("Ranger impurity importance (top 20):\n")
    print(head(imp, 20))

} else if (grepl("xgboost", model_type)) {
    # XGBoost
    imp <- xgboost::xgb.importance(model = inner_learner$model)
    cat("XGBoost gain importance (top 20):\n")
    print(head(imp[, c("Feature", "Gain", "Cover", "Frequency")], 20))

} else if (grepl("lightgbm", model_type)) {
    # LightGBM
    imp <- lightgbm::lgb.importance(inner_learner$model, percentage = TRUE)
    cat("LightGBM gain importance (top 20):\n")
    print(head(imp[, c("Feature", "Gain", "Cover", "Frequency")], 20))

} else if (grepl("catboost", model_type)) {
    # CatBoost
    imp <- catboost::catboost.get_feature_importance(inner_learner$model)
    names(imp) <- inner_learner$state$feature_names
    imp <- sort(imp, decreasing = TRUE)
    cat("CatBoost importance (top 20):\n")
    print(head(imp, 20))

} else if (grepl("rpart", model_type)) {
    # Decision Tree
    imp <- inner_learner$model$variable.importance
    imp <- sort(imp, decreasing = TRUE)
    cat("Rpart variable importance (top 20):\n")
    print(head(imp, 20))

} else if (grepl("log_reg", model_type)) {
    # Logistic Regression — report odds ratios
    s <- summary(inner_learner$model)
    coefs <- s$coefficients
    cat("Logistic regression coefficients:\n")
    print(coefs)
    cat("\nOdds ratios:\n")
    or <- exp(coefs[, 1])
    ci_lower <- exp(coefs[, 1] - 1.96 * coefs[, 2])
    ci_upper <- exp(coefs[, 1] + 1.96 * coefs[, 2])
    or_table <- data.frame(OR = round(or, 3), CI_lower = round(ci_lower, 3), CI_upper = round(ci_upper, 3), p = round(coefs[, 4], 4))
    print(or_table)

} else if (grepl("naive_bayes", model_type)) {
    cat("Naive Bayes: conditional probability tables available in inner_learner$model$tables\n")
    cat("Top features by Laplace-smoothed probability difference:\n")
    tables <- inner_learner$model$tables
    # Compute mean probability difference between classes for numeric features
    for (nm in names(tables)) {
        if (is.matrix(tables[[nm]])) {
            diff <- abs(tables[[nm]][1, ] - tables[[nm]][2, ])
            cat(sprintf("  %s: max diff = %.3f\n", nm, max(diff, na.rm = TRUE)))
        }
    }

} else {
    cat(sprintf("Model type '%s': no built-in importance available.\n", model_type))
}

# ---- permutation importance (model-agnostic, works for all) ----
cat("\n--- Permutation Importance (test set) ---\n")

# Rebuild test data
load("ih2.RData")
load("phase2_split.RData")

# Merge and filter to test patients (reuse logic from 4.1 benchCPP.r)
dfMor_full <- rbind(dfMor_train_imp, dfMor_test_imp)
exclude_patient <- c("icuid", "cppMean", "hospMortality")
patient_cols <- setdiff(names(dfMor_full), exclude_patient)
exclude_interval <- c("icp", "cpp", "mab")
interval_cols <- setdiff(names(ih2), c(exclude_interval, "interval"))
ih_all <- merge(ih2[, interval_cols], dfMor_full[, patient_cols], by = "icuid", all = FALSE)

feature_cols_full <- setdiff(names(ih_all), c("icuid", "outCPP"))
test_icuids <- dfMor_test_imp$icuid
ih_test <- ih_all[ih_all$icuid %in% test_icuids, ]

# Compute permutation importance on test set
# For each feature, permute and measure AUC drop
baseline_auc <- test_measures["classif.auc"]
feature_names <- intersect(feature_cols_full, names(ih_test))
perm_importance <- numeric(length(feature_names))
names(perm_importance) <- feature_names

set.seed(42)
for (i in seq_along(feature_names)) {
    f <- feature_names[i]
    ih_perm <- ih_test
    ih_perm[[f]] <- sample(ih_test[[f]])
    pred_perm <- finalModelLC$predict_newdata(ih_perm)
    perm_importance[i] <- baseline_auc - pred_perm$score(msr("classif.auc"))
}

perm_importance <- sort(perm_importance, decreasing = TRUE)
cat("Permutation importance (AUC drop, top 20):\n")
print(head(perm_importance, 20))

# Plot
top_n <- min(20, length(perm_importance))
df_plot <- data.frame(
    feature = factor(names(perm_importance)[1:top_n], levels = rev(names(perm_importance)[1:top_n])),
    importance = perm_importance[1:top_n]
)
p <- ggplot(df_plot, aes(x = importance, y = feature)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Permutation Importance (AUC Drop)", x = "AUC Decrease", y = "") +
    theme_minimal()
print(p)

cat("\n5.1 model_inspect.r: DONE\n")
# //!SECTION
