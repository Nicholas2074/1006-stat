# //SECTION - partial dependence / feature effects
#
# Shows how top features influence the predicted probability of low CPP.
# For logistic regression: OR forest plot (more clinically interpretable).
# For all other models: partial dependence curves.

library(tidyverse)
library(mlr3verse)

cat("========================================\n")
cat("  Feature Effects: Best CPP Predictor\n")
cat("========================================\n\n")

# ---- load ----
load("benchCPP_results.RData")
load("ih2.RData")
load("phase2_split.RData")

# ---- rebuild data ----
dfMor_full <- rbind(dfMor_train_imp, dfMor_test_imp)
exclude_patient <- c("icuid", "cppMean", "hospMortality")
patient_cols <- setdiff(names(dfMor_full), exclude_patient)
exclude_interval <- c("icp", "cpp", "mab")
interval_cols <- setdiff(names(ih2), c(exclude_interval, "interval"))
ih_all <- merge(ih2[, interval_cols], dfMor_full[, patient_cols], by = "icuid", all = FALSE)
feature_cols_full <- setdiff(names(ih_all), c("icuid", "outCPP"))

# ---- determine model type ----
ppo_names <- names(finalModelLC$graph$pipeops)
learner_name <- ppo_names[grepl("classif\\.", ppo_names)]
if (length(learner_name) > 0) {
    inner_learner <- finalModelLC$graph$pipeops[[learner_name]]$learner
} else {
    inner_learner <- finalModelLC
}
model_type <- class(inner_learner)[1]

# ---- get top features from permutation importance (if available) or use built-in ----
# Use a simple heuristic: compute variance of each feature in training data
train_icuids <- dfMor_train_imp$icuid
ih_train <- ih_all[ih_all$icuid %in% train_icuids, ]

# Select top numeric features by variance (heuristic for PDP)
numeric_cols <- feature_cols_full[sapply(ih_train[feature_cols_full], is.numeric)]
variances <- sapply(ih_train[numeric_cols], var, na.rm = TRUE)
top_num <- names(sort(variances, decreasing = TRUE))[1:min(5, length(variances))]

# Also include the original 4 vital signs (clinically relevant)
vital_cols <- c("isbp", "idbp", "hr", "rr")
top_features <- unique(c(vital_cols, top_num))
top_features <- head(top_features, 6)  # limit to 6 for readability

cat(sprintf("Top features for PDP: %s\n", paste(top_features, collapse = ", ")))

# ---- partial dependence ----
if (grepl("log_reg", model_type)) {

    # Logistic regression: odds ratios with confidence intervals
    cat("\nLogistic regression — reporting odds ratios:\n")
    s <- summary(inner_learner$model)
    coefs <- as.data.frame(s$coefficients)
    coefs$variable <- rownames(coefs)
    coefs <- coefs[coefs$variable != "(Intercept)", ]

    p <- ggplot(coefs, aes(x = Estimate, y = reorder(variable, Estimate))) +
        geom_point(size = 3, color = "steelblue") +
        geom_errorbarh(aes(xmin = Estimate - 1.96 * `Std. Error`,
                           xmax = Estimate + 1.96 * `Std. Error`),
                       height = 0.2, color = "steelblue") +
        geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
        labs(title = "Logistic Regression Coefficients (95% CI)",
             x = "Coefficient (log-odds)", y = "") +
        theme_minimal()
    print(p)

} else {

    # All other models: partial dependence curves
    # Compute PDP manually: average prediction over a grid of feature values
    pred_fun <- function(model, newdata) {
        pred <- model$predict_newdata(newdata)
        pred$prob[, "1"]
    }

    set.seed(42)
    bg_sample <- ih_train[sample(nrow(ih_train), min(1000, nrow(ih_train))), ]

    for (f in top_features) {

        cat(sprintf("\nComputing PDP for: %s\n", f))

        # Create grid of 30 values spanning the feature range
        f_range <- range(bg_sample[[f]], na.rm = TRUE)
        grid_vals <- seq(f_range[1], f_range[2], length.out = 30)

        pdp_vals <- numeric(length(grid_vals))
        for (i in seq_along(grid_vals)) {
            bg_modified <- bg_sample
            bg_modified[[f]] <- grid_vals[i]
            preds <- pred_fun(finalModelLC, bg_modified)
            pdp_vals[i] <- mean(preds, na.rm = TRUE)
        }

        df_pdp <- data.frame(x = grid_vals, y = pdp_vals)
        p <- ggplot(df_pdp, aes(x = x, y = y)) +
            geom_line(linewidth = 1.2, color = "steelblue") +
            labs(title = sprintf("Partial Dependence: %s", f),
                 x = f, y = "Predicted P(outCPP = 1)") +
            theme_minimal()
        print(p)
    }
}

cat("\n5.3 model_pdp.r: DONE\n")
# //!SECTION
