# //SECTION - preprocess

library(tidyverse)
library(mlr3verse)

# //ANCHOR - clinical rationale
#
# CLINICAL QUESTION:
#   "Can we predict dangerously low CPP (< 77 mmHg) using only
#    non-invasive measurements, without requiring ICP monitoring?"
#
# CPP = MAP - ICP. ICP monitoring requires invasive catheter placement,
# which carries risks and is not universally available. A model that
# predicts low CPP from non-invasive data addresses a real bedside need.
#
# FEATURE SET: All non-invasive variables available at the bedside:
#   - Per-interval vital signs: isbp, idbp, hr, rr (from ih2)
#   - Per-patient features (from dfMor, broadcast to each interval):
#     * Demographics: age, gender, bmi, race
#     * Scores: gcs, sofa, charlson, delirium
#     * Comorbidities: 17 binary (hypertension, diabetes, etc.)
#     * Surgeries: craniotomy, ventriculostomy, csfdrainage
#     * Drugs: hsaline, mannitol
#     * Vital aggregates: 20 features (avg + cv of hr, rr, spo2, sbp, dbp, mbp, temp)
#     * Lab values: ~20 features (wbc, hgb, creatinine, sodium, etc.)
#
# EXCLUDED (require ICP monitoring or cause target leakage):
#   icp, cpp, cppMean, mab, hospMortality, icuid, interval

# ---- load and merge ----

load("ih2.RData")
load("phase2_split.RData")

# Merge train+test patient features (all were imputed with training params)
dfMor_full <- rbind(dfMor_train_imp, dfMor_test_imp)

# Per-patient: exclude ICP-dependent and non-feature columns
exclude_patient <- c("icuid", "cppMean", "hospMortality")
patient_cols <- setdiff(names(dfMor_full), exclude_patient)

# Per-interval: exclude ICP-dependent columns
exclude_interval <- c("icp", "cpp", "mab")
interval_cols <- setdiff(names(ih2), c(exclude_interval, "interval"))

# Merge patient-level features onto each interval row (inner join by icuid)
ih_all <- merge(
    ih2[, interval_cols],
    dfMor_full[, patient_cols],
    by = "icuid",
    all = FALSE
)

cat(sprintf("Merged dataset: %d rows x %d cols\n", nrow(ih_all), ncol(ih_all)))

# Feature columns (exclude ID and target)
feature_cols_full <- setdiff(names(ih_all), c("icuid", "outCPP"))
cat(sprintf("Feature count (full): %d\n", length(feature_cols_full)))

# ---- build tasks ----

# Task A: Full non-invasive feature set
ih_full <- ih_all[, c(feature_cols_full, "outCPP")]
task_full_raw <- as_task_classif(ih_full, target = "outCPP", positive = "1")

# Task B: Vital-signs only (for comparison with original benchmark)
cols_vitals <- c("isbp", "idbp", "hr", "rr", "outCPP")
ih_vitals <- ih_all[, cols_vitals]
task_vitals_raw <- as_task_classif(ih_vitals, target = "outCPP", positive = "1")

# ---- preprocessing pipeline ----

preprocess <- po("removeconstants") %>>%
    po("scale", scale = TRUE, affect_columns = selector_type("numeric")) %>>%
    po("filter", filter = flt("find_correlation"), filter.cutoff = 0.6)

set.seed(0)
task_full <- preprocess$train(list(task_full_raw))[[1]]
cat(sprintf("Full task after preprocessing: %d features\n",
    length(names(task_full$data())) - 1))

set.seed(0)
task_vitals <- preprocess$train(list(task_vitals_raw))[[1]]
cat(sprintf("Vitals task after preprocessing: %d features\n",
    length(names(task_vitals$data())) - 1))

# Use full feature task as primary; vitals task kept for comparison
taskLC <- task_full

# NOTE: Class balancing is embedded inside each learner as a GraphLearner
# (po("classbalancing") %>>% lrn(...)) to ensure balancing happens independently
# within each CV fold, preventing cross-fold data leakage.
# The original unbalanced task (taskLC) is used directly in benchmark_grid.

# //!SECTION

# //SECTION - learners

library(pak)

# latest GitHub release
# pak::pak("mlr-org/mlr3extralearners@*release")

# # development version
# pak::pak("mlr-org/mlr3extralearners")

library(mlr3extralearners)

# install.packages(c("e1071", "kknn", "xgboost", "lightgbm", "catboost"))

# install.packages("remotes")

# remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.10/catboost-R-Windows-1.2.10.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))

library(xgboost)

library(lightgbm)

library(catboost)

# //ANCHOR - learnerLR

# logistic regression (with per-fold class balancing)
learnerLR <- as_learner(
    po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
    lrn("classif.log_reg", predict_type = "prob")
)
learnerLR$id <- "LR"

# //ANCHOR - learnerNB (now tunedNB — see tuning section)
# //ANCHOR - learnerSVM

learnerSVM <- lrn("classif.svm", type = "C-classification", kernel = "radial", predict_type = "prob")
learnerSVM$id <- "SVM"

# //!SECTION

# //SECTION - tuning

# //ANCHOR - tuningPara

# stop flag
terminator <- trm("evals", n_evals = 30)  # increased from 5 for expanded search spaces

# tuning method
tuner <- tnr("random_search")

# resampling method
resampling <- rsmp("cv", folds = 5)

# measure
measure <- msr("classif.ce")

# //ANCHOR - tunedRpart

# search space
# View(as.data.table(learnerRpart$param_set))
psRpart <- ps(
    cp = p_dbl(lower = 0.0001, upper = 0.1),
    minsplit = p_int(lower = 2, upper = 50),
    maxdepth = p_int(lower = 2, upper = 15),
    minbucket = p_int(lower = 1, upper = 20)
)

# autotuner (with per-fold class balancing)
tunedRpart <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.rpart", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psRpart,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedSVM

# search space
# View(as.data.table(learnerSVM$param_set))
psSVM <- ps(
    cost = p_dbl(lower = 1e-1, upper = 100),
    gamma = p_dbl(lower = 0.001, upper = 1)
)

# auto tuner
tunedSVM <- auto_tuner(
    tuner = tuner,
    learner = learnerSVM,
    search_space = psSVM,
    resampling = resampling,
    measure = measure,
    terminator = terminator, 
    store_models = TRUE
)

# //ANCHOR - tunedKNN

# search space
# View(as.data.table(learnerKNN$param_set))
psKNN <- ps(
    k = p_int(lower = 1, upper = 50),
    distance = p_dbl(lower = 0.5, upper = 5),
    kernel = p_fct(levels = c("rectangular", "triangular", "epanechnikov",
                              "biweight", "triweight", "cos", "inv",
                              "gaussian", "rank", "optimal"))
)

# autotuner (with per-fold class balancing)
tunedKNN <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.kknn", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psKNN,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedNNet

# search space
# View(as.data.table(learnerNNet$param_set))
psNNet <- ps(
    size = p_int(lower = 1, upper = 20),
    decay = p_dbl(lower = 0.0001, upper = 0.1),
    maxit = p_int(lower = 100, upper = 500)
)

# autotuner (with per-fold class balancing)
tunedNNet <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.nnet", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psNNet,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedRF

# search space
# View(as.data.table(learnerRF$param_set))
psRF <- ps(
    num.trees = p_int(lower = 100, upper = 1000),
    max.depth = p_int(lower = 2, upper = 10),
    mtry = p_int(lower = 1, upper = 4),
    min.node.size = p_int(lower = 1, upper = 10)
)

# autotuner (with per-fold class balancing)
tunedRF <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.ranger", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psRF,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedXGB

# search space
# View(as.data.table(learnerXGB$param_set))
psXGB <- ps(
    nrounds = p_int(lower = 50, upper = 500),
    eta = p_dbl(lower = 0.01, upper = 0.3),
    max_depth = p_int(lower = 2, upper = 8),
    subsample = p_dbl(lower = 0.5, upper = 1),
    colsample_bytree = p_dbl(lower = 0.5, upper = 1),
    min_child_weight = p_dbl(lower = 1, upper = 10)
)

# autotuner (with per-fold class balancing)
tunedXGB <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.xgboost", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psXGB,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedLGBM

# search space
# View(as.data.table(learnerLGBM$param_set))
psLGBM <- ps(
    num_leaves = p_int(lower = 10, upper = 200),
    max_depth = p_int(lower = 2, upper = 10),
    learning_rate = p_dbl(lower = 0.01, upper = 0.3),
    min_data_in_leaf = p_int(lower = 5, upper = 50),
    feature_fraction = p_dbl(lower = 0.5, upper = 1)
)

# autotuner (with per-fold class balancing)
tunedLGBM <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.lightgbm", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psLGBM,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedCatB

# search space
# View(as.data.table(learnerCatB$param_set))
psCatB <- ps(
    depth = p_int(lower = 2, upper = 8),
    learning_rate = p_dbl(lower = 0.01, upper = 0.3),
    iterations = p_int(lower = 100, upper = 1000),
    l2_leaf_reg = p_dbl(lower = 1, upper = 10),
    border_count = p_int(lower = 32, upper = 255)
)

# autotuner (with per-fold class balancing)
tunedCatB <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.catboost", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psCatB,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //ANCHOR - tunedNB (NEW: tuned naive Bayes with laplace smoothing)

# search space
# View(as.data.table(learnerNB$param_set))
psNB <- ps(
    laplace = p_dbl(lower = 0, upper = 5)
)

# autotuner (with per-fold class balancing)
tunedNB <- auto_tuner(
    tuner = tuner,
    learner = as_learner(
        po("classbalancing", reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1) %>>%
        lrn("classif.naive_bayes", predict_type = "prob")
    ),
    resampling = resampling,
    search_space = psNB,
    measure = measure,
    terminator = terminator,
    store_models = TRUE
)

# //!SECTION

# //SECTION - benchmark

# //ANCHOR - design

# a seed must be set
set.seed(0)

# multi-model buliding
designLC <- benchmark_grid(
    task = taskLC,
    learners = list(
        learnerLR,        # log_reg has no tunable hyperparameters in base glm
        tunedNB,          # naive Bayes with laplace tuning
        tunedRpart,
        # tunedSVM,
        tunedKNN,
        tunedNNet,
        tunedRF,
        tunedXGB,
        tunedLGBM,
        tunedCatB
    ),
    resampling = resampling
)

bmrLC <- benchmark(designLC, store_models = TRUE)

measuresLC <- bmrLC$aggregate(msrs(c(
    "classif.acc",
    "classif.recall",
    "classif.precision",
    "classif.fbeta",
    "classif.auc",
    "classif.bbrier"
)))
measuresLC

bmrLC$learners$learner

bmrDtLC <- as.data.table(bmrLC)

bmrModelsLC <- mlr3misc::map(bmrDtLC$learner, "model")

outerLearnersLC <- mlr3misc::map(bmrDtLC$learner, "learner")

bmrArchivesLC <- extract_inner_tuning_archives(bmrLC)

innerLearnersLC <- mlr3misc::map(bmrArchivesLC$resample_result, "learners")

# //ANCHOR - best model selection by AUC

# Aggregate performance by learner across CV folds
agg_scores <- bmrLC$aggregate(msr("classif.auc"))
cat("Per-learner aggregated AUC:\n")
print(agg_scores)

# Select best learner by AUC (programmatic, not hardcoded index)
best_learner_id <- agg_scores[which.max(agg_scores$classif.auc)]$learner_id
cat(sprintf("\nBest learner by AUC: %s\n", best_learner_id))

# Print all measures for the best learner
agg_all <- bmrLC$aggregate(msrs(c(
    "classif.acc", "classif.recall", "classif.precision",
    "classif.fbeta", "classif.auc", "classif.bbrier"
)))
cat("Best learner metrics:\n")
print(agg_all[learner_id == best_learner_id])

# Extract one trained instance of the best learner
bmrDtLC <- as.data.table(bmrLC)
best_trained <- bmrDtLC[learner_id == best_learner_id]$learner[[1]]

# Extract the final model:
# - AutoTuner: grab the inner learner (with tuned hyperparameters)
# - GraphLearner / bare Learner: use directly
if (inherits(best_trained, "AutoTuner")) {
    finalModelLC <- best_trained$learner
    cat("Tuned hyperparameters:\n")
    print(finalModelLC$param_set$values)
} else {
    finalModelLC <- best_trained
    cat("Hyperparameters:\n")
    print(finalModelLC$param_set$values)
}

cat(sprintf("Final model class: %s\n", class(finalModelLC)[1]))
cat(sprintf("Final model ID: %s\n", finalModelLC$id))

# //ANCHOR - internal test set evaluation

# Use the Phase 2 split already loaded; filter ih_all (pre-merged data)
test_icuids <- dfMor_test_imp$icuid

cat(sprintf("\n=== Internal test set evaluation (full features) ===\n"))
cat(sprintf("Test patients (from Phase 2 split): %d\n", length(test_icuids)))

# Filter merged data to test patients
ih_test <- ih_all[ih_all$icuid %in% test_icuids, ]
cat(sprintf("Test intervals: %d\n", nrow(ih_test)))

# Build test features matching training schema
ih_test_full <- ih_test[, c(feature_cols_full, "outCPP")]
taskLC_test <- as_task_classif(ih_test_full, target = "outCPP", positive = "1")

# Predict with the best model on held-out test data
predTest <- finalModelLC$predict_newdata(taskLC_test$data())

test_measures <- predTest$score(msrs(c(
    "classif.acc", "classif.recall", "classif.precision",
    "classif.fbeta", "classif.auc", "classif.bbrier"
)))
cat("\nInternal test set performance (full features):\n")
print(test_measures)

# ---- Vitals-only comparison ----
cat(sprintf("\n=== Internal test set evaluation (vitals only) ===\n"))
ih_test_vitals <- ih_test[, cols_vitals]
taskLC_test_vitals <- as_task_classif(ih_test_vitals, target = "outCPP", positive = "1")

# Build and evaluate best vitals-only model
# Re-use the same benchmark infrastructure for vitals-only
design_vitals <- benchmark_grid(
    task = task_vitals,
    learners = list(
        learnerLR, tunedNB, tunedRpart, tunedKNN, tunedNNet,
        tunedRF, tunedXGB, tunedLGBM, tunedCatB
    ),
    resampling = resampling
)
bmr_vitals <- benchmark(design_vitals, store_models = TRUE)

# Select best vitals model
agg_vitals <- bmr_vitals$aggregate(msr("classif.auc"))
best_vitals_id <- agg_vitals[which.max(agg_vitals$classif.auc)]$learner_id
bmrDt_vitals <- as.data.table(bmr_vitals)
best_vitals_trained <- bmrDt_vitals[learner_id == best_vitals_id]$learner[[1]]
finalModel_vitals <- if (inherits(best_vitals_trained, "AutoTuner"))
    best_vitals_trained$learner else best_vitals_trained

cat(sprintf("Best vitals model: %s\n", best_vitals_id))

# Predict vitals-only on test set
predTest_vitals <- finalModel_vitals$predict_newdata(taskLC_test_vitals$data())
test_measures_vitals <- predTest_vitals$score(msrs(c(
    "classif.acc", "classif.recall", "classif.precision",
    "classif.fbeta", "classif.auc", "classif.bbrier"
)))
cat("Internal test set performance (vitals only):\n")
print(test_measures_vitals)

# ---- Comparison table ----
cat(sprintf("\n=== Feature set comparison on internal test set ===\n"))
comparison <- rbind(
    data.frame(feature_set = "full",     t(test_measures)),
    data.frame(feature_set = "vitals_4", t(test_measures_vitals))
)
rownames(comparison) <- NULL
print(comparison)

# //ANCHOR - plot

autoplot(bmrLC)
autoplot(bmrLC, measure = msr("classif.auc"))
autoplot(bmrLC, type = "roc")
autoplot(bmrLC, type = "prc")

autoplot(predTest, type = "roc") + ggtitle("Internal test set ROC (full features)")
autoplot(predTest, type = "prc") + ggtitle("Internal test set PRC (full features)")

save(finalModelLC, file = "finalModelLC.RData")
save(bmrLC, finalModelLC, predTest, test_measures, test_measures_vitals,
     file = "benchCPP_results.RData")

# //!SECTION
