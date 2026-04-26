# //SECTION - preprocess

library(tidyverse)
library(mlr3verse)

load("ih2.RData")

# delete cols
ih3 <- ih2[, c("isbp", "idbp", "hr", "rr", "outCPP")]

# task definition
taskLC <- as_task_classif(ih3, target = "outCPP", positive = "1")

# pipeline building
# The predictive capability of the model declines after standardization.
poBalanceLC <-
    # po("scale",
    #     scale = TRUE, # scale
    #     affect_columns = selector_type("numeric")
    # ) %>>%
    po(
        "classbalancing",
        reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1
    )

# pipeline application
set.seed(0)

taskMorpoBalanceLC <- poBalanceLC$train(list(taskLC))[[1]]

table(taskMorpoBalanceLC$data()$outCPP)

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

# logistic regression
learnerLR <- lrn("classif.log_reg", predict_type = "prob")
learnerLR$id <- "LR"

# //ANCHOR - learnerNB

# nive_bayes
learnerNB <- lrn("classif.naive_bayes", predict_type = "prob")
learnerNB$id <- "NB"

# //ANCHOR - learnerRpart

learnerRpart <- lrn("classif.rpart", predict_type = "prob")
learnerRpart$id <- "Rpart"

# //ANCHOR - learnerSVM

learnerSVM <- lrn("classif.svm", type = "C-classification", kernel = "radial", predict_type = "prob")
learnerSVM$id <- "SVM"

# //ANCHOR - learnerKNN

learnerKNN <- lrn("classif.kknn", predict_type = "prob")
learnerKNN$id <- "KNN"

# //ANCHOR - learnerNNet

learnerNNet <- lrn("classif.nnet", predict_type = "prob")
learnerNNet$id <- "NNet"

# //ANCHOR - learnerRF

learnerRF <- lrn("classif.ranger", predict_type = "prob")
learnerRF$id <- "RF"

# //ANCHOR - learnerXGB

learnerXGB <- lrn("classif.xgboost", predict_type = "prob")
learnerXGB$id <- "XGBoost"

# //ANCHOR - learnerLGBM

learnerLGBM <- lrn("classif.lightgbm", predict_type = "prob")
learnerLGBM$id <- "LGBM"

# //ANCHOR - learnerCatB

learnerCatB <- lrn("classif.catboost", predict_type = "prob")
learnerCatB$id <- "CatBoost"

# //!SECTION

# //SECTION - tuning

# //ANCHOR - tuningPara

# stop flag
terminator <- trm("evals", n_evals = 5)

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
    cp = p_dbl(lower = 0.001, upper = 0.05),
    minsplit = p_int(lower = 10, upper = 30)
)

# autotuner
tunedRpart <- auto_tuner(
    tuner = tuner,
    learner = learnerRpart,
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
    cost = p_dbl(1, 5),
    gamma = p_dbl(0.01, 0.5)
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
    k = p_int(lower = 5, upper = 20),
    kernel = p_fct(levels = c("rectangular", "gaussian"))
)

# autotuner
tunedKNN <- auto_tuner(
    tuner = tuner,
    learner = learnerKNN,
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
    size = p_int(lower = 3, upper = 10),
    decay = p_dbl(lower = 0.001, upper = 0.05)
)

# autotuner
tunedNNet <- auto_tuner(
    tuner = tuner,
    learner = learnerNNet,
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
    num.trees = p_int(lower = 50, upper = 200),
    max.depth = p_int(lower = 2, upper = 6)
)

# autotuner
tunedRF <- auto_tuner(
    tuner = tuner,
    learner = learnerRF,
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
    max_depth = p_int(2, 4),
    subsample = p_dbl(0.7, 1)
)

# autotuner
tunedXGB <- auto_tuner(
    tuner = tuner,
    learner = learnerXGB,
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
    max_depth = p_int(2, 4),
    learning_rate = p_dbl(0.05, 0.3)
)

# autotuner
tunedLGBM <- auto_tuner(
    tuner = tuner,
    learner = learnerLGBM,
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
    depth = p_int(2, 5), 
    learning_rate = p_dbl(0.05, 0.2)
)

# autotuner
tunedCatB <- auto_tuner(
    tuner = tuner,
    learner = learnerCatB,
    resampling = resampling,
    search_space = psCatB,
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
    task = taskMorpoBalanceLC,
    learners = list(
        learnerLR,
        learnerNB,
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

# the best model
View(bmrModelsLC)

finalModelLC <- outerLearnersLC[[16]]
finalModelLC

# //ANCHOR - plot

autoplot(bmrLC)

autoplot(bmrLC, measure = msr("classif.auc"))

autoplot(bmrLC, type = "roc")

autoplot(bmrLC, type = "prc")

save.image()

# //!SECTION
