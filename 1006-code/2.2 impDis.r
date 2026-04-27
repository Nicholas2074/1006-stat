# //SECTION - boruta

library(mlr3verse)

library(tidyverse)

set.seed(0)

# //ANCHOR - preprocess

load("dfDis.Rdata")

# delete icuid
dfDisDel <- dfDis[, -c(1, 7:16)]

# task definition
taskDis <- as_task_classif(dfDisDel, target = "disgcs", positive = "1")

# pipline building
po1Dis <-
    po(
        "removeconstants" # rm constant vars
    ) %>>%
    # po("encode",
    #     method = "one-hot", # one-hot encoding
    #     affect_columns = selector_type("factor")
    # ) %>>%
    po("scale",
        scale = TRUE, # scale
        affect_columns = selector_type("numeric")
    ) %>>%
    po("filter", # rm highly correlated vars
        filter = flt("find_correlation"), filter.cutoff = 0.6
    ) %>>%
    # po("filter", # boruta filter
    #     filter = mlr3filters::flt("boruta"), filter.cutoff = 1
    # ) %>>%
    po("classbalancing", # classbalancing
        reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1
    )

# pipline application
taskDisPo1 <- po1Dis$train(taskDis)[[1]]
print(names(taskDisPo1$data()))

# //!SECTION

# //SECTION - shap

# //ANCHOR - feature selection

# load learner
learnerRpart1 = lrn("classif.rpart", predict_type = "prob")
learnerRanger1 = lrn("classif.ranger", importance = "impurity", predict_type = "prob")
learnerSVM1 <- lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob")

# feature selection
instanceDis <- fselect(
    fselector = fs("rfecv"),
    task = taskDisPo1,
    learner = learnerRpart1,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.ce"),
    store_models = TRUE
)

# best performing feature subset
instanceDis$result

# all evaluated feature subsets
as.data.table(instanceDis$archive)

# subset the task and fit the final model
taskDisPo1$select(instanceDis$result_feature_set)
learnerRpart1$train(taskDisPo1)

# //ANCHOR - visualization

dfShapDis <- taskDisPo1$data()
print(names(dfShapDis))

# shap
library(kernelshap)

shapKsDis <- kernelshap(learnerRpart1, dfShapDis[1:300, -1], predict_type = "prob")

# viz
library(shapviz)

vizKsDis <- shapviz(shapKsDis, which_class = 1)

sv_importance(vizKsDis, kind = "beeswarm")

save.image()

# //!SECTION