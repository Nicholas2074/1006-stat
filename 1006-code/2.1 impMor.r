# //SECTION - boruta

library(mlr3verse)

library(tidyverse)

set.seed(0)

# //ANCHOR - preprocess

load("dfMor.Rdata")

# delete icuid
dfMorDel <- dfMor[, -c(1, 7:16)]

# task definition
taskMor <- as_task_classif(dfMorDel, target = "hospMortality", positive = "1")

# pipline building
po1Mor <-
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
taskMorPo1 <- po1Mor$train(taskMor)[[1]]
print(names(taskMorPo1$data()))

# //!SECTION

# //SECTION - shap

# //ANCHOR - feature selection

# load learner
learnerRpart1 = lrn("classif.rpart", predict_type = "prob")
learnerRanger1 = lrn("classif.ranger", importance = "impurity", predict_type = "prob")
learnerSVM1 <- lrn("classif.svm", type = "C-classification", kernel = "linear", predict_type = "prob")

# feature selection
instanceMor <- fselect(
    fselector = fs("rfecv"),
    task = taskMorPo1,
    learner = learnerRpart1,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.ce"),
    store_models = TRUE
)

# best performing feature subset
instanceMor$result

# all evaluated feature subsets
as.data.table(instanceMor$archive)

# subset the task and fit the final model
taskMorPo1$select(instanceMor$result_feature_set)
learnerRpart1$train(taskMorPo1)

# //ANCHOR - visualization

dfShapMor <- taskMorPo1$data()
print(names(dfShapMor))

# shap
library(kernelshap)

shapKsMor <- kernelshap(learnerRpart1, dfShapMor[1:300, -1], predict_type = "prob")

# viz
library(shapviz)

vizKsMor <- shapviz(shapKsMor, which_class = 1)

sv_importance(vizKsMor, kind = "beeswarm")

save.image()

# //!SECTION