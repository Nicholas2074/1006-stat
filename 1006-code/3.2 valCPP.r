# //SECTION - import

library(tidyverse)

# ---------------------------------------------------------------------------- #
#                             time interval: 5 mins                            #
# ---------------------------------------------------------------------------- #

# //ANCHOR - hirid

# icp
hicp <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/hicp.csv", header = TRUE)

names(hicp)[1] <- "icuid"

# bp
hbp <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/hbp.csv", header = TRUE)

names(hbp)[1] <- "icuid"

# hr
hhr <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/hhr.csv", header = TRUE)

names(hhr)[1] <- "icuid"

# rr
hrr <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/hrr.csv", header = TRUE)

names(hrr)[1] <- "icuid"

# merge
hBpHr <- merge(hbp, hhr, by = c("icuid", "interval"), all = FALSE)
hBpHrRr <- merge(hBpHr, hrr, by = c("icuid", "interval"), all = FALSE)
hIcpBpHrRr <- merge(hicp, hBpHrRr, by = c("icuid", "interval"), all = FALSE)

# //!SECTION

# //SECTION - preprocess

# //ANCHOR - denoise

# denoise of icp
hIcpBpHrRr$icp <-
    ifelse(hIcpBpHrRr$icp >= 100, NA, hIcpBpHrRr$icp)

# denoise of sbp
hIcpBpHrRr$sbp <-
    ifelse(hIcpBpHrRr$sbp < 30 | hIcpBpHrRr$sbp > 300, NA, hIcpBpHrRr$sbp)

# denoise of dbp
hIcpBpHrRr$dbp <-
    ifelse(hIcpBpHrRr$dbp < 10 | hIcpBpHrRr$dbp > 200, NA, hIcpBpHrRr$dbp)

# denoise of hr
hIcpBpHrRr$hr <-
    ifelse(hIcpBpHrRr$hr < 10, NA, hIcpBpHrRr$hr)

# denoise of rr
hIcpBpHrRr$rr <-
    ifelse(hIcpBpHrRr$rr < 10, NA, hIcpBpHrRr$rr)

# //ANCHOR - imputation

# save(hIcpBpHrRr, file = "hIcpBpHrRr.RData")

# library(missForest)

# library(doParallel)

# set.seed(0)

# registerDoParallel(cores = 7)

# hTrajMf <- missForest(icpBpHrRr, parallelize = "variables")

# hTrajMf <- missForest(icpBpHrRr, ntree = 100, parallelize = "forests")

load("D:/Hai/321-stat/1006-stat/hTrajMf.RData")

hih0 <- hTrajMf$ximp

# //ANCHOR - pivoted

# mab, cpp
hih0 <- hih0  %>% 
    mutate(
        mab = round((2 * dbp + sbp) / 3),
        cpp = ifelse(mab - icp < 1, 0, mab - icp)
    )

# outcome of icp, cpp
hih1 <- hih0 %>% 
    mutate(
        outCPP = ifelse(cpp < 77, 1, 0)
    )

# //!SECTION

# //SECTION - validation

library(tidyverse)
library(mlr3verse)

# delete cols
hih2 <- hih1[, c("sbp", "dbp", "hr", "rr", "outCPP")]

names(hih2) <- c("isbp", "idbp", "hr", "rr", "outCPP")

# task definition
taskLCVal <- as_task_classif(hih2, target = "outCPP", positive = "1")

# pipeline building
poBalanceValLC <-
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

taskMorpoBalanceValLC <- poBalanceValLC$train(list(taskLCVal))[[1]]

table(taskMorpoBalanceValLC$data()$outCPP)

predCatLC <- finalModelLC$predict_newdata(taskMorpoBalanceValLC$data())

predMeasuresLC <- predCatLC$score(msrs(c(
    "classif.acc",
    "classif.recall",
    "classif.precision",
    "classif.fbeta",
    "classif.auc",
    "classif.bbrier"
)))
print(predMeasuresLC)

autoplot(predCatLC)
autoplot(predCatLC, type = "roc")

