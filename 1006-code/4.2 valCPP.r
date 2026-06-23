# # //SECTION - import

# library(tidyverse)

# # ---------------------------------------------------------------------------- #
# #                             time interval: 5 mins                            #
# # ---------------------------------------------------------------------------- #

# # //ANCHOR - hirid

# # icp
# hicp <- read.csv("1006-oridata/hicp.csv", header = TRUE)

# names(hicp)[1] <- "icuid"

# # bp
# hbp <- read.csv("1006-oridata/hbp.csv", header = TRUE)

# names(hbp)[1] <- "icuid"

# # hr
# hhr <- read.csv("1006-oridata/hhr.csv", header = TRUE)

# names(hhr)[1] <- "icuid"

# # rr
# hrr <- read.csv("1006-oridata/hrr.csv", header = TRUE)

# names(hrr)[1] <- "icuid"

# # merge
# hBpHr <- merge(hbp, hhr, by = c("icuid", "interval"), all = FALSE)
# hBpHrRr <- merge(hBpHr, hrr, by = c("icuid", "interval"), all = FALSE)
# hIcpBpHrRr <- merge(hicp, hBpHrRr, by = c("icuid", "interval"), all = FALSE)

# # save(hIcpBpHrRr, file = "hIcpBpHrRr.RData")

# # //!SECTION

# //SECTION - preprocess

load("hIcpBpHrRr.RData")

# //ANCHOR - denoise

source("1006-code/denoise.R")

# HiRID source data uses sbp/dbp; rename early to match the model's training columns,
# eliminating the need for post-hoc renaming before prediction.
names(hIcpBpHrRr)[names(hIcpBpHrRr) == "sbp"] <- "isbp"
names(hIcpBpHrRr)[names(hIcpBpHrRr) == "dbp"] <- "idbp"

hIcpBpHrRr <- denoise_vitals(hIcpBpHrRr, sbp_col = "isbp", dbp_col = "idbp")

# //ANCHOR - imputation (INDEPENDENT from eICU+MIMIC)

save(hIcpBpHrRr, file = "hIcpBpHrRr.RData")

# Within-patient LOCF + NOCB for missing vital signs (same method as 1.1 predictors.r)

hih0 <- hIcpBpHrRr %>%
    arrange(icuid, interval) %>%
    group_by(icuid) %>%
    tidyr::fill(icp, isbp, idbp, hr, rr, .direction = "downup") %>%
    ungroup()

# //ANCHOR - pivoted

# mab, cpp (using renamed columns: isbp/idbp)
hih0 <- hih0  %>%
    mutate(
        mab = round((2 * idbp + isbp) / 3),
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

# delete cols (columns already renamed to isbp/idbp matching training data)
hih2 <- hih1[, c("isbp", "idbp", "hr", "rr", "outCPP")]

# task definition
taskLCVal <- as_task_classif(hih2, target = "outCPP", positive = "1")

# NOTE: Validation set must NOT be resampled or class-balanced.
# Class balancing would generate synthetic samples and invalidate all evaluation metrics.
# Predict directly on the original validation data with its true class distribution.

predCatLC <- finalModelLC$predict_newdata(taskLCVal$data())

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

