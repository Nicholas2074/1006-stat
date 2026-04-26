# //SECTION - import

library(tidyverse)

# ---------------------------------------------------------------------------- #
#                             time interval: 5 mins                            #
# ---------------------------------------------------------------------------- #

# //ANCHOR - eicu

# icp
eicp <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/eicp.csv", header = TRUE)

names(eicp)[1] <- c("icuid")

# bp
ebp <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/ebp.csv", header = TRUE)

names(ebp)[1] <- c("icuid")

ebp <- ebp[, c("icuid", "interval", "isbp", "idbp")]

# hr
ehr <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/ehr.csv", header = TRUE)

names(ehr)[1] <- c("icuid")

# rr
err <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/err.csv", header = TRUE)

names(err)[1] <- c("icuid")

# //ANCHOR - mimic

# icp
micp <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/micp.csv", header = TRUE)

names(micp)[1] <- c("icuid")

# bp
mbp <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/mbp.csv", header = TRUE)

names(mbp)[1] <- c("icuid")

mbp <- mbp[, c("icuid", "interval", "isbp", "idbp")]

# hr
mhr <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/mhr.csv", header = TRUE)

names(mhr)[1] <- c("icuid")

# rr
mrr <- read.csv("D:/Hai/321-stat/1006-stat/1006-oridata/mrr.csv", header = TRUE)

names(mrr)[1] <- c("icuid")

# //ANCHOR - combine

# combine
icp <- rbind(eicp, micp)
bp <- rbind(ebp, mbp)
hr <- rbind(ehr, mhr)
rr <- rbind(err, mrr)

# merge
bpHr <- merge(bp, hr, by = c("icuid", "interval"), all = FALSE)
bpHrRr <- merge(bpHr, rr, by = c("icuid", "interval"), all = FALSE)
icpBpHrRr <- merge(icp, bpHrRr, by = c("icuid", "interval"), all = FALSE)

# //!SECTION

# //SECTION - preprocess

# //ANCHOR - denoise

# denoise of icp
icpBpHrRr$icp <-
    ifelse(icpBpHrRr$icp >= 100, NA, icpBpHrRr$icp)

# denoise of sbp
icpBpHrRr$isbp <-
    ifelse(icpBpHrRr$isbp < 30 | icpBpHrRr$isbp > 300, NA, icpBpHrRr$isbp)

# denoise of dbp
icpBpHrRr$idbp <-
    ifelse(icpBpHrRr$idbp < 10 | icpBpHrRr$idbp > 200, NA, icpBpHrRr$idbp)

# denoise of hr
icpBpHrRr$hr <-
    ifelse(icpBpHrRr$hr < 10, NA, icpBpHrRr$hr)

# denoise of rr
icpBpHrRr$rr <-
    ifelse(icpBpHrRr$rr < 1, NA, icpBpHrRr$rr)

# summary
# hist(icpBpHrRr$icp)
# qqnorm(icpBpHrRr$icp)
# qqline(icpBpHrRr$icp)

# summary(icpBpHrRr)

# //ANCHOR - imputation

# save(icpBpHrRr, file = "icpBpHrRr.RData")

# library(missForest)

# library(doParallel)

# set.seed(0)

# registerDoParallel(cores = 7)
 
# # Using an appropriate backend 'missForest' can be run parallel.
# # There are two possible ways to do this.
# # One way is to create the random forest object in parallel (parallelize = "forests").
# # This is most useful if a single forest object takes long to compute and there are not many variables in the data.
# # The second way is to compute multiple random forest classifiers parallel on different variables (parallelize = "variables").
# # This is most useful if the data contains many variables and computing the random forests is not taking too long.
# # For details on how to register a parallel backend see for instance the documentation of 'doParallel'.

# # trajMf <- missForest(icpBpHrRr, parallelize = "variables")

# trajMf <- missForest(icpBpHrRr, ntree = 100, parallelize = "forests")

load("D:/Hai/321-stat/1006-stat/trajMf.RData")

ih0 <- trajMf$ximp

print(paste("NA value:", any(is.na(ih0))))

print(length(unique(ih0$icuid)))
print(sum(unique(ih0$icuid) %in% micp$icuid))
print(sum(unique(ih0$icuid) %in% eicp$icuid))

# //ANCHOR - pivoted

# mab, cpp
ih0 <- ih0  %>% 
    mutate(
        mab = round((2 * idbp + isbp) / 3),
        cpp = ifelse(mab - icp < 1, 0, mab - icp)
    )

# 5day mean cpp
ih1 <- ih0 %>% 
    group_by(icuid) %>% 
    summarise(
        cppMean = round(mean(cpp), 2)
    )

# # 1stday mean cpp
# ih1 <- ih0 %>%
#     filter(interval < 288) %>% 
#     group_by(icuid) %>% 
#     summarise(
#         cppMean = round(mean(cpp), 2)
#     )

summary(ih1)
