# //SECTION - import

library(tidyverse)

# ---------------------------------------------------------------------------- #
#                             time interval: 5 mins                            #
# ---------------------------------------------------------------------------- #

# //ANCHOR - eicu

# icp
eicp <- read.csv("1006-oridata/eicp.csv", header = TRUE)

names(eicp)[1] <- c("icuid")

# bp
ebp <- read.csv("1006-oridata/ebp.csv", header = TRUE)

names(ebp)[1] <- c("icuid")

ebp <- ebp[, c("icuid", "interval", "isbp", "idbp")]

# hr
ehr <- read.csv("1006-oridata/ehr.csv", header = TRUE)

names(ehr)[1] <- c("icuid")

# rr
err <- read.csv("1006-oridata/err.csv", header = TRUE)

names(err)[1] <- c("icuid")

# //ANCHOR - mimic

# icp
micp <- read.csv("1006-oridata/micp.csv", header = TRUE)

names(micp)[1] <- c("icuid")

# bp
mbp <- read.csv("1006-oridata/mbp.csv", header = TRUE)

names(mbp)[1] <- c("icuid")

mbp <- mbp[, c("icuid", "interval", "isbp", "idbp")]

# hr
mhr <- read.csv("1006-oridata/mhr.csv", header = TRUE)

names(mhr)[1] <- c("icuid")

# rr
mrr <- read.csv("1006-oridata/mrr.csv", header = TRUE)

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

source("1006-code/denoise.R")

# eICU+MIMIC native column naming: isbp / idbp
icpBpHrRr <- denoise_vitals(icpBpHrRr, sbp_col = "isbp", dbp_col = "idbp")

# summary
# hist(icpBpHrRr$icp)
# qqnorm(icpBpHrRr$icp)
# qqline(icpBpHrRr$icp)

# summary(icpBpHrRr)

# //ANCHOR - imputation

save(icpBpHrRr, file = "icpBpHrRr.RData")

# Within-patient LOCF + NOCB for missing vital signs.
# 5-min interval time series: forward-fill then backward-fill is clinically
# appropriate (vitals are stable over short gaps) and computationally trivial
# compared to missForest on 340K rows.

ih0 <- icpBpHrRr %>%
    arrange(icuid, interval) %>%
    group_by(icuid) %>%
    tidyr::fill(icp, isbp, idbp, hr, rr, .direction = "downup") %>%
    ungroup()

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

save(ih0, ih1, file = "predictors.RData")
# save(ih0, file = "ih0.RData")  # kept for reference
# save(ih1, file = "ih1.RData")