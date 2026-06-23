# //SECTION - features

# //ANCHOR - patient

library(tidyverse)

# eicu
epatient <- read.csv("1006-oridata/epatient.csv", header = TRUE)

names(epatient) <- c("icuid", "age", "gender", "bmi", "race", "icuLos", "hospLos")

# mimic
mpatient <- read.csv("1006-oridata/mpatient.csv", header = TRUE)

names(mpatient) <- c("icuid", "age", "gender", "bmi", "race", "icuLos", "hospLos")

# combine
patient <- rbind(epatient, mpatient)

patient <- patient %>%
    mutate_if(is.integer, as.numeric)

# postprocess
# gender
# male 0
# female 1

# race factorization
# black 1
# white 2
# caucasian 3
# hispanic 4
# unknow 5
patient$race <-
    ifelse(
        patient$race %in% c(
            "African American",
            "BLACK/AFRICAN AMERICAN",
            "BLACK/CARIBBEAN ISLAND",
            "BLACK/CAPE VERDEAN"
        ),
        1,
        ifelse(
            patient$race %in% c("WHITE", "WHITE - OTHER EUROPEAN"),
            2,
            ifelse(
                patient$race == "Caucasian",
                3,
                ifelse(
                    patient$race %in% c(
                        "Hispanic",
                        "HISPANIC/LATINO - PUERTO RICAN",
                        "HISPANIC OR LATINO"
                    ),
                    4,
                    5
                )
            )
        )
    )

patient$bmi <- ifelse(patient$bmi < 0 | patient$bmi > 50, NA, patient$bmi)

# //ANCHOR - diagnosis

# eicu
ediagnosis <- read.csv("1006-oridata/ediagnosis.csv", header = TRUE)

names(ediagnosis)[1] <- "icuid"

# mimic
mdiagnosis <- read.csv("1006-oridata/mdiagnosis.csv", header = TRUE)

names(mdiagnosis)[1] <- "icuid"

# combine
diagnosis <- rbind(ediagnosis, mdiagnosis)

diagnosis <- diagnosis %>%
    mutate_if(is.integer, as.numeric)

# postprocess
diagnosis[, 2:18][is.na(diagnosis[, 2:18])] <- 0

# //ANCHOR - score

# eicu
escore <- read.csv("1006-oridata/escore.csv", header = TRUE)

names(escore)[1] <- "icuid"

# mimic
mscore <- read.csv("1006-oridata/mscore.csv", header = TRUE)

names(mscore)[1] <- "icuid"

# combine
score <- rbind(escore, mscore)

score <- score %>%
    mutate_if(is.integer, as.numeric)

score$gcs <- ifelse(score$gcs < 9, 1, 0)

# //ANCHOR - surgery

# eicu
esurgery <- read.csv("1006-oridata/esurgery.csv", header = TRUE)

names(esurgery)[1] <- "icuid"

# mimic
msurgery <- read.csv("1006-oridata/msurgery.csv", header = TRUE)

names(msurgery)[1] <- "icuid"

# combine
surgery <- rbind(esurgery, msurgery)

surgery <- surgery %>%
    mutate_if(is.integer, as.numeric)

# postprocess
surgery[, 2:4][is.na(surgery[, 2:4])] <- 0

# //ANCHOR - hsaline

# eicu
ehsaline <- read.csv("1006-oridata/ehsaline.csv", header = TRUE)

names(ehsaline)[1] <- "icuid"

# mimic
mhsaline <- read.csv("1006-oridata/mhsaline.csv", header = TRUE)

names(mhsaline)[1] <- "icuid"

# combine
hsaline <- rbind(ehsaline, mhsaline)

# //ANCHOR - mannitol

# eicu
emannitol <- read.csv("1006-oridata/emannitol.csv", header = TRUE)

names(emannitol)[1] <- "icuid"

# mimic
mmannitol <- read.csv("1006-oridata/mmannitol.csv", header = TRUE)

names(mmannitol)[1] <- "icuid"

# combine
mannitol <- rbind(emannitol, mmannitol)

# //ANCHOR - vital

# eicu
evital <- read.csv("1006-oridata/evital.csv", header = TRUE)

names(evital)[1] <- "icuid"

# mimic
mvital <- read.csv("1006-oridata/mvital.csv", header = TRUE)

names(mvital)[1] <- "icuid"

# combine
vital <- rbind(evital, mvital)

vital <- vital %>%
    mutate_if(is.integer, as.numeric)

# //ANCHOR - lab

# eicu
elab <- read.csv("1006-oridata/elab.csv", header = TRUE)

names(elab)[1] <- "icuid"

# mimic
mlab <- read.csv("1006-oridata/mlab.csv", header = TRUE)

names(mlab)[1] <- "icuid"

# combine
lab <- rbind(elab, mlab)

mlab$ctnt <- as.numeric(mlab$ctnt)

lab <- lab %>%
    mutate_if(is.integer, as.numeric)

# //ANCHOR - merge

# baseline
dt1 <- merge(patient, score, by = "icuid", all = TRUE)
dt2 <- merge(dt1, diagnosis, by = "icuid", all = TRUE)
dt3 <- merge(dt2, surgery, by = "icuid", all = TRUE)
dt4 <- merge(dt3, hsaline, by = "icuid", all = TRUE)
dt5 <- merge(dt4, mannitol, by = "icuid", all = TRUE)
# feature
dt6 <- merge(dt5, vital, by = "icuid", all = TRUE)
dt7 <- merge(dt6, lab, by = "icuid", all = TRUE)

feature <- dt7

# //ANCHOR - factorization
# gender[, 3]
# race[, 5]
# gcs[, 11]
# delirium[, 14]
# diseases[, 15:31]
# surgery[, 32:34]
# drug[, 35:36]

feature[, c(5, 11, 15:31, 32:34, 35:36)] <- lapply(feature[, c(5, 11, 15:31, 32:34, 35:36)], as.factor)
# feature[, c(3, 5, 11, 15:31, 32:34, 35:36)] <- lapply(feature[, c(3, 5, 11, 15:31, 32:34, 35:36)], as.factor)

feature0 <- feature

# delete icuLos, hospLos, eyes, verbal, motor
featureDel <- feature0[, -c(6, 7, 8, 9, 10)]

# duplicated id
featureDel <- featureDel %>% distinct(icuid, .keep_all = TRUE)

dupID <- featureDel %>% filter(duplicated(icuid)) %>% pull(icuid) %>% unique()
dupID

# //!SECTION

# //SECTION - variables

# //ANCHOR - link

varsDf <- merge(featureDel, ih1, by = "icuid", all.y = TRUE)

print(length(unique(ih1$icuid)))
print(length(unique(featureDel$icuid)))
print(length(unique(varsDf$icuid)))

varsNa <- colMeans(is.na(varsDf))
varsNa

varsDel <- names(varsNa[varsNa > 0.3])
varsDel

varsFinal <- varsDf[, !names(varsDf) %in% varsDel]

print(length(unique(varsFinal$icuid)))
print(colMeans(is.na(varsFinal))) 

# //ANCHOR - link (merge outcomes BEFORE imputation to enable stratified split)

dfMor_raw <- merge(varsFinal, mortality, by = "icuid")
dfDis_raw <- merge(varsFinal, gcs[, c(1, 3)], by = "icuid")
dfDev_raw <- merge(varsFinal, gcs[, c(1, 4)], by = "icuid")

# //ANCHOR - split (stratified by hospMortality, 70% train / 30% test)

set.seed(42)

# Stratified split: preserve outcome proportion in train and test
pos_idx <- which(dfMor_raw$hospMortality == 1)
neg_idx <- which(dfMor_raw$hospMortality == 0)

train_pos <- sample(pos_idx, size = floor(0.7 * length(pos_idx)))
train_neg <- sample(neg_idx, size = floor(0.7 * length(neg_idx)))
train_idx <- sort(c(train_pos, train_neg))
test_idx  <- setdiff(seq_len(nrow(dfMor_raw)), train_idx)

dfMor_train <- dfMor_raw[train_idx, ]
dfMor_test  <- dfMor_raw[test_idx, ]

# Use the same patient split for disgcs and devgcs cohorts
train_icuids <- dfMor_train$icuid
test_icuids  <- dfMor_test$icuid

dfDis_train <- dfDis_raw[dfDis_raw$icuid %in% train_icuids, ]
dfDis_test  <- dfDis_raw[dfDis_raw$icuid %in% test_icuids, ]
dfDev_train <- dfDev_raw[dfDev_raw$icuid %in% train_icuids, ]
dfDev_test  <- dfDev_raw[dfDev_raw$icuid %in% test_icuids, ]

cat(sprintf("Train: %d patients (%.1f%% mortality)\n",
    nrow(dfMor_train), 100 * mean(dfMor_train$hospMortality)))
cat(sprintf("Test:  %d patients (%.1f%% mortality)\n",
    nrow(dfMor_test),  100 * mean(dfMor_test$hospMortality)))

# //ANCHOR - imputation (parameters learned from TRAINING data only)

# mode function
getMode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Learn imputation parameters from training data
medians_num <- lapply(
    dfMor_train %>% select_if(is.numeric) %>% select(-hospMortality, -cppMean),
    median, na.rm = TRUE
)
modes_fct <- lapply(
    dfMor_train %>% select_if(is.factor),
    getMode
)

# Apply imputation to training data
dfMor_train_imp <- dfMor_train
for (col in names(medians_num)) {
    na_idx <- is.na(dfMor_train_imp[[col]])
    if (any(na_idx)) dfMor_train_imp[[col]][na_idx] <- medians_num[[col]]
}
for (col in names(modes_fct)) {
    na_idx <- is.na(dfMor_train_imp[[col]])
    if (any(na_idx)) dfMor_train_imp[[col]][na_idx] <- modes_fct[[col]]
}

# Apply SAME imputation parameters to test data
dfMor_test_imp <- dfMor_test
for (col in names(medians_num)) {
    na_idx <- is.na(dfMor_test_imp[[col]])
    if (any(na_idx)) dfMor_test_imp[[col]][na_idx] <- medians_num[[col]]
}
for (col in names(modes_fct)) {
    na_idx <- is.na(dfMor_test_imp[[col]])
    if (any(na_idx)) dfMor_test_imp[[col]][na_idx] <- modes_fct[[col]]
}

# Apply to disgcs and devgcs cohorts (same parameters from dfMor training data)
impute_df <- function(df, medians_num, modes_fct) {
    for (col in names(medians_num)) {
        if (col %in% names(df)) {
            na_idx <- is.na(df[[col]])
            if (any(na_idx)) df[[col]][na_idx] <- medians_num[[col]]
        }
    }
    for (col in names(modes_fct)) {
        if (col %in% names(df)) {
            na_idx <- is.na(df[[col]])
            if (any(na_idx)) df[[col]][na_idx] <- modes_fct[[col]]
        }
    }
    df
}

dfDis_train_imp <- impute_df(dfDis_train, medians_num, modes_fct)
dfDis_test_imp  <- impute_df(dfDis_test,  medians_num, modes_fct)
dfDev_train_imp <- impute_df(dfDev_train, medians_num, modes_fct)
dfDev_test_imp  <- impute_df(dfDev_test,  medians_num, modes_fct)

# Impute full datasets (Track 1 association analysis — no train/test split)
dfMor_full_imp <- impute_df(dfMor_raw, medians_num, modes_fct)
dfDis_full_imp <- impute_df(dfDis_raw, medians_num, modes_fct)
dfDev_full_imp <- impute_df(dfDev_raw, medians_num, modes_fct)

# //ANCHOR - save

# Clean up intermediate objects to reduce .RData size
rm(epatient, mpatient, patient,
   ediagnosis, mdiagnosis, diagnosis,
   escore, mscore, score,
   esurgery, msurgery, surgery,
   ehsaline, mhsaline, hsaline,
   emannitol, mmannitol, mannitol,
   evital, mvital, vital,
   elab, mlab, lab,
   dt1, dt2, dt3, dt4, dt5, dt6, dt7,
   feature, feature0, featureDel,
   varsDf, varsFinal, dfMor_raw, dfDis_raw, dfDev_raw,
   dfMor_train, dfMor_test, dfDis_train, dfDis_test, dfDev_train, dfDev_test)
invisible(gc())

# Training sets (backward-compatible variable names for downstream scripts)
dfMor <- dfMor_train_imp
dfDis <- dfDis_train_imp
dfDev <- dfDev_train_imp

save(dfMor, dfDis, dfDev,
     dfMor_train_imp, dfMor_test_imp,
     dfDis_train_imp, dfDis_test_imp,
     dfDev_train_imp, dfDev_test_imp,
     medians_num, modes_fct,
     file = "phase2_split.RData")

# Also save individual files for backward compatibility
save(dfMor, file = "dfMor.RData")
save(dfDis, file = "dfDis.RData")
save(dfDev, file = "dfDev.RData")

# Full datasets for Track 1 association analysis (no train/test split)
save(dfMor_full_imp, file = "dfMor_full.RData")
save(dfDis_full_imp, file = "dfDis_full.RData")
save(dfDev_full_imp, file = "dfDev_full.RData")

# //!SECTION