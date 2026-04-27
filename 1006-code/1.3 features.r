# //SECTION - features

# //ANCHOR - patient

library(tidyverse)

# eicu
epatient <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/epatient.csv", header = TRUE)

names(epatient) <- c("icuid", "age", "gender", "bmi", "race", "icuLos", "hospLos")

# mimic
mpatient <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mpatient.csv", header = TRUE)

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
ediagnosis <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/ediagnosis.csv", header = TRUE)

names(ediagnosis)[1] <- "icuid"

# mimic
mdiagnosis <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mdiagnosis.csv", header = TRUE)

names(mdiagnosis)[1] <- "icuid"

# combine
diagnosis <- rbind(ediagnosis, mdiagnosis)

diagnosis <- diagnosis %>%
    mutate_if(is.integer, as.numeric)

# postprocess
diagnosis[, 2:18][is.na(diagnosis[, 2:18])] <- 0

# //ANCHOR - score

# eicu
escore <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/escore.csv", header = TRUE)

names(escore)[1] <- "icuid"

# mimic
mscore <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mscore.csv", header = TRUE)

names(mscore)[1] <- "icuid"

# combine
score <- rbind(escore, mscore)

score <- score %>%
    mutate_if(is.integer, as.numeric)

score$gcs <- ifelse(score$gcs < 9, 1, 0)

# //ANCHOR - surgery

# eicu
esurgery <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/esurgery.csv", header = TRUE)

names(esurgery)[1] <- "icuid"

# mimic
msurgery <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/msurgery.csv", header = TRUE)

names(msurgery)[1] <- "icuid"

# combine
surgery <- rbind(esurgery, msurgery)

surgery <- surgery %>%
    mutate_if(is.integer, as.numeric)

# postprocess
surgery[, 2:4][is.na(surgery[, 2:4])] <- 0

# //ANCHOR - hsaline

# eicu
ehsaline <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/ehsaline.csv", header = TRUE)

names(ehsaline)[1] <- "icuid"

# mimic
mhsaline <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mhsaline.csv", header = TRUE)

names(mhsaline)[1] <- "icuid"

# combine
hsaline <- rbind(ehsaline, mhsaline)

# //ANCHOR - mannitol

# eicu
emannitol <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/emannitol.csv", header = TRUE)

names(emannitol)[1] <- "icuid"

# mimic
mmannitol <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mmannitol.csv", header = TRUE)

names(mmannitol)[1] <- "icuid"

# combine
mannitol <- rbind(emannitol, mmannitol)

# //ANCHOR - vital

# eicu
evital <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/evital.csv", header = TRUE)

names(evital)[1] <- "icuid"

# mimic
mvital <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mvital.csv", header = TRUE)

names(mvital)[1] <- "icuid"

# combine
vital <- rbind(evital, mvital)

vital <- vital %>%
    mutate_if(is.integer, as.numeric)

# //ANCHOR - lab

# eicu
elab <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/elab.csv", header = TRUE)

names(elab)[1] <- "icuid"

# mimic
mlab <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mlab.csv", header = TRUE)

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

# //ANCHOR - imputation

# median
varsImpNum <- varsFinal %>%
    select_if(is.numeric) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# mode
getMode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

varsImpFactor <- varsFinal %>%
    select_if(is.factor) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), getMode(.), .)))

# merge
varsImp <- cbind(varsImpNum, varsImpFactor)

# //ANCHOR - link

dfMor <- merge(varsImp, mortality, by = "icuid")
dfDis <- merge(varsImp, gcs[, c(1, 3)], by = "icuid")
dfDev <- merge(varsImp, gcs[, c(1, 4)], by = "icuid")

# save(dfMor, file = "dfMor.RData")
# save(dfDis, file = "dfDis.RData")
# save(dfDev, file = "dfDev.RData")

save.image()

# //!SECTION