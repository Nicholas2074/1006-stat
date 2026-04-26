# //SECTION - cutoff

# //ANCHOR - hospMortality

# ------------------------------- proc package ------------------------------- #

# library(pROC)

# rocMor <- roc(dfMor$hospMortality, dfMor$cppMean)

# youdenMor <- rocMor$sensitivities + rocMor$specificities - 1

# indexMor <- which.max(youdenMor)

# cutoffMor <- rocMor$thresholds[indexMor]

# cutoffMor

# ------------------------------ cutoff package ------------------------------ #

# install.packages("cutoff")

library(cutoff)

cutoff::roc(dfMor$cppMean, dfMor$hospMortality)

#                      type  auc cutoff sensitivity specificity
# 1 negative classification 0.63  76.91   0.5934579   0.6332288

dfMorCutoff <- dfMor %>% 
    mutate(
        cppCut = ifelse(dfMor$cppMean < 77, 1, 0)
    )

# //ANCHOR - disgcs

cutoff::roc(dfDis$cppMean, dfDis$disgcs)

#                      type  auc cutoff sensitivity specificity
# 1 negative classification 0.56  77.62    0.534413    0.585124

# still use the cutoff calculated by hospMortality
dfDisCutoff <- dfDis %>% 
    mutate(
        cppCut = ifelse(dfDis$cppMean < 77, 1, 0)
    )

# //ANCHOR - devgcs

cutoff::roc(dfDev$cppMean, dfDev$devgcs)

#                      type  auc cutoff sensitivity specificity
# 1 negative classification 0.53  72.17   0.3017408   0.8149254

# still use the cutoff calculated by hospMortality
dfDevCutoff <- dfDev %>% 
    mutate(
        cppCut = ifelse(dfDev$cppMean < 77, 1, 0)
    )

# //ANCHOR - cutoff

ih2 <- ih0 %>% 
    mutate(
        outCPP = ifelse(cpp < 77, 1, 0)
    )

# //!SECTION

# //SECTION - logcurde

# //SECTION - glm

# //ANCHOR - hospMortality

logMorCrude <- glm(hospMortality ~ cppCut, data = dfMorCutoff, family = binomial)

summary(logMorCrude)

pMorCrude <- coef(summary(logMorCrude))[, "Pr(>|z|)"]

orMorCrude <- exp(coef(logMorCrude))

ciMorCrude <- exp(confint(logMorCrude))

print(pMorCrude)
print(orMorCrude)
print(ciMorCrude)

# //ANCHOR - disgcs

logDisCrude <- glm(disgcs ~ cppCut, data = dfDisCutoff, family = binomial)

summary(logDisCrude)

pDisCrude <- coef(summary(logDisCrude))[, "Pr(>|z|)"]

orDisCrude <- exp(coef(logDisCrude))

ciDisCrude <- exp(confint(logDisCrude))

print(pDisCrude)
print(orDisCrude)
print(ciDisCrude)

# //ANCHOR - devgcs

logDevCrude <- glm(devgcs ~ cppCut, data = dfDevCutoff, family = binomial)

summary(logDevCrude)

pDevCrude <- coef(summary(logDevCrude))[, "Pr(>|z|)"]

orDevCrude <- exp(coef(logDevCrude))

ciDevCrude <- exp(confint(logDevCrude))

print(pDevCrude)
print(orDevCrude)
print(ciDevCrude)

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorCrude <- data.frame(
    "Variable" = names(pMorCrude),
    "P value" = pMorCrude,
    "OR" = orMorCrude,
    "Lower" = ciMorCrude[, 1],
    "Upper" = ciMorCrude[, 2],
    row.names = NULL
)

dfForestMorCrude[, -1] <- round(dfForestMorCrude[, -1], 3)

dfForestMorCrude <- dfForestMorCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorCrude)

resMorCrude <- dfForestMorCrude

resMorCrude$" " <- paste(rep("    ", nrow(resMorCrude)), collapse = " ")

colnames(resMorCrude) <- paste0(colnames(resMorCrude), "1")

dim(resMorCrude)
print(resMorCrude)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisCrude <- data.frame(
    "Variable" = names(pDisCrude),
    "P value" = pDisCrude,
    "OR" = orDisCrude,
    "Lower" = ciDisCrude[, 1],
    "Upper" = ciDisCrude[, 2],
    row.names = NULL
)

dfForestDisCrude[, -1] <- round(dfForestDisCrude[, -1], 3)

dfForestDisCrude <- dfForestDisCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisCrude)

resDisCrude <- dfForestDisCrude

resDisCrude$" " <- paste(rep("    ", nrow(resDisCrude)), collapse = " ")

colnames(resDisCrude) <- paste0(colnames(resDisCrude), "2")

dim(resDisCrude)
print(resDisCrude)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevCrude <- data.frame(
    "Variable" = names(pDevCrude),
    "P value" = pDevCrude,
    "OR" = orDevCrude,
    "Lower" = ciDevCrude[, 1],
    "Upper" = ciDevCrude[, 2],
    row.names = NULL
)

dfForestDevCrude[, -1] <- round(dfForestDevCrude[, -1], 3)

dfForestDevCrude <- dfForestDevCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevCrude)

resDevCrude <- dfForestDevCrude

resDevCrude$" " <- paste(rep("    ", nrow(resDevCrude)), collapse = " ")

colnames(resDevCrude) <- paste0(colnames(resDevCrude), "3")

dim(resDevCrude)
print(resDevCrude)

# //ANCHOR - cbind

resAll0 <- cbind(resMorCrude, resDisCrude, resDevCrude)

resAll0$" " <- paste(rep("NA", nrow(resAll0)))

resAll0 <- resAll0 %>%
    mutate(Variable1 = recode(Variable1,
        "cppCut" = "CPP < 77mmHg",
        .default = Variable1
    ))

resAll0 <- resAll0[-1, ]

# //!SECTION

# //!SECTION

# //SECTION - logadjusted1

# //SECTION - covariate

# //ANCHOR - hospMortality

library(tidyverse)

# subset
dfCovMor1 <- dfMorCutoff[, c(
    "icuid", 
    "hospMortality", 
    "cppCut",
    "age", 
    "gender", 
    "bmi"
    )]

# //ANCHOR - disgcs

# subset
dfCovDis1 <- dfDisCutoff[, c(
    "icuid", 
    "disgcs", 
    "cppCut",
    "age", 
    "gender", 
    "bmi"
    )]

# //ANCHOR - devgcs

# subset
dfCovDev1 <- dfDevCutoff[, c(
    "icuid", 
    "devgcs", 
    "cppCut",
    "age", 
    "gender", 
    "bmi"
    )]

# //!SECTION

# //SECTION - glm

# //ANCHOR - hospMortality

# glm
print(names(dfCovMor1))

logMorAdjusted1 <- glm(
    hospMortality ~
        cppCut +
        age +
        gender +
        bmi,
    family = binomial,
    data = dfCovMor1
)

# summary
summary(logMorAdjusted1)

pMorAdjusted1 <- coef(summary(logMorAdjusted1))[, "Pr(>|z|)"]
pMorAdjusted1

orMorAdjusted1 <- exp(coef(logMorAdjusted1))
orMorAdjusted1

ciMorAdjusted1 <- exp(confint(logMorAdjusted1))
ciMorAdjusted1

# //ANCHOR - disgcs

# glm
print(names(dfCovDis1))

logDisAdjusted1 <- glm(
    disgcs ~
        cppCut +
        age +
        gender +
        bmi,
    family = binomial,
    data = dfCovDis1
)

# summary
summary(logDisAdjusted1)

pDisAdjusted1 <- coef(summary(logDisAdjusted1))[, "Pr(>|z|)"]
pDisAdjusted1

orDisAdjusted1 <- exp(coef(logDisAdjusted1))
orDisAdjusted1

ciDisAdjusted1 <- exp(confint(logDisAdjusted1))
ciDisAdjusted1

# //ANCHOR - devgcs

# glm
print(names(dfCovDev1))

logDevAdjusted1 <- glm(
    devgcs ~
        cppCut +
        age +
        gender +
        bmi,
    family = binomial,
    data = dfCovDev1
)

# summary
summary(logDevAdjusted1)

pDevAdjusted1 <- coef(summary(logDevAdjusted1))[, "Pr(>|z|)"]
pDevAdjusted1

orDevAdjusted1 <- exp(coef(logDevAdjusted1))
orDevAdjusted1

ciDevAdjusted1 <- exp(confint(logDevAdjusted1))
ciDevAdjusted1

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorAdjusted1 <- data.frame(
    "Variable" = names(pMorAdjusted1),
    "P value" = pMorAdjusted1,
    "OR" = orMorAdjusted1,
    "Lower" = ciMorAdjusted1[, 1],
    "Upper" = ciMorAdjusted1[, 2],
    row.names = NULL
)

dfForestMorAdjusted1[, -1] <- round(dfForestMorAdjusted1[, -1], 3)

dfForestMorAdjusted1 <- dfForestMorAdjusted1 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorAdjusted1)

resMorAdjusted1 <- dfForestMorAdjusted1

resMorAdjusted1$" " <- paste(rep("    ", nrow(resMorAdjusted1)), collapse = " ")

colnames(resMorAdjusted1) <- paste0(colnames(resMorAdjusted1), "1")

dim(resMorAdjusted1)
print(resMorAdjusted1)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisAdjusted1 <- data.frame(
    "Variable" = names(pDisAdjusted1),
    "P value" = pDisAdjusted1,
    "OR" = orDisAdjusted1,
    "Lower" = ciDisAdjusted1[, 1],
    "Upper" = ciDisAdjusted1[, 2],
    row.names = NULL
)

dfForestDisAdjusted1[, -1] <- round(dfForestDisAdjusted1[, -1], 3)

dfForestDisAdjusted1 <- dfForestDisAdjusted1 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisAdjusted1)

resDisAdjusted1 <- dfForestDisAdjusted1

resDisAdjusted1$" " <- paste(rep("    ", nrow(resDisAdjusted1)), collapse = " ")

colnames(resDisAdjusted1) <- paste0(colnames(resDisAdjusted1), "2")

dim(resDisAdjusted1)
print(resDisAdjusted1)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevAdjusted1 <- data.frame(
    "Variable" = names(pDevAdjusted1),
    "P value" = pDevAdjusted1,
    "OR" = orDevAdjusted1,
    "Lower" = ciDevAdjusted1[, 1],
    "Upper" = ciDevAdjusted1[, 2],
    row.names = NULL
)

dfForestDevAdjusted1[, -1] <- round(dfForestDevAdjusted1[, -1], 3)

dfForestDevAdjusted1 <- dfForestDevAdjusted1 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevAdjusted1)

resDevAdjusted1 <- dfForestDevAdjusted1

# Add a blank column for the forest plot to display CI
# Adjust the column width with space
resDevAdjusted1$" " <- paste(rep("    ", nrow(resDevAdjusted1)), collapse = " ")

colnames(resDevAdjusted1) <- paste0(colnames(resDevAdjusted1), "3")

dim(resDevAdjusted1)
print(resDevAdjusted1)

# //ANCHOR - cbind

resAll1 <- cbind(resMorAdjusted1, resDisAdjusted1, resDevAdjusted1)

resAll1$" " <- paste(rep("NA", nrow(resAll1)))

resAll1 <- resAll1 %>%
    mutate(Variable1 = recode(Variable1,
        "cppCut" = "CPP < 77mmHg",
        "age" = "Age",
        "gender" = "Gender",
        "bmi" = "BMI",
        # "hypertension" = "Hypertension",
        # "cerebrovascular_disease" = "Cerebrovascular disease",
        .default = Variable1
    ))

resAll1 <- resAll1[-1, ]

# //!SECTION

# //!SECTION

# //SECTION - logadjusted2

# //SECTION - covariate

# //ANCHOR - hospMortality

library(tidyverse)

# subset
dfCovMor2 <- dfMorCutoff[, c(
    "icuid", 
    "hospMortality", 
    "cppCut",
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease"
    )]

# //ANCHOR - disgcs

# subset
dfCovDis2 <- dfDisCutoff[, c(
    "icuid", 
    "disgcs", 
    "cppCut",
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease"
    )]

# //ANCHOR - devgcs

# subset
dfCovDev2 <- dfDevCutoff[, c(
    "icuid", 
    "devgcs", 
    "cppCut",
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease"
    )]

# //!SECTION

# //SECTION - glm

# //ANCHOR - hospMortality

# glm
print(names(dfCovMor2))

logMorAdjusted2 <- glm(
    hospMortality ~
        cppCut +
        age +
        gender +
        bmi +
        hypertension + 
        cerebrovascular_disease,
    family = binomial,
    data = dfCovMor2
)

# summary
summary(logMorAdjusted2)

pMorAdjusted2 <- coef(summary(logMorAdjusted2))[, "Pr(>|z|)"]
pMorAdjusted2

orMorAdjusted2 <- exp(coef(logMorAdjusted2))
orMorAdjusted2

ciMorAdjusted2 <- exp(confint(logMorAdjusted2))
ciMorAdjusted2

# //ANCHOR - disgcs

# glm
print(names(dfCovDis2))

logDisAdjusted2 <- glm(
    disgcs ~
        cppCut +
        age +
        gender +
        bmi +
        hypertension + 
        cerebrovascular_disease,
    family = binomial,
    data = dfCovDis2
)

# summary
summary(logDisAdjusted2)

pDisAdjusted2 <- coef(summary(logDisAdjusted2))[, "Pr(>|z|)"]
pDisAdjusted2

orDisAdjusted2 <- exp(coef(logDisAdjusted2))
orDisAdjusted2

ciDisAdjusted2 <- exp(confint(logDisAdjusted2))
ciDisAdjusted2

# //ANCHOR - devgcs

# glm
print(names(dfCovDev2))

logDevAdjusted2 <- glm(
    devgcs ~
        cppCut +
        age +
        gender +
        bmi +
        hypertension + 
        cerebrovascular_disease,
    family = binomial,
    data = dfCovDev2
)

# summary
summary(logDevAdjusted2)

pDevAdjusted2 <- coef(summary(logDevAdjusted2))[, "Pr(>|z|)"]
pDevAdjusted2

orDevAdjusted2 <- exp(coef(logDevAdjusted2))
orDevAdjusted2

ciDevAdjusted2 <- exp(confint(logDevAdjusted2))
ciDevAdjusted2

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorAdjusted2 <- data.frame(
    "Variable" = names(pMorAdjusted2),
    "P value" = pMorAdjusted2,
    "OR" = orMorAdjusted2,
    "Lower" = ciMorAdjusted2[, 1],
    "Upper" = ciMorAdjusted2[, 2],
    row.names = NULL
)

dfForestMorAdjusted2[, -1] <- round(dfForestMorAdjusted2[, -1], 3)

dfForestMorAdjusted2 <- dfForestMorAdjusted2 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorAdjusted2)

resMorAdjusted2 <- dfForestMorAdjusted2

resMorAdjusted2$" " <- paste(rep("    ", nrow(resMorAdjusted2)), collapse = " ")

colnames(resMorAdjusted2) <- paste0(colnames(resMorAdjusted2), "1")

dim(resMorAdjusted2)
print(resMorAdjusted2)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisAdjusted2 <- data.frame(
    "Variable" = names(pDisAdjusted2),
    "P value" = pDisAdjusted2,
    "OR" = orDisAdjusted2,
    "Lower" = ciDisAdjusted2[, 1],
    "Upper" = ciDisAdjusted2[, 2],
    row.names = NULL
)

dfForestDisAdjusted2[, -1] <- round(dfForestDisAdjusted2[, -1], 3)

dfForestDisAdjusted2 <- dfForestDisAdjusted2 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisAdjusted2)

resDisAdjusted2 <- dfForestDisAdjusted2

resDisAdjusted2$" " <- paste(rep("    ", nrow(resDisAdjusted2)), collapse = " ")

colnames(resDisAdjusted2) <- paste0(colnames(resDisAdjusted2), "2")

dim(resDisAdjusted2)
print(resDisAdjusted2)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevAdjusted2 <- data.frame(
    "Variable" = names(pDevAdjusted2),
    "P value" = pDevAdjusted2,
    "OR" = orDevAdjusted2,
    "Lower" = ciDevAdjusted2[, 1],
    "Upper" = ciDevAdjusted2[, 2],
    row.names = NULL
)

dfForestDevAdjusted2[, -1] <- round(dfForestDevAdjusted2[, -1], 3)

dfForestDevAdjusted2 <- dfForestDevAdjusted2 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevAdjusted2)

resDevAdjusted2 <- dfForestDevAdjusted2

# Add a blank column for the forest plot to display CI
# Adjust the column width with space
resDevAdjusted2$" " <- paste(rep("    ", nrow(resDevAdjusted2)), collapse = " ")

colnames(resDevAdjusted2) <- paste0(colnames(resDevAdjusted2), "3")

dim(resDevAdjusted2)
print(resDevAdjusted2)

# //ANCHOR - cbind

resAll2 <- cbind(resMorAdjusted2, resDisAdjusted2, resDevAdjusted2)

resAll2$" " <- paste(rep("NA", nrow(resAll2)))

resAll2 <- resAll2 %>%
    mutate(Variable1 = recode(Variable1,
        "cppCut" = "CPP < 77mmHg",
        "age" = "Age",
        "gender" = "Gender",
        "bmi" = "BMI",
        "hypertension" = "Hypertension",
        "cerebrovascular_disease" = "Cerebrovascular disease",
        .default = Variable1
    ))

print(resAll2$Variable1)

resAll2 <- resAll2[-1, ]

# //!SECTION

# //!SECTION
 
# //SECTION - coplot

resAllCo <- rbind(resAll0, resAll1, resAll2)

library(forestploter)

resForestCo <- forest(
    data = resAllCo[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAllCo$Lower1, 
        resAllCo$Lower2,
        resAllCo$Lower3
    ),
    upper = list(
        resAllCo$Upper1, 
        resAllCo$Upper2,
        resAllCo$Upper3
    ),
    est = list(
        resAllCo$OR1, 
        resAllCo$OR2,
        resAllCo$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1
    # xlim = c(0, 2)
)

# Insert text at the top
library(grid)

resForestCo <- insert_text(resForestCo,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

resForestCo <- insert_text(resForestCo,
    text = c("Crude model", "Adjusted model 1", "Adjusted model 2"),
    row = c(1, 2, 6),
    col = 1,
    part = "body",
    just = "left",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForestCo <- add_border(resForestCo, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForestCo <- add_border(resForestCo, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForestCo <- add_border(resForestCo, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForestCo)

# //!SECTION