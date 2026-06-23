# //ANCHOR - hospMortality

# Load full datasets for association analysis (Track 1 — no train/test split)
load("dfMor_full.RData")
load("dfDis_full.RData")
load("dfDev_full.RData")
dfMor <- dfMor_full_imp
dfDis <- dfDis_full_imp
dfDev <- dfDev_full_imp

# ------------------------------ rcssci package ------------------------------ #

install.packages("rcssci")

library(rcssci)

rcs_logistic.ushap(
    # knot = 3,
    data = dfMor,
    y = "hospMortality",
    x = "cppMean",
    covs = c(
        "age",
        "gender",
        "bmi",
        "hypertension",
        "cerebrovascular_disease"
    ),
    prob = 0.1,
    filepath = "output/"
)

# -------------------------------- rms package ------------------------------- #

library(rms)

dist <- datadist(dfMor)

options(datadist = dist)

fitMor <- lrm(
    hospMortality ~ rcs(cppMean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfMor,
    x = TRUE, 
    y = TRUE
)

summary(fitMor)

anova(fitMor)

predMor <- rms::Predict(
    fitMor, 
    cppMean, 
    fun = exp
    )

ggplot(predMor)

# //ANCHOR - disgcs

library(rms)

dist <- datadist(dfDis)

options(datadist = dist)

fitDis <- lrm(
    disgcs ~ rcs(cppMean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfDis,
    x = TRUE, y = TRUE
)

summary(fitDis)

anova(fitDis)

predDis <- rms::Predict(
    fitDis, 
    cppMean, 
    fun = exp
    )

ggplot(predDis)

# //ANCHOR - devgcs

library(rms)

dist <- datadist(dfDev)

options(datadist = dist)

fitDev <- lrm(
    devgcs ~ rcs(cppMean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfDev,
    x = TRUE, y = TRUE
)

summary(fitDev)

anova(fitDev)

predDev <- rms::Predict(
    fitDev, 
    cppMean, 
    fun = exp
    )

ggplot(predDev)
