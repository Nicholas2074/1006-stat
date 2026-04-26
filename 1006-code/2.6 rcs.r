# //ANCHOR - hospMortality

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
    filepath = "D:/"
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

dist <- datadist(dfCovDis)

options(datadist = dist)

fitDis <- lrm(
    disgcs ~ rcs(cppMean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfCovDis,
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

dist <- datadist(dfCovDev)

options(datadist = dist)

fitDev <- lrm(
    devgcs ~ rcs(cppMean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfCovDev,
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
