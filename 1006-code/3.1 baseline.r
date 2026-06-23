# //ANCHOR - mortality (full cohort — Track 1 association analysis)

# Load full dataset (not split — Track 1 uses all patients)
load("dfMor_full.RData")
dfMor_full <- dfMor_full_imp

library(compareGroups)

# Table 1: Baseline characteristics stratified by hospital mortality
baselineMor <- dfMor_full[, c(1:6, 17:73)]

tableMor <- descrTable(hospMortality ~ . - icuid,
    data = baselineMor,
    method = NA,
    show.all = TRUE
)
# tableMor

# export2word(tableMor, file = "tableMor.docx")

# //ANCHOR - train vs test comparison

# Verify comparability of train/test split
# Uses the Phase 2 split saved in phase2_split.RData
load("phase2_split.RData")

# Tag patients by split
train_icuids <- dfMor_train_imp$icuid
test_icuids  <- dfMor_test_imp$icuid

baselineMor$split <- ifelse(baselineMor$icuid %in% train_icuids, "train",
                     ifelse(baselineMor$icuid %in% test_icuids, "test", NA))

# Drop patients not in either split (should be none)
baselineSplit <- baselineMor[!is.na(baselineMor$split), ]

tableSplit <- descrTable(split ~ . - icuid - hospMortality,
    data = baselineSplit,
    method = NA,
    show.all = TRUE
)
# tableSplit

# export2word(tableSplit, file = "tableSplit.docx")
# //!SECTION
