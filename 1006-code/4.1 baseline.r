# //ANCHOR - mortality

baselineMor <- dfMor[, c(1:6, 17:73)]

library(compareGroups)

tableMor <- descrTable(hospMortality ~ . - icuid,
    data = baselineMor,
    method = NA,
    show.all = TRUE
)
# tableMor

# export2word(tableMor, file = "tableMor.docx")