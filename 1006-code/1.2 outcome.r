# //ANCHOR - hospMortality

# eicu
emortality <- read.csv("1006-oridata/emortality.csv", header = TRUE)

names(emortality) <- c("icuid", "hospMortality")

# mimic
mmortality <- read.csv("1006-oridata/mmortality.csv", header = TRUE)

names(mmortality) <- c("icuid", "hospMortality")

# combine
mortality <- rbind(emortality, mmortality)

# filling
mortality$hospMortality[is.na(mortality$hospMortality)] <- 0

# //ANCHOR - gcs

# eicu
egcs <- read.csv("1006-oridata/edev_gcs.csv", header = TRUE)

names(egcs) <- c("icuid", "admgcs", "disgcs", "devgcs")

# mimic
mgcs <- read.csv("1006-oridata/mdev_gcs.csv", header = TRUE)

names(mgcs) <- c("icuid", "admgcs", "disgcs", "devgcs")

# combine
gcs <- rbind(mgcs, egcs)

# filling
gcs$disgcs[is.na(gcs$disgcs)] <- 0
# without missing data !!!

gcs$devgcs[is.na(gcs$devgcs)] <- 0
# without missing data !!!

# relable disgcs
gcs$disgcs <- ifelse(gcs$disgcs <= 8, 1, 0)

# relable devgcs: deterioration = GCS decreased (negative change), not unchanged (zero)
gcs$devgcs <- ifelse(gcs$devgcs < 0, 1, 0)
