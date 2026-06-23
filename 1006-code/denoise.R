# //SECTION - denoise
#
# Shared denoising utility: applies physiological plausibility bounds to vital signs.
# Used by both 1.1 predictors.r (eICU+MIMIC training) and 3.2 valCPP.r (HiRID validation).
#
# Args:
#   df: data frame with columns icp, sbp (or isbp), dbp (or idbp), hr, rr
#   sbp_col: name of systolic BP column ("sbp" or "isbp")
#   dbp_col: name of diastolic BP column ("dbp" or "idbp")
#
# Returns:
#   Modified data frame with out-of-range values replaced by NA.
#
# IMPORTANT: Both training and validation sets must use IDENTICAL thresholds
# to ensure consistent feature distributions at training and inference time.

denoise_vitals <- function(df, sbp_col = "isbp", dbp_col = "idbp") {

    # ICP: >= 100 mmHg is physiologically implausible (suggests transducer artifact)
    df$icp <- ifelse(df$icp >= 100, NA, df$icp)

    # SBP: < 30 mmHg or > 300 mmHg is outside viable perfusion range
    df[[sbp_col]] <- ifelse(df[[sbp_col]] < 30 | df[[sbp_col]] > 300, NA, df[[sbp_col]])

    # DBP: < 10 mmHg or > 200 mmHg is outside viable range
    df[[dbp_col]] <- ifelse(df[[dbp_col]] < 10 | df[[dbp_col]] > 200, NA, df[[dbp_col]])

    # HR: < 10 bpm is asystole or monitor artifact
    df$hr <- ifelse(df$hr < 10, NA, df$hr)

    # RR: < 1 breath/min is physiologically impossible (apnea codes as 0)
    df$rr <- ifelse(df$rr < 1, NA, df$rr)

    df
}
# //!SECTION
