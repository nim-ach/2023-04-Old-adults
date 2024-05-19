
# Prepare workspace -------------------------------------------------------

## Load main packages
library(data.table)
library(correlation)

## Load the data
data("elderly")

# Exploring the dataset ---------------------------------------------------

vars <- c("edad" = "Age", "pss_total" = "Total PSS", "sppb_total" = "Total SPPB",
          "se_sueno_otono" = "Fall sleep", "se_sueno_primavera" = "Spring sleep", "se_sueno_invierno" = "Winter sleep", "se_sueno_verano" = "Summer sleep",
          "hrv_vlf_1" = "VLF (pre)", "hrv_sdnn_1" = "SDNN (pre)", "hrv_rmssd_1" = "RMSSD (pre)", "hrv_stress_1" = "Stress (pre)", "hrv_hf_1" = "HF (pre)", "hrv_lf_1" = "LF (pre)",
          "hrv_vlf_3" = "VLF (post)", "hrv_sdnn_3" = "SDNN (post)", "hrv_rmssd_3" = "RMSSD (post)", "hrv_stress_3" = "Stress (post)", "hrv_hf_3" = "HF (post)", "hrv_lf_3" = "LF (post)")

dat <- elderly[, .SD, .SDcols = names(vars)]

cor <- correlation(dat, p_adjust = "none", method = "pearson", bayesian = TRUE)

cor_signif <- dplyr::filter(.data = cor,
                            !((Parameter1 %like% "hrv") & (Parameter2 %like% "hrv")),
                            !((Parameter1 %like% "^c_") & (Parameter2 %like% "^c_")),
                            !((Parameter1 %like% "^m_") & (Parameter2 %like% "^m_")),
                            !((Parameter1 %like% "^se_") & (Parameter2 %like% "^se_"))) |>
  dplyr::arrange(abs(rho))

cor_signif$Parameter1 <- vars[cor_signif$Parameter1]
cor_signif$Parameter2 <- vars[cor_signif$Parameter2]

print(cor_signif)

