# Prepare workspace -------------------------------------------------------

## Load main packages
library(data.table)

## Load the data
data("elderly")

## Auxiliary function
any_of <- function(a, b, word) {
  a %ilike% word | b %ilike% word
}

# Exploring the dataset ---------------------------------------------------

dim(elderly) # rows and cols
#> [1] 77 65

vars <- c("m_agua" = "Water", "m_grasa_total_perc" = "Total fat (%)", "edad" = "Age",
          "hrv_vlf_1" = "VLF (pre)", "hrv_sdnn_1" = "SDNN (pre)", "hrv_rmssd_1" = "RMSSD (pre)", "hrv_stress_1" = "Stress (pre)", "hrv_pns_1" = "PNS (pre)", "hrv_sns_1" = "SNS (pre)", "hrv_hf_1" = "HF (pre)", "hrv_lf_1" = "LF (pre)", "hrv_mean_hr_1" = "Mean FC (pre)", "hrv_mean_rr_1" = "Mean RR (pre)",
          "hrv_vlf_2" = "VLF (at)", "hrv_sdnn_2" = "SDNN (at)", "hrv_rmssd_2" = "RMSSD (at)", "hrv_stress_2" = "Stress (at)", "hrv_pns_2" = "PNS (at)", "hrv_sns_2" = "SNS (at)", "hrv_hf_2" = "HF (at)", "hrv_lf_2" = "LF (at)", "hrv_mean_hr_2" = "Mean FC (at)", "hrv_mean_rr_2" = "Mean RR (at)",
          "hrv_vlf_3" = "VLF (post)", "hrv_sdnn_3" = "SDNN (post)", "hrv_rmssd_3" = "RMSSD (post)", "hrv_stress_3" = "Stress (post)", "hrv_pns_3" = "PNS (post)", "hrv_sns_3" = "SNS (post)", "hrv_hf_3" = "HF (post)", "hrv_lf_3" = "LF (post)", "hrv_mean_hr_3" = "Mean FC (post)", "hrv_mean_rr_3" = "Mean RR (post)",
          "se_sueno_otono" = "Fall sleep", "c_pad" = "DP", "c_pas" = "SP", "m_talla" = "Height", "m_masa_osea" = "Bone mass", "m_metab_basal" = "Basal metabolism",
          "se_sueno_primavera" = "Spring sleep", "sppb_total" = "Total SPPB", "se_sueno_invierno" = "Winter sleep", "se_sueno_verano" = "Summer sleep",
          "pss_total" = "Total PSS", "c_pp" = "PP", "m_musc_total" = "Muscle mass",
          "m_imc" = "BMI", "m_peso_kg_1" = "Weight (kg)")

dat <- elderly[, .SD, .SDcols = names(vars)]
names(dat) <- vars[names(dat)]

cor <- correlation::correlation(dat, p_adjust = "none", method = "spearman")

subset(cor,
       p < 0.05 &
         any_of(Parameter1, Parameter2, "VLF|HF|RMSSD|LF|Stress|SPPB|Sleep|PSS") &
         !any_of(Parameter1, Parameter2, "Muscle|Weight|Bone|SP|PP|DP|Basal|Height|fat|Water") &
         !()
)
