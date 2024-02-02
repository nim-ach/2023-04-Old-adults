
# Prepare workspace -------------------------------------------------------

## Load main packages
library(data.table)

## Load the data
data("elderly")


# Exploring the dataset ---------------------------------------------------

dim(elderly) # rows and cols
#> [1] 77 65

cor <- correlation::correlation(elderly[, -c("id")], p_adjust = "none", method = "spearman")

cor_signif <- dplyr::filter(.data = cor, p < 0.05,
                          # !Parameter1 %like% "_2$", !Parameter2 %like% "_2$",
                          # !Parameter1 %like% "_3$", !Parameter2 %like% "_3$",
                            !((Parameter1 %like% "hrv") & (Parameter2 %like% "hrv")),
                            !((Parameter1 %like% "^c_") & (Parameter2 %like% "^c_")),
                            !((Parameter1 %like% "^m_") & (Parameter2 %like% "^m_")),
                            !((Parameter1 %like% "^se_") & (Parameter2 %like% "^se_"))) |>
  dplyr::arrange(rho)

vars = c("m_agua" = "Water", "m_grasa_total_perc" = "Total fat (%)", "edad" = "Age",
         "hrv_vlf_1" = "VLF (pre)", "hrv_sdnn_1" = "SDNN (pre)", "hrv_rmssd_1" = "RMSSD (pre)", "hrv_stress_1" = "Stress (pre)", "hrv_pns_1" = "PNS (pre)", "hrv_sns_1" = "SNS (pre)", "hrv_hf_1" = "HF (pre)", "hrv_lf_1" = "LF (pre)", "hrv_mean_hr_1" = "Mean FC (pre)", "hrv_mean_rr_1" = "Mean RR (pre)",
         "hrv_vlf_2" = "VLF (at)", "hrv_sdnn_2" = "SDNN (at)", "hrv_rmssd_2" = "RMSSD (at)", "hrv_stress_2" = "Stress (at)", "hrv_pns_2" = "PNS (at)", "hrv_sns_2" = "SNS (at)", "hrv_hf_2" = "HF (at)", "hrv_lf_2" = "LF (at)", "hrv_mean_hr_2" = "Mean FC (at)", "hrv_mean_rr_2" = "Mean RR (at)",
         "hrv_vlf_3" = "VLF (post)", "hrv_sdnn_3" = "SDNN (post)", "hrv_rmssd_3" = "RMSSD (post)", "hrv_stress_3" = "Stress (post)", "hrv_pns_3" = "PNS (post)", "hrv_sns_3" = "SNS (post)", "hrv_hf_3" = "HF (post)", "hrv_lf_3" = "LF (post)", "hrv_mean_hr_3" = "Mean FC (post)", "hrv_mean_rr_3" = "Mean RR (post)",
         "se_sueno_otono" = "Fall sleep", "c_pad" = "DP", "c_pas" = "SP", "m_talla" = "Height", "m_masa_osea" = "Bone mass", "m_metab_basal" = "Basal metabolism",
         "se_sueno_primavera" = "Spring sleep", "sppb_total" = "Total SPPB", "se_sueno_invierno" = "Winter sleep", "se_sueno_verano" = "Summer sleep",
         "pss_total" = "Total PSS", "c_pp" = "PP", "m_musc_total" = "Muscle mass",
         "m_imc" = "BMI", "m_peso_kg_1" = "Weight (kg)")

cor_signif$Parameter1 <- vars[cor_signif$Parameter1]
cor_signif$Parameter2 <- vars[cor_signif$Parameter2]

cor_signif <- na.omit(cor_signif)

corrplot::corrplot(
  corr = cor
)


dplyr::mutate_if(cor_signif[,c(1,2,3,5,6,8,10)], is.numeric, round, digits = 2) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(full_width = F) |>
  kableExtra::save_kable(file = "output/correlations.pdf")
