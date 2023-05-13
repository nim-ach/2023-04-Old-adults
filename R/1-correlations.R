
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

vars = c("m_agua" = "Agua", "m_grasa_torso_perc" = "Grasa torso (%)", "m_grasa_total_perc" = "Grasa total (%)", "edad" = "Edad",
         "hrv_vlf_1" = "VLF (pre)", "hrv_sdnn_1" = "SDNN (pre)", "hrv_rmssd_1" = "RMSSD (pre)", "hrv_stress_1" = "Stress (pre)", "hrv_pns_1" = "PNS (pre)", "hrv_sns_1" = "SNS (pre)", "hrv_hf_1" = "HF (pre)", "hrv_lf_1" = "LF (pre)", "hrv_mean_hr_1" = "FC media (pre)", "hrv_mean_rr_1" = "RR media (pre)",
         "hrv_vlf_2" = "VLF (peri)", "hrv_sdnn_2" = "SDNN (peri)", "hrv_rmssd_2" = "RMSSD (peri)", "hrv_stress_2" = "Stress (peri)", "hrv_pns_2" = "PNS (peri)", "hrv_sns_2" = "SNS (peri)", "hrv_hf_2" = "HF (peri)", "hrv_lf_2" = "LF (peri)", "hrv_mean_hr_2" = "FC media (peri)", "hrv_mean_rr_2" = "RR media (peri)",
         "hrv_vlf_3" = "VLF (post)", "hrv_sdnn_3" = "SDNN (post)", "hrv_rmssd_3" = "RMSSD (post)", "hrv_stress_3" = "Stress (post)", "hrv_pns_3" = "PNS (post)", "hrv_sns_3" = "SNS (post)", "hrv_hf_3" = "HF (post)", "hrv_lf_3" = "LF (post)", "hrv_mean_hr_3" = "FC media (post)", "hrv_mean_rr_3" = "RR media (post)",
         "se_sueno_otono" = "Sueño otoño", "c_pad" = "Presión diastólica", "m_talla" = "Talla", "m_masa_osea" = "Masa ósea", "m_metab_basal" = "Metabolismo basal",
         "se_sueno_primavera" = "Sueño primavera", "sppb_total" = "SPPB total", "se_sueno_invierno" = "Sueño invierno", "se_sueno_verano" = "Sueño verano",
         "pss_total" = "PSS total", "c_pp" = "Presión de pulso", "m_peso_kg_2" = "Peso (Kg)", "m_musc_total" = "Masa muscular",
         "m_imc" = "IMC", "m_peso_kg_1" = "Peso (kg)", "m_peso_kg_2" = "Peso (kg)")

cor_signif$Parameter1 <- vars[cor_signif$Parameter1]
cor_signif$Parameter2 <- vars[cor_signif$Parameter2]

dplyr::mutate_if(cor_signif[,c(1,2,3,5,6,8,10)], is.numeric, round, digits = 2) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(full_width = F) |>
  kableExtra::save_kable(file = "output/correlations.pdf")
