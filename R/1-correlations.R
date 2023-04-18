
# Prepare workspace -------------------------------------------------------

## Load main packages
library(data.table)

## Load the data
data("elderly")


# Exploring the dataset ---------------------------------------------------

dim(elderly) # rows and cols
#> [1] 77 84

cor <- correlation::correlation(elderly[, -c("id")], p_adjust = "none", method = "spearman")

cor_signif <- dplyr::filter(.data = cor, p < 0.05,
                            !Parameter1 %like% "_2$", !Parameter2 %like% "_2$",
                            !Parameter1 %like% "_3$", !Parameter2 %like% "_3$",
                            !((Parameter1 %like% "hrv") & (Parameter2 %like% "hrv")),
                            !((Parameter1 %like% "^c_") & (Parameter2 %like% "^c_")),
                            !((Parameter1 %like% "^m_") & (Parameter2 %like% "^m_")),
                            !((Parameter1 %like% "^se_") & (Parameter2 %like% "^se_"))) |>
  dplyr::arrange(rho)

vars = c("m_agua" = "Agua", "hrv_sd1_1" = "SD1", "hrv_sd2_1" = "SD2", "m_grasa_torso_perc" = "Grasa torso (%)",
         "m_edad_calc" = "Edad metabólica", "m_musc_torso" = "Músculo torso", "edad" = "Edad", "hrv_vlf_1" = "VLF",
         "hrv_sdnn_1" = "SDNN", "se_gss" = "GSS", "m_grasa_mid_perc" = "Grasa MID (%)", "hrv_stress_1" = "Stress",
         "m_grasa_mii_perc" = "Grasa MII (%)",  "m_grasa_msd_perc" = "Grasa MSD (%)", "m_grasa_total_perc" = "Grasa total (%)",
         "m_grasa_msi_perc" = "Grasa MSI (%)", "hrv_pns_1" = "PNS", "se_sueno_otono" = "Sueño otoño", "c_pad" = "Presión diastólica",
         "se_sueno_primavera" = "Sueño primavera", "sppb_total" = "SPPB total", "se_sueno_invierno" = "Sueño invierno",
         "pss_total" = "PSS total", "hrv_mean_hr_1" = "FC media", "hrv_mean_rr_1" = "RR media", "c_pp" = "Presión de pulso",
         "m_imc" = "IMC", "m_peso_kg_1" = "Peso (kg)", "m_musc_msi" = "Músculo MSD", "hrv_sns_1" = "SNS", "hrv_lf_1" = "LF")

cor_signif$Parameter1 <- vars[cor_signif$Parameter1]
cor_signif$Parameter2 <- vars[cor_signif$Parameter2]

dplyr::mutate_if(cor_signif[,c(1,2,3,5,6,8,10)], is.numeric, round, digits = 2) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(full_width = F) |>
  kableExtra::save_kable(file = "output/correlations.pdf")
