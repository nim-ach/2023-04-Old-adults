##
## Influencia del IMC sobre los análisis
##


# Preparamos el entorno de trabajo ----------------------------------------

## Paquetes
library(data.table)
## Datos
data("elderly")


# Correlaciones puras -----------------------------------------------------


## BMI sobre SPPB
elderly[, cor.test(m_imc, sppb_total, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, sppb_total, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre PSS
elderly[, cor.test(m_imc, pss_total, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, pss_total, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS


# HRV parameters ----------------------------------------------------------


## BMI sobre SNS index
## pre
elderly[, cor.test(m_imc, hrv_sns_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_sns_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_sns_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_sns_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # Mujer: rho = 0.27 p = 0.034

## BMI sobre PNS index
## pre
elderly[, cor.test(m_imc, hrv_pns_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_pns_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_pns_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_pns_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre Stress index
## pre
elderly[, cor.test(m_imc, hrv_stress_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_stress_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_stress_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_stress_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre SDNN
## pre
elderly[, cor.test(m_imc, hrv_sdnn_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_sdnn_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_sdnn_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_sdnn_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre RMSSD
## pre
elderly[, cor.test(m_imc, hrv_rmssd_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_rmssd_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_rmssd_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_rmssd_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # Mujer: rho = -0.26 p = 0.042

## BMI sobre Mean HR
## pre
elderly[, cor.test(m_imc, hrv_mean_hr_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_mean_hr_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_mean_hr_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_mean_hr_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre Mean R-R
## pre
elderly[, cor.test(m_imc, hrv_mean_rr_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_mean_rr_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_mean_rr_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_mean_rr_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre HF
## pre
elderly[, cor.test(m_imc, hrv_hf_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_hf_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_hf_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_hf_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # Mujer: rho = -0.31 p = 0.016

## BMI sobre LF
## pre
elderly[, cor.test(m_imc, hrv_lf_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_lf_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_lf_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_lf_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS

## BMI sobre VLF
## pre
elderly[, cor.test(m_imc, hrv_vlf_1, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_vlf_1, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # NS
## post
elderly[, cor.test(m_imc, hrv_vlf_3, method = "spearman", exact = FALSE)] # NS
elderly[, cor.test(m_imc, hrv_vlf_3, method = "spearman", exact = FALSE)[c("estimate", "p.value")], sexo] # Hombre: rho = 0.65 p = 0.005



