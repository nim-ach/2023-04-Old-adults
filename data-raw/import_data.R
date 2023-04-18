# Prepare workspace -------------------------------------------------------

## Load packages
library(readxl)
library(data.table)

## Import excel file
elderly <- read_excel("data-raw/Datos Adultos Mayores v1.xlsx", skip = 8)
elderly <- as.data.table(elderly)

## Transform to factor variables
elderly[, `:=`(
  ## Sex and anthropometric variables
  sexo = factor(sexo, levels = 1:2, labels = c("Hombre", "Mujer")),
  m_imc_cat = factor(m_imc_cat, levels = c("Insuficiente", "Normal", "Sobrepeso", "Obesidad")),
  ## Seasonal sensitivity data
  se_patron_invierno = factor(se_patron_invierno, levels = 1:2, labels = c("Sí", "No")),
  se_patron_verano = factor(se_patron_verano, levels = 1:2, labels = c("Sí", "No")),
  se_patron_tipo = factor(se_patron_tipo, levels = 0:3, labels = c("Ausente", "Invierno", "Verano", "Mixto")),
  se_ssi = factor(se_ssi, levels = 1:3, labels = c("Típico", "Winter Blues", "SAD")),
  se_severidad = factor(se_severidad, levels = 0:5, labels = c("No es problema", "Leve", "Moderado", "Importante", "Severo", "Grave")),
  se_variacion_peso = factor(se_variacion_peso, levels = 0:5, labels = c("0 a 1","1.5 a 2","2.5 a 3","3.5 a 4","4.5 a 5","> 5")),
  ## SPPB data
  sppb_cat = factor(sppb_cat, levels = c("Mínima", "Leve", "Moderada", "Grave")),
  sppb_juntos = factor(sppb_juntos, levels = 0:2, labels = c("< 3 seg", "3 a 9.99 seg", "> 10 seg")),
  sppb_semi_tandem = factor(sppb_semi_tandem, levels = 0:2, labels = c("< 3 seg", "3 a 9.99 seg", "> 10 seg")),
  sppb_tandem = factor(sppb_tandem, levels = 0:2, labels = c("< 3 seg", "3 a 9.99 seg", "> 10 seg")),
  sppb_4mts_puntos = factor(sppb_4mts_puntos, levels = 1:4, labels = c("> 8.7 seg", "6.21 a 8.7 seg", "4.82 a 6.2 seg", "< 4.82 seg")),
  sppb_sit_to_stand_puntaje = factor(sppb_sit_to_stand_puntaje, levels = 0:4, labels = c("> 60 seg", "> 16.7 seg", "13.7 a 16.69 seg", "11.2 a 13.69 seg", "< 11.19 seg"))
)][]

## Export data
save(elderly, file = "data/elderly.RData")
fwrite(elderly, file = "data/elderly.csv")
