# Prepare workspace -------------------------------------------------------

## Load packages
library(readxl)
library(data.table)

## Import excel file
elderly <- read_excel("data-raw/raw_data.xlsx",
                      skip = 8,
                      col_names = readLines("data-raw/helpers/colnames.txt"))
elderly <- as.data.table(elderly)

elderly[, sppb_cat := fcase(
  grepl("minima", tolower(sppb_cat)), "Mínima",
  grepl("leve", tolower(sppb_cat)), "Leve",
  grepl("moderada", tolower(sppb_cat)), "Moderada",
  grepl("grave", tolower(sppb_cat)), "Grave"
)]

## Transform to factor variables
elderly[, `:=`(
  ## Sex
  sexo = factor(sexo, levels = 1:2, labels = c("Hombre", "Mujer")),
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

## Composición corporal
elderly[m_talla > 100, m_talla := m_talla/100]
elderly[m_peso_kg_1 > 300, m_peso_kg_1 := m_peso_kg_1/10]
elderly[, m_peso_kg := round(x = mean(x = c(m_peso_kg_1, m_peso_kg_2)), digits = 1), id]
elderly[, `:=`(m_peso_kg_1 = NULL, m_peso_kg_2 = NULL)]
elderly[, m_imc := round(m_peso_kg / m_talla^2, digits = 2)]
elderly[, m_imc_cat := cut(m_imc, breaks = c(0, 18.5, 24.9, 29.9, Inf), labels = c("Infrapeso", "Normopeso", "Sobrepeso", "Obeso"))]

## Presión arterial
elderly[, c_pam := round((c_pas + (2 * c_pad)) / 3, 1)]

## SPPB
elderly[, .SD, .SDcols = grepl("sppb", names(elderly))] |>
  lapply(class)

## Export data
save(elderly, file = "data/elderly.RData")
fwrite(elderly, file = "data/elderly.csv")

