
library(data.table)
library(brms)
library(bayestestR)

data("elderly")

dat <- copy(elderly)

dat <- dat[edad > 60,]

dat[, se_severidad := C(se_severidad, "contr.poly", 3)]
dat[, se_ssi := C(se_ssi, "contr.poly", 2)]

dat[, se_sueno_horas := mean(c(se_sueno_invierno, se_sueno_verano, se_sueno_otono, se_sueno_primavera)), id]

## Variables
## - sppb_total: capacidad_fisica
## - edad, sexo
## - peso, talla, imc, grasa, musculo, agua, hueso
## - presiones arteriales
## - HRV (pre, peri y post ejercicio)
## - sensibilidad estacional + sueño (horas por estacion)
## - pss_total: estres percibido

priors <-
  prior(normal(0, 10), class = b) +
  prior(cauchy(0, 1), class = sigma, lb = 0)

## H1: El estrés percibido incrementa el tono simpático previo al ejercicio
## pss_total ~ tono_autonomico(simpatico + parasimpatico) + sexo + edad
m_h1_01 <- brm(formula = hrv_sns_1 ~ pss_total,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h1_01");
               describe_posterior(m_h1_01)
               # Observamos que un mayor estrés percibido se asocia a un
               # menor tono simpático previo al ejercicio

m_h1_02 <- brm(formula = hrv_sns_1 ~ pss_total + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h1_02")
               describe_posterior(m_h1_02)
               # Al incluir el sexo y la edad como factores en el análisis,
               # no observamos un cambio en el efecto observado del estrés
               # percibido sobre el tono simpático.

## H2: Una mayor masa muscular se relaciona con un mejor desempeño fisico, y está
## mediado por la cantidad de grasa corporal
## sppb_total ~ musculo*grasa + sexo + edad
m_h2_01 <- brm(formula = sppb_total ~ m_musc_total*m_grasa_total_perc,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h2_01")
               describe_posterior(m_h2_01)
               # La masa muscular no pareciera ejercer un efecto sobre
               # la aptitud física, aún considerando la interacción con
               # la grasa corporal

m_h2_02 <- brm(formula = sppb_total ~ m_musc_total*m_grasa_total_perc + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h2_02")
               describe_posterior(m_h2_02)
               # Al incluir la edad y el sexo como variables de control,
               # no observamos un cambio en el efecto observado.

## H3: El sexo influye sobre la respuesta autonomica cardiaca frente al ejercicio
## HRV_post ~ sexo + HRV_pre + edad
m_h3_01 <- brm(formula = hrv_sns_3 ~ sexo + hrv_sns_1,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h3_01")
               describe_posterior(m_h3_01)
               # hrv_sns_1 *

m_h3_02 <- brm(formula = hrv_sns_3 ~ sexo + hrv_sns_1 + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h3_02")
               describe_posterior(m_h3_02)
               # hrv_sns_1 *

## H4: El PSS no es buen indicador del estrés autonomico cardíaco
## HRV ~ pss_total + sexo + edad
m_h4_01 <- brm(formula = hrv_stress_1 ~ pss_total,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h4_01")
               describe_posterior(m_h4_01)

m_h4_02 <- brm(formula = hrv_stress_1 ~ pss_total + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h4_02")
               describe_posterior(m_h4_02)

## H5: Las personas que refieren mayor sensibilidad estacional son aquellas que también
## experimentan o perciben mayores niveles de estrés
## pss_total ~ sensibilidad_estacional + sexo + edad
m_h5_01 <- brm(formula = pss_total ~ se_ssi + se_severidad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h5_01")
               describe_posterior(m_h5_01)
               ## se_severidad - Severo *

m_h5_02 <- brm(formula = pss_total ~ se_ssi + se_severidad + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h5_02")
               describe_posterior(m_h5_02)
               ## se_ssi - WinterBlues *
               ## se_severidad - Severo *
               plot(m_h5_02)

## H6: El mayor estrés percibido se asocia con una menor HRV y una mayor presión arterial.
## Pss_total + HRV + Presión arterial + edad + sexo
m_h6_01 <- brm(formula = pss_total ~ hrv_sns_1 + hrv_pns_1 + c_pas + c_pad + c_pp,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h6_01")
               describe_posterior(m_h6_01)
               ## hrv_sns_1 *

m_h6_02 <- brm(formula = pss_total ~ hrv_sns_1 + hrv_pns_1 + c_pas + c_pad + c_pp + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h6_02")
               describe_posterior(m_h6_02)
               ## hrv_sns_1 *

## H7: La mayor sensibilidad estacional se relaciona con un menor HRV y con niveles más altos de estrés percibido.
## Pss_total + HRV + Sensibilidad estacional.
m_h7_01 <- brm(formula = pss_total ~ hrv_sns_1 + hrv_pns_1 + se_ssi + se_severidad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h7_01")
               describe_posterior(m_h7_01)
               ## hrv_sns_1 *
               ## se_severidad-Severo *

m_h7_02 <- brm(formula = pss_total ~ hrv_sns_1 + hrv_pns_1 + se_ssi + se_severidad + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h7_02")
               describe_posterior(m_h7_02)

## H8: La mayor sensibilidad estacional se relaciona con un menor desempeño físico y una mayor presión arterial.
## Presión arterial + sensibilidad estacional + sppb_total
m_h8_01 <- brm(formula = sppb_total ~ se_ssi + se_severidad + c_pas + c_pad + c_pp,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h8_01")
               describe_posterior(m_h8_01)

m_h8_02 <- brm(formula = sppb_total ~ se_ssi + se_severidad + c_pas + c_pad + c_pp + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h8_02")
               describe_posterior(m_h8_02)

## H9: Las mayores horas de sueño influyen en la presión arterial y el desempeño físico.
## Sueño + presión arterial + sppb total.
m_h9_01 <- brm(formula = sppb_total ~ se_sueno_horas + c_pas + c_pad + c_pp,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h9_01")
               describe_posterior(m_h9_01)

m_h9_02 <- brm(formula = sppb_total ~ se_sueno_horas + c_pas + c_pad + c_pp + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h9_02")
               describe_posterior(m_h9_02)

## H10: El sexo y las horas de sueño son factores que se relacionan con los niveles de estrés percibido.
## Sexo + horas de sueño + pss_total.
m_h10_01 <- brm(formula = pss_total ~ se_sueno_horas + sexo,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h10_01")
               describe_posterior(m_h10_01)

m_h10_02 <- brm(formula = pss_total ~ se_sueno_horas + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h10_02")
               describe_posterior(m_h10_02)

## H11: Las horas de sueño afectan a los niveles de desarrollo muscular incidiendo en el desempeño físico.
## sppb_total <- Muscular <- horas_sueño
## Horas de sueño + músculo + IMC + sppb_total
m_h11_01 <- brm(formula = sppb_total ~ m_musc_total + se_sueno_horas + m_imc,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h11_01")
               describe_posterior(m_h11_01)

m_h11_02 <- brm(formula = sppb_total ~ m_musc_total + se_sueno_horas + m_imc + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h11_02")
               describe_posterior(m_h11_02)

## H12: Los altos niveles de presión arterial se relacionan con menores niveles de estrés percibido frente a la tarea.
## Presión arterial + pss_total + sexo + edad
m_h12_01 <- brm(formula = pss_total ~ c_pas,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h12_01")
               describe_posterior(m_h12_01)

m_h12_02 <- brm(formula = pss_total ~ c_pas + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h12_02")
               describe_posterior(m_h12_02)

## H13: Las horas de sueño se relacionan con la presión arterial.
## Horas de sueño + presión arterial + edad + sexo
m_h13_01 <- brm(formula = c_pam ~ se_sueno_horas,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h13_01")
               describe_posterior(m_h13_01)

m_h13_02 <- brm(formula = c_pam ~ se_sueno_horas + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h13_02")
               describe_posterior(m_h13_02)

## H14: la sensibilidad estacional se relaciona con las horas de sueño y una menor HRV.
## Sensibilidad estacional + horas de sueño + HRV
m_h14_01 <- brm(formula = se_tiene ~ se_sueno_horas + hrv_sns_1,
               data = within(dat, {se_tiene = as.numeric(se_ssi != "Típico")}),
               family = bernoulli(link = "logit"),
               prior = prior(normal(0, 10), class = b),
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h14_01")
               describe_posterior(m_h14_01)

m_h14_02 <- brm(formula = se_tiene ~ se_sueno_horas + hrv_sns_1 + sexo + edad,
                data = within(dat, {se_tiene = as.numeric(se_ssi != "Típico")}),
                family = bernoulli(link = "logit"),
                prior = prior(normal(0, 10), class = b),
                cores = 5, chains = 5, seed = 1234,
                iter = 4000, warmup = 2000,
                file = "output/modelos/m_h14_02")
                describe_posterior(m_h14_02)

## H15: Las horas de sueño se relacionan con la HRV y con menores niveles de estrés percibido.
## Horas de sueño + pss_total + HRV
m_h15_01 <- brm(formula = se_sueno_horas ~ hrv_sns_1 + hrv_pns_1 + pss_total,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h15_01")
               describe_posterior(m_h15_01)

m_h15_02 <- brm(formula = se_sueno_horas ~ hrv_sns_1 + hrv_pns_1 + pss_total + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h15_02")
               describe_posterior(m_h15_02)

## H16: El índice de masa corporal se relaciona con el desempeño físico.
## IMC + sppb total + sexo + edad
m_h16_01 <- brm(formula = sppb_total ~ m_imc,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h16_01")
               describe_posterior(m_h16_01)

m_h16_02 <- brm(formula = sppb_total ~ m_imc + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h16_02")
               describe_posterior(m_h16_02)

## H17: La mayor sensibilidad estacional se relaciona con el IMC y el desempeño físico.
## Sppb_total + sensibilidad estacional + IMC
m_h17_01 <- brm(formula = sppb_total ~ m_imc + se_ssi + se_severidad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h17_01")
               describe_posterior(m_h17_01)

m_h17_02 <- brm(formula = sppb_total ~ m_imc + se_ssi + se_severidad + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h17_02")
               describe_posterior(m_h17_02)

## H18: El desempeño físico se relaciona con el IMC y con mayores niveles de estrés percibido.
## IMC + pss_total + sppb_total
m_h18_01 <- brm(formula = sppb_total ~ m_imc_cat,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h18_01")
               describe_posterior(m_h18_01)

m_h18_02 <- brm(formula = sppb_total ~ m_imc_cat + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h18_02")
               describe_posterior(m_h18_02)

## H19: El índice de masa corporal se relaciona con la presión arterial y el desempeño físico.
## IMC + presión arterial + sppb_total
m_h19_01 <- brm(formula = m_imc ~ c_pam + sppb_total,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h19_01")
               describe_posterior(m_h19_01)

m_h19_02 <- brm(formula = m_imc ~ c_pam + sppb_total + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 10000, warmup = 2000,
               file = "output/modelos/m_h19_02")
               describe_posterior(m_h19_02)

## H20: Estrés percibido se relaciona con aptitud física.
## pss_total ~ sppb_total
m_h20_01 <- brm(formula = sppb_total ~ pss_total,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               file = "output/modelos/m_h20_01")
               describe_posterior(m_h20_01)

m_h20_02 <- brm(formula = sppb_total ~ pss_total + sexo + edad,
               data = dat,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 10000, warmup = 2000,
               file = "output/modelos/m_h20_02")
               describe_posterior(m_h20_02)
