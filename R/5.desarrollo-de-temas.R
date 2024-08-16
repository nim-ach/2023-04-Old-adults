# ### Impacto de la Sensibilidad Estacional en la Aptitud Física y el Estrés Percibido
#
# **Objetivo**: Explorar cómo la sensibilidad estacional afecta la aptitud
# física y el estrés percibido en personas mayores, y determinar si estas
# relaciones son lineales o no lineales.
#
# **Justificación**:
#   - Los hallazgos 5, 7 y 8 indican que la sensibilidad estacional
#   (especialmente el winter blues) se asocia tanto con el estrés percibido
#   como con la aptitud física.
#   - Se observan relaciones no lineales, lo que sugiere que diferentes niveles
#   de sensibilidad estacional podrían tener efectos variados.
#
# **Hipótesis**:
#   - Las personas mayores con winter blues tienen mayor aptitud física
#   y mayor estrés percibido comparado con aquellos con SAD o sin trastornos
#   estacionales.
#   - La severidad de la sensibilidad estacional modula estas relaciones,
#   independiente de la edad y el sexo.

## Paquetes
library(data.table)
library(brms)

## Importación de datos
data("elderly")

## Filtrado de datos
elderly[edad < 60, edad := edad + 10]
elderly[, se_severidad := C(se_severidad, "contr.poly", 3)]
elderly[, se_ssi := C(se_ssi, "contr.poly", 2)]
elderly[, se_sueno_horas := mean(c(se_sueno_invierno, se_sueno_verano,
                               se_sueno_otono, se_sueno_primavera)), id]

# Tabla de descriptivos ---------------------------------------------------

library(gtsummary)
theme_gtsummary_language("es")

vars <- c("sexo", "edad", "se_ssi", "se_severidad", "sppb_total", "pss_total")
var_labels <- list("sexo" ~ "Sexo", "edad" ~ "Edad", "se_ssi" ~ "Categoría SE", "se_severidad" ~ "Severidad SE", "sppb_total" ~ "Puntaje total SPPB", "pss_total" ~ "Puntaje PSS")

tbl_data <- elderly[, .SD, .SDcols = vars]

tbl1 <- tbl_summary(tbl_data, missing = "no",
                    label = var_labels,
                    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
                    digits = all_continuous() ~ 1)

tbl2 <- tbl_summary(tbl_data, missing = "no",
                    label = var_labels[-1L], by = "sexo",
                    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
                    digits = all_continuous() ~ 1) |>
  add_difference(test = all_continuous() ~ "smd", include = all_continuous())

tbl_final <- tbl_merge(tbls = list(tbl1,tbl2),
                       tab_spanner = c("**Global**",
                                       "**Sexo**"))

print(tbl_final)

saveRDS(tbl_final, file = "docs/table-1.RDS")

# Modelos -----------------------------------------------------------------

elderly[, edad := scale(edad)]
elderly[, sppb_total := scale(sppb_total)]
elderly[, pss_total := scale(pss_total)]

## Priors
priors <-
  prior(normal(0, 10), class = b) +
  prior(normal(0, 10), class = b, dpar = sigma)

#   - 5: El estrés percibido (pss_total) pareciera estar relacionado linealmente
#   con la severidad autopercibida de la sensibilidad estacional (se_severidad),
#   donde una mayor severidad se relaciona con un mayor estrés percibido.
#   Del mismo modo, pareciera haber una relación no lineal entre la sensibilidad
#   estacional medida y el estrés, donde aquellos con winter blues experimentan
#   mayor estrés percibido que aquellos con SAD o trastornos estacionales,
#   independiente de la edad y el sexo.

mc_pss <- brm(formula = bf(pss_total ~ se_ssi + se_severidad + sexo + edad,
                            sigma ~ sexo + edad),
               data = elderly,
               prior = priors,
               cores = 5, chains = 5, seed = 1234,
               iter = 4000, warmup = 2000,
               control = list(adapt_delta = .99),
               file = "docs/models/mc_pss.RDS")
# mc_pss |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf"))
#
#> Parameter       | Median |         95% CI |     pd |   ps |          ROPE | % in ROPE |    BF |  Rhat |      ESS
#> ----------------------------------------------------------------------------------------------------------------
#> (Intercept)     |   1.20 | [ 0.55,  1.84] | 99.99% | 1.00 | [-0.10, 0.10] |        0% | 11.00 | 1.000 |  9965.00
#> sigma_Intercept |  -0.38 | [-0.72,  0.06] | 95.66% | 0.90 | [-0.10, 0.10] |     7.78% | 0.120 | 1.000 |  8308.00
#> se_ssi.L        |   0.04 | [-0.45,  0.53] | 57.02% | 0.41 | [-0.10, 0.10] |    32.57% | 0.025 | 1.000 |  8591.00
#> se_ssi.Q        |  -0.52 | [-0.96, -0.07] | 98.75% | 0.97 | [-0.10, 0.10] |     0.64% | 0.338 | 1.000 | 10042.00
#> se_severidad.L  |   1.16 | [-0.22,  2.54] | 94.86% | 0.93 | [-0.10, 0.10] |     3.27% | 0.270 | 1.000 |  7769.00
#> se_severidad.Q  |  -0.11 | [-1.44,  1.23] | 56.59% | 0.51 | [-0.10, 0.10] |    12.04% | 0.068 | 1.000 |  7480.00
#> se_severidad.C  |   0.17 | [-0.97,  1.28] | 61.47% | 0.55 | [-0.10, 0.10] |    14.33% | 0.060 | 1.000 |  9441.00
#> sexoMujer       |  -0.66 | [-1.14, -0.18] | 99.59% | 0.99 | [-0.10, 0.10] |        0% | 0.982 | 1.000 |  8868.00
#> edad            |  -0.14 | [-0.38,  0.11] | 86.56% | 0.62 | [-0.10, 0.10] |    37.33% | 0.023 | 1.000 |  8625.00
#> sigma_sexoMujer |   0.38 | [-0.09,  0.80] | 94.40% | 0.88 | [-0.10, 0.10] |     9.69% | 0.086 | 1.000 |  8931.00
#> sigma_edad      |   0.10 | [-0.06,  0.27] | 87.59% | 0.48 | [-0.10, 0.10] |    52.17% | 0.016 | 1.000 | 10251.00


#   - 8: La aptitud física (sppb_total) pareciera tener una relación no lineal
#   con la sensibilidad estacional (se_ssi), donde aquellos con winter blues
#   tienen una mayor aptitud física que aquellos con SAD o trastornos
#   estacionales, independiente de la edad y el sexo.

mc_sppb_total <- brm(
  formula = bf(sppb_total ~ se_ssi + se_severidad + sexo + edad,
               sigma ~ sexo + edad),
  data = elderly,
  prior = priors,
  cores = 5, chains = 5, seed = 1234,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = .99),
  file = "docs/models/mc_sppb_total.RDS")
# mc_sppb_total |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf"))
#
#> Parameter       |    Median |        95% CI |     pd |   ps |          ROPE | % in ROPE |    BF |  Rhat |     ESS
#> -----------------------------------------------------------------------------------------------------------------
#> (Intercept)     |      0.39 | [-0.44, 1.20] | 83.07% | 0.77 | [-0.10, 0.10] |    12.36% | 0.057 | 1.000 | 8728.00
#> sigma_Intercept |      0.12 | [-0.22, 0.56] | 73.91% | 0.56 | [-0.10, 0.10] |    34.11% | 0.027 | 1.000 | 8350.00
#> se_ssi.L        |      0.35 | [-0.18, 0.84] | 90.49% | 0.84 | [-0.10, 0.10] |    11.93% | 0.070 | 1.000 | 7032.00
#> se_ssi.Q        |     -0.33 | [-0.80, 0.14] | 92.27% | 0.85 | [-0.10, 0.10] |    12.18% | 0.068 | 1.000 | 9618.00
#> se_severidad.L  |      0.01 | [-1.39, 1.48] | 50.73% | 0.46 | [-0.10, 0.10] |    11.07% | 0.073 | 1.000 | 6472.00
#> se_severidad.Q  |     -0.08 | [-1.35, 1.10] | 54.84% | 0.49 | [-0.10, 0.10] |    12.80% | 0.063 | 1.001 | 6886.00
#> se_severidad.C  |      0.23 | [-0.84, 1.33] | 67.13% | 0.60 | [-0.10, 0.10] |    14.04% | 0.058 | 1.000 | 8023.00
#> sexoMujer       |     -0.10 | [-0.80, 0.58] | 62.48% | 0.51 | [-0.10, 0.10] |    23.16% | 0.036 | 1.000 | 9062.00
#> edad            | -3.36e-03 | [-0.26, 0.27] | 51.04% | 0.24 | [-0.10, 0.10] |    56.48% | 0.013 | 1.000 | 7735.00
#> sigma_sexoMujer |     -0.22 | [-0.68, 0.20] | 83.33% | 0.70 | [-0.10, 0.10] |    23.29% | 0.035 | 1.000 | 8713.00
#> sigma_edad      |      0.13 | [-0.04, 0.31] | 93.34% | 0.66 | [-0.10, 0.10] |    33.01% | 0.028 | 1.000 | 7954.00

mc_sppb_equilibrio <- brm(
  formula = sppb_equilibrio ~ se_ssi + se_severidad + sexo + edad,
  data = elderly,
  prior = prior(normal(0,10), class = b),
  family = cumulative(link = "logit"),
  cores = 5, chains = 5, seed = 1234,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = .99),
  file = "docs/models/mc_sppb_equilibrio.RDS")
# mc_sppb_equilibrio |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf"))
#
#> Parameter      | Median |          95% CI |     pd |   ps |          ROPE | % in ROPE |       BF |  Rhat |      ESS
#> -------------------------------------------------------------------------------------------------------------------
#> Intercept[1]   |  -6.49 | [-10.27, -3.77] |   100% | 1.00 | [-0.18, 0.18] |        0% | 3.80e+03 | 1.000 |  4294.00
#> Intercept[2]   |  -5.55 | [ -9.20, -3.02] |   100% | 1.00 | [-0.18, 0.18] |        0% | 5.53e+03 | 1.000 |  4428.00
#> Intercept[3]   |  -4.50 | [ -8.05, -2.06] |   100% | 1.00 | [-0.18, 0.18] |        0% |   168.17 | 1.000 |  4428.00
#> Intercept[4]   |  -4.21 | [ -7.74, -1.80] |   100% | 1.00 | [-0.18, 0.18] |        0% |   114.94 | 1.000 |  4432.00
#> Intercept[5]   |  -3.87 | [ -7.40, -1.46] | 99.99% | 1.00 | [-0.18, 0.18] |        0% |    43.26 | 1.000 |  4395.00
#> Intercept[6]   |  -3.20 | [ -6.70, -0.82] | 99.78% | 1.00 | [-0.18, 0.18] |        0% |     4.39 | 1.000 |  4396.00
#> se_ssi.L       |   0.33 | [ -0.91,  1.71] | 69.76% | 0.59 | [-0.18, 0.18] |    20.27% |    0.072 | 1.000 |  7736.00
#> se_ssi.Q       |  -0.26 | [ -1.42,  0.85] | 68.08% | 0.56 | [-0.18, 0.18] |    23.60% |    0.065 | 1.000 |  8440.00
#> se_severidad.L |   4.88 | [ -0.96, 15.26] | 93.66% | 0.93 | [-0.18, 0.18] |     1.78% |    0.734 | 1.001 |  3757.00
#> se_severidad.Q |   3.97 | [ -0.88, 11.98] | 93.10% | 0.92 | [-0.18, 0.18] |     2.68% |    0.584 | 1.001 |  3494.00
#> se_severidad.C |   2.51 | [ -1.03,  7.10] | 91.20% | 0.89 | [-0.18, 0.18] |     3.79% |    0.412 | 1.000 |  4372.00
#> sexoMujer      |  -0.90 | [ -2.50,  0.47] | 89.56% | 0.84 | [-0.18, 0.18] |     9.96% |    0.149 | 1.000 | 10109.00
#> edad           |  -0.17 | [ -0.70,  0.36] | 73.90% | 0.48 | [-0.18, 0.18] |    44.89% |    0.034 | 1.000 |  9322.00

mc_sppb_4mts_puntos <- brm(
  formula = sppb_4mts_puntos ~ se_ssi + se_severidad + sexo + edad,
  data = elderly,
  prior = prior(normal(0,10), class = b),
  family = cumulative(link = "logit"),
  cores = 5, chains = 5, seed = 1234,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = .99),
  file = "docs/models/mc_sppb_4mts_puntos.RDS")
# mc_sppb_4mts_puntos |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf"))
#
#> Parameter      |   Median |         95% CI |     pd |   ps |          ROPE | % in ROPE |    BF |  Rhat |      ESS
#> -----------------------------------------------------------------------------------------------------------------
#> Intercept[1]   |    -1.92 | [-3.50, -0.43] | 99.45% | 0.99 | [-0.18, 0.18] |        0% |  1.79 | 1.000 | 10242.00
#> Intercept[2]   |    -1.43 | [-2.98,  0.02] | 97.32% | 0.95 | [-0.18, 0.18] |     2.27% | 0.422 | 1.000 | 10763.00
#> Intercept[3]   | 9.24e-03 | [-1.51,  1.45] | 50.60% | 0.41 | [-0.18, 0.18] |    20.23% | 0.065 | 1.000 | 11366.00
#> se_ssi.L       |     0.58 | [-0.50,  1.77] | 85.08% | 0.76 | [-0.18, 0.18] |    16.42% | 0.092 | 1.000 |  8111.00
#> se_ssi.Q       |    -0.20 | [-1.15,  0.77] | 65.53% | 0.51 | [-0.18, 0.18] |    28.65% | 0.052 | 1.000 |  9150.00
#> se_severidad.L |    -0.21 | [-3.02,  2.57] | 55.92% | 0.51 | [-0.18, 0.18] |    10.82% | 0.139 | 1.000 |  7931.00
#> se_severidad.Q |    -0.84 | [-3.40,  1.51] | 75.53% | 0.71 | [-0.18, 0.18] |     9.84% | 0.156 | 1.000 |  8864.00
#> se_severidad.C |    -1.24 | [-3.61,  1.03] | 85.72% | 0.82 | [-0.18, 0.18] |     7.36% | 0.199 | 1.000 |  9728.00
#> sexoMujer      |    -0.22 | [-1.36,  0.91] | 65.46% | 0.53 | [-0.18, 0.18] |    24.80% | 0.062 | 1.000 | 10654.00
#> edad           | 1.03e-03 | [-0.49,  0.51] | 50.18% | 0.24 | [-0.18, 0.18] |    55.40% | 0.026 | 1.000 | 10083.00

mc_sppb_sit_to_stand_puntaje <- brm(
  formula = sppb_sit_to_stand_puntaje ~ se_ssi + se_severidad + sexo + edad,
  data = elderly,
  prior = prior(normal(0,10), class = b),
  family = cumulative(link = "logit"),
  cores = 5, chains = 5, seed = 1234,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = .99),
  file = "docs/models/mc_sppb_sit_to_stand_puntaje.RDS")
# mc_sppb_sit_to_stand_puntaje |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf"))
#
#> Parameter      | Median |          95% CI |     pd |   ps |          ROPE | % in ROPE |       BF |  Rhat |     ESS
#> ------------------------------------------------------------------------------------------------------------------
#> Intercept[1]   |  -6.49 | [-10.64, -3.60] |   100% | 1.00 | [-0.18, 0.18] |        0% | 4.09e+03 | 1.002 | 3476.00
#> Intercept[2]   |  -3.93 | [ -7.47, -1.61] |   100% | 1.00 | [-0.18, 0.18] |        0% |    49.81 | 1.001 | 3866.00
#> Intercept[3]   |  -2.98 | [ -6.49, -0.70] | 99.64% | 0.99 | [-0.18, 0.18] |        0% |     3.57 | 1.001 | 3834.00
#> Intercept[4]   |  -2.10 | [ -5.59,  0.17] | 96.13% | 0.94 | [-0.18, 0.18] |     3.31% |    0.419 | 1.001 | 3837.00
#> se_ssi.L       |  -0.42 | [ -1.46,  0.63] | 79.31% | 0.68 | [-0.18, 0.18] |    20.86% |    0.071 | 1.001 | 8641.00
#> se_ssi.Q       |  -1.19 | [ -2.36, -0.15] | 98.86% | 0.97 | [-0.18, 0.18] |     0.41% |    0.672 | 1.000 | 9109.00
#> se_severidad.L |   6.50 | [  0.74, 17.21] | 99.07% | 0.99 | [-0.18, 0.18] |        0% |     2.75 | 1.001 | 3291.00
#> se_severidad.Q |   4.37 | [ -0.44, 12.28] | 95.54% | 0.94 | [-0.18, 0.18] |     2.11% |    0.750 | 1.001 | 3175.00
#> se_severidad.C |   1.85 | [ -1.38,  5.99] | 85.43% | 0.83 | [-0.18, 0.18] |     5.04% |    0.282 | 1.001 | 3976.00
#> sexoMujer      |   0.34 | [ -0.86,  1.52] | 71.81% | 0.61 | [-0.18, 0.18] |    21.05% |    0.072 | 1.000 | 9573.00
#> edad           |  -0.13 | [ -0.62,  0.35] | 69.85% | 0.41 | [-0.18, 0.18] |    50.99% |    0.028 | 1.000 | 8694.00

if (interactive()) {
  set.seed(1234)
  results <- list(
    pss = mc_pss |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf")),
    sppb_total = mc_sppb_total |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf")),
    sppb_equilibrio = mc_sppb_equilibrio |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf")),
    sppb_velocidad = mc_sppb_4mts_puntos |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf")),
    sppb_sts = mc_sppb_sit_to_stand_puntaje |> bayestestR::describe_posterior(test = c("pd", "ps", "rope", "bf"))
  )
  saveRDS(results, file = "docs/models/results.RDS")
}

# Graficos ----------------------------------------------------------------

## PSS as response
library(ggplot2)

fig1 <- local({
  posterior <- mc_pss |>
    as_draws_df(variable = "^b", regex = T)
  names(posterior) <- c("b_Intercept", "b_sigma_Intercept", "SE categoría (Lineal)", "SE categoría (Cuadrático)",
                        "SE Severidad (Lineal)", "SE Severidad (Cuadrático)", "SE Severidad (Cúbico)", "Sexo: (Mujer) - (Hombre)",
                        "Edad (+1 SD)", "b_sigma_sexoMujer", "b_sigma_edad", ".chain", ".iteration",
                        ".draw")
  posterior <-as.data.table(posterior) |>
    melt(id.vars = c(".chain", ".iteration", ".draw"))
  posterior[!grepl("Intercept|sigma", variable)]
}) |>
  ggplot(aes(value, forcats::fct_rev(variable))) +
  tidybayes::stat_halfeye(normalize = "all") +
  geom_vline(xintercept = seq(-0.1, 0.1, .01), alpha = .2, col = "darkred") +
  scale_y_discrete(expand = c(0,0.1)) +
  labs(y = NULL, x = "Efecto lineal estandarizado", title = "Efecto sobre el estrés percibido") +
  theme_classic()

print(fig1)

ggsave(filename = "docs/fig1.svg", plot = fig1, device = "svg",
       width = 9, height = 6, units = "in")

## SPPB as response
fig2 <- local({
  posterior <- mc_sppb_total |>
    as_draws_df(variable = "^b", regex = T)
  names(posterior) <- c("b_Intercept", "b_sigma_Intercept", "SE categoría (Lineal)", "SE categoría (Cuadrático)",
                        "SE Severidad (Lineal)", "SE Severidad (Cuadrático)", "SE Severidad (Cúbico)", "Sexo: (Mujer) - (Hombre)",
                        "Edad (+1 SD)", "b_sigma_sexoMujer", "b_sigma_edad", ".chain", ".iteration",
                        ".draw")
  posterior <-as.data.table(posterior) |>
    melt(id.vars = c(".chain", ".iteration", ".draw"))
  posterior[!grepl("Intercept|sigma", variable)]
}) |>
  ggplot(aes(value, forcats::fct_rev(variable))) +
  tidybayes::stat_halfeye(normalize = "all") +
  geom_vline(xintercept = seq(-0.1, 0.1, .01), alpha = .2, col = "darkred") +
  scale_y_discrete(expand = c(0,0.1)) +
  labs(y = NULL, x = "Efecto lineal estandarizado", title = "Efecto sobre la aptitud física") +
  theme_classic()

print(fig2)

ggsave(filename = "docs/fig2.svg", plot = fig2, device = "svg",
       width = 9, height = 6, units = "in")

## SPPB balance as response
fig3 <- local({
  posterior <- mc_sppb_equilibrio |>
    as_draws_df(variable = "^b", regex = T)
  names(posterior) <- c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]",
                        "b_Intercept[5]", "b_Intercept[6]", "SE categoría (Lineal)", "SE categoría (Cuadrático)",
                        "SE Severidad (Lineal)", "SE Severidad (Cuadrático)", "SE Severidad (Cúbico)", "Sexo: (Mujer) - (Hombre)",
                        "Edad (+1 SD)", ".chain", ".iteration", ".draw")
  posterior <-as.data.table(posterior) |>
    melt(id.vars = c(".chain", ".iteration", ".draw"))
  posterior[!grepl("Intercept|sigma", variable)]
}) |>
  ggplot(aes(value, forcats::fct_rev(variable))) +
  tidybayes::stat_halfeye(normalize = "all", p_limits = c(0.01, 0.99)) +
  geom_vline(xintercept = seq(-0.18, 0.18, .025), alpha = .2, col = "darkred") +
  scale_y_discrete(expand = c(0,0.1)) +
  labs(y = NULL, x = "Efecto log-odds", title = "Efecto sobre el equilibrio") +
  theme_classic()

print(fig3)

ggsave(filename = "docs/fig3.svg", plot = fig3, device = "svg",
       width = 9, height = 6, units = "in")

## SPPB sit-to-stand as response
fig4 <- local({
  posterior <- mc_sppb_sit_to_stand_puntaje |>
    as_draws_df(variable = "^b", regex = T)
  names(posterior) <- c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]",
                        "SE categoría (Lineal)", "SE categoría (Cuadrático)",
                        "SE Severidad (Lineal)", "SE Severidad (Cuadrático)", "SE Severidad (Cúbico)", "Sexo: (Mujer) - (Hombre)",
                        "Edad (+1 SD)", ".chain", ".iteration", ".draw")
  posterior <-as.data.table(posterior) |>
    melt(id.vars = c(".chain", ".iteration", ".draw"))
  posterior[!grepl("Intercept|sigma", variable)]
}) |>
  ggplot(aes(value, forcats::fct_rev(variable))) +
  tidybayes::stat_halfeye(normalize = "all", p_limits = c(0.01, 0.99)) +
  geom_vline(xintercept = seq(-0.18, 0.18, .025), alpha = .2, col = "darkred") +
  scale_y_discrete(expand = c(0,0.1)) +
  labs(y = NULL, x = "Efecto log-odds", title = "Efecto sobre el Sit-to-stand") +
  theme_classic()

print(fig4)

ggsave(filename = "docs/fig4.svg", plot = fig4, device = "svg",
       width = 9, height = 6, units = "in")

## SPPB sit-to-stand as response
fig5 <- local({
  posterior <- mc_sppb_4mts_puntos |>
    as_draws_df(variable = "^b", regex = T)
  names(posterior) <- c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]",
                        "SE categoría (Lineal)", "SE categoría (Cuadrático)",
                        "SE Severidad (Lineal)", "SE Severidad (Cuadrático)", "SE Severidad (Cúbico)", "Sexo: (Mujer) - (Hombre)",
                        "Edad (+1 SD)", ".chain", ".iteration", ".draw")
  posterior <-as.data.table(posterior) |>
    melt(id.vars = c(".chain", ".iteration", ".draw"))
  posterior[!grepl("Intercept|sigma", variable)]
}) |>
  ggplot(aes(value, forcats::fct_rev(variable))) +
  tidybayes::stat_halfeye(normalize = "all", p_limits = c(0.01, 0.99)) +
  geom_vline(xintercept = seq(-0.18, 0.18, .01), alpha = .2, col = "darkred") +
  scale_y_discrete(expand = c(0,0.1)) +
  labs(y = NULL, x = "Efecto log-odds", title = "Efecto sobre el Velocidad de Marcha") +
  theme_classic()

print(fig5)

ggsave(filename = "docs/fig5.svg", plot = fig5, device = "svg",
       width = 9, height = 6, units = "in")

