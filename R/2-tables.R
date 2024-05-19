
# Preparing the workspace ----------------------------------------

## Load main packages
library(data.table)
library(gtsummary)

## Load the dataset
data("elderly")

# Table 1 - Anthropometric and sleep parameters --------------------

## Data preparation ----

vars <- c("edad" = "Age", "sexo" = "Sex", "m_peso_kg" = "Weight (kg)",  "m_talla" = "Height (m)", "m_imc" = "BMI", "m_imc_cat" = "Cat IMC",
          "m_musc_total" = "Muscle mass", "m_grasa_total_perc" = "Total fat (%)", "m_agua" = "Water", "m_masa_osea" = "Bone mass", "m_metab_basal" = "Basal metabolism",
          "se_sueno_otono" = "Fall sleep", "se_sueno_invierno" = "Winter sleep", "se_sueno_primavera" = "Spring sleep", "se_sueno_verano" = "Summer sleep")

dat <- elderly[, .SD, .SDcols = names(vars)]
names(dat) <- vars[names(dat)]
dat[, Sex := `levels<-`(Sex, c("Male","Female"))]

## Table 1 ----

tbl_1 <- tbl_summary(
  data = dat,
  by = Sex,
  digits = list(everything() ~ 1,
                "Height (m)" ~ 2,
                "Basal metabolism" ~ 0)
) |> add_overall()


# Table 2 - Hemodynamic parameters --------------------------------------

## Data preparation ----

vars2 <- grep("^hrv_.+_[1|3]|^c_", names(elderly), value = TRUE)

dat2 <- melt.data.table(elderly,
                id.vars = c("id", "c_pas", "c_pad", "c_pam", "c_pp"),
                measure.vars = grep("^hrv_.+_[1|3]", names(elderly), value = TRUE))

dat2[, time := fcase(
  grepl("_1$", variable), "Pre",
  grepl("_3$", variable), "Post"
)]
dat2[, variable := gsub("_1$|_3$", "", variable)]

dat2 <- dcast.data.table(
  data = dat2,
  formula = id + c_pas + c_pad + c_pam + c_pp + time ~ variable,
  value.var = "value"
)

dat2[, time := factor(time, levels = c("Pre", "Post"))]

vars2 <- c("id" = "ID", "c_pas" = "SBP (mmHg)", "c_pad" = "DBP (mmHg)", "c_pam" = "MAP (mmHg)", "c_pp" = "PP (mmHg)",
  "time" = "Time", "hrv_hf" = "HF", "hrv_lf" = "LF", "hrv_vlf" = "VLF", "hrv_mean_hr" = "Mean HR",
  "hrv_mean_rr" = "Mean R-R", "hrv_rmssd" = "RMSSD", "hrv_sdnn" = "SDNN",
   "hrv_pns" = "PNS index", "hrv_sns" = "SNS index", "hrv_stress" = "Stress index")

dat2 <- dat2[, .SD, .SDcols = names(vars2)]
colnames(dat2) <- vars2

## Table 2 ----

tbl_2 <- tbl_summary(
  data = dat2[, -c("ID")],
  by = Time,
  digits = everything() ~ 1
)
