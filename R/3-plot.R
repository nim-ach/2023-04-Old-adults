# Prepare workspace -------------------------------------------------------

## Load main packages
library(data.table)
library(corrplot)

## Load the data
data("elderly")

# Preparing the dataset ---------------------------------------------------

vars <- c("edad" = "Age", "pss_total" = "Total PSS", "sppb_total" = "Total SPPB",
          "se_sueno_otono" = "Fall sleep", "se_sueno_primavera" = "Spring sleep", "se_sueno_invierno" = "Winter sleep", "se_sueno_verano" = "Summer sleep",
          "hrv_vlf_1" = "VLF (pre)", "hrv_sdnn_1" = "SDNN (pre)", "hrv_rmssd_1" = "RMSSD (pre)", "hrv_stress_1" = "Stress (pre)", "hrv_hf_1" = "HF (pre)", "hrv_lf_1" = "LF (pre)",
          "hrv_vlf_3" = "VLF (post)", "hrv_sdnn_3" = "SDNN (post)", "hrv_rmssd_3" = "RMSSD (post)", "hrv_stress_3" = "Stress (post)", "hrv_hf_3" = "HF (post)", "hrv_lf_3" = "LF (post)")

dat <- elderly[, .SD, .SDcols = names(vars)]
names(dat) <- vars[names(dat)]

corr <- cor(dat, method = "spearman")
corr_p <- cor.mtest(dat, conf.level = .95, method = "spearman")

fig <- corrplot(corr, type = "upper", diag = TRUE, p.mat = corr_p$p,
         sig.level = 0.05, insig = "pch", pch = "X", pch.cex = 1,
         tl.col = "gray20") |> substitute()
eval(fig)

jpeg("output/fig-1.jpeg", width = 1080*2.5, height = 1080*2.5, res = 350)
eval(fig)
dev.off()

