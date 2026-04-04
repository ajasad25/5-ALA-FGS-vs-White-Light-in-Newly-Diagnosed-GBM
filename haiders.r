###############################################################################
#                                                                             #
#   5-ALA FGS vs White-Light in Newly Diagnosed GBM                          #
#   Systematic Review & Meta-Analysis                                         #
#                                                                             #
#   INSTRUCTIONS:                                                             #
#   1. Open in RStudio, set output dir on line 17                             #
#   2. Select All (Cmd+A / Ctrl+A) then Run (Cmd+Enter / Ctrl+Enter)         #
#   3. All PDFs save to the Figures folder — open them from there             #
#                                                                             #
###############################################################################

if (!require("meta"))    install.packages("meta",    repos = "https://cloud.r-project.org")
if (!require("metafor")) install.packages("metafor", repos = "https://cloud.r-project.org")
library(meta)
library(metafor)

out_dir <- file.path(getwd(), "Figures")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
cat("Figures will be saved to:", out_dir, "\n\n")


###############################################################################
#  DATA ENTRY (verified against Extraction_Sheet_5ALA_HGG.xlsx)              #
###############################################################################

# GTR data — all 7 studies
gtr <- data.frame(
  study     = c("Stummer 2006", "Picart 2024", "Mirza 2021",
                "Wong 2023", "Ryskelddiyev 2025", "Roder 2014", "Kim 2014"),
  year      = c(2006, 2024, 2021, 2023, 2025, 2014, 2014),
  design    = c("RCT", "RCT", "Observational",
                "Observational", "Observational", "Observational", "Observational"),
  event_ala = c(90,  53, 120,  37,  51,  12,  32),
  n_ala     = c(139, 67, 253,  50,  71,  27,  40),
  event_wl  = c(47,  33,  20,  76,  20,   4,  17),
  n_wl      = c(131, 69,  90, 189,  70,  33,  40),
  stringsAsFactors = FALSE
)

cat("GTR: ", sum(gtr$event_ala), "/", sum(gtr$n_ala), " vs ",
    sum(gtr$event_wl), "/", sum(gtr$n_wl), " = ",
    sum(gtr$n_ala) + sum(gtr$n_wl), " patients\n\n")

# OS hazard ratio — 5 studies
# Mirza 2021: INVERTED from 2.07 [1.47, 2.93] to 1/2.07 = 0.48 [0.34, 0.68]
os <- data.frame(
  study = c("Stummer 2006", "Picart 2024", "Mirza 2021",
            "Wong 2023", "Ryskelddiyev 2025"),
  year  = c(2006, 2024, 2021, 2023, 2025),
  hr    = c(0.82,  1.03,  1/2.07,  0.88,  1.003),
  lower = c(0.62,  0.68,  1/2.93,  0.55,  0.623),
  upper = c(1.07,  1.54,  1/1.47,  1.39,  1.615),
  stringsAsFactors = FALSE
)
os$log_hr <- log(os$hr)
os$se     <- (log(os$upper) - log(os$lower)) / (2 * qnorm(0.975))

# Median OS for bar chart — 5 studies
os_bar <- data.frame(
  study  = c("Stummer 2006", "Picart 2024", "Mirza 2021", "Wong 2023", "Kim 2014"),
  os_ala = c(15.2, 18.7, 17.47, 14.8, 24.0),
  os_wl  = c(12.8, 20.1, 10.63, 12.5, 14.0),
  stringsAsFactors = FALSE
)


###############################################################################
#  FIGURE 3: GTR Risk Ratio — Random-Effects (All 7 Studies)                 #
###############################################################################

meta_rr <- metabin(
  event.e = event_ala, n.e = n_ala,
  event.c = event_wl,  n.c = n_wl,
  studlab = study, data = gtr,
  sm = "RR", method = "MH", method.tau = "DL",
  random = TRUE, fixed = TRUE, prediction = TRUE,
  incr = 0.5, allincr = FALSE
)
print(summary(meta_rr))

pdf(file.path(out_dir, "Figure3_GTR_RR_forest.pdf"), width = 12, height = 8)
forest(meta_rr,
       sortvar    = gtr$year,
       xlim       = c(0.5, 4),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       lab.e = "5-ALA", lab.c = "White Light",
       leftcols  = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       leftlabs  = c("Study", "Events", "Total", "Events", "Total"),
       rightcols = c("effect", "ci"), rightlabs = c("RR", "95% CI"),
       print.tau2 = TRUE, print.I2 = TRUE, print.pval.Q = TRUE,
       prediction = TRUE,
       col.diamond = "steelblue", col.predict = "darkred",
       fontsize = 12, spacing = 2.0, squaresize = 0.5,
       comb.fixed = FALSE,
       smlab = "Risk Ratio (Random Effects)")
dev.off()
cat("Saved: Figure3_GTR_RR_forest.pdf\n")


###############################################################################
#  FIGURE 4: Subgroup Analysis by Study Design                               #
###############################################################################

meta_rr_sub <- update(meta_rr, subgroup = gtr$design, print.subgroup.name = TRUE)
print(summary(meta_rr_sub))

pdf(file.path(out_dir, "Figure4_GTR_RR_subgroup.pdf"), width = 12, height = 15)
forest(meta_rr_sub,
       sortvar    = gtr$year,
       xlim       = c(0.5, 4),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       lab.e = "5-ALA", lab.c = "White Light",
       leftcols  = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       leftlabs  = c("Study", "Events", "Total", "Events", "Total"),
       rightcols = c("effect", "ci"), rightlabs = c("RR", "95% CI"),
       print.tau2 = TRUE, print.I2 = TRUE, print.pval.Q = TRUE,
       test.subgroup.random = TRUE,
       col.diamond = "steelblue", col.diamond.random = "steelblue",
       col.diamond.common = "grey50",
       fontsize = 12, spacing = 2.2, squaresize = 0.5,
       comb.fixed = FALSE,
       smlab = "Risk Ratio by Study Design")
dev.off()
cat("Saved: Figure4_GTR_RR_subgroup.pdf\n")


###############################################################################
#  FIGURE 5: Leave-One-Out Sensitivity Analysis                              #
###############################################################################

inf <- metainf(meta_rr, pooled = "random")
print(inf)

pdf(file.path(out_dir, "Figure5_LeaveOneOut.pdf"), width = 11, height = 10)
forest(inf,
       xlim       = c(0.8, 3),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       rightcols  = c("effect", "ci"), rightlabs = c("RR", "95% CI"),
       col.diamond = "steelblue",
       fontsize = 12, spacing = 2.0,
       smlab = "Omitting One Study at a Time")
dev.off()
cat("Saved: Figure5_LeaveOneOut.pdf\n")


###############################################################################
#  FIGURE 6: Overall Survival Hazard Ratio                                   #
###############################################################################

meta_os <- metagen(
  TE = os$log_hr, seTE = os$se,
  studlab = os$study, data = os,
  sm = "HR", method.tau = "DL",
  random = TRUE, fixed = TRUE, prediction = TRUE
)
print(summary(meta_os))

pdf(file.path(out_dir, "Figure6_OS_HR_forest.pdf"), width = 11, height = 7)
forest(meta_os,
       sortvar    = os$year,
       xlim       = c(0.2, 2.5),
       label.left = "Favors 5-ALA", label.right = "Favors White Light",
       leftcols  = c("studlab"), leftlabs = c("Study"),
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("HR", "95% CI", "Weight"),
       print.tau2 = TRUE, print.I2 = TRUE, print.pval.Q = TRUE,
       prediction = TRUE,
       col.diamond = "firebrick", col.predict = "darkblue",
       fontsize = 12, spacing = 2.0, squaresize = 0.5,
       comb.fixed = TRUE,
       smlab = "Hazard Ratio (Random Effects)")
dev.off()
cat("Saved: Figure6_OS_HR_forest.pdf\n")


###############################################################################
#  FIGURE 7: Median Overall Survival Bar Chart                               #
###############################################################################

pdf(file.path(out_dir, "Figure7_Median_OS_bar.pdf"), width = 10, height = 7)
bar_mat <- rbind(os_bar$os_ala, os_bar$os_wl)
par(mar = c(8, 5, 3, 2))
bp <- barplot(bar_mat, beside = TRUE, names.arg = os_bar$study,
              col = c("steelblue", "grey65"), border = NA,
              ylim = c(0, max(bar_mat) * 1.3),
              ylab = "Median Overall Survival (months)",
              main = "Median Overall Survival: 5-ALA vs White Light",
              las = 2, cex.names = 1.0, cex.lab = 1.1, cex.main = 1.2)
text(bp[1,], os_bar$os_ala + 0.5, sprintf("%.1f", os_bar$os_ala),
     pos = 3, cex = 0.9, col = "steelblue4", font = 2)
text(bp[2,], os_bar$os_wl + 0.5, sprintf("%.1f", os_bar$os_wl),
     pos = 3, cex = 0.9, col = "grey30", font = 2)
legend("topright", legend = c("5-ALA", "White Light"),
       fill = c("steelblue", "grey65"), border = NA, bty = "n", cex = 1.1)
dev.off()
cat("Saved: Figure7_Median_OS_bar.pdf\n")


###############################################################################
#  SUPPLEMENTARY S1: GTR Odds Ratio                                          #
###############################################################################

meta_or <- metabin(
  event.e = event_ala, n.e = n_ala,
  event.c = event_wl,  n.c = n_wl,
  studlab = study, data = gtr,
  sm = "OR", method = "MH", method.tau = "DL",
  random = TRUE, fixed = TRUE
)
print(summary(meta_or))

pdf(file.path(out_dir, "FigureS1_GTR_OR_forest.pdf"), width = 12, height = 8)
forest(meta_or,
       sortvar    = gtr$year,
       xlim       = c(0.3, 10),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       lab.e = "5-ALA", lab.c = "White Light",
       leftcols  = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       leftlabs  = c("Study", "Events", "Total", "Events", "Total"),
       rightcols = c("effect", "ci"), rightlabs = c("OR", "95% CI"),
       print.tau2 = TRUE, print.I2 = TRUE,
       col.diamond = "darkorange",
       fontsize = 12, spacing = 2.0, squaresize = 0.5,
       comb.fixed = FALSE,
       smlab = "Odds Ratio (Random Effects)")
dev.off()
cat("Saved: FigureS1_GTR_OR_forest.pdf\n")


###############################################################################
#  SUPPLEMENTARY S2: GTR Risk Difference + NNT                               #
###############################################################################

meta_rd <- metabin(
  event.e = event_ala, n.e = n_ala,
  event.c = event_wl,  n.c = n_wl,
  studlab = study, data = gtr,
  sm = "RD", method = "MH", method.tau = "DL",
  random = TRUE, fixed = TRUE
)
print(summary(meta_rd))
cat(sprintf("\n  NNT = 1 / RD = 1 / %.4f = %.1f\n\n",
            meta_rd$TE.random, 1 / meta_rd$TE.random))

pdf(file.path(out_dir, "FigureS2_GTR_RD_forest.pdf"), width = 12, height = 8)
forest(meta_rd,
       sortvar    = gtr$year,
       xlim       = c(-0.2, 0.7),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       lab.e = "5-ALA", lab.c = "White Light",
       leftcols  = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       leftlabs  = c("Study", "Events", "Total", "Events", "Total"),
       rightcols = c("effect", "ci"), rightlabs = c("RD", "95% CI"),
       print.tau2 = TRUE, print.I2 = TRUE,
       col.diamond = "darkgreen",
       fontsize = 12, spacing = 2.0, squaresize = 0.5,
       comb.fixed = FALSE,
       smlab = "Risk Difference (Random Effects)")
dev.off()
cat("Saved: FigureS2_GTR_RD_forest.pdf\n")


###############################################################################
#  SUPPLEMENTARY S3: Sensitivity — Low/Moderate RoB Only                     #
###############################################################################

rob_idx <- gtr$study %in% c("Stummer 2006", "Picart 2024", "Ryskelddiyev 2025")
gtr_rob <- gtr[rob_idx, ]

meta_rr_rob <- metabin(
  event.e = event_ala, n.e = n_ala,
  event.c = event_wl,  n.c = n_wl,
  studlab = study, data = gtr_rob,
  sm = "RR", method = "MH", method.tau = "DL",
  random = TRUE, fixed = TRUE
)
print(summary(meta_rr_rob))

pdf(file.path(out_dir, "FigureS3_Sensitivity_RoB.pdf"), width = 13, height = 6)
forest(meta_rr_rob,
       sortvar    = gtr_rob$year,
       xlim       = c(0.5, 3),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       lab.e = "5-ALA", lab.c = "White Light",
       leftcols  = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       leftlabs  = c("Study", "Events", "Total", "Events", "Total"),
       rightcols = c("effect", "ci"), rightlabs = c("RR", "95% CI"),
       print.tau2 = TRUE, print.I2 = TRUE,
       col.diamond = "purple4",
       fontsize = 12, spacing = 2.0, squaresize = 0.5,
       comb.fixed = FALSE,
       smlab = "RR (Low/Moderate RoB)")
dev.off()
cat("Saved: FigureS3_Sensitivity_RoB.pdf\n")


###############################################################################
#  SUPPLEMENTARY S4: GTR Risk Ratio — Fixed-Effect Model                     #
###############################################################################

pdf(file.path(out_dir, "FigureS4_GTR_RR_fixed.pdf"), width = 12, height = 8)
forest(meta_rr,
       sortvar    = gtr$year,
       xlim       = c(0.5, 8),
       label.left = "Favors White Light", label.right = "Favors 5-ALA",
       lab.e = "5-ALA", lab.c = "White Light",
       leftcols  = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       leftlabs  = c("Study", "Events", "Total", "Events", "Total"),
       rightcols = c("effect", "ci"), rightlabs = c("RR", "95% CI"),
       print.tau2 = TRUE, print.I2 = TRUE,
       col.diamond = "grey50",
       fontsize = 12, spacing = 2.0, squaresize = 0.5,
       comb.fixed = TRUE, comb.random = FALSE,
       smlab = "Risk Ratio (Fixed-Effect, MH)")
dev.off()
cat("Saved: FigureS4_GTR_RR_fixed.pdf\n")


###############################################################################
#  SUPPLEMENTARY S5: GTR Rates Bar Chart                                     #
###############################################################################

pdf(file.path(out_dir, "FigureS5_GTR_rates_bar.pdf"), width = 11, height = 7)
gtr_pct_ala <- 100 * gtr$event_ala / gtr$n_ala
gtr_pct_wl  <- 100 * gtr$event_wl  / gtr$n_wl
bar_mat2    <- rbind(gtr_pct_ala, gtr_pct_wl)
ord <- order(gtr$year)
par(mar = c(9, 5, 3, 2))
bp2 <- barplot(bar_mat2[, ord], beside = TRUE, names.arg = gtr$study[ord],
               col = c("steelblue", "grey65"), border = NA,
               ylim = c(0, 115),
               ylab = "Gross Total Resection Rate (%)",
               main = "GTR Rates by Study: 5-ALA vs White Light",
               las = 2, cex.names = 0.95, cex.lab = 1.1, cex.main = 1.2)
text(bp2[1,], gtr_pct_ala[ord] + 1.5, sprintf("%.0f%%", gtr_pct_ala[ord]),
     pos = 3, cex = 0.85, col = "steelblue4", font = 2)
text(bp2[2,], gtr_pct_wl[ord] + 1.5, sprintf("%.0f%%", gtr_pct_wl[ord]),
     pos = 3, cex = 0.85, col = "grey30", font = 2)
legend("topright", legend = c("5-ALA", "White Light"),
       fill = c("steelblue", "grey65"), border = NA, bty = "n", cex = 1.1)
dev.off()
cat("Saved: FigureS5_GTR_rates_bar.pdf\n")


###############################################################################
#  RESULTS SUMMARY                                                           #
###############################################################################

cat("\n")
cat("===============================================================\n")
cat("  RESULTS SUMMARY\n")
cat("===============================================================\n\n")

cat("PRIMARY OUTCOME - Gross Total Resection:\n")
cat(sprintf("  Random-effects RR = %.2f [%.2f, %.2f], p %s, I2=%.1f%%\n",
            exp(meta_rr$TE.random), exp(meta_rr$lower.random),
            exp(meta_rr$upper.random),
            ifelse(meta_rr$pval.random < 0.0001, "< 0.0001",
                   sprintf("= %.4f", meta_rr$pval.random)),
            meta_rr$I2 * 100))
cat(sprintf("  Fixed-effect RR   = %.2f [%.2f, %.2f]\n",
            exp(meta_rr$TE.common), exp(meta_rr$lower.common),
            exp(meta_rr$upper.common)))
cat(sprintf("  Odds Ratio        = %.2f [%.2f, %.2f]\n",
            exp(meta_or$TE.random), exp(meta_or$lower.random),
            exp(meta_or$upper.random)))
cat(sprintf("  Risk Difference   = %.2f [%.2f, %.2f]\n",
            meta_rd$TE.random, meta_rd$lower.random, meta_rd$upper.random))
cat(sprintf("  NNT               = %.1f\n", 1 / meta_rd$TE.random))

cat("\nSUBGROUP ANALYSIS:\n")
cat(sprintf("  RCT:           RR = %.2f [%.2f, %.2f]\n",
            exp(meta_rr_sub$TE.random.w[1]),
            exp(meta_rr_sub$lower.random.w[1]),
            exp(meta_rr_sub$upper.random.w[1])))
cat(sprintf("  Observational: RR = %.2f [%.2f, %.2f]\n",
            exp(meta_rr_sub$TE.random.w[2]),
            exp(meta_rr_sub$lower.random.w[2]),
            exp(meta_rr_sub$upper.random.w[2])))
cat(sprintf("  Interaction p:    %.4f\n", meta_rr_sub$pval.Q.b.random))

cat("\nSENSITIVITY - Low/Moderate RoB:\n")
cat(sprintf("  RR = %.2f [%.2f, %.2f], I2=%.1f%%\n",
            exp(meta_rr_rob$TE.random), exp(meta_rr_rob$lower.random),
            exp(meta_rr_rob$upper.random), meta_rr_rob$I2 * 100))

cat("\nSECONDARY OUTCOME - Overall Survival:\n")
cat(sprintf("  Random-effects HR = %.2f [%.2f, %.2f], p = %.4f, I2=%.1f%%\n",
            exp(meta_os$TE.random), exp(meta_os$lower.random),
            exp(meta_os$upper.random), meta_os$pval.random, meta_os$I2 * 100))
cat(sprintf("  Common-effect HR  = %.2f [%.2f, %.2f], p = %.4f\n",
            exp(meta_os$TE.common), exp(meta_os$lower.common),
            exp(meta_os$upper.common), meta_os$pval.common))

cat("\n===============================================================\n")
cat("  ALL DONE - 10 PDFs saved to:\n  ", out_dir, "\n")
cat("===============================================================\n")