# 5-ALA Fluorescence-Guided Surgery vs White Light in Newly Diagnosed Glioblastoma

## Systematic Review & Meta-Analysis

This repository contains the R code for a systematic review and meta-analysis comparing **5-aminolevulinic acid (5-ALA) fluorescence-guided surgery** versus **conventional white-light surgery** in patients with newly diagnosed glioblastoma (GBM).

## Study Overview

- **Primary Outcome:** Gross Total Resection (GTR) rates
- **Secondary Outcome:** Overall Survival (OS)
- **Studies Included:** 7 studies (2 RCTs, 5 observational) comprising 1,269 patients
- **Statistical Methods:** Random-effects and fixed-effect meta-analysis using Risk Ratio, Odds Ratio, Risk Difference, and Hazard Ratio

## Included Studies

| Study | Year | Design |
|-------|------|--------|
| Stummer et al. | 2006 | RCT |
| Roder et al. | 2014 | Observational |
| Kim et al. | 2014 | Observational |
| Mirza et al. | 2021 | Observational |
| Wong et al. | 2023 | Observational |
| Picart et al. | 2024 | RCT |
| Ryskelddiyev et al. | 2025 | Observational |

## Generated Figures

| File | Description |
|------|-------------|
| `Figure3_GTR_RR_forest.pdf` | GTR Risk Ratio forest plot (random-effects) |
| `Figure4_GTR_RR_subgroup.pdf` | Subgroup analysis by study design (RCT vs Observational) |
| `Figure5_LeaveOneOut.pdf` | Leave-one-out sensitivity analysis |
| `Figure6_OS_HR_forest.pdf` | Overall Survival Hazard Ratio forest plot |
| `Figure7_Median_OS_bar.pdf` | Median Overall Survival bar chart |
| `FigureS1_GTR_OR_forest.pdf` | GTR Odds Ratio forest plot (supplementary) |
| `FigureS2_GTR_RD_forest.pdf` | GTR Risk Difference forest plot + NNT (supplementary) |
| `FigureS3_Sensitivity_RoB.pdf` | Sensitivity analysis — low/moderate risk-of-bias studies only |
| `FigureS4_GTR_RR_fixed.pdf` | GTR Risk Ratio fixed-effect model (supplementary) |
| `FigureS5_GTR_rates_bar.pdf` | GTR rates bar chart by study (supplementary) |

## Requirements

- **R** (>= 4.0)
- R packages: `meta`, `metafor` (auto-installed if missing)

## Usage

1. Open `5ALA_vs_WhiteLight_MetaAnalysis.r` in RStudio
2. Select All (`Ctrl+A`) then Run (`Ctrl+Enter`)
3. All PDF figures are saved to the `Figures/` folder in the working directory

## License

This project is for academic and research purposes. Originally done by DR Haider, all rights reserved to Dr. Haider Randhawa
