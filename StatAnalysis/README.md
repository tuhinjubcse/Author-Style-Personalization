# Reproducibility bundle — Statistics behind Figures 2–4

This repo provides the **exact analysis scripts and outputs** that back the figures 2-4.

- `Fig2.html` 
- `Fig3.html` 
- `Fig4.html`


# Build the analysis tables (run these in order)
Rscript 01_build_data.R
Rscript 02_fit_fig2_models.R
Rscript 03_detectability_H3.R
Rscript 04_stylometric_mediation_fig4A_B.R
Rscript 05_author_rates_fig3A_B.R
Rscript 06_costing_fig4C.R

Finally run verify_figures.py. It will verify whether the results saved by the above files match the frozen results in the html figures and in the submitted manuscript.

