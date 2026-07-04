# Lepto Johor 2025 — Analysis Pipeline

Reproducible analysis of 2025 data with targets workflow.

---

## Quick Start

```r
# First time: install packages
source(here::here("00_setup_reproducibility.R"))

# Run analysis (2-3 minutes)
targets::tar_make()
```

**Outputs:** 6 figures + 9 tables saved to `outputs/` folder

---

## What's Inside

- `_targets.R` — Analysis pipeline (one-line execution)
- `data/clean/` — Anonymised data (case IDs only)
- `outputs/` — Figures & summary statistics
- `renv/` — Reproducible package environment

---

## Analyses

Death is the single outcome throughout — every figure and test builds toward it.

1. **Descriptive** — Cases by district (split by outcome, ranked by CFR), age by outcome, sex by outcome, urbanisation category by outcome
2. **Ecological (descriptive only)** — Incidence Rate (IR) by urbanisation category, district-level (n=10)
3. **Univariate** — Crude/single-predictor screens vs death: chi-square (sex), univariate logistic regression (age, IR, urbanisation category)
4. **Regression** — Binary logistic models: crude (`death ~ ur_category`) and adjusted (`death ~ ur_category + incidence_rate + age_years + sex`) — district IR enters as a contextual covariate, not a separate outcome
5. **Evaluation** — ROC curves, model comparison (AIC, calibration, AUC)

---

## Dependencies

- R 4.0+
- Packages auto-installed via `00_setup_reproducibility.R`
- Uses renv for version control

---

## Quick Commands

```r
targets::tar_status()          # Check pipeline status
targets::tar_visnetwork()      # View dependencies
targets::tar_make(names = "fig1_district_outcome")  # Run one target
```

---

## Data

- **Period:** 2025 only
- **Anonymisation:** Case IDs only—zero PII
- **Source:** CDCIS e-Notifikasi

---

## Customise

Edit `_targets.R` to change:
- Model formulas (search `model_crude` / `model_adjusted`)
- Colour schemes (`ur_colours` / `outcome_colours`, near the top)
- Figure sizes (search `ggsave`)

Then re-run: `targets::tar_make()`

---

## Issues?

| Problem | Fix |
|---------|-----|
| Packages fail to install | Run `00_setup_reproducibility.R` |
| Data not found | Check `data/clean/lepto_2025_clean.rds` exists |
| Figures missing | Run `tar_status()` to diagnose |

---

**Ready to run. See outputs/ for results.**
