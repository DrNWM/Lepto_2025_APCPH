# Security & Data Protection

Reproducible analysis of anonymised 2025 Leptospirosis surveillance data from Johor, Malaysia.

## Data Protection

### Anonymisation ✅
- Case IDs only (CASE_XXXX format)
- Zero PII: No names, ID numbers, dates, contact info, or medical record numbers
- 881 anonymised cases, district-level analysis
- Includes: Age, sex, ethnicity, district, laboratory results, outcomes

## Ethics & Governance

- **Source:** Ministry of Health Malaysia (CDCIS e-Notifikasi)
- **Ethics Approval:** NMRR-26-01388-O7V
- **Governance:** MOH, Johor State Health Department
- **Principles:** FAIR data, transparent, reproducible methodology

## Data Access

**In this repository:**
- Analysis code (`_targets.R`)
- Anonymised case-level data (`data/clean/lepto_2025_clean.rds`)
- Summary statistics & figures (`outputs/`)

**To request raw CDCIS data:**
- Contact: Ministry of Health Malaysia (KKM)
- Portal: http://enotifikasi.moh.gov.my
- Requirements: Research justification, institutional affiliation, ethics approval

**Reproducibility:** Researchers with approved data access can run `targets::tar_make()` to regenerate all analyses.

## Security Issues

Report privately to **drngweimeng@gmail.com** (reference: "Security Report - Lepto 2025"). Do not open public GitHub issues.

## Code Security

**Dependencies:** tidyverse, targets, ggplot2, rstatix, pROC, gtsummary, gt (managed via renv)

**No secrets:** No API keys, passwords, credentials, or database strings in repository

## Public Health Context

**Purpose:** Understand leptospirosis burden and risk factors across Johor districts for evidence-based interventions.

**Limitations:** Single-year data (2025), district-level ecological analysis, small mortality sample (n=38). Multi-year surveillance needed for trend confirmation.

**Key Findings** (mortality is the primary outcome throughout):
- Mersing & Kluang: Highest Case Fatality Rate → clinical training / referral pathway priority
- Rural and Urban-Rural categories: Higher adjusted odds of death vs City Centre, even after accounting for age, sex, and district incidence rate
- Segamat: Highest incidence rate (background/descriptive context, not a mortality finding) → still relevant for vector control planning

## Contact & References

📧 **drngweimeng@gmail.com** | 🔗 linkedin/n/drngweimeng | 💻 github.com/DrNWM

- CDCIS: http://enotifikasi.moh.gov.my
- FAIR Data: https://www.go-fair.org/fair-principles/
