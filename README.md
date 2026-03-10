# ECON 687 — Replication Project

**Paper:** Mogstad, Romano, Shaikh, and Wilhelm. "Statistical Uncertainty in the Ranking of Journals and Universities." *AEA Papers and Proceedings*, Vol. 112, May 2022, pp. 626–631.

**Authors:** Jason Jankoski, Alan Lamb, Linna Wang

---

## Repository Structure

```
econ687-replication/
├── data/
│   ├── journal IF/        # Journal impact factor data (Stern, 2013)
│   └── Repec/             # University publication data (Zimmermann, 2013)
├── code/
│   └── 00_data_check.R    # Data diagnostic and proof-of-concept figures
├── output/                # Generated figures and tables (not tracked by Git)
├── report/                # Final report (R Markdown)
└── README.md
```

## Getting Started

1. Clone this repository
2. Open `econ687-replication.Rproj` in RStudio
3. Set working directory to the repo root: `Session > Set Working Directory > To Project Directory`
4. Install required packages if needed (the script will prompt you):
   - `ggplot2`, `dplyr`, `readxl`, `devtools`
   - `csranks` from GitHub: `devtools::install_github("danielwilhelm/R-CS-ranks")`
5. Run `code/00_data_check.R` to confirm data loads and generate proof-of-concept figures

## Data

Both datasets are included in this repository. No external data access is required.

- **Journal IF data:** `data/journal IF/JEL_Data.xlsx` — article-level citation counts for 232 economics journals (Stern, 2013)
- **University data:** `data/Repec/top100unis.csv` — average impact factors for 100 universities derived from RePEC (Zimmermann, 2013)

## Requirements

- R 4.1.2 or later
- RTools (Windows users): https://cran.r-project.org/bin/windows/Rtools/
- Estimated runtime: ~12 minutes on a standard laptop
