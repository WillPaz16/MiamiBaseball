# Miami Baseball Analytics

Trackman-based pitching/hitting analytics for Miami University Baseball: data cleaning, scouting reports, a Shiny dashboard, and exploratory Python analysis.

## Structure

```
r/
  preprocessing.Rmd        # Cleans raw Trackman CSVs into cleanedPitcherGames.csv / cleanedBatterGames.csv
  fallDataCleaning.R       # Fall-season data cleaning
  games2025.R              # 2025 season game processing
  reports/                 # Per-player report generators (pitcher, hitter, catcher, umpire, Scully)
  shiny/                   # Interactive Shiny dashboard (shinyPitchers.R, shinyBatters.R)

python/
  analysis/                # Player-specific analysis (Ahmad comparison notebook, helpers)
  preprocessing/           # Percentile preprocessing for the pitcher-controllables model

scripts/
  trackman-extract/        # Scripts to pull/concatenate/filter raw Trackman exports
  riley/                   # Riley's scripts (Stuff+, xBA, bullpen, pre-season reports)
  experimental/            # One-off analyses (VAA, cutoffs, PCA) with their small input CSVs

sample-reports/            # Example rendered report images
```

## Setup

### R

Requires R with:

```r
install.packages(c("tidyverse", "readr", "lubridate", "DT", "patchwork",
                    "gridExtra", "grid", "kableExtra", "pander", "knitr",
                    "plyr", "shiny", "rsconnect"))

# sportyR (strike zone plotting) is not on CRAN
install.packages("remotes")
remotes::install_github("sportsdataverse/sportyR")
```

### Python

Requires Python 3 with `pandas`, `numpy`, `matplotlib`, `scipy`, and `catboost` (used by `python/preprocessing/preprocess_percentiles.py` and the Ahmad analysis notebook).

## Usage

**Cleaning raw data:** point `r/preprocessing.Rmd` at a directory of raw Trackman game CSVs (see `scripts/trackman-extract/` for pulling/filtering exports) and knit it. It writes `cleanedPitcherGames.csv` / `cleanedBatterGames.csv`.

**Generating a scouting report:** run the relevant script in `r/reports/` (e.g. `pitcherReports.R`) against a cleaned CSV; each writes report images like the ones in `sample-reports/`.

**Running the Shiny dashboard locally:** open `r/shiny/shinyPitchers.R` or `shinyBatters.R` in RStudio and click Run App. Each script currently expects its cleaned CSV at a hardcoded path (`~/Miami/Miami Baseball/ShinyApps/cleaned{Pitcher,Batter}Games.csv`) — update that `read.csv()` call to point at your local cleaned data before running.

## Related repos

- [miami-pitcher-controllables](https://github.com/WillPaz16/miami-pitcher-controllables) — deployed web app for pitcher controllables (xBABIP model), kept separate since it has its own deploy pipeline.

## Data

Raw and cleaned Trackman CSVs are not tracked in this repo (see `.gitignore`) — regenerate them locally by running `r/preprocessing.Rmd` or the scripts in `scripts/trackman-extract/` against raw exports.
