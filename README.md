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

## Related repos

- [miami-pitcher-controllables-dashboard](https://github.com/WillPaz16/miami-pitcher-controllables-dashboard) — deployed web app for pitcher controllables (xBABIP model), kept separate since it has its own deploy pipeline.

## Data

Raw and cleaned Trackman CSVs are not tracked in this repo (see `.gitignore`) — regenerate them locally by running `r/preprocessing.Rmd` or the scripts in `scripts/trackman-extract/` against raw exports.
