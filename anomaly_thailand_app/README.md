# Web
https://tanaphumw.shinyapps.io/Oraya_malaria_Warning_2/

# Building the Cache for the Malaria Anomaly Detection App

The cache makes the app start fast and show results instantly when 4 or 520 weeks is selected (every species × every method, 54 selections in total).
Without the cache the app still works, but it recomputes everything each time, which is much slower.

## File structure

| File | Purpose |
|---|---|
| `app.R` | UI, server, and functions used only by the UI/server |
| `setup.R` | Code shared by the app and the cache builder: packages, defaults, data loading, cache reading, map-drawing functions |
| `build_cache.R` | Builds the cache (does not launch the app) |
| `run_build_cache.R` | Click-to-run file for RStudio/Positron that calls `build_cache.R` |
| `ano_algo_fxns.R`, `load_data.R`, `packages.R` | Algorithms, data preparation, and packages |
| `app_cache/` | Cache output (must be deployed together with the app) |

## How to build the cache

1. Close the app if it is running.
2. Choose one of the following:
   - **RStudio/Positron:** open `run_build_cache.R` and click **Source**
     (if you move the folder, update `app_dir` in this file to point to the new location)
   - **Terminal:** go to this folder and run
     ```
     Rscript --vanilla build_cache.R
     ```
3. Wait until you see `Compute cache ready: 54/54 selections` and `Cache built: ...`
4. Restart the app (`shiny::runApp()`). The console should show
   `Startup cache: HIT (data, Summary and default four-week maps).`

Approximate time (measured on the dev machine): first build ~6.5 minutes, re-run when the cache is still valid ~30 seconds.

If the build stops partway, just run it again. Selections that already finished are skipped.

## When to rebuild

The cache carries a signature, and the app will not use a cache whose signature does not match. Rebuild when any of the following change:

- Data files: `unfilt_data.csv`, `gadm36_THA_1_sp.rds`, temperature/rainfall files, `sector_01_11201_EN_.csv`
- Code: `setup.R`, `packages.R`, `load_data.R`, `ano_algo_fxns.R`
- R or package versions (sf, ggplot2, dplyr, plyr, lubridate, zoo, tidyr, readr, dtwclust, fpc, forecast, data.table, sp)
- The machine's timezone

Editing only `app.R` (UI/server) does **not** require a rebuild.

If the app shows `Startup cache: stale or unreadable` or `Cache signature mismatch: ...` on startup, the cache needs rebuilding. The message also tells you which part does not match.

## Caveats

- **Build the cache with the same R and package versions as the deployment server.** Otherwise the signature will not match and the server will not use the cache.
- `setup.R` sets `TZ = "UTC"` to match shinyapps.io. Do not change it to `Asia/Bangkok`: every method's date ranges would shift by one day and the results would change.
- When deploying, include the `app_cache/` folder.
