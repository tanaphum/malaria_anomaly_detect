# Shared runtime for app.R and build_cache.R: packages, settings, data loading,
# startup-cache reading, algorithms and helpers used by both.
# Changing this file (or packages.R, load_data.R, ano_algo_fxns.R) invalidates the cache.
# UTC matches the deployed shinyapps.io server; date windows use as.Date(POSIXct),
# so Asia/Bangkok shifts every window back one day and changes results.
Sys.setenv(TZ = "Asia/Bangkok")
.cache_build <- isTRUE(getOption("malaria.build_cache", FALSE))
if (.cache_build) message("[build] Loading packages")
source("packages.R", echo = .cache_build)
.cache_path <- file.path("app_cache", "startup_4weeks.rds")
.cache_defaults <- list(weeks = 4, species = "Vivax",
                       method = "Baseline: Weekly Three Year Median",
                       date = "2022-05-25")
.species_choices <- c(
  "Vivax",
  "Falciparum",
  "All"
)
.method_choices <- c(
  "Baseline: Weekly Three Year Median",
  "Statistical Profiling",
  "Predictive Confidence Interval",
  "Historical Average",
  "Weekly Case Previous Year",
  "Monthly Case Four Years",
  "Unsupervised Clustering",
  "Density-Based Profiling",
  "Density-Based Profiling with Temp and Precip"
)
.cache_objects <- c("total_data", "prov_name", "thaimap", "temp_data_long",
                    "precip_data_long", "approx_daily_temp", "approx_daily_precip")

cache_signature <- function() {
  scripts <- c("setup.R", "packages.R", "load_data.R", "ano_algo_fxns.R")
  inputs <- c("unfilt_data.csv", "gadm36_THA_1_sp.rds",
              "tas_timeseries_monthly_cru_1901-2021_THA.csv",
              "pr_timeseries_monthly_cru_1901-2021_THA.csv",
              "sector_01_11201_EN_.csv")
  missing_files <- c(scripts, inputs)[!file.exists(c(scripts, inputs))]
  if (length(missing_files)) {
    stop("Missing input files: ", paste(missing_files, collapse = ", "))
  }
  # Content hashes also work when deployment changes file timestamps.
  # This reads source bytes but avoids CSV parsing and data preparation.
  packages <- c("sf", "ggplot2", "dplyr", "plyr", "lubridate", "zoo", "tidyr", "readr",
                "dtwclust", "fpc", "forecast", "data.table", "sp")
  list(schema = 1L, defaults = .cache_defaults,
       scripts = tools::md5sum(scripts),
       inputs = tools::md5sum(inputs),
       R = as.character(getRversion()), timezone = Sys.timezone(),
       packages = vapply(packages, function(x) as.character(utils::packageVersion(x)), ""))
}

# The original algorithm file sources load_data.R and packages.R at top level.
# Load their contents only once, retaining all algorithm definitions unchanged.
source_without_bootstrap <- function(file, envir = .GlobalEnv) {
  expressions <- parse(file = file, keep.source = TRUE)
  for (expression in expressions) {
    duplicate_source <- is.call(expression) &&
      identical(expression[[1L]], as.name("source")) &&
      length(expression) >= 2L && is.character(expression[[2L]]) &&
      length(expression[[2L]]) == 1L &&
      expression[[2L]] %in% c("load_data.R", "packages.R")
    if (!duplicate_source) eval(expression, envir = envir)
  }
  invisible(NULL)
}

if (.cache_build) message("[build] Checking input files and package versions")
.signature <- cache_signature()
.disk_cache <- NULL
if (!.cache_build && file.exists(.cache_path)) {
  candidate <- tryCatch(
    readRDS(.cache_path),
    error = function(e) {
      message("Cache read failed: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.list(candidate) && !is.null(candidate$signature)) {
    fields <- union(names(candidate$signature), names(.signature))
    different <- fields[vapply(fields, function(key) {
      !identical(candidate$signature[[key]], .signature[[key]])
    }, logical(1))]
    
    if (length(different)) message("Cache signature mismatch: ",
            paste(different, collapse = ", "))
  }
  required <- c("signature", "data", "geometry", "weekly_summary", "weekly_cases",
                "default_result", "anomaly_grob", "sir_grob")
  if (is.list(candidate) && all(required %in% names(candidate)) &&
      identical(candidate$signature, .signature) &&
      all(.cache_objects %in% names(candidate$data))) {
    .disk_cache <- candidate
    message("Startup cache: HIT (data, Summary and default four-week maps).")
  } else {
    message("Startup cache: stale or unreadable; loading original data. Rebuild cache.")
  }
  rm(candidate)
}
if (is.null(.disk_cache)) {
  if (.cache_build) message("[build] Loading load_data.R")
  source_without_bootstrap("load_data.R")
  message("Startup data: prepared from source files (once).")
} else {
  list2env(.disk_cache$data, envir = .GlobalEnv)
}
if (.cache_build) message("[build] Loading ano_algo_fxns.R")
source_without_bootstrap("ano_algo_fxns.R")

# Keep the omitted species argument for All: find_anomaly uses missing(species).
compute_result <- function(settings) {
  period <- lubridate::weeks(settings$weeks)
  end <- as.POSIXct(settings$date, tz = "UTC")
  pop <- SIR_EV(total_data, period, end)
  if (settings$species == "All") {
    return(find_anomaly(period, end, settings$method, pop, temp_data_long, precip_data_long))
  }
  code <- switch(settings$species, Vivax = "V", Falciparum = "F", stop("Unknown species"))
  find_anomaly(period, end, settings$method, pop, temp_data_long, precip_data_long, code)
}

compute_path <- function(settings) {
  file.path("app_cache", "compute", sprintf("%s_%03d_%s_%02d.rds",
    settings$date, settings$weeks, settings$species, match(settings$method, .method_choices)))
}

valid_result <- function(result) {
  is.data.frame(result) &&
    all(c("province", "status", "cases", "SIR") %in% names(result)) &&
    nrow(result) == length(prov_name) && !anyDuplicated(result$province) &&
    all(result$status %in% 0:1)
}

read_compute <- function(settings) {
  if (!settings$weeks %in% c(4, 520)) return(NULL)
  path <- compute_path(settings)
  if (!file.exists(path)) return(NULL)
  entry <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.list(entry) && identical(entry$signature, .signature) &&
      identical(entry$settings, settings) && valid_result(entry$result)) return(entry$result)
  NULL
}


# Identical plot definitions for building and displaying the cached maps.
make_map_plot <- function(geometry, kind = c("anomaly", "sir")) {
  kind <- match.arg(kind)
  if (kind == "anomaly") {
    plot <- ggplot2::ggplot(geometry) +
      ggplot2::geom_sf(ggplot2::aes(fill = status), colour = NA) +
      ggplot2::scale_fill_manual(
        values = c("0" = "dodgerblue", "1" = "brown1"), breaks = c("0", "1"),
        labels = c("No Anomaly Detected", "Anomaly Detected"),
        drop = FALSE, name = "Status")
  } else {
    plot <- ggplot2::ggplot(geometry) +
      ggplot2::geom_sf(ggplot2::aes(fill = SIR), colour = NA)
  }
  plot + ggplot2::coord_sf(crs = 4326, default_crs = 4326,
                          xlim = c(96, 106), ylim = c(6, 20.5)) +
    ggplot2::labs(x = "longitude", y = "latitude", title = "") +
    ggplot2::theme_minimal()
}

attach_map_values <- function(result, geometry) {
  unmatched <- setdiff(as.character(result$province), as.character(geometry$NAME_1))
  if (length(unmatched)) stop("Unmatched provinces: ", paste(unmatched, collapse = ", "))
  index <- match(as.character(geometry$NAME_1), as.character(result$province))
  geometry$status <- factor(result$status[index], levels = c(0, 1))
  geometry$SIR <- result$SIR[index]
  geometry
}

# Performance: immutable source data are loaded once per R process above.
# Cache successful results only; restart the app after changing source data.
# Keep expensive Summary work out of server() startup and the About page.
get_weekly_summary <- local({
  cached <- if (!is.null(.disk_cache)) .disk_cache$weekly_summary else NULL
  function() {
    if (is.null(cached)) {
      end_date <- as.POSIXct("2022-05-25", tz = "UTC")
      result <- find_anomaly(lubridate::weeks(1), end_date, "Historical Average")
      cached <<- result
    }
    cached
  }
})

get_weekly_cases <- local({
  cached <- if (!is.null(.disk_cache)) .disk_cache$weekly_cases else NULL
  function() {
    if (is.null(cached)) {
      end_date <- as.POSIXct("2022-05-25", tz = "UTC")
      cached <<- filter_data(total_data, end_date - lubridate::weeks(1), end_date)
    }
    cached
  }
})

# Preserve the complete original boundaries (including polygon holes).
# Convert once, lazily, rather than fortifying on every plot redraw.
get_thai_geometry <- local({
  cached <- if (!is.null(.disk_cache)) .disk_cache$geometry else NULL
  function() {
    if (is.null(cached)) {
      geometry <- sf::st_as_sf(thaimap)
      stopifnot("NAME_1" %in% names(geometry))
      if (is.na(sf::st_crs(geometry))) {
        stop("The Thai map has no CRS. Set its verified source CRS before plotting.")
      }
      cached <<- geometry[, "NAME_1", drop = FALSE]
    }
    cached
  }
})
