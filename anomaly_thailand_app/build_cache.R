# Builds app_cache/ (startup bundle + 4/520-week Compute results). Does not launch Shiny.
# Run from the app directory: Rscript --vanilla build_cache.R  (or use run_build_cache.R)
# Stop the app first, then restart it after the build.
if (!file.exists("setup.R")) stop("Run from the directory containing setup.R")
options(malaria.build_cache = TRUE)
withCallingHandlers(
  source("setup.R", local = .GlobalEnv),
  error = function(e) {
    calls <- tail(sys.calls(), 20L)
    labels <- vapply(calls, function(call) {
      head <- call[[1L]]
      if (is.symbol(head)) as.character(head) else "<function>"
    }, character(1))
    message("[build] ", conditionMessage(e), "\n[build] Calls: ",
            paste(labels, collapse = " -> "))
  }
)

build_compute_cache <- function(default_result) {
  dir.create(file.path("app_cache", "compute"), showWarnings = FALSE, recursive = TRUE)
  failures <- character()
  for (weeks in c(4, 520)) {
    for (species in .species_choices) {
      for (method in .method_choices) {
        settings <- list(weeks = weeks, species = species, method = method, date = .cache_defaults$date)
        if (!is.null(read_compute(settings))) next
        label <- paste(weeks, species, method, sep = " / ")
        message("Building ", label)
        tryCatch({
          result <- if (identical(settings, .cache_defaults)) default_result else compute_result(settings)
          stopifnot(valid_result(result))
          path <- compute_path(settings)
          tmp <- tempfile(tmpdir = dirname(path))
          tryCatch({
            saveRDS(list(signature = .signature, settings = settings, result = result), tmp)
            stopifnot(identical(readRDS(tmp)$result, result))
            # On Windows a stale target must be removed before rename.
            if (file.exists(path) && !file.remove(path)) stop("Cannot replace ", path)
            if (!file.rename(tmp, path)) stop("Cannot publish ", path)
          }, finally = unlink(tmp))
        }, error = function(e) {
          failures <<- c(failures, paste(label, conditionMessage(e), sep = " -- "))
          message(tail(failures, 1))
        })
      }
    }
  }
  stopifnot(identical(.signature, cache_signature()))
  if (length(failures)) stop("Incomplete Compute cache:\n", paste(failures, collapse = "\n"))
  message("Compute cache ready: 54/54 selections")
}

message("Building persistent cache; this step can take time and does not launch Shiny.")
message("[build] Default Compute")
default_result <- compute_result(.cache_defaults)
# Validate results before storing: never persist a failed or empty computation.
stopifnot(is.data.frame(default_result), nrow(default_result) > 0L,
          all(c("province", "status", "SIR", "cases") %in% names(default_result)),
          !anyDuplicated(default_result$province),
          all(default_result$status %in% c(0, 1)))
message("[build] Preparing map geometry")
geometry <- get_thai_geometry()
default_map <- attach_map_values(default_result, geometry)
message("[build] Preparing weekly summary and map grobs")
bundle <- list(signature = .signature,
               data = mget(.cache_objects, envir = .GlobalEnv),
               geometry = geometry, weekly_summary = get_weekly_summary(),
               weekly_cases = get_weekly_cases(), default_result = default_result,
               anomaly_grob = ggplot2::ggplotGrob(make_map_plot(default_map, "anomaly")),
               sir_grob = ggplot2::ggplotGrob(make_map_plot(default_map, "sir")))
# Do not save a mixed snapshot if an input changed during the build.
stopifnot(identical(.signature, cache_signature()))
dir.create(dirname(.cache_path), recursive = TRUE, showWarnings = FALSE)
temporary <- tempfile("startup_", tmpdir = dirname(.cache_path), fileext = ".rds")
# Uncompressed RDS trades disk space for faster startup reads.
message("[build] Saving and checking startup cache")
saveRDS(bundle, temporary, compress = FALSE)
restored <- readRDS(temporary)
stopifnot(identical(restored$signature, bundle$signature),
          isTRUE(all.equal(restored$default_result, default_result)),
          isTRUE(all.equal(restored$data$total_data, total_data)))
# Publish only after successful serialization checks (app must be stopped).
if (!file.copy(temporary, .cache_path, overwrite = TRUE)) {
  stop("Could not publish cache: ", .cache_path)
}
unlink(temporary)
build_compute_cache(default_result)
message("Cache built: ", normalizePath(.cache_path),
        "\nRestart R, then run shiny::runApp().")
