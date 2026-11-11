# Open this file in RStudio/Positron and click Source to rebuild app_cache/.
# Runs build_cache.R in a fresh R process so the result does not depend on this session.
# Rebuild after changing data, packages.R, load_data.R, ano_algo_fxns.R, setup.R or the R/package versions.
app_dir <- "D:/Work/2027/malaria_Oraya-20261104T041006Z-1-001/deploy"

rscript <- file.path(R.home("bin"), "Rscript.exe")
stopifnot(file.exists(rscript), file.exists(file.path(app_dir, "build_cache.R")))


status <- system2(rscript, c("--vanilla", "build_cache.R"))
system2(rscript, c("--vanilla", "build_cache.R"))

if (!identical(status, 0L)) stop("Cache build failed (exit status ", status, "). See messages above.")
message("Cache build finished. Restart the app.")
