library(rsconnect)

app_dir <- "D:/Work/2027/malaria_Oraya-20261104T041006Z-1-001/deploy"

# ไฟล์ทั้งหมดที่ต้องการ Deploy
deploy_files <- c(
  "app.R",
  "packages.R",
  "load_data.R",
  "ano_algo_fxns.R",
  "setup.R",
  "gadm36_THA_1_sp.rds",
  "unfilt_data.csv",
  "outbreak_malaria_th_5.csv",
  "sector_01_11201_EN_.csv",
  "tas_timeseries_monthly_cru_1901-2021_THA.csv",
  "pr_timeseries_monthly_cru_1901-2021_THA.csv",
  list.files(
    "app_cache",
    recursive = TRUE,
    full.names = TRUE
  )
)

deploy_files <- unique(deploy_files)

# ตรวจสอบไฟล์ที่ขาด
missing <- deploy_files[!file.exists(deploy_files)]
stopifnot(length(missing) == 0)

# ตรวจสอบ Cache
print(grep("^app_cache/", deploy_files, value = TRUE))

# Deploy
rsconnect::deployApp(
  appDir = app_dir,
  appFiles = deploy_files,
  appName = "Oraya_malaria_Warning_2"
)
