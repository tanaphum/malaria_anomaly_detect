# Unqualified calls in the current app, load_data.R and ano_algo_fxns.R.
library(shiny)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(zoo)
library(forecast)
library(dtwclust)
# The existing density methods use fpc::dbscan (capital MinPts).
library(fpc)
library(lubridate)
library(plyr)
library(readr)
library(DT)
# Register the spatial classes used by the original province map RDS.
loadNamespace("sp")
