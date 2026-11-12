source("load_data.R")
source("packages.R")

# Split by province once per process; every per-province filter below then scans only that province.
data_by_prov <- split(total_data, total_data$p_site_province_name)
# NA/unknown province falls back to the full data, so filter_data(%in%) behaves exactly as before.
get_prov_data <- function(p) {
  if (!is.na(p) && !is.null(data_by_prov[[p]])) data_by_prov[[p]] else total_data
}


#function to filter data based input parameters. Included are the start date, end date, province, border type, and species ( user input parameters)
filter_data <- function(data,start_date,end_date,province_filter_values = NULL, border_filter_values = NULL, species = NULL) {
  #set the end of the filtering dates based on year. 2022-05-25 is the end of the data set
  if (end_date == 2022){
    end = as.Date(paste(end_date, 5, 25, sep = "-"))
  }else{
    end = as.Date(paste(end_date, 12, 31, sep = "-")) # end of year
  }
  start = as.Date(paste(start_date, 1, 1, sep = "-")) # beginning of year
  
  #filter the data depending on species
  if(missing(species)) data <- data %>% filter(blood_draw_date >= start) %>% filter(blood_draw_date <= end)
  else data <- data %>% filter(blood_draw_date >= start) %>% filter(blood_draw_date <= end) %>% filter(result_code_detail_1 %in% species)
  
  #selects the parameters of interest and filter based on province and border type 
  if (missing(province_filter_values) && missing(border_filter_values)) data <- data %>% dplyr::select(blood_draw_date,p_site_province_id, org_type,result_code_detail_1,type_border,type_patient, p_site_district_name, p_site_subdistrict_name, p_site_mu_name, p_site_village_name,ep1_ep3_case_classification,p_site_province_name)
  else if (missing(province_filter_values)) data <- data %>% dplyr::select(blood_draw_date,p_site_province_id, org_type,result_code_detail_1,type_border,type_patient, p_site_district_name, p_site_subdistrict_name, p_site_mu_name, p_site_village_name,ep1_ep3_case_classification,p_site_province_name) %>% filter(type_border %in% border_filter_values)
  else if (missing(border_filter_values)) data <- data %>% dplyr::select(blood_draw_date,p_site_province_id, org_type,result_code_detail_1,type_border,type_patient, p_site_district_name, p_site_subdistrict_name, p_site_mu_name, p_site_village_name,ep1_ep3_case_classification,p_site_province_name) %>% filter(p_site_province_name %in% province_filter_values)
  else data <- data %>% dplyr::select(blood_draw_date,p_site_province_id, org_type,result_code_detail_1,type_border,type_patient, p_site_district_name, p_site_subdistrict_name, p_site_mu_name, p_site_village_name,ep1_ep3_case_classification,p_site_province_name) %>% filter(type_border %in% border_filter_values && p_site_province_name %in% province_filter_values)
  
  return(data)
}

#finds anomalies for all provinces depending the user input
find_anomaly <- function(prev_time,latest_date, method, province_pop = NULL,temp_data_long, precip_data_long, species = NULL) {
  
  #converts the province population into a data frame 
  province_population <- data.frame(province_pop)
  prev_date <- latest_date - prev_time
  
  #get the sequence of dates for the time frame selected by the user
  dates_filt <- seq(prev_date, latest_date, by="days")

  #create empty data frame to store anomaly detection output
  ano_df <- as.data.frame(matrix(0, nrow = length(dates_filt),ncol = length(prov_name)+1))
  colnames(ano_df) <- c("dates",prov_name)

  ano_df$dates = dates_filt
  
  #create empty data frame to store the case information for the sparkline widget
  case_df <- as.data.frame(matrix(0, nrow = length(dates_filt),ncol = length(prov_name)+1))
  colnames(case_df) <- c("dates",prov_name)
  case_df$dates = dates_filt

  for(i in 1:length(prov_name)){
  #for(i in 1:20){
    
    #filter for species testing
    d_prov <- get_prov_data(prov_name[i])
    if(missing(species)) data <- filter_data(d_prov,prev_date,latest_date,province_filter_values = prov_name[i])
    else data <- filter_data(d_prov,prev_date,latest_date,province_filter_values = prov_name[i], species = species)
    #using the x time from previous date

    #total cases within the selected time frame
    total_cases <- nrow(data)
    province_population$cases[province_population$prov_name == prov_name[i]] <- total_cases
    
    #check if there is still case information after filtering the data based on user input
    if(nrow(data) > 0){ 
      
      #find the anomlies depending on method defined by the user
      df_comp <- ano_algo(method,data, d_prov, prev_date,latest_date, prov_name[i],temp_data_long, precip_data_long)

      #match the values for activity in the results based on the dates from the final data frame and result data frame.
      ano_df[i+1] <- df_comp$activity[match(as.Date(ano_df$dates),as.Date(df_comp$blood_draw_date))]
      
      #add case information to the sparkline data frame
      case_df[i+1] <- df_comp$cases[match(as.Date(case_df$dates),as.Date(df_comp$blood_draw_date))]
      
      
    }else{ 
      #if no cases reported, this means anomalies are reported 
      ano_df[i+1] = 0
      
      #if no cases reported, this means no cases are reported 
      case_df[i+1] = 0
      
    }

  }
  
  #calculate the provincial standardised incidence ratio
  province_population$SIR <- province_population$cases/province_population$ev
  
  rownames(ano_df) <- ano_df$dates
  ano_df$dates = NULL
  #transform the anomaly data frame to make each province their own column
  ano_df_transformed <- as.data.frame(t(ano_df))
  
  #check if anomalies have been alerted for each province and return 1 if anomalies have been identified
  ano_act <- data.frame(apply(ano_df_transformed, 1, function(r) any(r %in% c("High Anomalous Activity","Anomalous Activity Detected"))))
  ano_act$province <- rownames(ano_act)
  
  #rename the columns and convert the status to numeric form
  colnames(ano_act) <- c("status","province")
  ano_act$status <- as.numeric(ano_act$status)
  
  #match the standardised incidence ratio to the province
  ano_act$SIR <- province_population$SIR[match(ano_act$province, province_population$prov_name)]
  
  #create matrix for cases for sparkline widget
  ano_act$cases <- matrix(0, nrow = nrow(ano_act))

  if(exists("case_df") == TRUE){
    for(i in 1:nrow(ano_act)){
      #create and reformat sparkline data to match the format for sparkline widger
      prov <- ano_act$province[i]
      spark_dat <- case_df[,prov]
      spark_dat[is.na(spark_dat)] <- 0 
      ano_act$cases[i] <- toString(spark_dat)
    }
  }else{
    print("test")
  }
  
  return(ano_act)
}


#function for statistical profiling method 
stat_profile <- function(data) {
  #convert data into case counts per read 
  cases <- dplyr::count(data, blood_draw_date)
  #calculate the rolling mean using 7 days time frame 
  moving_average <- zoo::rollmean(cases$n, k = 7, na.pad = TRUE)
  df = data.frame(moving_average)
  
  cases$mov_avg = df$moving_average
  
  #calculate the standard deviation
  cases$sd1 = sd(cases$mov_avg, na.rm=TRUE)
  cases$sd2 = 2*cases$sd1
  cases$sd3 = 3*cases$sd1
  
  cases <- data.frame(cases)
  
  #define cases above the mean plus 3 standard deviations as highly anomalous
  cases$activity = "Medium Anomalous Activity"
  cases$activity[cases$n > cases$mov_avg + cases$sd3] <- "High Anomalous Activity"
  cases$activity[cases$n < cases$mov_avg + cases$sd2] <- "Low Anomalous Activity"
  
  names(cases)[names(cases) == "n"] <- "cases"
 
  return(cases)
  }

#predictive confidence interval method
pred_CI <- function(totalData,previousDate, latestDate, province) {
  
  #filter all data from that province to create predictive model
  totdata_prov <- totalData %>% filter(p_site_province_name %in% province)
  
  #calculate the cases per date 
  cases <- dplyr::count(totdata_prov, blood_draw_date)
  date <- as.POSIXct(cases$blood_draw_date, tz = "UTC")
  
  date_subset <- cases %>% filter(blood_draw_date < latestDate) %>% filter(blood_draw_date > previousDate)

  case_dat <- cases$n
  
  #divide the data set by taking the input weeks as the points we are forecasting. Divide into test and train
  div <- length(case_dat)- nrow(date_subset)
  train <- case_dat[1:div]
  test_mod <- case_dat[div: length(case_dat)]
  
  #create a predictive model using arima
  model <- auto.arima(train) 
  
  #forecast the points using the predictive model
  forecast_data <- forecast(model, length(test_mod)) 
  
  #calculate the error by taking the difference between the actual data and the predictive data points
  error <- test_mod - forecast_data$mean
  
  #calculate the mean predictive error
  moving_avg <- rollmean(error, k = 7, na.pad = TRUE)
  moving = data.frame(moving_avg)
  
  #calculate the standard deviations of the mean predictive error
  moving$sd1 = sd(moving$moving_avg,na.rm=TRUE)
  moving$sd2 = 2*moving$sd1
  moving$sd3 = 3*moving$sd1
  
  #get dates from test set 
  moving$blood_draw_date <- cases$blood_draw_date[div:length(case_dat)]
  
  #isolate actual data we forecasted
  moving$actual_case <- cases$n[div:length(case_dat)]
  
  moving$pred <- forecast_data$mean
  
  #replace NAs with the closest values
  moving <- moving %>%
    fill(moving_avg)
  
  moving <- moving %>%
    fill(moving_avg, .direction = "up")
  
  # #define anomalous data points. Highly anomalous data points are defined as being 3 standard deviations greater than the mean error
  # moving$activity = "Medium Anomalous Activity"
  # moving$activity[moving$actual_case > moving$moving_avg + moving$sd3] <- "High Anomalous Activity"
  # moving$activity[moving$actual_case < moving$moving_avg + moving$sd2] <- "Low Anomalous Activity"
  
  #define anomalous data points. Highly anomalous data points are defined as being 3 standard deviations greater than the predicted value
  moving$activity = "Medium Anomalous Activity"
  moving$activity[moving$actual_case > moving$pred + moving$sd3] <- "High Anomalous Activity"
  moving$activity[moving$actual_case < moving$pred + moving$sd2] <- "Low Anomalous Activity"
  
  names(moving)[names(moving) == "actual_case"] <- "cases"
  return(moving)
}

#historical average method
range_hist <- function(total_data, prov_name) {
  latest_date <- as.POSIXct("2022-05-25", tz = "UTC")
  
  #get all the data for specific provinces anf calculate the cases per date
  data_all <- total_data  %>% filter(p_site_province_name %in% prov_name)
  cases_tot <- data.table(dplyr::count(data_all, blood_draw_date))
  
  #get the sequence of dates from all time 
  comp_cases <- data.table(seq(as.Date("2012-01-01"), as.Date(latest_date), by ="days"))
  
  #match the cases numbers to the date in the complete sequence of dates
  comp_cases$cases <- cases_tot$n[match(as.Date(comp_cases$V1),as.Date(cases_tot$blood_draw_date))]
  comp_cases$cases[is.na(comp_cases$cases)] = 0
  
  #calculate the average cases for 7 days prior to the current date and 7 days after the current date
  #comp_cases[, MeanVal := c(rollmean(comp_cases$cases, 14, fill = NA, align ="right"))]
  comp_cases[, MeanVal := c(rollmean(comp_cases$cases, 14, fill = NA, align ="center"))]
  
  #average the average cases from that date for the last three years 
  comp_cases[ , LaggedVal := (lag(MeanVal,365) + lag(MeanVal,365*2) + lag(MeanVal,356*3))/3]
  
  #define anomalous data point if the current observation is 1.5 times greater than the mean value from previous
  #three years
  comp_cases$activity = "No Anomalous Activity"
  #comp_cases$activity[comp_cases$cases > comp_cases$LaggedVal*1.5] <- "Anomalous Activity Detected"
  comp_cases$activity[comp_cases$cases > comp_cases$LaggedVal] <- "Anomalous Activity Detected"
  
  #rename the column to be blood draw date
  names(comp_cases)[names(comp_cases) == 'V1'] <- 'blood_draw_date'
  
  return(comp_cases)
}

#weekly case count comparison from previous year method
week_case_comp <- function(total_data, prov_name){
  latest_date <- as.POSIXct("2022-05-25", tz = "UTC")
  
  #filter the data for the specific province
  data_all <- total_data  %>% filter(p_site_province_name %in% prov_name)
  
  #calculate the cases for each date
  cases_tot <- data.table(dplyr::count(data_all, blood_draw_date))
  
  #create sequence of dates 
  comp_cases <- data.table(seq(as.Date("2012-01-01"), as.Date(latest_date), by ="days"))
  
  #match the cases to the specific dates
  comp_cases$cases <- cases_tot$n[match(as.Date(comp_cases$V1),as.Date(cases_tot$blood_draw_date))]
  
  #if NA is given, put 0 in that place
  comp_cases$cases[is.na(comp_cases$cases)] = 0
  
  #change the format of the date to year and week
  comp_cases$dateweek <- format(comp_cases$V1, format="%Y-%U")
  
  #sum the cases by week
  comp_cases[, week_sum := sum(cases), by = dateweek]
  
  #calculate lagged value which is the previous year weekly cases from the previous year
  comp_cases[ , LaggedVal := lag(week_sum,364)]
  
  #define anomalous data points if the week sum is greater than the week cumulative cases from the previous year
  comp_cases$activity = "No Anomalous Activity"
  #comp_cases$activity[comp_cases$week_sum > comp_cases$LaggedVal*1.5] <- "Anomalous Activity Detected"
  comp_cases$activity[comp_cases$week_sum > comp_cases$LaggedVal] <- "Anomalous Activity Detected"
  names(comp_cases)[names(comp_cases) == 'V1'] <- 'blood_draw_date'
  
  return(comp_cases)
}

#weekly three year median comparison method (baseline method currently used in Thailand)
week_three_yr_median <- function(total_data, prov_name){
  latest_date <- as.POSIXct("2022-05-25", tz = "UTC")
  
  #filter the data for the specific province
  data_all <- total_data  %>% filter(p_site_province_name %in% prov_name)
  
  #calculate the cases per date
  cases_tot <- data.table(dplyr::count(data_all, blood_draw_date))
  
  #calculate sequence of dates
  comp_cases <- data.table(seq(as.Date("2012-01-01"), as.Date(latest_date), by ="days"))
  
  #match the cases with the complete dates 
  comp_cases$cases <- cases_tot$n[match(as.Date(comp_cases$V1),as.Date(cases_tot$blood_draw_date))]
  
  #replace NA cases with 0 for formatting
  comp_cases$cases[is.na(comp_cases$cases)] = 0
  
  #convert the date formate into year and week
  comp_cases$dateweek <- format(comp_cases$V1, format="%Y-%U")
  
  #sum the cases by week
  comp_cases[, week_sum := sum(cases), by = dateweek]
  
  #the lagged value compared is the previous year weekly cumulative cases
  comp_cases[ , LaggedVal_1 := lag(week_sum,364)]
  comp_cases[ , LaggedVal_2 := lag(week_sum,364*2)]
  comp_cases[ , LaggedVal_3 := lag(week_sum,364*3)]
  
  #calculate the median value from the previous three years 
  threeyr_median <- apply(comp_cases[,LaggedVal_1:LaggedVal_3], 1, median, na.rm = TRUE)
  
  #bind the two data frames
  comp_cases <- cbind(comp_cases, threeyr_median)
  
  #define the anomalous activity if the weekly case counts is greater than the previous three year median weekly cumulative value
  comp_cases$activity = "No Anomalous Activity"
  comp_cases$activity[comp_cases$week_sum > comp_cases$threeyr_median] <- "Anomalous Activity Detected"
  names(comp_cases)[names(comp_cases) == 'V1'] <- 'blood_draw_date'
  
  return(comp_cases)
}

#month case count comparison for prior 4 years
month_case_comp <- function(total_data, prov_name){
  latest_date <- as.POSIXct("2022-05-25", tz = "UTC")
  #filter the data based on the province
  data_all <- total_data  %>% filter(p_site_province_name %in% prov_name)
  
  #calculate the cases per date
  cases_tot <- data.table(dplyr::count(data_all, blood_draw_date))
  
  #create sequence of all dates
  comp_cases <- data.table(seq(as.Date("2012-01-01"), as.Date(latest_date), by ="days"))
  
  #match the cases to the complete date sequence dataset
  comp_cases$cases <- cases_tot$n[match(as.Date(comp_cases$V1),as.Date(cases_tot$blood_draw_date))]
  
  #set NA values to 0
  comp_cases$cases[is.na(comp_cases$cases)] = 0
  
  #convert date format to year and month
  comp_cases$datemonth <- format(comp_cases$V1, format="%Y-%m")
  
  #sum the cases by month
  comp_cases[, month_sum := sum(cases), by = datemonth]
  
  #calculate the average cumulative monthly cases from the last 4 years 
  comp_cases[, average_fouryr := (lag(month_sum,364) + lag(month_sum,364*2) + lag(month_sum,364*3) + lag(month_sum,364*4))/4]
  
  comp_cases[ , LaggedVal_1 := lag(average_fouryr,364)]
  comp_cases[ , LaggedVal_2 := lag(average_fouryr,364*2)]
  comp_cases[ , LaggedVal_3 := lag(average_fouryr,364*3)]
  comp_cases[ , LaggedVal_4 := lag(average_fouryr,364*4)]
  
  #calculate the standard deviation for the previous 4 year cumulative cases 
  row_sd <- apply(comp_cases[,LaggedVal_1:LaggedVal_4], 1, sd, na.rm = TRUE)
  
  #bind the data frames and calculate the 2 standard deviation band
  comp_cases <- cbind(comp_cases, sd = 2*row_sd)
  
  #define the anomalous data points as being 2 standard deviations greater than the mean value
  comp_cases$activity = "No Anomalous Activity"
  comp_cases$activity[comp_cases$month_sum > comp_cases$average_fouryr + comp_cases$sd] <- "Anomalous Activity Detected"
  names(comp_cases)[names(comp_cases) == 'V1'] <- 'blood_draw_date'
  
  return(comp_cases)
}

#unsupervised clustering with DBSCAN
unsup_db <- function(data){
 
  #calculate cases per date
  cases_db <- as.data.frame(dplyr::count(data, blood_draw_date))
  
  #define test as the cases
  test <- cases_db$n
  
  #use dbscan to cluster
  dbscanResult <- dbscan(as.data.frame(test), eps= 10, MinPts=5)
  
  dbscanResult
  
  #identify the clusters
  dbscanResult$cluster
  
  #define all the data points as non anomalous 
  if(length(unique(dbscanResult$cluster)) == 1){
    cases_db$activity = "No Anomalous Activity"
    
  }else{
  #identify anomalies and plot them 
  #function dbscan very sensitive to input parameters.
  cases_db$cluster = dbscanResult$cluster
  
  #sorting clusters from min to max
  cluster_res <- table(dbscanResult$cluster)
  cluster_res <- data.frame(cluster_res[order(cluster_res,decreasing = FALSE)])
  
  #remove all levels 
  cluster_res$Var1 <- as.numeric(as.character(cluster_res$Var1))
  
  #define all the data points in the smallest cluster as anomalous
  cases_db$activity = "No Anomalous Activity"
  cases_db$activity[cases_db$cluster == cluster_res$Var1[1]] <- "Anomalous Activity Detected"
  }
  
  #rename the column to cases
  names(cases_db)[names(cases_db) == "n"] <- "cases"
  return(cases_db)
}

#unsupervised clustering with temperature, precipitation, and cases 
unsup_db_temp_precip <- function(data, temp_data_long, precip_data_long){
  #calculate cases per date
  cases_db <- data.frame(dplyr::count(data, blood_draw_date))
  #combine the temperature data frame to the cases based on date
  combined_dat <- left_join(cases_db, approx_daily_temp, by=c("blood_draw_date"))
 
  #combine the previous data frame with the precipitation data 
  total_data <- left_join(combined_dat, approx_daily_precip, by=c("blood_draw_date"))
  
  #extract specific columns
  cases_temp <- data.frame(total_data[,c("n","measurement.x","measurement.y")])
  #identify if there more than one observations for cases, temperature, and precipitation
  if(length(unique(cases_temp$n)) == 1 ||length(unique(cases_temp$measurement.x)) == 1 || length(unique(cases_temp$measurement.y)) == 1  ){
    
    #cluster using dbscan
    dbscanResult <- dbscan(na.omit(cases_temp), eps = 0.45, MinPts =  5)
  
  # #db_temp <- dbscan(na.omit(cases_temp), eps = 30, MinPts =  5)
  # dbscanResult <- dbscan(na.omit(cases_temp), eps = 0.45, MinPts =  5)
  # 
  }else{
    #scale the data so they are comparable
    cases_temp <- scale(cases_temp)
    #db_temp <- dbscan(na.omit(cases_temp), eps = 30, MinPts =  5)
    
    #unsupervised clustering with dbscan
    dbscanResult <- dbscan(na.omit(cases_temp), eps = 0.45, MinPts =  5)
   }
  
  #extract the cluster information
  dbscanResult$cluster
  
  #check if there are more than one cluster so anomalies can be defined
  if(length(unique(dbscanResult$cluster)) == 1){
    cases_db$activity = "No Anomalous Activity"
    
  }else{
    #identify clusters
    cases_db$cluster = dbscanResult$cluster
    
    #sorting clusters from min to max
    cluster_res <- table(dbscanResult$cluster)
    cluster_res <- data.frame(cluster_res[order(cluster_res,decreasing = FALSE)])
    
    #remove all levels 
    cluster_res$Var1 <- as.numeric(as.character(cluster_res$Var1))
    
    #classify the anomalous data points as the observations in the smallest cluster 
    cases_db$activity = "No Anomalous Activity"
    cases_db$activity[cases_db$cluster == cluster_res$Var1[1]] <- "Anomalous Activity Detected"
  }
  
  #redefine the column to cases
  names(cases_db)[names(cases_db) == "n"] <- "cases"
  return(cases_db)
}

#unsupervised time series clustering
unsup_tsclust <- function(data){
 
  #calculate the cases per date
  clust_dat <- as.data.frame(dplyr::count(data, blood_draw_date))
  
  #define the cases
  cases <- as.data.frame(clust_dat$n)
  
  #check if there are more than 2 data points usable for clustering using tsclust (requirement for the function)
  if(nrow(cases) <= 2){
    clust_dat$activity = "No Anomalous Activity"

  }else{
  
  # Fast exact equivalent of the original call:
  #   tsclust(cases, type = "hierarchical", k = 2L, distance = "sbd",
  #           control = hierarchical_control(method = "average"))
  # Each row is a length-1 series and counts are positive, so every SBD distance is 0.
  # hclust/cutree(k = 2) on an all-zero matrix puts only the last row in cluster 2
  # (verified n = 3..4000 on R 4.3.3 and 4.6.2; see unsup_check/verify.R).
  clust_dat$cluster <- c(rep(1L, nrow(cases) - 1L), 2L)

  #classify points in cluster 2 (= nrow(hc@clusinfo) in the original) as anomalous
  clust_dat$activity = "No Anomalous Activity"
  clust_dat$activity[clust_dat$cluster == 2L] <- "Anomalous Activity Detected"
  }
  #change the name of the column to cases
  names(clust_dat)[names(clust_dat) == "n"] <- "cases"
  return(clust_dat)
}

#function to select method depending on the user input
ano_algo <- function(method,data, totalData, previousDate, latestDate, province, temp_data_long,precip_data_long){
  if(method == "Statistical Profiling") df_comp <- stat_profile(data)
  else if(method == "Predictive Confidence Interval") df_comp <- pred_CI(totalData, previousDate, latestDate, province)
  else if(method == "Density-Based Profiling") df_comp <- unsup_db(data)
  else if(method == "Density-Based Profiling with Temp and Precip") df_comp <- unsup_db_temp_precip(data,temp_data_long,precip_data_long)
  else if(method == "Historical Average") df_comp <- range_hist(totalData,province)
  else if(method == "Weekly Case Previous Year") df_comp <- week_case_comp(totalData,province)
  else if(method == "Monthly Case Four Years") df_comp <- month_case_comp(totalData, province)
  else if(method == "Baseline: Weekly Three Year Median") df_comp <- week_three_yr_median(totalData,province)
  else df_comp <- unsup_tsclust(data)
  
  return(df_comp)
}

#function for the summary page of the dashboard to give weekly summary (fix when case data is actively inputted in system)
week_summary <- function(ano_act){
  # summarise data to count how many provinces have anomalies identified and how many do not
  data <- data.frame(
    category=c("Anomaly Detected", "No Anomaly"),
    count=c(length(ano_act$status[ano_act$status == 1]), length(ano_act$status[ano_act$status == 0]))
  )
  
  # Compute percentage of provinces with anomalies
  data$fraction <- data$count / sum(data$count)
  
  # calculating the label position
  data$ymax <- cumsum(data$fraction)
  data$ymin <- c(0, head(data$ymax, n=-1))
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Make the label
  data$label <- paste0(data$category, "\n Province(s): ", data$count)
  
  return(data)
}

#function to calculate the standardised incidence ratio
SIR_EV <- function(total_data,prev_time, latest_date){
  #calculate the date range
  prev_date <- latest_date - prev_time
  
  #calculate the cases for each date
  case_counts_all <- dplyr::count(total_data,blood_draw_date)
  
  #calculate the cases from the last three full years of current data set
  year_month_mean <- case_counts_all %>% filter(blood_draw_date < "2021-12-31" ) %>% filter(blood_draw_date > "2019-01-01")
  
  #calculate the mean case counts per month over the from 2019 to 2021 for each province
  mean_counts <- year_month_mean %>% 
    mutate(Year = year(blood_draw_date), Month = month(blood_draw_date)) %>%
    group_by(Year, Month) %>%
    dplyr::summarise(result = sum(n)) %>% group_by(Month) %>% dplyr::summarise(avg = mean(result))
  
  #turn mean counts into data frame
  mean_counts <- data.frame(mean_counts)
  
  #load the province population file
  pop_file <- read.csv("sector_01_11201_EN_.csv")
  prov_pop <- as.data.frame(prov_name)
  
  #edit the names to match the right spelling
  pop_file$X[pop_file$X == "Bangkok"] <- 'Bangkok Metropolis'
  pop_file$X[pop_file$X == "Phattalung"] <- 'Phatthalung'
  pop_file$X[pop_file$X == "Satun "] <- 'Satun'
  pop_file$X[pop_file$X == "Khon Kaen "] <- 'Khon Kaen'
  
  #match the province name and population data and merge
  prov_pop$pop <- pop_file$X.5[match(prov_pop$prov_name,pop_file$X)]
  
  #reformat the string
  prov_pop$pop <- as.numeric(gsub(",", "", prov_pop$pop))
  
  #calculate the total population
  total_pop <- sum(prov_pop$pop)
  
  #expected value calculations for each provinces
  months_EV <- month(seq(prev_date,latest_date, by= "month"))
  months_EV <- as.data.frame(table(months_EV))
  
  #match the expected values for each province to the actual cases observed and calculate the cases for that month
  mean_counts$freq<- months_EV$Freq[match(mean_counts$Month,months_EV$months_EV)]
  mean_counts$abs_case <- mean_counts$freq*mean_counts$avg
  
  #calculates the constant rate
  ref_rate <- sum(mean_counts$abs_case, na.rm = TRUE)/total_pop
  
  #calculate the expected value based on the province
  prov_pop$ev <- (ref_rate)*prov_pop$pop
  
  prov_pop$cases <- 0
  
  return(prov_pop)
}

##accuracy testing for each province and for each method
method_compare <- function(){
  #list the methods we want to compare
  methods_list <- c("Statistical Profiling",
                    "Predictive Confidence Interval",
                    "Unsupervised Clustering",
                    "Density-Based Profiling",
                    "Density-Based Profiling with Temp and Precip",
                    "Historical Average",
                    "Weekly Case Previous Year",
                    "Monthly Case Four Years",
                    "Baseline: Weekly Three Year Median")
  
  #extract the unique list of province names 
  prov_list <- unique(total_data$p_site_province_name)
  
  #load the outbreak csv and rename the columns 
  outbreak_dat <- read.csv("outbreak_malaria_th_5.csv")
  colnames(outbreak_dat) <- c("dates","province")
  
  #setting the date range we want to conduct our analysis on
  latest_date <- as.POSIXct("2022-05-25", tz = "UTC")
  prev_date <- as.POSIXct("2012-01-01", tz = "UTC")
  
  dates_filt <- seq(prev_date, latest_date, by="days")
  
  #function to check if anomalies have been detected in the previous 14 days 
  inRange = function(x){
    #checks the last 14 days for if high anomalies have been detected
    if( x > -14 && x < 0 ) return(TRUE)
    else{return(FALSE)}
  }
  
  # iterate over methods -> rbindlist is used to create a final data table
  # for each method, compute all anomalies in all provinces
  dt = rbindlist(llply(methods_list, function(method){
    
    #iterate over provinces
    rbindlist(llply(prov_list, function(province_name){
      
      #filter the data for each province 
      data_filt <- filter_data(total_data,prev_date,latest_date,province_filter_values = province_name) #using the x time from previous date
      
      #check if the filtered data has reads
      if(nrow(data_filt) > 0){ 
        
        #apply anomaly detection algorithm to current province data 
        df_comp <- ano_algo(method,data_filt, total_data, prev_date,latest_date, province_name,temp_data_long, precip_data_long)
        df_comp <- data.frame(df_comp)
        
        #check for obeservations where anomalies have been detected 
        detectedAnomalies <- df_comp[df_comp$activity=="High Anomalous Activity"|df_comp$activity == "Anomalous Activity Detected",]["blood_draw_date"]
        
      }else{ 
      }
      
      #subset for true anomalies in the current province to get vector of true dates reported
      trueAnomalies = outbreak_dat[outbreak_dat$province == province_name,]["dates"]
      
      # find difference in time between all detected and true dates
      M = outer(as.Date(detectedAnomalies$blood_draw_date), as.Date(trueAnomalies$dates), "-")
      
      # apply 'inrange' function to check for close dates
      matches = apply(M, 1:2, inRange)
      
      # for each true anomaly check if it was detected
      trueAnomaliesDetected = apply(matches, 2, any)

      #return the province name, the method used, the total number of anomalies triggered, the number of actual anomalies tested,
      #and the number of true anomalies caught by each method and province. 
      return(data.table(Province = province_name, 
                        Method = method, 
                        NumberReported = nrow(detectedAnomalies),
                        NumberActual = nrow(trueAnomalies),
                        NumberActualDetected = sum(trueAnomaliesDetected)))
    }))
  }))
  
  #summarise the data and to show how many true anomalies were caught for each method 
  dt[dt$NumberActualDetected ==1,]
  x <- dt[, lapply(.SD, sum) , .(Method), .SDcols = NumberReported:NumberActualDetected]
  x[, acc:= NumberActualDetected/NumberActual]
  
return(x)
}



#' Joins user polygon attribute data to a map
#' 
#' Joins user polygon attribute data to a map of polygon boundaries.  The map
#' can either be one stored in the package or provided by the user.  Returns a
#' spatialPolygonsDataFrame ready for plotting using \code{\link{mapPolys}}.
#' Reports join successes and failures.
#' 
#' Joins user polygon attribute data provided in a 'data frame' to a map of
#' polygon boundaries.  The map can either be one stored in the package or
#' provided by the user.  Returns a spatialPolygonsDataFrame ready for plotting
#' using \code{\link{mapPolys}}.  Reports join successes and failures.
#' 
#' The user specifies the name of the column in their data containing polygon
#' referencing.
#' 
#' The user can choose from different internal map resolutions.  Uses the
#' function \code{\link{getMap}} to retrieve the map.
#' 
#' @param dF R data frame with at least one column of polygon IDs and one
#' column of data
#' @param nameMap the map to join the attribute data too
#' @param nameJoinIDMap the name of the joinIDs in the map
#' @param nameJoinColumnData name of column in the data containing country
#' referencing
#' @param nameNameColumnData optional name of column in the data containing
#' polygon names (used in reporting of success/failure)
#' @param suggestForFailedCodes NOT YET ENABLED T/F whether you want system to
#' suggest for failed codes
#' @param projection DEPRECATED JUNE 2012
#' @param mapResolution resolution of the borders in the internal map: options
#' 'coarse','low', 'less islands'
#' @param verbose if set to FALSE progress messages to console are restricted
#' @return An R 'SpatialPolygonsDataFrame' [package "sp"] object with the data
#' joined to it
#' @author andy south
#' @seealso \code{\link{mapPolys}}, \code{\link{getMap}}
#' @keywords dplot
#' @examples
#' 
#' 
#' ## this example uses downloaded files
#' ## to run it download the files
#' ## and remove the comment symbols '#' from all the lines starting with a single '#'
#' 
#' ## US states map downloaded from :
#' ## http://www2.census.gov/cgi-bin/shapefiles2009/national-files
#' 
#' #inFile <- 'tl_2009_us_stateec.shp'
#' #sPDF <- readShapePoly(inFile)
#' 
#' ##################
#' ## use mapPolys to map the sPDF
#' #mapPolys(sPDF,nameColumnToPlot = "ALANDEC")
#' #mapPolys(sPDF,nameColumnToPlot = "AWATEREC",mapRegion='North America')
#' 
#' ##################
#' ## join some other data to it
#' ## education data downloaded from here as xls then saved as csv
#' ## http://nces.ed.gov/ccd/drpcompstatelvl.asp
#' 
#' #dataFile <- 'SDR071A_xls.csv'
#' #dF <- read.csv(dataFile,as.is=TRUE)
#' #str(dF)
#' ## STATENAME
#' ## DRP912 Dropout Rate, Grades 9 through 12
#' 
#' ## joining the data to the map
#' ## based upon state names (column NAMEEC in map, and STATENAME in the data)
#' #sPDF2 <- joinData2Map(dF
#' #        , nameMap = sPDF
#' #        , nameJoinIDMap = "NAMEEC"
#' #        , nameJoinColumnData = "STATENAME")
#' 
#' #################
#' ## plot one of the attribute variables
#' #mapDevice()# to set nice shape map window
#' #mapPolys(sPDF2,nameColumnToPlot = "DRP912",mapRegion='North America')
#' 
#' 

`joinData2MapFixed` <- function( dF = NULL
                            , nameMap = ""
                            , nameJoinIDMap = "ISO3"
                            #, joinCode = "ISO3" #options "ISO2","ISO3","FIPS","NAME","UN"
                            , nameJoinColumnData = "ISO3V10"
                            , nameNameColumnData = "Country"
                            , suggestForFailedCodes = FALSE 
                            , projection=NA  #deprecated june 2012
                            , mapResolution="coarse" 
                            , verbose = FALSE #if set to FALSE it doesn't print progress messages to console                         
)
{
  functionName <- as.character(sys.call()[[1]])
  
  #browser()
  
  if (is.null(dF) || ( class(nameMap)=='character' && nameMap=="" ) )
  {
    stop("you haven't specfied data (dF) and/or a map to join it to (nameMap)") 
    return(FALSE)
  } # else add other checks for dF & nameMap
  #if ( dF="" || nameMap="" )
  #   {
  #    stop("the first argument to ",functionName," should be a file name, 2D array or matrix, or SpatialGridDataFrame, yours is, ", class(dataset)) 
  #    return(FALSE)
  #   }       
  
  #getting the map polygons to join the data to
  if ( class(nameMap)=='SpatialPolygonsDataFrame' )
  {
    mapWithData <- nameMap
  } else if ( nameMap != "" && class(nameMap)=="character" )
  {
    mapWithData <- eval(parse(text=nameMap))
  } else
    mapWithData <- getMap(resolution=mapResolution)
  
  #test whether user nameJoinIDMap is one of permitted
  #listJoinCodes <- c("ISO2","ISO3","FIPS","NAME","UN") 
  #if (joinCode %in% listJoinCodes == FALSE)
  #   {
  #    stop("your joinCode (",joinCode,") in ",functionName," is not one of those supported. Options are :",paste(listJoinCodes,""),"\n")
  #    return(FALSE)
  #   }
  
  ## check that the join column exists in the user data
  if ( is.na(match(nameJoinColumnData, names(dF)) )){
    stop("your chosen nameJoinColumnData :'",nameJoinColumnData,"' seems not to exist in your data, columns = ", paste(names(dF),""))
    return(FALSE)
  }       
  
  
  #dF2 <- merge.data.frame(dF, dFlookupCodes, by=nameJoinColumn)
  
  #using match rather than merge, faster and enables greater reporting of success & failure
  
  #match returns a vector of the positions of (first) matches of its first argument in its second. 
  #so perhaps I would also want to check that codes aren't repeated
  #!also want to find a way of coping with Namibia, the code NA gets interpreted as no data
  
  #copy the users nameJoinColumn to a new column named the same as the column in the map for the join code
  #e.g if user has ISO3166_3 it will be copied to ISO3
  #6/2/13 not sure why I did this
  #can't I just remove & use nameJoinColumnData below
  #dF[[nameJoinIDMap]] <- dF[[nameJoinColumnData]]
  
  
  matchPosnsInLookup <- match(as.character(dF[[nameJoinColumnData]])
                              , as.character(mapWithData@data[[nameJoinIDMap]]))
  
  
  #count the NAs to find user countries that have failed to match
  failedCodes <- dF[[nameJoinColumnData]][is.na(matchPosnsInLookup)]
  numFailedCodes <- length(failedCodes) 
  
  #count num successful matches
  numMatchedCountries <- nrow(dF) - numFailedCodes
  #printing info to console    
  cat(numMatchedCountries,"codes from your data successfully matched countries in the map\n")
  
  
  #failedCountries : reports on names of failed countries 
  #if user has specified the name of a country column in the function call
  failedCountries <- dF[[nameNameColumnData]][is.na(matchPosnsInLookup)]
  failedCountries <- cbind(failedCodes,"failedCountries"=as.character(failedCountries))
  
  #printing info to console    
  cat(numFailedCodes,"codes from your data failed to match with a country code in the map\n")
  if (verbose) print(failedCountries)
  
  #     failedCodes failedCountries                 
  #[1,] "CIV"       "Ivory Coast"                   
  #[2,] "COD"       "Congo, Democratic Republic"    
  
  #!could create an optional loop here to go through the failed codes 
  #& prompt the user for a choice fro a suggested list 
  if ( suggestForFailedCodes )
  {
    for( i in 1 : numFailedCodes)
    {
      #search for similar codes/countried & ask user to choose one
      
    }
  }
  #can also get at countries in the lookup that don't appear in user data, by reversing match arguments
  matchPosnsInUserData <- match(as.character(mapWithData@data[[nameJoinIDMap]])
                                , as.character(dF[[nameJoinColumnData]])) 
  
  #these are the codes in lookup that aren't found in user data
  codesMissingFromUserData <- as.character( mapWithData@data[[nameJoinIDMap]][is.na(matchPosnsInUserData)] )                            
  countriesMissingFromUserData <- as.character( mapWithData@data[["NAME"]][is.na(matchPosnsInUserData)] )
  #  
  numMissingCodes <- length(codesMissingFromUserData) 
  
  #printing info to console
  cat(numMissingCodes,"codes from the map weren't represented in your data\n")
  if (verbose) #if (verbose) print more messages to console
  {
    if (nameJoinColumnData!="NAME")                             
    {   print(cbind(codesMissingFromUserData,countriesMissingFromUserData))
    }else #if joined on NAME don't want to print names twice 
      print(codesMissingFromUserData)
  } # 
  
  
  ###############################################################
  #merging lookup table onto user data for those codes that match
  #dF2 <- cbind(dFlookupCodes[matchPosnsInLookup,],dF)    
  #the other way around to before, i.e. joining data onto map    
  #mapWithData@data <- cbind(mapWithData@data, dF[matchPosnsInUserData,])
  
  #6/2/13 to avoid having the join column repeated
  dF2 <- dF[,-which(names(dF)==nameJoinColumnData), drop=FALSE] #drop=FALSE stops it from converting from dF if just 1 column
  mapWithData@data <- cbind(mapWithData@data, dF2[matchPosnsInUserData,,drop=FALSE], deparse.level = 0) #deparse stops R creating new column label when just 1 column    
  
  
  #test colouring map by region & subregion seems to show order has been retained
  #plot(mapWithData,col=mapWithData@data$REGION)
  #plot(mapWithData,col=mapWithData@data$SUBREGION)
  
  #returning the sPDF with the user data joined to the map polygons
  invisible(mapWithData)
  
} 




