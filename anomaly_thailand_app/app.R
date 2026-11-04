# Malaria app: persistent startup + default four-week map cache.
# Build ONCE in the app directory, before deployment, in a fresh R session:
#   options(malaria.build_cache = TRUE)
#   source("app.R")
#   options(malaria.build_cache = FALSE)
# Then restart R and run shiny::runApp(). Include app_cache/ in deployment.
# Rebuild after changing data, algorithms, app code, packages, or timezone.
source("packages.R")
library(sf)

.cache_build <- isTRUE(getOption("malaria.build_cache", FALSE))
.cache_path <- file.path("app_cache", "startup_4weeks.rds")
.cache_defaults <- list(weeks = 4, species = "Vivax",
                       method = "Baseline: Weekly Three Year Median",
                       date = "2022-05-25")
.cache_objects <- c("total_data", "prov_name", "thaimap", "temp_data_long",
                    "precip_data_long", "approx_daily_temp", "approx_daily_precip")

cache_signature <- function() {
  scripts <- c("app.R", "packages.R", "load_data.R", "ano_algo_fxns.R")
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
  packages <- c("sf", "ggplot2", "dplyr", "plyr", "lubridate", "zoo", "tidyr", "readr")
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

.signature <- cache_signature()
.disk_cache <- NULL
if (!.cache_build && file.exists(.cache_path)) {
  candidate <- tryCatch(readRDS(.cache_path), error = function(e) NULL)
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
  source_without_bootstrap("load_data.R")
  message("Startup data: prepared from source files (once).")
} else {
  list2env(.disk_cache$data, envir = .GlobalEnv)
}
source_without_bootstrap("ano_algo_fxns.R")

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
      end_date <- as.POSIXct("2022-05-25")
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
      end_date <- as.POSIXct("2022-05-25")
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

# The spinner wrapper stays hidden until the first Compute click.
# Client-side conditions also reveal it while the server is busy computing.
map_output_panel <- function(output_id) {
  shiny::tagList(
    shiny::conditionalPanel(
      condition = "!input.go || input.go === 0",
      shiny::div(
        style = "padding: 40px 20px; color: #6c757d; text-align: center;",
        "Select the options and click Compute to display the map."
      )
    ),
    shiny::conditionalPanel(
      condition = "input.go > 0",
      style = "display: none;",
      addSpinner(
        shiny::plotOutput(output_id, width = "100%", height = "80vh"),
        spin = "cube", color = "royalblue"
      )
    )
  )
}

ui <- bs4Dash::dashboardPage(
  title = "Shiny Application",
  help = NULL,
  bs4Dash::dashboardHeader(
   
    align = "Anomaly Detection of Malaria in Thailand"
  ),
  #creates three main tabs
  bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      bs4Dash::menuItem(
        text = "About",
        tabName = "about",
        icon = icon("circle-info")
      ),
      bs4Dash::menuItem(
        text = "Summary",
        tabName = "summary",
        icon = icon("sort")
      ),
      bs4Dash::menuItem(
        text = "Analytics",
        tabName = "Analytics",
        icon = icon("map")
      )
      
    )
    
  )
  , bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "about", 
        #uncomment to include translation 
        # selectInput(inputId = "lang", label = "Select Language", choices = c("en", 
        #                                                                                         "th"
        # ) 
        # ),
        #uncomment to include translation
        # bs4Dash::bs4Card(
        #   title = "General",
        #   status = "white",
        #   background = "white",
        #   width = 12, solidHeader = TRUE,
        #   collapsible = TRUE,usei18n(i18n),i18n$at(
        #   "The purpose of this project is to develop a user 
        #   interface that supports anomaly detection of malaria 
        #   outbreaks in Thailand. The overarching goal is to support malaria surveillance efforts in Thailand. Using malaria case data from 2012, 
        #   anomaly detection algorithms were initially created using statistical 
        #   and machine learning approaches. This is further described below. 
        #   An anomaly is defined as strange or unusual behaviour. In terms of this project, 
        #  an anomaly is higher than usual malaria cases at a provincial level and at a specific 
        #  time of year. Each algorithm has different definitions of anomalies and 
        #  is better defined in the methods section. The user interface was created to allow clear 
        #   visualisation of anomalous activity while allowing the user to select their preferred method and time frame for the analysis.")
        # )
        
        #adds project description 
        bs4Dash::bs4Card(
          title = "General",
          status = "white",
          background = "white",
          width = 12, solidHeader = TRUE,
          collapsible = TRUE,
          "The purpose of this project is to develop a user 
          interface that supports anomaly detection for early warning of malaria 
          outbreaks in Thailand. The overarching goal is to support malaria surveillance efforts in Thailand. Using malaria case data from 2012 to 2022, 
          anomaly detection algorithms were initially created using statistical 
          and machine learning approaches. This is further described below. 
          An anomaly is defined as strange or unusual behaviour. In terms of this project, 
         an anomaly is higher than usual malaria cases at a provincial level and at a specific 
         time of year. Each algorithm has different definitions of anomalies and 
         is better defined in the methods section. The user interface was created to allow clear 
          visualisation of anomalous activity while allowing the user to select their preferred method and time frame for the analysis.")
        , fluidRow(
          bs4Dash::bs4Card(
            title = "Methods",
            status = "white",
            background = "white",
            width = 6, solidHeader = TRUE,
            collapsible = TRUE,
            #describes the methods used in this application
            strong("Statistical Profiling:"),"Calculates the moving average at weekly intervals and uses standard
          deviation bands to define the thresholds. Cases observations above 3 SD above the mean are defined as anomalous (definition can be set by user)",br(),br(),
            strong("Predictive Confidence Interval:"),"Uses historical data to create a predictive model using ARIMA. The model is 
                 then used to forecast the user-defined time period. The mean absolute percentage error is 
                 calculated and used to calculate the confidence interval bands. Case observations 3 SD  above the mean error are defined as anomalous", br(), br(),
            strong("Unsupervised Clustering:"), "Time series unsupervised clustering is applied to case data. More specifically, a hierarchical type is used. Cases in the smallest 
                 cluster with a minimum 5 observations is classified as anomalous", br(), br(),
            strong("Density-Based Profiling:"),"DBSCAN is used for density-based clustering of malaria cases. Cases in the smallest cluster with minimum 5 observations is classified as anomalous ",br(),br(),
            strong("Density-Based Profiling with Temp and Precip:"),"DBSCAN is used for density-based clustering of malaria cases combined with
          precipitation and temperature data. Cases in the smallest cluster with minimum 5 observations is classified as anomalous",br(), br(),
            strong("Historical Average:"),"Uses the historical average to calculate the rolling mean using the previous 14 days and comparing current 
          observations to the average number of cases for the previous 3 years. Current cases greater than the prior 3 year average value is 
          classified as an anomaly",br(), br(),
            strong("Weekly Case Previous Year:"),"Historical data is used to calculate the weekly sum of cases. Current weekly observations are compared to the weekly sum of cases from the 
          previous year. Weekly sum of cases that is greater than the weekly sum from the previous year is classified as an anomaly",br(), br(),
            strong("Monthly Case Four Years:"), "Historical data is used to calculate the monthly total of cases. The current monthly observation is compared to mean monthly total from prior 4 years. Current
          observations with case counts higher than 2 SD from the mean monthly value is classified as an anomaly",br(), br(),
            strong("General:"),"Methods highlighted blue in the dropdown menu under the 'Analysis' tab are time-series and statistical based while methods highlighted in orange are machine-learning based."
            ),
          
          bs4Dash::bs4Card(
            title = "Tabs",
            status = "white",
            background = "white",
            width = 6, solidHeader = TRUE,
            collapsible = TRUE,
            strong("Summary"), "Contains information on the current's week anomaly status across all of Thailand. This page 
         lists the provinces where anomalies have been identified this week. A trend plot is also provided to show the total malaria
         case counts during this week and broken down by species",br(),br(),
            strong("Analytics"), "This page provides more details on how the analysis is implemented and maps the 
         provinces with anomalies for improved visualisation. The user selects the method and the range in weeks used for the analysis. The results are shown on a map and in a list"
          ))
        
      ),
      bs4Dash::tabItem(
        tabName = "summary", fluidRow(
          bs4Dash::bs4TabCard(
           
            collapsible = FALSE,
            width = 8,
            tabPanel(title = "WEEKLY SUMMARY", value = "ano_sum",
                     plotOutput("summary_plot", width = "100%", height = "80vh"),align = "right"), 
            tabPanel(title = "TREND PLOT", value = "trend_week",
                     plotOutput("summary_trend_plot", width = "100%", height = "80vh"),align = "center")
            #align = "center"
          ),bs4Dash::box(
            title = h6("ANOMALIES IDENTIFIED", style ='color:gray'),
            tableOutput("summary_table"),
            collapsible = FALSE,
            width = 4,
            align = "center"
          ))
        
      
      ),
      bs4Dash::tabItem(
        tabName = "Analytics",
        
        fluidRow(
          
          bs4Dash::bs4Card(
            title = "INSTRUCTIONS",
            status = "white",
            background = "white",
            width = 4,solidHeader = TRUE, collapsible = TRUE, strong("1:"), "Select Species",br(),
            strong("2:"), "Select 'Weeks Prior' for analysis", br(), strong("3:"),"Select Method in the drop down box",
            br(), strong("4:"), "Press 'Compute'", height = "120px", algin = "center",style = 'margin-top: 0px'
            
            
          ),
          bs4Dash::bs4Card( title = "SPECIES", status = "white", width = 4, background = "white", 
                            solidHeader = TRUE,
                            selectInput(
                              inputId = "species",
                              label = "",
                              choices = c("Vivax", 
                                          "Falciparum", 
                                          "All") 
                            ),height = "135px"), align = "center"
          ,
          bs4Dash::bs4Card(
            title = "% PROV W/ ANOMALIES",
            status = "white",
            background = "white",
            width = 4,solidHeader = TRUE, height = "135px",h4(textOutput("percent_prov"), style ='color:#1e90ff ; text-align: bottom ;margin-top: 25px')
          )
        )
        ,
        
        fluidRow(
          column(
            width = 4,
            sliderInput(
              inputId = "Weeks_Prior",
              label = "Weeks Prior",
              min = 1,
              max = 520,
              value = 4,
              step = 1
            )
          ),
          #colour codes the methods. The time series-based method is blue and the machine learning-based ones are orange
          column(
            width = 4,tags$style("#method .option[data-value='Historical Average'],.option[data-value='Statistical Profiling'],.option[data-value='Historical Average'],.option[data-value='Predictive Confidence Interval'] ,.option[data-value='Weekly Case Previous Year'],.option[data-value='Monthly Case Four Years'],.option[data-value='Baseline: Weekly Three Year Median'] {
          background: #99CCFF !important;
          color: black !important;
        
        }
          .option[data-value='Unsupervised Clustering'],.option[data-value='Unsupervised Clustering'],.option[data-value='Density-Based Profiling'],.option[data-value='Density-Based Profiling with Temp and Precip']  {
          background: #FFCC99 !important;
          color: black !important;
        }"),
            #define the methods used for anomaly detection
            selectInput(
              inputId = "method",
              label = "Method",
              choices = c("Baseline: Weekly Three Year Median",
                          "Statistical Profiling", 
                          "Predictive Confidence Interval",
                          "Historical Average",
                          "Weekly Case Previous Year",
                          "Monthly Case Four Years",
                          "Unsupervised Clustering", 
                          "Density-Based Profiling",
                          "Density-Based Profiling with Temp and Precip"
              ), selectize = TRUE
            )
            , align = "center"),
          column(
            width = 4, align = "center",
            HTML("<br>"),
            #creates the action button
            actionButton(
              inputId = "go",
              class = "btn-primary",
              width = "300px", style = "color: white; background-color: #0275d8",
              "Compute"
            )
          )
        )
        
        ,fluidRow(
          #outputs the data table and the trend line using sparkline
          bs4Dash::box(
            title = h6("ANOMALIES IDENTIFIED", style ='color:gray'),
            sparklineOutput("test_app"),
            DT::dataTableOutput("first"),
            collapsible = FALSE,
            width = 4,
            align = "center"
          )
          , 
          
          bs4Dash::bs4TabCard(
            #outputs the anomaly detection map and the standardised incidence ratio map 
            width = 8, height = 800,
            collapsible = FALSE,
            status = "white",
            tabPanel(title = "ANOMALY MAP", value = "test3",
                     map_output_panel("modelPlot"),align = "center"), 
            tabPanel(title = "STANDARDISED INCIDENCE RATIO MAP", value = "test2",
                     map_output_panel("modelPlot2"),align = "center"),
            # tabPanel(title = "METHOD COMPARISON", value = "test4",
            #          addSpinner(DT::dataTableOutput("comparison"),spin = "cube", color = "royalblue"),align = "center"),
            tabPanel(title = "METHOD COMPARISON", value = "test4",
                    DT::dataTableOutput("comparison"),align = "center"),
            tabPanel(title = "MAP DESCRIPTIONS",align = "left", strong("Anomaly Map:"),"This map highlights provinces with unsual or anomalous activity
                   activity as a result of the time frame and method selected.", br(),br(),
                     strong("Standardised Incidence Ratio Map:"),"This map shows the ratio of the observed number of malaria cases compared to the expected
                   value for each province standardised by the total and provincial population of Thailand. The SIR of each province is calculated and compared to each other. The higher
                   the SIR, the higher the incidence cases compared to the expected value.")
            
            
          )
        )
      )
    )
  ) 
)

# Define the server
server <- function(input, output, session) {
  
  #uncomment to enable translation feature (not working for now)
  # eventReactive(input$lang, {
  #   default_lang <- input$lang
  #   print(default_lang)
  #   i18n$set_translation_language(input$lang)
  # 
  #   update_lang( session, input$lang)
  #   
  #   target_lang <- input$lang
  #   renderUI(
  #     #i18n$set_translation_language(input$lang)
  #     update_lang(input$lang, session)
  #     #target_lang <- input$lang
  #   )
  #   
  # })
  
  computed_settings <- eventReactive(input$go, {
    list(weeks = as.numeric(input$Weeks_Prior), species = input$species,
         method = input$method, date = "2022-05-25")
  })

  use_default_cache <- reactive({
    !is.null(.disk_cache) && identical(computed_settings(), .cache_defaults)
  })

  modelOut <- eventReactive(input$go, {
    if (use_default_cache()) return(.disk_cache$default_result)

    #define the nujmber of weeks wanted to analysis
    week_num <- input$Weeks_Prior
    
    #convert the number of previous weeks into days 
    prev_time <- weeks(week_num)
    latest_date <- as.POSIXct('2022-05-25')
    
    #calculates the province for standardised incidence calculations
    prov <- SIR_EV(total_data,prev_time,latest_date)
    #filters the data based on species selected
    if(input$species == "All"){
      ano_act <- find_anomaly(prev_time,latest_date,input$method, prov,temp_data_long, precip_data_long)
    }else if(input$species == "Vivax"){
      species_v = "V"
      ano_act <- find_anomaly(prev_time,latest_date,input$method, prov,temp_data_long, precip_data_long, species_v )
    }else {
      species_f = "F"
      ano_act <- find_anomaly(prev_time,latest_date,input$method, prov,temp_data_long, precip_data_long, species_f)}
    
    return(ano_act)
  })
  
  # One map attribute match for non-default computations.
  model_map <- reactive({
    attach_map_values(modelOut(), get_thai_geometry())
  })

  output$modelPlot <- renderPlot({
    req(input$go > 0)
    if (use_default_cache()) {
      grid::grid.newpage()
      grid::grid.draw(.disk_cache$anomaly_grob)
    } else {
      print(make_map_plot(model_map(), "anomaly"))
    }
  }, height = 700, width = 700)

  #adding sparklines
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '#A7DBD8', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
  
  cd <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))
  
  cb = JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ", 
                 line_string, " });\n}"), collapse = "")
  
  
  #render the table that shows cases averted and costs
  output$first <- DT::renderDataTable({
    validate(
      need(input$go, ""),
    )
    dt <- modelOut()[modelOut()$status > 0,]
    dt <- dt[c("province","cases")]
    prov_ano <- DT::datatable(dt,rownames = FALSE, options = list(columnDefs = cd,fnDrawCallback = cb))
    return(prov_ano)
    
  })
  
  #render the table that shows accuracy when comparing methods (static output comparing methods using
  #all historical data)
  output$comparison <- DT::renderDataTable({
    Method <- c("Statistical Profiling","Predictive Confidence Interval",
                "Unsupervised Clustering","Density-Based Profiling",
                "Density-Based Profiling with Temp and Precip",
                "Historical Average"," Weekly Case Previous Year",
                "Monthly Case Four Years","Baseline: Weekly Three Year Median")
    NumberReported <- c(882 ,6493,75, 5, 452, 10875, 35602, 5577, 32630 )
    NumberActual <- rep(7,9)
    NumberActualDetected <- c(1,3,0,0,0,6,6,0,6)
    AccuracyPercentage <- (NumberActualDetected/NumberActual)*100
    dt <- data.table(Method,NumberReported,NumberActual,NumberActualDetected,AccuracyPercentage)
    
    return(dt)

  })
  
  # These lazy reactives run only when their visible outputs request them.
  data_week_summary <- reactive(get_weekly_summary())
  data_week <- reactive(get_weekly_cases())

  #creates the proportion map for the summary page 
  output$summary_plot <- renderPlot({
    
    data <- week_summary(data_week_summary())
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = c("brown3","cornflowerblue"))) +
      geom_rect() +
      geom_text(x=0.5, aes(y=labelPosition, label=label,family = "AvantGarde"), size=8, colour = "gray56") +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
  })
  
  #calculates the percentage of provinces with anomalies detected
  output$percent_prov <- renderText({
    validate(
      need(input$go, ""),
    )
    percent <- nrow(modelOut()[modelOut()$status == 1,])/ nrow(modelOut()[modelOut()$status == 0,])*100
  })
  
  #outputs the trend plot for the weekly summary page. The plot is separated by border type and and contains 
  #information about the species makeup of these cases
  output$summary_trend_plot <- renderPlot({
    
    ggplot(data_week()) +
      aes(x = blood_draw_date, fill = result_code_detail_1) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      theme_minimal()+
      facet_wrap(vars(type_border)) + labs(y = "Malaria Case Counts", x = "Date") + scale_fill_discrete(name = "Species")
    
  })
  
  #render the summary table containing provinces with anomalies detected
  output$summary_table <- renderTable({
    
    prov_ano <- data.frame(data_week_summary()$province[data_week_summary()$status > 0])
    colnames(prov_ano) <- "Provinces with Anomalies"
    return(prov_ano)
  })
  
  output$modelPlot2 <- renderPlot({
    req(input$go > 0)
    if (use_default_cache()) {
      grid::grid.newpage()
      grid::grid.draw(.disk_cache$sir_grob)
    } else {
      print(make_map_plot(model_map(), "sir"))
    }
  }, height = 700, width = 700)

  
  
}
#rsconnect::configureApp(appName = "Malaria_Anomaly_Detection_App", account = "moru",size="xlarge")
#rsconnect::showLogs()
# Run the application 
if (.cache_build) {
  message("Building persistent cache; this step can take time and does not launch Shiny.")
  latest_date <- as.POSIXct(.cache_defaults$date)
  prev_time <- lubridate::weeks(.cache_defaults$weeks)
  prov <- SIR_EV(total_data, prev_time, latest_date)
  default_result <- find_anomaly(prev_time, latest_date, .cache_defaults$method,
                                 prov, temp_data_long, precip_data_long, "V")
  # Validate results before storing: never persist a failed or empty computation.
  stopifnot(is.data.frame(default_result), nrow(default_result) > 0L,
            all(c("province", "status", "SIR", "cases") %in% names(default_result)),
            !anyDuplicated(default_result$province),
            all(default_result$status %in% c(0, 1)))
  geometry <- get_thai_geometry()
  default_map <- attach_map_values(default_result, geometry)
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
  message("Cache built: ", normalizePath(.cache_path),
          "\nRestart R, then run shiny::runApp().")
} else {
  shinyApp(ui = ui, server = server)
} 