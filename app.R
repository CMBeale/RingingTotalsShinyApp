Sys.setlocale("LC_ALL", "en_GB.UTF-8")
Sys.setenv(LANG = "en_GB.UTF-8")

library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(htmlwidgets)
library(gridExtra)
library(cowplot)
library(lme4)
library(shiny)

options(shiny.maxRequestSize = 15 * 1024^2)

# Source helper functions
if (file.exists("ReadRecovery.R")) {
  source("ReadRecovery.R")
}

# Helper function to convert dataframe to spatial line vector
df_to_lines <- function(df, x = "lon", y = "lat", groupvar = "ring") {
  groups <- unique(df[[groupvar]])
  xy <- as.matrix(df[, c(x, y)])
  
  xy_tmp <- xy[df[[groupvar]] == groups[1], ]
  df_tmp <- df[df[[groupvar]] == groups[1] & !is.na(df$Duration), ]
  lines_vect <- vect(xy_tmp, type = "lines", atts = df_tmp)
  
  if (length(groups) > 1) {
    for (g in groups[-1]) {
      df_tmp <- df[df[[groupvar]] == g & !is.na(df$Duration), ]
      xy_tmp <- xy[df[[groupvar]] == g, ]
      if (nrow(xy_tmp) >= 2) {
        lines_vect <- rbind(lines_vect, vect(xy_tmp, type = "lines", atts = df_tmp))
      }
    }
  }
  return(lines_vect)
}

# Helper function for CES Imputation (handles multiple missing visits)
impute_and_adjust <- function(df, missing_visits) {
  if (is.null(missing_visits) || nrow(missing_visits) == 0) {
    return(df)
  }
  
  missing_records <- data.frame(
    year = missing_visits$year - 2000,
    session = missing_visits$visit,
    count = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(missing_visits))) {
    missing_year <- missing_visits$year[i] - 2000
    missing_visit <- missing_visits$visit[i]
    tmp <- df[df$year == missing_year, ]
    tmp1 <- tmp[tmp$session < missing_visit, ]
    tmp2 <- tmp[tmp$session > missing_visit, ]
    
    tmp <- rbind(tmp1,
                 data.frame(year = missing_year, session = missing_visit, count = NA),
                 tmp2)
    df <- rbind(df[df$year != missing_year, ], tmp)
  }
  
  impute_model <- lme4::glmer(count ~ (1 | session) + (1 | year),
                              data = df,
                              family = poisson)
  
  imputed_values <- predict(impute_model, missing_records, type = "response")
  
  for (i in seq_len(nrow(missing_records))) {
    m_yr <- missing_records$year[i]
    m_sess <- missing_records$session[i]
    df$count[df$year == m_yr & df$session == m_sess & is.na(df$count)] <- imputed_values[i]
  }
  
  return(df)
}

# Default site definitions
default_hes_sites <- c(
  'HEESR (Hes East Short Reed)',
  'HEEOW (Hes East Old Woodland)', 
  'HEELR (Hes East Long Reed)', 
  'HEEFW (Heslington East Far Woodland)',
  'HEELF (Hes East Long Feeder)', 
  'HEELL (Heslington East Low Lane)', 
  'HEENW (Heaslington East New Woodland)',
  'HENRb4 (Hes East Box 4)', 
  'HENRb2 (Hes East Nest Box 2)', 
  'HENRb3 (Hes East Box 3)',
  'HEENRb1 (Heslington East Nature Reserve (refined))',
  'HEENR (Heslington East Nature Reserve)',
  'HEEDT (Duck Trap)', 
  'HEEKW (Heslington East Kimberlow Hill Wood)', 
  'CWUOY (Campus West UoY)',
  'HEEAWE (Heslington East, West Square)', 
  'UNIEAS (York Uni Heslington West, East square)',
  'UNIWES (York Uni Heslington West, West Square)',
  'UNIWES (York Uni Heslington West, West Square (refined))',
  'HEEAEA (Heslington East, East Square)'
)

default_ces_sites <- c(
  'HEESR (Hes East Short Reed)',
  'HEEOW (Hes East Old Woodland)',
  'HEELR (Hes East Long Reed)',
  'HEEFW (Heslington East Far Woodland)',
  'HEELF (Hes East Long Feeder)',
  'HEENW (Heaslington East New Woodland)'
)

# --- UI DEFINITION ---
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Ringing Summaries"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Setup", tabName = "upload", icon = icon("database")),
      menuItem("Recovery Map", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Annual Summaries", tabName = "summaries", icon = icon("table")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat")),
      menuItem("CES Analyses", tabName = "chart-line", icon = icon("chart-line")),
      menuItem("Ringer Totals", tabName = "ringers", icon = icon("user-check"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .table-responsive-container {
          overflow-x: auto;
          max-width: 100%;
          margin-bottom: 15px;
        }
        .table-responsive-container table {
          width: 100% !important;
          white-space: nowrap;
        }
        .summary-text-p {
          font-size: 16px;
          line-height: 1.6;
          margin-bottom: 8px;
        }
      "))
    ),
    tabItems(
      # --- TAB 1: DATA SETUP ---
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "1. Main & Extra Data Selection", width = 6, status = "primary", solidHeader = TRUE,
            radioButtons(
              "data_mode", 
              "Select Main Data Source Mode:",
              choices = c(
                "Use Default Datasets (AllRecords.csv & ExtraRings.csv)" = "default",
                "Upload Custom Data Files" = "upload"
              ),
              selected = "default"
            ),
            conditionalPanel(
              condition = "input.data_mode == 'upload'",
              fileInput("file_main", "Choose Main Dataset (AllRecords.csv)", accept = ".csv"),
              fileInput("file_extras", "Choose Extra Dataset (ExtraRings.csv)", accept = ".csv")
            )
          ),
          
          box(
            title = "2. Recoveries Folder Selection", width = 6, status = "primary", solidHeader = TRUE,
            radioButtons(
              "rec_folder_mode",
              "Select Recoveries Directory:",
              choices = c(
                "Use Default 'Recoveries' Folder" = "default",
                "Specify Custom Local Folder Path" = "custom"
              ),
              selected = "default"
            ),
            conditionalPanel(
              condition = "input.rec_folder_mode == 'custom'",
              textInput("custom_rec_path", "Folder Path:", value = getwd())
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "success",
            actionButton("btn_process", "Load & Process All Datasets", class = "btn-success btn-lg", icon = icon("play"))
          )
        )
      ),
      
      # --- TAB 2: RECOVERY MAP ---
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 3, status = "warning", title = "Map Controls",
            numericInput("focal_year", "Focal Year:", value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2030)
          ),
          box(
            width = 9, status = "primary",
            leafletOutput("map_leaflet", height = "650px")
          )
        )
      ),
      
      # --- TAB 3: ANNUAL SUMMARIES ---
      tabItem(
        tabName = "summaries",
        fluidRow(
          box(
            width = 12, status = "warning", title = "Summary Parameters", solidHeader = TRUE,
            fluidRow(
              column(
                width = 3,
                numericInput("summary_focal_year", "Focal Year:", value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2030)
              ),
              column(
                width = 5,
                radioButtons(
                  "summary_site_mode", "Site Filtering Mode:",
                  choices = c(
                    "Use All Sites (No filtering)" = "all",
                    "Use Default Heslington Sites" = "default",
                    "Select Custom Sites from Dataset" = "custom"
                  ),
                  selected = "default"
                ),
                conditionalPanel(
                  condition = "input.summary_site_mode == 'custom'",
                  uiOutput("ui_summary_site_select")
                )
              ),
              column(
                width = 4,
                p(strong("Download CSV Outputs:")),
                downloadButton("dl_summary_all", "All Summaries CSV", class = "btn-sm btn-info"),
                br(), br(),
                downloadButton("dl_summary_recent", "Recent CSV", class = "btn-sm btn-primary"),
                br(), br(),
                downloadButton("dl_summary_older", "Older CSV", class = "btn-sm btn-secondary")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "success", title = "Annual Overview", solidHeader = TRUE,
            uiOutput("ui_summary_text")
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "primary", title = "Recent Species Summary (Focal/Previous Year Captures)",
            div(class = "table-responsive-container",
                tableOutput("table_summary_recent")
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "info", title = "Older Species Summary (Historical Captures Only)",
            fluidRow(
              column(
                width = 6,
                div(class = "table-responsive-container",
                    tableOutput("table_summary_older_1")
                )
              ),
              column(
                width = 6,
                div(class = "table-responsive-container",
                    tableOutput("table_summary_older_2")
                )
              )
            )
          )
        )
      ),
      
      # --- TAB: SURVIVAL ANALYSIS ---
      tabItem(
        tabName = "survival",
        fluidRow(
          box(
            width = 12, status = "warning", title = "Survival Parameters", solidHeader = TRUE,
            fluidRow(
              column(
                width = 3,
                numericInput("survival_focal_year", "Focal Year:", value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2030)
              ),
              column(
                width = 9,
                radioButtons(
                  "survival_site_mode", "Site Filtering Mode:",
                  choices = c(
                    "Use All Sites (No filtering)" = "all",
                    "Use Default Heslington Sites" = "default",
                    "Select Custom Sites from Dataset" = "custom"
                  ),
                  selected = "default"
                ),
                conditionalPanel(
                  condition = "input.survival_site_mode == 'custom'",
                  uiOutput("ui_survival_site_select")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", title = "1. All-Time Max Longevity per Species (Full Historical Record)", solidHeader = TRUE,
            p("Shows the individual bird with the longest recorded elapsed lifespan for each species calculated across all historical records in the dataset."),
            downloadButton("dl_survival_alltime", "Download All-Time Longevity CSV", class = "btn-sm btn-info"),
            br(), br(),
            div(class = "table-responsive-container",
                tableOutput("table_survival_alltime")
            )
          )
        ),
        fluidRow(
          box(
            width = 12, status = "success", title = "2. Focal Year Max Longevity (Retraps in Selected Year)", solidHeader = TRUE,
            p("Shows birds retrapped in the selected focal year with the longest duration between their original ringing date (from any year) and their retrap date in the focal year."),
            downloadButton("dl_survival_focal", "Download Focal Year Longevity CSV", class = "btn-sm btn-info"),
            br(), br(),
            div(class = "table-responsive-container",
                tableOutput("table_survival_focal")
            )
          )
        )
      ),
      
      # --- TAB 4: CES ANALYSES ---
      tabItem(
        tabName = "chart-line",
        fluidRow(
          column(
            width = 3,
            box(
              width = 12, status = "primary", title = "1. CES Site Selection", solidHeader = TRUE,
              radioButtons(
                "ces_site_mode", "Select Site Mode:",
                choices = c("Use Default CES Sites" = "default", "Custom Site Selection" = "custom"),
                selected = "default"
              ),
              conditionalPanel(
                condition = "input.ces_site_mode == 'custom'",
                uiOutput("ui_ces_site_select")
              )
            ),
            box(
              width = 12, status = "warning", title = "2. CES Missing Visits Imputation", solidHeader = TRUE,
              p("Select a year and visit to add to the missing visits queue for statistical GLMM imputation:"),
              uiOutput("ui_ces_year_select"),
              selectInput("ces_visit_select", "Visit / Session Number:", choices = 1:12, selected = 4),
              actionButton("btn_add_missing", "Add Missing Visit", class = "btn-primary btn-sm", icon = icon("plus")),
              actionButton("btn_clear_missing", "Clear All", class = "btn-danger btn-sm", icon = icon("trash")),
              hr(),
              p(strong("Configured Missing Visits:")),
              tableOutput("table_missing_visits")
            )
          ),
          column(
            width = 9,
            tabBox(
              width = 12, id = "ces_tabs",
              tabPanel("1. Cumulative Unique", plotOutput("plot_ces_1", height = "550px")),
              tabPanel("2. Cumulative Captures", plotOutput("plot_ces_2", height = "550px")),
              tabPanel("3. Recapture Proportion", plotOutput("plot_ces_3", height = "550px")),
              tabPanel("4. Raw Captures", plotOutput("plot_ces_4", height = "550px")),
              tabPanel("5. Top 10 Totals", plotOutput("plot_ces_5", height = "550px")),
              tabPanel("6. Top 10 Productivity", plotOutput("plot_ces_6", height = "650px"))
            )
          )
        )
      ),
      
      # --- TAB 5: RINGER TOTALS ---
      tabItem(
        tabName = "ringers",
        fluidRow(
          box(
            width = 4, status = "primary", title = "Select Ringer & Options",
            uiOutput("ui_ringer_select"),
            br(),
            numericInput("ringer_focal_year", "Focal Year (for Focal CSV):", 
                         value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2030),
            p(strong("Download Ringer CSVs:")),
            downloadButton("dl_ringer_summary", "Summary Totals CSV", class = "btn-sm btn-success"),
            br(), br(),
            downloadButton("dl_ringer_all", "All Records CSV", class = "btn-sm btn-info"),
            shiny::span(" "),
            downloadButton("dl_ringer_focal", "Focal Year CSV", class = "btn-sm btn-primary")
          ),
          box(
            width = 8, status = "info", title = "Ringer Summary",
            uiOutput("ui_ringer_text"),
            hr(),
            div(class = "table-responsive-container",
                tableOutput("table_ringer")
            )
          )
        )
      )
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    all_dat = NULL,
    recoveries_raw = NULL,
    processed = FALSE
  )
  
  rv_missing_visits <- reactiveVal(data.frame(year = 2025, visit = 4, stringsAsFactors = FALSE))
  
  output$ui_summary_site_select <- renderUI({
    req(rv$processed, rv$all_dat)
    sites <- sort(unique(rv$all_dat$location_name))
    selectizeInput(
      "selected_summary_sites",
      "Select Custom Sites:",
      choices = sites,
      selected = intersect(default_hes_sites, sites),
      multiple = TRUE,
      options = list(placeholder = "Select one or more sites...")
    )
  })
  
  output$ui_survival_site_select <- renderUI({
    req(rv$processed, rv$all_dat)
    sites <- sort(unique(rv$all_dat$location_name))
    selectizeInput(
      "selected_survival_sites",
      "Select Custom Sites:",
      choices = sites,
      selected = intersect(default_hes_sites, sites),
      multiple = TRUE,
      options = list(placeholder = "Select one or more sites...")
    )
  })
  
  observeEvent(input$btn_process, {
    all_dat <- NULL
    extras <- NULL
    
    if (input$data_mode == "default") {
      main_path <- "AllRecords.csv"
      extras_path <- "ExtraRings.csv"
      
      if (!file.exists(main_path)) {
        showNotification("Error: Default 'AllRecords.csv' not found.", type = "error")
        return()
      }
      all_dat <- read.csv(main_path, stringsAsFactors = FALSE)
      if (file.exists(extras_path)) extras <- read.csv(extras_path, stringsAsFactors = FALSE)
      
    } else {
      req(input$file_main)
      all_dat <- read.csv(input$file_main$datapath, stringsAsFactors = FALSE)
      if (!is.null(input$file_extras)) extras <- read.csv(input$file_extras$datapath, stringsAsFactors = FALSE)
    }
    
    if (any(names(all_dat) == "loc_id")) names(all_dat)[which(names(all_dat) == "loc_id")] <- "location_name"
    
    if (!is.null(extras)) {
      if (any(names(extras) == "loc_id")) names(extras)[which(names(extras) == "loc_id")] <- "location_name"
      extras$species_name <- gsub("(?<!\\w)(.)", "\\U\\1", extras$species_name, perl = TRUE)
      shared_names <- names(extras)[names(extras) %in% names(all_dat)]
      all_dat <- rbind(all_dat[, shared_names], extras[, shared_names])
    }
    
    all_dat$record_type <- as.factor(all_dat$record_type)
    all_dat$raw_age <- as.character(all_dat$age)
    all_dat$age <- as.factor(gsub('J', '', all_dat$age))
    
    all_dat$initials <- all_dat$ringer_initials
    all_dat$initials[is.na(all_dat$initials) | all_dat$initials == '-'] <- 
      all_dat$processor_initials[is.na(all_dat$initials) | all_dat$initials == '-']
    
    all_dat$date_time <- as.POSIXct(paste(all_dat$visit_date, all_dat$capture_time), format = '%d/%m/%Y %H:%M')
    all_dat$visit_date <- as.Date(all_dat$visit_date, "%d/%m/%Y")
    all_dat$year <- as.numeric(format(all_dat$date_time, "%Y"))
    
    all_dat$initials[all_dat$initials == 'CC']  <- 'CAC'
    all_dat$initials[all_dat$initials == 'RL']  <- 'RJL'
    all_dat$initials[all_dat$initials == 'NP']  <- 'NMP'
    all_dat$initials[all_dat$initials == 'LJM'] <- 'LCM'
    all_dat$initials[all_dat$initials == 'VJM'] <- 'VJB'
    all_dat$initials[all_dat$initials == 'VMB'] <- 'VJB'
    all_dat$initials[all_dat$initials == 'CB']  <- 'CMB'
    
    all_dat$initials[all_dat$initials == 'KH' & all_dat$visit_date < as.Date("2017-01-01")] <- 'KCH'
    all_dat$initials[all_dat$initials == 'B1' | all_dat$initials == '-'] <- 'CMB'
    
    if (file.exists("AviList-v2025.csv")) {
      AviList_names <- read.csv('AviList-v2025.csv', stringsAsFactors = FALSE)
      BTO_names <- data.frame(
        'BTO' = as.character(unique(all_dat$species_name)),
        'AviList_names' = NA_character_,
        stringsAsFactors = FALSE
      )
      
      for(i in seq_len(nrow(BTO_names))){
        bto_name <- BTO_names$BTO[i]
        bto_opts <- c(bto_name,
                      paste("Common",   bto_name),
                      paste("Eurasian", bto_name),
                      paste("European", bto_name),
                      paste("Northern", bto_name),
                      paste("Western",  bto_name))
        avi_name <- NA_character_
        for (col_name in c("English_name_AviList",
                           "English_name_Clements_v2024",
                           "English_name_BirdLife_v9")) {
          if (col_name %in% names(AviList_names)) {
            opts <- bto_opts %in% AviList_names[[col_name]]
            if (any(opts)) {
              avi_name <- AviList_names$English_name_AviList[AviList_names[[col_name]] == bto_opts[opts]][1]
              break
            }
          }
        }
        BTO_names$AviList_names[i] <- avi_name
      }
      
      BTO_names <- BTO_names %>%
        mutate(AviList_names = case_when(
          BTO == 'Swallow' ~ 'Barn Swallow',
          BTO == 'Guillemot' ~ 'Common Murre',
          BTO == 'Knot' ~ 'Red Knot',
          BTO == 'Turnstone' ~ 'Ruddy Turnstone',
          BTO == 'Teal' ~ 'Green-winged Teal',
          BTO == 'Peregrine' ~ 'Peregrine Falcon',
          BTO == 'Pied/White Wagtail' ~ 'White Wagtail',
          BTO == "Pallas's Warbler" ~ "Pallas's Leaf Warbler",
          BTO == "Bearded Tit" ~ "Bearded Reedling",
          BTO == "Waxwing" ~ "Bohemian Waxwing",
          grepl("Redpoll", BTO) ~ "Redpoll",
          BTO == "Common Crossbill" ~ "Red Crossbill",
          BTO == "Lapland Bunting" ~ "Lapland Longspur",
          BTO == "Subalpine (Eastern/Western) / Moltoni's Warbler" ~ "Eastern Subalpine Warbler",
          BTO == "Cormorant" ~ "Great Cormorant",
          BTO == "Kittiwake" ~ "Black-legged Kittiwake",
          BTO == "Avocet" ~ "Pied Avocet",
          BTO == "Lesser Whitethroat (halimodendri)" ~ "Lesser Whitethroat",
          BTO == "Shore Lark" ~ "Horned Lark",
          BTO == "Lesser Black-backed Gull (graellsii)" ~ "Lesser Black-backed Gull",
          BTO == "Chiffchaff (Siberian - tristis)" ~ "Common Chiffchaff",
          BTO == "Redwing (iliacus)" ~ "Redwing",
          BTO == "Lesser Black-backed Gull (intermedius)" ~ "Lesser Black-backed Gull",
          BTO == "Puffin" ~ "Atlantic Puffin",
          TRUE ~ AviList_names
        ))
      
      BTO_names_enriched <- merge(
        BTO_names, AviList_names,
        by.x = "AviList_names",
        by.y = "English_name_AviList",
        all.x = TRUE
      )
      
      cols_to_merge <- c("AviList_names", "BTO", "Order", "Sequence", "Family", "Scientific_name")
      cols_to_merge <- cols_to_merge[cols_to_merge %in% names(BTO_names_enriched)]
      
      all_dat <- merge(
        all_dat,
        BTO_names_enriched[, cols_to_merge],
        by.x = "species_name", by.y = "BTO", all.x = TRUE
      )
    }
    
    rec_dir <- if (input$rec_folder_mode == "default") file.path(getwd(), "Recoveries") else input$custom_rec_path
    if (dir.exists(rec_dir)) {
      recovery_files <- dir(rec_dir, pattern = 'html', full.names = TRUE)
      if (length(recovery_files) > 0 && exists("read_recovery")) {
        recoveries <- read_recovery(recovery_files[1])
        if (length(recovery_files) > 1) {
          for (f in recovery_files[2:length(recovery_files)]) {
            recoveries <- rbind(recoveries, read_recovery(f))
          }
        }
        Has_rows <- grep("Dunnington", recoveries$Site)
        if (length(Has_rows) > 0) recoveries <- recoveries[!recoveries$Ring_no %in% recoveries$Ring_no[Has_rows], ]
        
        recoveries$scheme <- NA
        names(recoveries)[c(4:5, 7)] <- c('lat', 'lon', 'ring')
        recoveries$year <- as.numeric(format(recoveries$Date, "%Y"))
        recoveries$subs <- recoveries$Event != 'Ringed'
        recoveries$dist <- as.numeric(gsub('[^0-9]', '', recoveries$Distance)) / 10
        
        recoveries$lon[grep('Heslington East', recoveries$Site)]  <- -1.037568
        recoveries$lat[grep('Heslington East', recoveries$Site)]  <- 53.946226 
        recoveries$lon[grep('Campus East', recoveries$Site)]  <- -1.037568
        recoveries$lat[grep('Campus East', recoveries$Site)]  <- 53.946226 
        recoveries$lon[grep('Kimberlow Hill', recoveries$Site)]  <- -1.019766
        recoveries$lat[grep('Kimberlow Hill', recoveries$Site)]  <- 53.952833
        recoveries$lon[grep('Low Lane', recoveries$Site)]  <- -1.027799
        recoveries$lat[grep('Low Lane', recoveries$Site)]  <- 53.944957
        recoveries$lon[grep('Village Velodrome', recoveries$Site)]  <- -1.017148
        recoveries$lat[grep('Village Velodrome', recoveries$Site)]  <- 53.947921
        recoveries$lon[grep("Monk's Cross", recoveries$Site)]  <- -1.056145
        recoveries$lat[grep("Monk's Cross", recoveries$Site)]  <- 53.978352
        
        rv$recoveries_raw <- recoveries
      }
    }
    
    rv$all_dat <- all_dat
    rv$processed <- TRUE
    showNotification("All data successfully loaded and processed!", type = "message")
  })
  
  output$ui_ces_site_select <- renderUI({
    req(rv$processed)
    sites_in_data <- sort(unique(rv$all_dat$location_name))
    selectizeInput(
      "selected_ces_sites",
      "Select Sites to Include in CES Analysis:",
      choices = sites_in_data,
      selected = intersect(default_ces_sites, sites_in_data),
      multiple = TRUE,
      options = list(placeholder = "Select CES sites...")
    )
  })
  
  output$map_leaflet <- renderLeaflet({
    req(rv$processed, rv$recoveries_raw)
    recoveries <- rv$recoveries_raw
    yr <- input$focal_year
    
    recoveries$FocalYear <- recoveries$year == yr
    rings_in_focal <- unique(recoveries$ring[recoveries$FocalYear])
    recoveries$FocalYear[recoveries$ring %in% rings_in_focal] <- TRUE
    recoveries <- recoveries[!is.na(recoveries$lat) & !is.na(recoveries$lon), ]
    
    req(nrow(recoveries) > 0)
    
    recovery_lines <- df_to_lines(recoveries)
    recovery_lines$FocalYear <- recovery_lines$year == yr
    rings_in_focal_lines <- unique(recovery_lines$ring[recovery_lines$FocalYear])
    recovery_lines$FocalYear[recovery_lines$ring %in% rings_in_focal_lines] <- TRUE
    recovery_lines_new <- recovery_lines[recovery_lines$FocalYear, ]
    
    dist_york <- as.matrix(dist(rbind(recoveries[, c("lat", "lon")], c(53.94623, -1.0375680))))
    recoveries$dist_york <- dist_york[-nrow(dist_york), nrow(dist_york)]
    recoveries$FirstCapture <- recoveries$Distance == "0km"
    recoveries$days <- as.numeric(gsub(" days", "", recoveries$Duration))
    recoveries$days[is.na(recoveries$days)] <- recoveries$days[!is.na(recoveries$days)]
    recoveries$years <- recoveries$days / 365.25
    recoveries$tidy <- ifelse(
      floor(recoveries$years) == 0,
      paste(round(recoveries$days), "days"),
      paste(floor(recoveries$years), "years,", round(recoveries$days - floor(recoveries$years) * 365.25), "days")
    )
    
    focal_recoveries_df <- recoveries[recoveries$FocalYear == TRUE & recoveries$dist_york > 0.02, ]
    recoveries_first <- focal_recoveries_df[focal_recoveries_df$FirstCapture, ]
    recoveries_subseq <- focal_recoveries_df[!focal_recoveries_df$FirstCapture, ]
    
    circleIcon <- makeIcon(iconUrl = "http://clipart-library.com/images/6Tp5aB97c.png", iconWidth = 18, iconHeight = 18)
    triangleIcon <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/red-triangle-png-20.png", iconWidth = 18, iconHeight = 18)
    
    crs(recovery_lines) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
    crs(recovery_lines_new) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
    
    m <- leaflet() %>%
      setView(-1, 53.8, zoom = 8.5) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", group = "world") %>%
      addPolylines(data = recovery_lines, color = 'blue', opacity = 0.25) %>%
      addPolylines(data = recovery_lines_new, color = 'red')
    
    if (nrow(recoveries_first) > 0) {
      m <- m %>% addMarkers(
        data = recoveries_first, lng = ~lon, lat = ~lat, icon = circleIcon,
        popup = ~paste0(Species, " ", ring, "<br/>Originally ringed: ", format(Date, format = "%d %B %Y"), "<br/>Duration: ", tidy)
      )
    }
    if (nrow(recoveries_subseq) > 0) {
      m <- m %>% addMarkers(
        data = recoveries_subseq, lng = ~lon, lat = ~lat, icon = triangleIcon,
        popup = ~paste0(Species, " ", ring, "<br/>Subsequently found: ", format(Date, format = "%d %B %Y"), "<br/>Duration: ", tidy)
      )
    }
    m
  })
  
  summary_tables <- reactive({
    req(rv$processed)
    df <- rv$all_dat
    
    focal_yr <- input$summary_focal_year
    prev_yr <- focal_yr - 1
    
    if (input$summary_site_mode == "default") {
      df_filtered <- df %>% filter(location_name %in% default_hes_sites)
    } else if (input$summary_site_mode == "custom") {
      req(input$selected_summary_sites)
      df_filtered <- df %>% filter(location_name %in% input$selected_summary_sites)
    } else {
      df_filtered <- df
    }
    
    if ("Sequence" %in% names(df_filtered)) {
      species_info <- df_filtered %>%
        group_by(species_name) %>%
        summarise(seq_val = suppressWarnings(min(Sequence, na.rm = TRUE)), .groups = "drop") %>%
        mutate(seq_val = ifelse(is.infinite(seq_val), 99999, seq_val)) %>%
        arrange(seq_val, species_name)
      all_species <- species_info$species_name
    } else {
      all_species <- sort(unique(df_filtered$species_name))
    }
    
    dat_tab <- data.frame(Species = all_species, stringsAsFactors = FALSE) %>%
      rowwise() %>%
      mutate(
        Total_previous = as.integer(sum(df_filtered$species_name == Species & df_filtered$year == prev_yr, na.rm = TRUE)),
        First_year = as.integer(sum(df_filtered$species_name == Species & df_filtered$year == focal_yr & df_filtered$record_type == 'N' & df_filtered$age %in% c('1', '3', 'J'), na.rm = TRUE)),
        Adult = as.integer(sum(df_filtered$species_name == Species & df_filtered$year == focal_yr & df_filtered$record_type == 'N' & !df_filtered$age %in% c('1', '3', 'J'), na.rm = TRUE)),
        New_current = as.integer(First_year + Adult),
        Retraps_current = as.integer(sum(df_filtered$species_name == Species & df_filtered$year == focal_yr & df_filtered$record_type != 'N', na.rm = TRUE)),
        Total_current = as.integer(New_current + Retraps_current),
        Total_new = as.integer(sum(df_filtered$species_name == Species & df_filtered$record_type == 'N', na.rm = TRUE))
      ) %>%
      ungroup()
    
    annual_summary <- dat_tab %>%
      select(Species, Total_previous, First_year, Adult, New_current, Retraps_current, Total_current, Total_new)
    
    num_cols <- colSums(annual_summary[, 2:8])
    totals_row <- data.frame(
      Species = "Totals",
      Total_previous = as.integer(num_cols[1]),
      First_year = as.integer(num_cols[2]),
      Adult = as.integer(num_cols[3]),
      New_current = as.integer(num_cols[4]),
      Retraps_current = as.integer(num_cols[5]),
      Total_current = as.integer(num_cols[6]),
      Total_new = as.integer(num_cols[7]),
      stringsAsFactors = FALSE
    )
    
    annual_summary_with_totals <- rbind(annual_summary, totals_row)
    colnames(annual_summary_with_totals) <- c("Species", "Total_previous", "First_year", "Adult", "New_current", "Retraps_current", "Total_current", "Total_new")
    
    recent_mask <- (annual_summary_with_totals$Total_previous + annual_summary_with_totals$Total_current) > 0
    recent_mask[nrow(annual_summary_with_totals)] <- TRUE
    
    annual_summary_recent <- annual_summary_with_totals[recent_mask, ]
    
    annual_summary_older_full <- annual_summary[!(annual_summary$Total_previous + annual_summary$Total_current) > 0, ]
    annual_summary_older_2col  <- annual_summary_older_full %>% select(Species, Total_ringed = Total_new)
    
    n_older <- nrow(annual_summary_older_2col)
    mid_idx <- ceiling(n_older / 2)
    
    older_half1 <- if (n_older > 0) annual_summary_older_2col[1:mid_idx, ] else annual_summary_older_2col
    older_half2 <- if (n_older > 1) annual_summary_older_2col[(mid_idx + 1):n_older, ] else data.frame(Species = character(0), Total_ringed = integer(0))
    
    list(
      raw_dat_tab = dat_tab, filtered_df = df_filtered, all = annual_summary_with_totals,
      recent = annual_summary_recent, older_full = annual_summary_older_full,
      older_1 = older_half1, older_2 = older_half2
    )
  })
  
  output$ui_summary_text <- renderUI({
    req(rv$processed)
    sum_data <- summary_tables()
    dat_tab <- sum_data$raw_dat_tab
    df_filtered <- sum_data$filtered_df
    yr <- input$summary_focal_year
    
    N_sp        <- sum(dat_tab$Total_current > 0)
    N_ringers   <- NROW(unique(df_filtered$initials[df_filtered$year == yr]))
    N_sessions  <- NROW(unique(df_filtered$visit_date[df_filtered$year == yr]))
    N_birds     <- sum(dat_tab$Total_current)
    N_new_birds <- sum(dat_tab$New_current)
    
    new_sp_mask <- (dat_tab$New_current >= dat_tab$Total_new) & (dat_tab$New_current > 0)
    N_new_sp    <- sum(new_sp_mask)
    New_species <- dat_tab$Species[new_sp_mask]
    
    p1 <- paste("In", yr, N_sessions, "ringing sessions were undertaken at the selected sites, involving",
                N_ringers, "ringers and making a total of", N_birds,
                "captures, including", N_new_birds, "newly ringed birds of", N_sp, "species.")
    
    p2 <- if (N_new_sp > 0) {
      paste0("These include ", N_new_sp, " species ringed for the first time in ", yr, ": ",
             paste0(New_species, collapse = ", "), ".")
    } else {
      paste0("No new species were ringed for the first time in ", yr, ".")
    }
    
    p3 <- paste("You have now ringed a total of", sum(dat_tab$Total_new), "birds of",
                sum(dat_tab$Total_new > 0), "species at these sites.")
    
    tagList(
      p(class = "summary-text-p", p1),
      p(class = "summary-text-p", p2),
      p(class = "summary-text-p", p3)
    )
  })
  
  output$table_summary_recent <- renderTable({ summary_tables()$recent }, rownames = FALSE, digits = 0)
  output$table_summary_older_1 <- renderTable({ summary_tables()$older_1 }, rownames = FALSE, digits = 0)
  output$table_summary_older_2 <- renderTable({ summary_tables()$older_2 }, rownames = FALSE, digits = 0)
  
  output$dl_summary_all <- downloadHandler(
    filename = function() { "AnnualSummariesAll.csv" },
    content = function(file) { write.csv(summary_tables()$all, file, row.names = FALSE) }
  )
  
  output$dl_summary_recent <- downloadHandler(
    filename = function() { "AnnualSummariesRecent.csv" },
    content = function(file) { write.csv(summary_tables()$recent, file, row.names = FALSE) }
  )
  
  output$dl_summary_older <- downloadHandler(
    filename = function() { "AnnualSummariesOlder.csv" },
    content = function(file) { write.csv(summary_tables()$older_full, file, row.names = FALSE) }
  )
  
  # --- SURVIVAL ANALYSIS: TABLE 1 (ALL-TIME MAX LONGEVITY) ---
  survival_alltime_table <- reactive({
    req(rv$processed)
    all_dat <- rv$all_dat
    
    if (input$survival_site_mode == "default") {
      all_dat <- all_dat %>% filter(location_name %in% default_hes_sites)
    } else if (input$survival_site_mode == "custom") {
      req(input$selected_survival_sites)
      all_dat <- all_dat %>% filter(location_name %in% input$selected_survival_sites)
    }
    
    req(nrow(all_dat) > 0)
    
    # Calculate earliest and latest capture per bird, excluding zero-day durations
    bird_durations <- all_dat %>%
      group_by(ring_no, species_name) %>%
      summarise(
        first = min(date_time, na.rm = TRUE),
        last = max(date_time, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        days = as.numeric(round(difftime(last, first, units = 'days'))),
        ring = ring_no,
        species = species_name
      ) %>%
      filter(days > 0)
    
    req(nrow(bird_durations) > 0)
    
    max_age_mat <- bird_durations %>%
      group_by(species) %>%
      slice_max(order_by = days, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        years = days / 365.25,
        tidy = paste(floor(years), "years,", round(days - floor(years) * 365.25), "days")
      ) %>%
      arrange(desc(last))
    
    return(as.data.frame(max_age_mat, stringsAsFactors = FALSE))
  })
  
  output$table_survival_alltime <- renderTable({
    df <- survival_alltime_table()
    df %>%
      mutate(
        First_Capture = format(first, "%Y-%m-%d"),
        Last_Capture = format(last, "%Y-%m-%d")
      ) %>%
      select(Species = species, Ring = ring, Lifespan = tidy, Days = days, First_Capture, Last_Capture)
  }, digits = 0)
  
  output$dl_survival_alltime <- downloadHandler(
    filename = function() { "SurvivalAnalysis_AllTime_MaxAge.csv" },
    content = function(file) { write.csv(survival_alltime_table(), file, row.names = FALSE) }
  )
  
  # --- SURVIVAL ANALYSIS: TABLE 2 (FOCAL YEAR RETRAPS MAX LONGEVITY) ---
  survival_focal_table <- reactive({
    req(rv$processed)
    focal_yr <- input$survival_focal_year
    all_dat <- rv$all_dat
    
    if (input$survival_site_mode == "default") {
      all_dat <- all_dat %>% filter(location_name %in% default_hes_sites)
    } else if (input$survival_site_mode == "custom") {
      req(input$selected_survival_sites)
      all_dat <- all_dat %>% filter(location_name %in% input$selected_survival_sites)
    }
    
    req(nrow(all_dat) > 0)
    
    # Filter birds retrapped in focal year
    focal_retraps <- all_dat %>% filter(year == focal_yr, record_type != 'N')
    req(nrow(focal_retraps) > 0)
    
    focal_rings <- unique(focal_retraps$ring_no)
    
    # Original ringing date across all years in dataset
    first_dates <- all_dat %>%
      filter(ring_no %in% focal_rings) %>%
      group_by(ring_no) %>%
      summarise(first = min(date_time, na.rm = TRUE), .groups = "drop")
    
    # Last retrap date in focal year
    last_dates <- focal_retraps %>%
      group_by(ring_no, species_name) %>%
      summarise(last = max(date_time, na.rm = TRUE), .groups = "drop")
    
    bird_durations <- inner_join(last_dates, first_dates, by = "ring_no") %>%
      mutate(
        days = as.numeric(round(difftime(last, first, units = 'days'))),
        ring = ring_no,
        species = species_name
      ) %>%
      filter(days > 0)
    
    req(nrow(bird_durations) > 0)
    
    max_age_mat <- bird_durations %>%
      group_by(species) %>%
      slice_max(order_by = days, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        years = days / 365.25,
        tidy = paste(floor(years), "years,", round(days - floor(years) * 365.25), "days")
      ) %>%
      arrange(desc(last))
    
    return(as.data.frame(max_age_mat, stringsAsFactors = FALSE))
  })
  
  output$table_survival_focal <- renderTable({
    df <- survival_focal_table()
    df %>%
      mutate(
        First_Capture = format(first, "%Y-%m-%d"),
        Last_Capture = format(last, "%Y-%m-%d")
      ) %>%
      select(Species = species, Ring = ring, Lifespan = tidy, Days = days, First_Capture, Last_Capture)
  }, digits = 0)
  
  output$dl_survival_focal <- downloadHandler(
    filename = function() { paste0("SurvivalAnalysis_Focal_", input$survival_focal_year, "_MaxAge.csv") },
    content = function(file) { write.csv(survival_focal_table(), file, row.names = FALSE) }
  )
  
  # --- RINGER TOTALS LOGIC ---
  output$ui_ringer_select <- renderUI({
    req(rv$processed)
    selectInput("selected_ringer", "Ringer Initials:", choices = sort(unique(rv$all_dat$initials)))
  })
  
  output$ui_ringer_text <- renderUI({
    req(rv$processed, input$selected_ringer)
    
    df_ringer <- rv$all_dat %>% filter(initials == input$selected_ringer)
    req(nrow(df_ringer) > 0)
    
    total_birds <- nrow(df_ringer)
    total_sp <- n_distinct(df_ringer$species_name)
    
    df_new <- df_ringer %>% filter(record_type == 'N')
    new_birds <- nrow(df_new)
    new_sp <- n_distinct(df_new$species_name)
    
    df_retrap <- df_ringer %>% filter(record_type != 'N')
    retrap_birds <- nrow(df_retrap)
    retrap_sp <- n_distinct(df_retrap$species_name)
    
    summary_msg <- sprintf(
      "Ringer %s has processed a total of %d birds of %d species, including %d new birds of %d species, and %d retraps of %d species.",
      input$selected_ringer, total_birds, total_sp, new_birds, new_sp, retrap_birds, retrap_sp
    )
    
    p(class = "summary-text-p", summary_msg)
  })
  
  ringer_summary_df <- reactive({
    req(rv$processed, input$selected_ringer)
    df_ringer <- rv$all_dat %>%
      filter(initials == input$selected_ringer) %>%
      group_by(species_name, record_type) %>%
      summarise(Count = as.integer(n()), .groups = "drop") %>%
      pivot_wider(names_from = record_type, values_from = Count, values_fill = 0L)
    
    numeric_cols <- names(df_ringer)[names(df_ringer) != "species_name"]
    col_totals <- colSums(df_ringer[, numeric_cols, drop = FALSE], na.rm = TRUE)
    
    totals_row <- as.data.frame(t(c(species_name = "Total", col_totals)), stringsAsFactors = FALSE)
    for (col in numeric_cols) {
      totals_row[[col]] <- as.integer(totals_row[[col]])
    }
    
    bind_rows(df_ringer, totals_row)
  })
  
  output$table_ringer <- renderTable({
    ringer_summary_df()
  }, digits = 0)
  
  output$dl_ringer_summary <- downloadHandler(
    filename = function() {
      paste0("Ringer_", input$selected_ringer, "_SummaryTotals.csv")
    },
    content = function(file) {
      write.csv(ringer_summary_df(), file, row.names = FALSE)
    }
  )
  
  output$dl_ringer_all <- downloadHandler(
    filename = function() {
      paste0("Ringer_", input$selected_ringer, "_AllRecords.csv")
    },
    content = function(file) {
      req(rv$processed, input$selected_ringer)
      data_export <- rv$all_dat %>% filter(initials == input$selected_ringer)
      write.csv(data_export, file, row.names = FALSE)
    }
  )
  
  output$dl_ringer_focal <- downloadHandler(
    filename = function() {
      paste0("Ringer_", input$selected_ringer, "_Year_", input$ringer_focal_year, ".csv")
    },
    content = function(file) {
      req(rv$processed, input$selected_ringer, input$ringer_focal_year)
      data_export <- rv$all_dat %>% filter(initials == input$selected_ringer & year == input$ringer_focal_year)
      write.csv(data_export, file, row.names = FALSE)
    }
  )
  
  # --- CES CALCULATIONS & DYNAMIC MISSING VISITS ---
  
  # UI element to select years present in dataset
  output$ui_ces_year_select <- renderUI({
    if (rv$processed && !is.null(rv$all_dat)) {
      yr_choices <- sort(unique(rv$all_dat$year), decreasing = TRUE)
    } else {
      yr_choices <- 2025:2010
    }
    selectInput("ces_year_select", "Select Year:", choices = yr_choices, selected = max(yr_choices))
  })
  
  # Add Missing Visit
  observeEvent(input$btn_add_missing, {
    req(input$ces_year_select, input$ces_visit_select)
    new_entry <- data.frame(
      year = as.numeric(input$ces_year_select),
      visit = as.numeric(input$ces_visit_select),
      stringsAsFactors = FALSE
    )
    current <- rv_missing_visits()
    if (!any(current$year == new_entry$year & current$visit == new_entry$visit)) {
      rv_missing_visits(rbind(current, new_entry))
    }
  })
  
  # Clear Missing Visits
  observeEvent(input$btn_clear_missing, {
    rv_missing_visits(data.frame(year = numeric(0), visit = numeric(0), stringsAsFactors = FALSE))
  })
  
  # Output missing visits summary table
  output$table_missing_visits <- renderTable({
    df <- rv_missing_visits()
    if (nrow(df) == 0) {
      return(data.frame(Status = "No missing visits set"))
    }
    colnames(df) <- c("Year", "Session / Visit")
    df
  }, digits = 0)
  
  # Dynamic CES Filtering & Calculations
  ces_processed <- reactive({
    req(rv$processed)
    all_dat <- rv$all_dat
    missing_visits <- rv_missing_visits()
    
    # Filter by user selected CES sites
    ces_sites_to_use <- if (input$ces_site_mode == "default") {
      default_ces_sites
    } else {
      input$selected_ces_sites
    }
    
    req(length(ces_sites_to_use) > 0)
    ces_dat <- all_dat[all_dat$location_name %in% ces_sites_to_use, ]
    req(nrow(ces_dat) > 0)
    
    ces_dat$count <- 1
    ces_dat$visit_date[ces_dat$visit_date == as.Date("2024-07-07")] <- as.Date("2024-07-08")
    ces_dat$visit_date[ces_dat$visit_date == as.Date("2024-04-28")] <- as.Date("2024-05-07")
    
    ces_dat$day <- as.numeric(format(ces_dat$visit_date, format = "%d"))
    ces_dat$month <- as.numeric(format(ces_dat$visit_date, format = "%m"))
    ces_dat <- ces_dat[(ces_dat$month > 4 | (ces_dat$month == 4 & ces_dat$day > 26)) &
                         (ces_dat$month < 9 | (ces_dat$month == 9 & ces_dat$day < 4)),]
    ces_dat <- ces_dat[order(ces_dat$date_time),]
    
    date_to_session <- data.frame(visit_date = sort(unique(ces_dat$visit_date)))
    date_to_session$year <- as.numeric(format(date_to_session$visit_date, "%y"))
    date_to_session$session <- ave(rep(1, NROW(date_to_session)), date_to_session$year, FUN = seq_along)
    
    if (nrow(missing_visits) > 0) {
      for(i in 1:NROW(missing_visits)){
        tmp <- date_to_session[date_to_session$year == missing_visits$year[i]-2000,]
        tmp1 <- tmp[tmp$session %in% 1:(missing_visits$visit[i]-1),]
        tmp2 <- tmp[tmp$session %in% missing_visits$visit[i]: NROW(unique(tmp$session)),]
        tmp2$session <- tmp2$session+1
        tmp <- rbind(tmp1, tmp2)
        date_to_session <- rbind(tmp, date_to_session[date_to_session$year != missing_visits$year[i]-2000,])
      }
    }
    
    ces_dat$year <- as.numeric(format(ces_dat$visit_date, "%y"))
    
    ces_captures <- merge(ces_dat, date_to_session, by = c("visit_date", "year"))
    
    ces_individuals <- ces_dat[!duplicated(ces_dat[, c("ring_no", "year")]),]
    ces_individuals <- merge(ces_individuals, date_to_session, by = c("visit_date", "year"))
    
    ces_summary_captures <- aggregate(ces_captures["count"], by = list(year = ces_captures$year, session = ces_captures$session), FUN = "sum")
    names(ces_summary_captures)[3] <- "count"
    ces_summary_captures_imputed <- impute_and_adjust(ces_summary_captures, missing_visits)
    
    ces_summary_individuals <- aggregate(ces_individuals["count"], by = list(year = ces_individuals$year, session = ces_individuals$session), FUN = "sum")
    names(ces_summary_individuals)[3] <- "count"
    ces_summary_individuals_imputed <- impute_and_adjust(ces_summary_individuals, missing_visits)
    
    ces_summary_juv_captures <- aggregate(ces_captures[ces_captures$age %in% c("3", "3J"), "count"],
                                          by = list(year = ces_captures$year[ces_captures$age %in% c("3", "3J")], session = ces_captures$session[ces_captures$age %in% c("3", "3J")]), FUN = "sum")
    names(ces_summary_juv_captures)[3] <- "count"
    ces_summary_juv_captures_imputed <- impute_and_adjust(ces_summary_juv_captures, missing_visits)
    
    ces_summary_juv_individuals <- aggregate(ces_individuals[ces_individuals$age %in% c("3", "3J"), "count"],
                                             by = list(year = ces_individuals$year[ces_individuals$age %in% c("3", "3J")], session = ces_individuals$session[ces_individuals$age %in% c("3", "3J")]), FUN = "sum")
    names(ces_summary_juv_individuals)[3] <- "count"
    ces_summary_juv_individuals_imputed <- impute_and_adjust(ces_summary_juv_individuals, missing_visits)
    
    ces_summary_recaptures <- aggregate(ces_captures[ces_captures$record_type == "S", "count"],
                                        by = list(year = ces_captures$year[ces_captures$record_type == "S"], session = ces_captures$session[ces_captures$record_type == "S"]), FUN = "sum")
    names(ces_summary_recaptures)[3] <- "count"
    ces_summary_recaptures_imputed <- impute_and_adjust(ces_summary_recaptures, missing_visits)
    names(ces_summary_recaptures_imputed)[3] <- "recaptures"
    
    list(
      missing_visits = missing_visits,
      ces_captures = ces_captures,
      ces_individuals = ces_individuals,
      ces_summary_captures_imputed = ces_summary_captures_imputed,
      ces_summary_individuals_imputed = ces_summary_individuals_imputed,
      ces_summary_juv_captures_imputed = ces_summary_juv_captures_imputed,
      ces_summary_juv_individuals_imputed = ces_summary_juv_individuals_imputed,
      ces_summary_recaptures_imputed = ces_summary_recaptures_imputed
    )
  })
  
  # Helper to check if a row is inside missing visits
  check_imputed <- function(data_year, data_session, missing_df) {
    if (is.null(missing_df) || nrow(missing_df) == 0) return(rep(FALSE, length(data_year)))
    m_pairs <- paste(missing_df$year - 2000, missing_df$visit, sep = "_")
    d_pairs <- paste(data_year, data_session, sep = "_")
    d_pairs %in% m_pairs
  }
  
  # CES Plot 1
  output$plot_ces_1 <- renderPlot({
    d <- ces_processed()
    missing_visits <- d$missing_visits
    plot1_data <- merge(d$ces_summary_individuals_imputed, d$ces_summary_juv_individuals_imputed, by = c("year", "session"), all = TRUE)
    names(plot1_data) <- c("year", "session", "total_ind", "juv_ind")
    plot1_data[is.na(plot1_data)] <- 0
    plot1_data$total_ind_cum <- ave(plot1_data$total_ind, plot1_data$year, FUN=cumsum)
    plot1_data$juv_ind_cum <- ave(plot1_data$juv_ind, plot1_data$year, FUN=cumsum)
    plot1_data$Year <- as.factor(paste0("20", plot1_data$year))
    plot1_data$is_imputed <- check_imputed(plot1_data$year, plot1_data$session, missing_visits)
    
    plot1_data_long <- rbind(
      data.frame(year = plot1_data$year, session = plot1_data$session, Year = plot1_data$Year,
                 Cumulative_Count = plot1_data$total_ind_cum, Category = "Total Individuals", 
                 is_imputed = plot1_data$is_imputed, stringsAsFactors = FALSE),
      data.frame(year = plot1_data$year, session = plot1_data$session, Year = plot1_data$Year,
                 Cumulative_Count = plot1_data$juv_ind_cum, Category = "Juveniles", 
                 is_imputed = plot1_data$is_imputed, stringsAsFactors = FALSE)
    )
    
    # Explicitly force factor with defined levels so scale_shape_manual maps cleanly
    plot1_data_long$Data_Type <- factor(ifelse(plot1_data_long$is_imputed, "Imputed", "Observed"), 
                                        levels = c("Observed", "Imputed"))
    
    ggplot(plot1_data_long, aes(x = session, y = Cumulative_Count, group = paste(Year, Category), colour = Year, linetype = Category)) +
      geom_line() +
      geom_point(aes(shape = Data_Type), size = 2.5, stroke = 1.1) +
      scale_shape_manual(values = c("Imputed" = 1, "Observed" = 19), name = "Data Type", labels = c("Observed", "Imputed")) +
      scale_x_continuous(breaks = 1:12) +
      xlab("Session") + ylab("Cumulative Sum of Unique Individuals") +
      ggtitle("Cumulative Unique Individuals per Season") +
      scale_linetype_manual(values = c("dashed", "solid")) +
      theme_minimal()
  })
  
  # CES Plot 2
  output$plot_ces_2 <- renderPlot({
    d <- ces_processed()
    missing_visits <- d$missing_visits
    plot2_data <- merge(d$ces_summary_captures_imputed, d$ces_summary_juv_captures_imputed, by = c("year", "session"), all = TRUE)
    names(plot2_data) <- c("year", "session", "total_captures", "juv_captures")
    plot2_data[is.na(plot2_data)] <- 0
    plot2_data$total_captures_cum <- ave(plot2_data$total_captures, plot2_data$year, FUN=cumsum)
    plot2_data$juv_captures_cum <- ave(plot2_data$juv_captures, plot2_data$year, FUN=cumsum)
    plot2_data$Year <- as.factor(paste0("20", plot2_data$year))
    plot2_data$is_imputed <- check_imputed(plot2_data$year, plot2_data$session, missing_visits)
    
    plot2_data_long <- rbind(
      data.frame(year = plot2_data$year, session = plot2_data$session, Year = plot2_data$Year,
                 Cumulative_Count = plot2_data$total_captures_cum, Category = "Total Captures", 
                 is_imputed = plot2_data$is_imputed, stringsAsFactors = FALSE),
      data.frame(year = plot2_data$year, session = plot2_data$session, Year = plot2_data$Year,
                 Cumulative_Count = plot2_data$juv_captures_cum, Category = "Juvenile Captures", 
                 is_imputed = plot2_data$is_imputed, stringsAsFactors = FALSE)
    )
    
    
    ggplot(plot2_data_long, aes(x = session, y = Cumulative_Count, group = paste(Year, Category), colour = Year, linetype = Category)) +
      geom_line() +
      geom_point(aes(shape = is_imputed), size = 2.5, stroke = 1.1) +
      scale_shape_manual(values = c("FALSE" = 19, "TRUE" = 1), name = "Data Type", labels = c("Observed", "Imputed")) +
      scale_x_continuous(breaks = 1:12) +
      xlab("Session") + ylab("Cumulative Sum of All Captures") +
      ggtitle("Cumulative All Captures per Season") +
      scale_linetype_manual(values = c("dashed", "solid")) +
      theme_minimal()
  })
  
  # CES Plot 3
  output$plot_ces_3 <- renderPlot({
    d <- ces_processed()
    missing_visits <- d$missing_visits
    plot3_data <- merge(d$ces_summary_captures_imputed, d$ces_summary_recaptures_imputed, by = c("year", "session"), all.x = TRUE)
    plot3_data$recaptures[is.na(plot3_data$recaptures)] <- 0
    plot3_data$prop_recapture <- plot3_data$recaptures / plot3_data$count
    plot3_data$Year <- as.factor(paste0("20", plot3_data$year))
    plot3_data$is_imputed <- check_imputed(plot3_data$year, plot3_data$session, missing_visits)
    
    ggplot(plot3_data, aes(session, prop_recapture, group = Year, colour = Year)) +
      geom_line() +
      geom_point(aes(shape = is_imputed), size = 2.5, stroke = 1.1) +
      scale_shape_manual(values = c("FALSE" = 19, "TRUE" = 1), name = "Data Type", labels = c("Observed", "Imputed")) +
      scale_x_continuous(breaks = 1:12) +
      xlab("Session") + ylab("Proportion of Recaptures") +
      ggtitle("Proportion of Recaptures per Session") +
      ylim(0, 1) +
      theme_minimal()
  })
  
  # CES Plot 4
  output$plot_ces_4 <- renderPlot({
    d <- ces_processed()
    missing_visits <- d$missing_visits
    plot4_data <- d$ces_summary_captures_imputed
    plot4_data$Year <- as.factor(paste0("20", plot4_data$year))
    plot4_data$is_imputed <- check_imputed(plot4_data$year, plot4_data$session, missing_visits)
    
    ggplot(plot4_data, aes(session, count, group = Year, colour = Year)) +
      geom_line() +
      geom_point(aes(shape = is_imputed), size = 2.5, stroke = 1.1) +
      scale_shape_manual(values = c("FALSE" = 19, "TRUE" = 1), name = "Data Type", labels = c("Observed", "Imputed")) +
      scale_x_continuous(breaks = 1:12) +
      ylim(0, max(plot4_data$count)) +
      xlab("Session") + ylab("Raw Captures") +
      ggtitle("Raw Captures per Session") +
      theme_minimal()
  })
  
  # CES Plot 5: Top 10 Totals
  output$plot_ces_5 <- renderPlot({
    d <- ces_processed()
    ces_individuals <- d$ces_individuals
    
    species_col <- if ("species_name" %in% names(ces_individuals)) "species_name" else "species"
    
    species_totals <- aggregate(ces_individuals$count, by = list(species = ces_individuals[[species_col]]), FUN = sum)
    names(species_totals)[2] <- "total_count"
    species_totals <- species_totals[order(species_totals$total_count, decreasing = TRUE),]
    top_10_species_order <- head(species_totals$species, 10)
    
    plot5_data_raw <- ces_individuals[ces_individuals[[species_col]] %in% top_10_species_order,]
    plot5_data_raw <- plot5_data_raw[plot5_data_raw$session <= plot5_data_raw$session[which.max(plot5_data_raw$date_time)],]
    
    all_years <- unique(plot5_data_raw$year)
    all_age_classes <- c("Adult", "Juvenile")
    
    full_grid <- expand.grid(
      year = all_years,
      species_name = top_10_species_order,
      age_class = all_age_classes,
      stringsAsFactors = FALSE
    )
    
    observed_counts <- plot5_data_raw %>%
      mutate(age_class = ifelse(age %in% c("3", "3J"), "Juvenile", "Adult")) %>%
      group_by(year, species_name = .data[[species_col]], age_class) %>%
      summarise(total_unique = n(), .groups = 'drop')
    
    plot5_data <- left_join(full_grid, observed_counts, by = c("year", "species_name", "age_class")) %>%
      mutate(total_unique = ifelse(is.na(total_unique), 0, total_unique))
    
    plot5_data$species <- factor(plot5_data$species_name, levels = top_10_species_order)
    plot5_data$Year <- as.factor(paste0("20", plot5_data$year))
    
    focal_yr_num <- max(plot5_data$year)
    
    hist_data <- plot5_data %>% filter(year < focal_yr_num)
    focal_data <- plot5_data %>% filter(year == focal_yr_num)
    
    ggplot() +
      geom_boxplot(data = hist_data, aes(x = species, y = total_unique, fill = age_class),
                   position = position_dodge(width = 0.75), width = 0.6, outlier.shape = NA, alpha = 0.5) +
      geom_point(data = focal_data, aes(x = species, y = total_unique, color = age_class),
                 position = position_dodge(width = 0.75), size = 3, stroke = 1.1) +
      scale_fill_manual(values = c("Adult" = "#377EB8", "Juvenile" = "#E41A1C"), name = "Age Class (Historical)") +
      scale_color_manual(values = c("Adult" = "#08306B", "Juvenile" = "#800000"), name = paste0("Age Class (", paste0("20", focal_yr_num), ")")) +
      xlab("Species") +
      ylab("Total Unique Individuals") +
      ggtitle("Unique Adult and Juvenile Individuals for Top 10 Species") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # CES Plot 6: Top 10 Productivity
  output$plot_ces_6 <- renderPlot({
    d <- ces_processed()
    ces_individuals <- d$ces_individuals
    
    species_col <- if ("species_name" %in% names(ces_individuals)) "species_name" else "species"
    
    species_totals <- aggregate(ces_individuals$count, by = list(species = ces_individuals[[species_col]]), FUN = sum)
    names(species_totals)[2] <- "total_count"
    species_totals <- species_totals[order(species_totals$total_count, decreasing = TRUE),]
    top_10_species_order <- head(species_totals$species, 10)
    
    plot6_data_raw <- ces_individuals[ces_individuals[[species_col]] %in% top_10_species_order,]
    plot6_data_raw <- plot6_data_raw[plot6_data_raw$session <= plot6_data_raw$session[which.max(plot6_data_raw$date_time)],]
    
    all_years <- sort(unique(plot6_data_raw$year))
    all_age_classes <- c("Adult", "Juvenile")
    
    full_grid <- expand.grid(
      year = all_years,
      species_name = top_10_species_order,
      age_class = all_age_classes,
      stringsAsFactors = FALSE
    )
    
    observed_counts <- plot6_data_raw %>%
      mutate(age_class = ifelse(age %in% c("3", "3J"), "Juvenile", "Adult")) %>%
      group_by(year, species_name = .data[[species_col]], age_class) %>%
      summarise(total_unique = n(), .groups = 'drop')
    
    plot6_data <- left_join(full_grid, observed_counts, by = c("year", "species_name", "age_class")) %>%
      mutate(total_unique = ifelse(is.na(total_unique), 0, total_unique))
    
    total_counts_data <- plot6_data %>%
      group_by(year, species_name) %>%
      summarise(Total_Individuals = sum(total_unique), .groups = 'drop')
    
    productivity_data <- plot6_data %>%
      pivot_wider(names_from = age_class, values_from = total_unique, values_fill = 0) %>%
      mutate(Productivity = ifelse(Adult > 0, Juvenile / Adult, NA_real_))
    
    merged_data <- left_join(productivity_data, total_counts_data, by = c("year", "species_name"))
    merged_data$species <- factor(merged_data$species_name, levels = top_10_species_order)
    merged_data$Year <- factor(paste0("20", merged_data$year), levels = paste0("20", all_years))
    
    create_species_plot <- function(species_name, data) {
      species_data <- data[data$species == species_name,]
      if (nrow(species_data) == 0) return(ggplot() + theme_void())
      
      max_prod <- max(species_data$Productivity, na.rm = TRUE)
      max_tot <- max(species_data$Total_Individuals, na.rm = TRUE)
      
      if (is.infinite(max_prod) || is.na(max_prod) || max_prod == 0) max_prod <- 1
      if (is.infinite(max_tot) || is.na(max_tot)) max_tot <- 1
      
      scaling_factor <- max_tot / max_prod
      if (is.infinite(scaling_factor) || is.na(scaling_factor) || scaling_factor == 0) scaling_factor <- 1
      
      ggplot(species_data, aes(x = Year)) +
        geom_col(aes(y = Total_Individuals / scaling_factor), fill = "grey", alpha = 0.5, width = 0.6) +
        geom_line(aes(y = Productivity, group = 1), color = "red", size = 1, na.rm = TRUE) +
        geom_point(aes(y = Productivity), color = "red", size = 2, na.rm = TRUE) +
        scale_x_discrete(drop = FALSE) +
        scale_y_continuous(
          name = NULL,
          sec.axis = sec_axis(~ . * scaling_factor, name = NULL)
        ) +
        labs(title = species_name) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10, hjust = 0.5)
        )
    }
    
    plot_list <- lapply(top_10_species_order, create_species_plot, data = merged_data)
    
    legend_plot <- ggplot() +
      annotate("segment", x = 1, xend = 2, y = 1.2, yend = 1.2, color = "red", size = 1) +
      annotate("point", x = 1.5, y = 1.2, color = "red", size = 2) +
      annotate("text", x = 2.8, y = 1.2, label = "Productivity", hjust = 0, size = 4) +
      annotate("rect", xmin = 1, xmax = 2, ymin = 0.8, ymax = 1, fill = "grey", alpha = 0.5) +
      annotate("text", x = 2.8, y = 0.9, label = "Total Individuals", hjust = 0, size = 4) +
      xlim(0.5, 5) + ylim(0.5, 1.5) +
      theme_void() +
      coord_cartesian(clip = "off")
    
    all_plots <- c(plot_list, list(legend_plot))
    final_plot <- plot_grid(plotlist = all_plots, ncol = 3, align = 'hv')
    
    title <- ggdraw() + draw_label("Productivity and Total Individuals for Top 10 Species", fontface = 'bold', size = 16)
    left_y_label <- ggdraw() + draw_label("Productivity (Juveniles / Adult)", angle = 90, vjust = 1, size = 12)
    right_y_label <- ggdraw() + draw_label("Total Individuals", angle = 270, vjust = 1, size = 12)
    
    plot_grid(
      title,
      plot_grid(left_y_label, final_plot, right_y_label, ncol = 3, rel_widths = c(0.05, 1, 0.05)),
      ncol = 1, rel_heights = c(0.05, 1)
    )
  })
}

shinyApp(ui = ui, server = server)