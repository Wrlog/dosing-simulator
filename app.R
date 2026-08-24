# ============================================================================
# Piperacillin Pharmacokinetic Simulation Dashboard - Version 2
# Enhanced UI with improved aesthetics and additional information
# ============================================================================

# Data import
library(haven)
library(tidyverse)
library(mrgsolve) 
library(knitr)
library(shiny)
library(shinydashboard)
library(DT)

# library(rsconnect)  # Only needed for deployment

# The mrgsolve model lives in models/Piperacillin.cpp, resolved relative to this file.
mod1 <- mread("Piperacillin", project = "models")

# Custom CSS for enhanced aesthetics
custom_css <- tags$head(
  tags$style(HTML("
    /* Main body styling */
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
    }
    
    /* Sidebar styling */
    .sidebar {
      background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
      color: white;
      box-shadow: 2px 0 10px rgba(0,0,0,0.1);
    }
    
    .sidebar .shiny-input-container {
      color: white;
      margin-bottom: 20px;
    }
    
    .sidebar .shiny-input-container label {
      color: white !important;
      font-weight: bold;
      font-size: 14px;
      margin-bottom: 8px;
      display: block;
    }
    
    .sidebar h3 {
      color: #ecf0f1;
      font-weight: 600;
      margin-bottom: 20px;
      border-bottom: 2px solid #3498db;
      padding-bottom: 10px;
    }
    
    /* Slider styling */
    .irs-bar {
      background: linear-gradient(to bottom, #3498db 0%, #2980b9 100%);
      border: none;
    }
    
    .irs-single {
      background: #3498db;
      border: none;
      color: white;
    }
    
    .irs-handle {
      border: 3px solid #3498db;
      background: white;
      cursor: pointer;
    }
    
    .irs-handle:hover {
      border-color: #2980b9;
    }
    
    .irs-handle.state_hover {
      border-color: #2980b9;
    }
    
    /* Ensure slider inputs are visible and functional */
    .shiny-input-container {
      margin-bottom: 0;
      width: 100% !important;
    }
    
    .form-group {
      margin-bottom: 0;
      width: 100% !important;
    }
    
    /* Sidebar inputs container */
    .sidebar-inputs {
      position: relative;
      z-index: 1;
    }
    
    /* Ensure sliders are clickable and visible */
    .irs {
      position: relative;
      display: block !important;
      -webkit-touch-callout: none;
      -webkit-user-select: none;
      -moz-user-select: none;
      -ms-user-select: none;
      user-select: none;
      width: 100% !important;
      height: 40px !important;
      z-index: 10 !important;
    }
    
    .irs-slider {
      cursor: pointer !important;
      z-index: 11 !important;
      position: absolute !important;
    }
    
    .irs-handle {
      cursor: pointer !important;
      cursor: grab !important;
      z-index: 12 !important;
      position: absolute !important;
      width: 20px !important;
      height: 20px !important;
      top: 20px !important;
      background: white !important;
      border: 3px solid #3498db !important;
      border-radius: 50% !important;
    }
    
    .irs-handle:active {
      cursor: grabbing !important;
    }
    
    /* Make sure slider bar is visible */
    .irs-bar {
      display: block !important;
      position: absolute !important;
      width: 100% !important;
      height: 4px !important;
      top: 25px !important;
      background: linear-gradient(to bottom, #3498db 0%, #2980b9 100%) !important;
      z-index: 9 !important;
    }
    
    .irs-line {
      display: block !important;
      position: absolute !important;
      width: 100% !important;
      height: 4px !important;
      top: 25px !important;
      background: #ecf0f1 !important;
      z-index: 8 !important;
    }
    
    /* Value boxes */
    .value-box {
      border-radius: 10px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      transition: transform 0.2s;
    }
    
    .value-box:hover {
      transform: translateY(-2px);
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
    }
    
    /* Info boxes */
    .info-box {
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Plot container */
    .plot-container {
      background: white;
      border-radius: 10px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }
    
    /* Table styling */
    .dataTables_wrapper {
      background: white;
      border-radius: 8px;
      padding: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Header styling */
    .main-header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 20px;
      border-radius: 0;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    .main-header .logo {
      font-size: 24px;
      font-weight: bold;
    }
    
    /* Content area */
    .content-wrapper {
      background: transparent;
    }
    
    /* Section headers */
    h2 {
      color: #2c3e50;
      font-weight: 600;
      margin-top: 20px;
      margin-bottom: 15px;
    }
  "))
)

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      tags$span("Piperacillin", style = "font-weight: bold; font-size: 24px;"),
      tags$span(" PK Simulation Dashboard", style = "font-size: 18px; opacity: 0.9;")
    ),
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    custom_css,
    sidebarMenu(
      menuItem("Simulation Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Input controls - using sidebarPanel approach
    tags$div(
      class = "sidebar-inputs",
      style = "padding: 20px; padding-top: 10px;",
      
      tags$h3("Simulation Parameters", style = "color: white; margin-bottom: 20px; font-size: 16px;"),
      
      # Creatinine Clearance input
      sliderInput("CRCL", 
                 "Creatinine Clearance (mL/min/1.73m²)",
                 min = 10, max = 140, value = 140, step = 10,
                 ticks = TRUE,
                 width = "100%"),
      
      # Weight input
      sliderInput("WT", 
                 "Body Weight (kg)",
                 min = 10, max = 100, value = 20, step = 10,
                 ticks = TRUE,
                 width = "100%"),
      
      # Dose input
      sliderInput("dose", 
                 "Dose (mg/kg)",
                 min = 5, max = 100, value = 55, step = 5,
                 ticks = TRUE,
                 width = "100%"),
      
      # Dosing information display
      tags$div(
        style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; margin-top: 20px;",
        tags$h4("Dosing Regimen", style = "color: white; margin-bottom: 10px; font-size: 14px;"),
        tags$p("Interval: Q2H (every 2 hours)", style = "color: #ecf0f1; font-size: 12px; margin: 5px 0;"),
        tags$p("Infusion Duration: 30 minutes", style = "color: #ecf0f1; font-size: 12px; margin: 5px 0;"),
        tags$p("Total Doses: 13 doses over 24h", style = "color: #ecf0f1; font-size: 12px; margin: 5px 0;")
      )
    )
  ),
  
  # Body
  dashboardBody(
    custom_css,
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        
        # Summary value boxes
        fluidRow(
          valueBoxOutput("pta_8_box", width = 3),
          valueBoxOutput("pta_32_box", width = 3),
          valueBoxOutput("cl_box", width = 3),
          valueBoxOutput("dose_total_box", width = 3)
        ),
        
        # Main plot
        fluidRow(
          box(
            title = tags$div(
              tags$strong("Piperacillin Concentration-Time Profile"),
              tags$span(" (n=1000 virtual patients)", style = "color: #7f8c8d; font-size: 14px; font-weight: normal;")
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            tags$div(class = "plot-container",
              plotOutput("main_plot", height = "600px")
            )
          )
        ),
        
        # Additional information boxes
        fluidRow(
          box(
            title = "Target Attainment Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            tags$div(
              style = "padding: 15px;",
              tags$p(
                tags$strong("1× MIC Target (8 mg/L):"),
                tags$span(" Minimum inhibitory concentration threshold for efficacy.", 
                         style = "color: #7f8c8d;")
              ),
              tags$p(
                tags$strong("4× MIC Target (32 mg/L):"),
                tags$span(" Higher threshold for enhanced efficacy.", 
                         style = "color: #7f8c8d;")
              ),
              tags$hr(),
              tags$p(
                tags$em("Note: "),
                "Probability of Target Attainment (PTA) is calculated based on trough concentrations 
                (minimum concentrations) at steady state (after 20 hours).",
                style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;"
              )
            )
          ),
          
          box(
            title = "Simulation Details",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            tags$div(
              style = "padding: 15px;",
              tags$p(
                tags$strong("Virtual Patients: "),
                "1,000 patients simulated"
              ),
              tags$p(
                tags$strong("Simulation Duration: "),
                "24 hours"
              ),
              tags$p(
                tags$strong("Time Resolution: "),
                "0.1 hours"
              ),
              tags$p(
                tags$strong("Body Weight Range: "),
                textOutput("wt_range", inline = TRUE)
              ),
              tags$p(
                tags$strong("CrCl Range: "),
                textOutput("crcl_range", inline = TRUE)
              ),
              tags$hr(),
              tags$p(
                tags$strong("Clearance (CL): "),
                textOutput("cl_value", inline = TRUE),
                " L/h"
              )
            )
          )
        ),
        
        # Detailed statistics table
        fluidRow(
          box(
            title = "Detailed Statistics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput("stats_table")
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        box(
          title = "About This Application",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          tags$div(
            style = "padding: 20px;",
            tags$h3("Piperacillin Pharmacokinetic Simulation Dashboard"),
            tags$p(
              "This interactive dashboard simulates piperacillin pharmacokinetics in pediatric patients 
              using a population pharmacokinetic model. The simulation allows you to explore how different 
              patient characteristics (body weight, renal function) and dosing regimens affect drug 
              exposure and target attainment."
            ),
            tags$h4("Key Features:"),
            tags$ul(
              tags$li("Monte Carlo simulation of 1,000 virtual patients"),
              tags$li("Real-time visualization of concentration-time profiles"),
              tags$li("Probability of Target Attainment (PTA) calculations"),
              tags$li("90% prediction intervals for concentration profiles"),
              tags$li("Interactive parameter adjustment")
            ),
            tags$h4("Model Information:"),
            tags$p(
              "The pharmacokinetic model accounts for:"),
            tags$ul(
              tags$li("Body weight effects on clearance and volume of distribution"),
              tags$li("Creatinine clearance effects on drug elimination"),
              tags$li("Inter-individual variability in pharmacokinetic parameters")
            ),
            tags$h4("Dosing Regimen:"),
            tags$p(
              "Q2H dosing: Piperacillin administered every 2 hours via 30-minute intravenous infusion."
            ),
            tags$hr(),
            tags$p(
              tags$em("Version 2.0 - Enhanced UI and Additional Features"),
              style = "color: #7f8c8d; font-size: 12px;"
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to run simulation
  run_simulation <- reactive({
    set.seed(123)
    vp <- data.frame(ID = c(1:1000))
    dose <- input$dose
    
    # Generate patient dataset
    vp <- mutate(vp, WT = runif(1000, min = input$WT, max = input$WT+9))
    vp <- mutate(vp, CRCL = runif(1000, min = input$CRCL, max = input$CRCL+9))
    
    # Create dosing information
    # IMPORTANT: Must include WT and CRCL in the dataframe for mrgsolve to use them in the model
    df <- vp %>%
      transmute(
        C = rep("", 1000),
        ID, 
        TIME = rep(0, 1000),
        AMT = WT * dose,
        DV = rep(0, 1000),
        CMT = rep(1, 1000),
        EVID = rep(1, 1000),
        RATE = (WT * dose) / 0.5,
        MDV = rep(1, 1000),
        OCC = rep(1, 1000),
        ADDL = rep(12, 1000),
        II = rep(2, 1000),
        WT = WT,      # Include WT for model calculations
        CRCL = CRCL  # Include CRCL for model calculations
      ) %>%
      arrange(ID, TIME, desc(EVID))
    
    set.seed(123)
    sim_out <- mod1 %>%
      data_set(df) %>%
      mrgsim(start = 0, end = 24, delta = 0.1)
    
    sim_out_df <- as_tibble(sim_out) %>% filter(EVID == 0)
    
    # Calculate trough concentrations
    d_summary2 <- sim_out_df %>%
      filter(TIME > 20) %>%
      filter(EVID == 0) %>%
      group_by(ID) %>% 
      summarize(trough = min(CP)) %>%
      ungroup()
    
    d_summary2 <- d_summary2 %>%
      mutate(
        above_target_8 = if_else(trough > 8, TRUE, FALSE),
        above_target_32 = if_else(trough > 32, TRUE, FALSE)
      )
    
    # Calculate PTA
    pta_8 <- mean(d_summary2$above_target_8)
    pta_32 <- mean(d_summary2$above_target_32)
    
    # Calculate summary statistics
    cl_mean <- mean(sim_out_df$CL)
    
    # Calculate additional statistics
    summary_stats <- sim_out_df %>%
      filter(TIME > 20) %>%
      summarize(
        median_trough = median(CP),
        mean_trough = mean(CP),
        q5_trough = quantile(CP, 0.05),
        q95_trough = quantile(CP, 0.95),
        min_trough = min(CP),
        max_trough = max(CP)
      )
    
    list(
      sim_out_df = sim_out_df,
      d_summary2 = d_summary2,
      pta_8 = pta_8,
      pta_32 = pta_32,
      cl_mean = cl_mean,
      summary_stats = summary_stats,
      wt_min = input$WT,
      wt_max = input$WT + 9,
      crcl_min = input$CRCL,
      crcl_max = input$CRCL + 9,
      dose_total = input$dose * input$WT
    )
  })
  
  # Value boxes
  output$pta_8_box <- renderValueBox({
    sim_data <- run_simulation()
    pta <- sim_data$pta_8 * 100
    
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.1f", pta), style = "font-size: 36px; font-weight: bold;"),
        tags$span("%", style = "font-size: 24px;")
      ),
      subtitle = tags$div(
        tags$strong("PTA for 1× MIC (8 mg/L)"),
        tags$br(),
        tags$span("Trough concentration target", style = "font-size: 11px;")
      ),
      icon = icon("check-circle"),
      color = ifelse(pta >= 90, "green", ifelse(pta >= 70, "yellow", "red")),
      width = NULL
    )
  })
  
  output$pta_32_box <- renderValueBox({
    sim_data <- run_simulation()
    pta <- sim_data$pta_32 * 100
    
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.1f", pta), style = "font-size: 36px; font-weight: bold;"),
        tags$span("%", style = "font-size: 24px;")
      ),
      subtitle = tags$div(
        tags$strong("PTA for 4× MIC (32 mg/L)"),
        tags$br(),
        tags$span("Enhanced efficacy target", style = "font-size: 11px;")
      ),
      icon = icon("star"),
      color = ifelse(pta >= 90, "green", ifelse(pta >= 70, "yellow", "red")),
      width = NULL
    )
  })
  
  output$cl_box <- renderValueBox({
    sim_data <- run_simulation()
    
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.2f", sim_data$cl_mean), style = "font-size: 36px; font-weight: bold;"),
        tags$span(" L/h", style = "font-size: 20px;")
      ),
      subtitle = tags$div(
        tags$strong("Mean Clearance"),
        tags$br(),
        tags$span("Population average", style = "font-size: 11px;")
      ),
      icon = icon("tint"),
      color = "blue",
      width = NULL
    )
  })
  
  output$dose_total_box <- renderValueBox({
    sim_data <- run_simulation()
    
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.0f", sim_data$dose_total), style = "font-size: 36px; font-weight: bold;"),
        tags$span(" mg", style = "font-size: 20px;")
      ),
      subtitle = tags$div(
        tags$strong("Total Dose per Administration"),
        tags$br(),
        tags$span(sprintf("%.0f mg/kg × %.0f kg", input$dose, input$WT), 
                 style = "font-size: 11px;")
      ),
      icon = icon("syringe"),
      color = "purple",
      width = NULL
    )
  })
  
  # Text outputs
  output$wt_range <- renderText({
    sim_data <- run_simulation()
    sprintf("%.1f - %.1f kg", sim_data$wt_min, sim_data$wt_max)
  })
  
  output$crcl_range <- renderText({
    sim_data <- run_simulation()
    sprintf("%.0f - %.0f mL/min/1.73m²", sim_data$crcl_min, sim_data$crcl_max)
  })
  
  output$cl_value <- renderText({
    sim_data <- run_simulation()
    sprintf("%.2f", sim_data$cl_mean)
  })
  
  # Main plot
  output$main_plot <- renderPlot({
    sim_data <- run_simulation()
    sim_out_df <- sim_data$sim_out_df
    
    # Summarize simulation output
    d_summary <- sim_out_df %>%
      group_by(TIME) %>%
      summarize(
        med = median(CP, na.rm = TRUE),
        min5 = quantile(CP, 0.05, na.rm = TRUE),
        max95 = quantile(CP, 0.95, na.rm = TRUE),
        q25 = quantile(CP, 0.25, na.rm = TRUE),
        q75 = quantile(CP, 0.75, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create enhanced plot
    g <- ggplot(d_summary, aes(x = TIME)) +
      # 90% prediction interval
      geom_ribbon(aes(ymin = min5, ymax = max95), 
                  fill = "#3498db", alpha = 0.15, 
                  color = NA) +
      # 50% prediction interval
      geom_ribbon(aes(ymin = q25, ymax = q75), 
                  fill = "#2980b9", alpha = 0.25, 
                  color = NA) +
      # Median line
      geom_line(aes(y = med), color = "#2c3e50", size = 1.5, linetype = "solid") +
      # Target lines
      geom_hline(yintercept = 8, color = "#e74c3c", linetype = "dashed", 
                 linewidth = 1.2, alpha = 0.8) +
      geom_hline(yintercept = 32, color = "#c0392b", linetype = "dashed", 
                 linewidth = 1.2, alpha = 0.8) +
      # Labels for target lines
      annotate("text", x = 23, y = 8.5, label = "1× MIC (8 mg/L)", 
               hjust = 1, color = "#e74c3c", size = 4.5, fontface = "bold",
               bg = "white", label.padding = unit(0.3, "lines")) +
      annotate("text", x = 23, y = 33, label = "4× MIC (32 mg/L)", 
               hjust = 1, color = "#c0392b", size = 4.5, fontface = "bold",
               bg = "white", label.padding = unit(0.3, "lines")) +
      # Axis labels and title
      labs(
        y = "Piperacillin Concentration (mg/L)", 
        x = "Time (hours)",
        title = "Population Pharmacokinetic Simulation: Concentration-Time Profile",
        subtitle = "Median (solid line) with 50% (dark blue) and 90% (light blue) prediction intervals"
      ) +
      # Y-axis scale
      scale_y_continuous(
        limits = c(0, 200), 
        breaks = seq(0, 200, 25),
        expand = expansion(c(0, 0.02))
      ) +
      # X-axis scale
      scale_x_continuous(
        limits = c(0, 24), 
        breaks = seq(0, 24, 2),
        expand = expansion(c(0, 0.02))
      ) +
      # Theme
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                  margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                     margin = margin(b = 15)),
        axis.title = element_text(size = 13, face = "bold", color = "#2c3e50"),
        axis.text = element_text(size = 11, color = "#34495e"),
        panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.5),
        panel.grid.minor = element_line(color = "#f8f9fa", linewidth = 0.3),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "#34495e", linewidth = 0.8),
        axis.ticks = element_line(color = "#34495e", linewidth = 0.6),
        axis.ticks.length = unit(0.3, "cm"),
        plot.margin = margin(15, 15, 15, 15)
      )
    
    print(g)
  })
  
  # Statistics table
  output$stats_table <- DT::renderDataTable({
    sim_data <- run_simulation()
    
    stats_df <- data.frame(
      Metric = c(
        "PTA for 1× MIC (8 mg/L)",
        "PTA for 4× MIC (32 mg/L)",
        "Mean Clearance (L/h)",
        "Median Trough Concentration (mg/L)",
        "Mean Trough Concentration (mg/L)",
        "5th Percentile Trough (mg/L)",
        "95th Percentile Trough (mg/L)",
        "Minimum Trough (mg/L)",
        "Maximum Trough (mg/L)"
      ),
      Value = c(
        sprintf("%.1f%%", sim_data$pta_8 * 100),
        sprintf("%.1f%%", sim_data$pta_32 * 100),
        sprintf("%.2f", sim_data$cl_mean),
        sprintf("%.2f", sim_data$summary_stats$median_trough),
        sprintf("%.2f", sim_data$summary_stats$mean_trough),
        sprintf("%.2f", sim_data$summary_stats$q5_trough),
        sprintf("%.2f", sim_data$summary_stats$q95_trough),
        sprintf("%.2f", sim_data$summary_stats$min_trough),
        sprintf("%.2f", sim_data$summary_stats$max_trough)
      ),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      stats_df,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c("Metric", "Value")
    ) %>%
      DT::formatStyle(
        "Metric",
        fontWeight = "bold",
        color = "#2c3e50"
      ) %>%
      DT::formatStyle(
        "Value",
        color = "#34495e"
      )
  })
}

# Create Shiny app
shinyApp(ui, server)

