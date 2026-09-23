# ============================================================================
# Dosing Simulator
#
# Monte Carlo simulation of intravenous dosing regimens from a generic
# two-compartment population PK model. The drug is not fixed: typical PK
# parameters, covariate effects and variability are all inputs, so any
# compound described by two-compartment linear kinetics can be explored.
#
# The model specification lives in models/TwoCompartment.cpp.
# ============================================================================

# The PK engine is a closed-form solution of the two-compartment model in
# models/TwoCompartment.cpp, not a numerical integration of it. mrgsolve is
# no longer a dependency: it compiles C++ at run time, which a browser cannot
# do, and this model is linear so it has an exact solution anyway. Dropping
# it is what lets the app be published as a static page through webR.
#
# tests/test_pk_engine.R checks the engine against RK4 integration of the
# same ODEs and fails the build if they disagree.
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)

source(file.path("R", "pk_engine.R"))

cv_to_var <- function(cv_percent) log(1 + (cv_percent / 100)^2)

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

    .sidebar h3, .sidebar h4 {
      color: #ecf0f1;
      font-weight: 600;
      margin-bottom: 15px;
      border-bottom: 2px solid #3498db;
      padding-bottom: 8px;
    }

    /* Numeric inputs inside the dark sidebar */
    .sidebar input[type=number] {
      background: rgba(255,255,255,0.95);
      color: #2c3e50;
      border-radius: 4px;
      border: none;
      width: 100%;
    }

    .sidebar .radio label, .sidebar .shiny-options-group label {
      color: #ecf0f1 !important;
      font-weight: normal;
    }

    /* Slider styling */
    .irs-bar {
      background: linear-gradient(to bottom, #3498db 0%, #2980b9 100%);
      border: none;
    }

    .irs-single, .irs-from, .irs-to {
      background: #3498db;
      border: none;
      color: white;
    }

    .irs-handle {
      border: 3px solid #3498db;
      background: white;
      cursor: pointer;
    }

    .irs-handle:hover, .irs-handle.state_hover {
      border-color: #2980b9;
    }

    .shiny-input-container {
      margin-bottom: 0;
      width: 100% !important;
    }

    .form-group {
      margin-bottom: 0;
      width: 100% !important;
    }

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

    .irs-bar {
      display: block !important;
      position: absolute !important;
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

    .info-box {
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

    .plot-container {
      background: white;
      border-radius: 10px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }

    .dataTables_wrapper {
      background: white;
      border-radius: 8px;
      padding: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

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

    .content-wrapper {
      background: transparent;
    }

    h2 {
      color: #2c3e50;
      font-weight: 600;
      margin-top: 20px;
      margin-bottom: 15px;
    }
  "))
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$div(
      tags$span("Dosing", style = "font-weight: bold; font-size: 24px;"),
      tags$span(" Simulator", style = "font-size: 18px; opacity: 0.9;")
    ),
    titleWidth = 350
  ),

  dashboardSidebar(
    width = 320,
    custom_css,
    sidebarMenu(
      menuItem("Simulation", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Model setup", tabName = "setup", icon = icon("sliders")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    # The sidebar holds what gets swept during a session. Drug parameters,
    # which are set once for a given compound, live on the Model setup tab.
    tags$div(
      class = "sidebar-inputs",
      style = "padding: 18px; padding-top: 8px;",

      tags$h4("Patient population", style = "font-size: 15px;"),

      sliderInput("wt_range", "Body weight (kg)",
                  min = 1, max = 150, value = c(15, 25), step = 1,
                  width = "100%"),

      sliderInput("renal_range", "Renal function (mL/min/1.73m²)",
                  min = 5, max = 200, value = c(100, 140), step = 5,
                  width = "100%"),

      tags$h4("Dosing regimen", style = "font-size: 15px; margin-top: 10px;"),

      numericInput("dose", "Dose", value = 50, min = 0, step = 5, width = "100%"),

      radioButtons("dose_basis", NULL,
                   choices = c("per kg body weight" = "mgkg", "flat dose" = "flat"),
                   selected = "mgkg", width = "100%"),

      numericInput("interval", "Dosing interval (h)",
                   value = 8, min = 0.25, step = 1, width = "100%"),

      numericInput("infdur", "Infusion duration (h, 0 = bolus)",
                   value = 0.5, min = 0, step = 0.25, width = "100%"),

      numericInput("duration", "Simulation duration (h)",
                   value = 48, min = 1, step = 12, width = "100%"),

      tags$h4("Targets", style = "font-size: 15px; margin-top: 10px;"),

      # Defaults sit either side of the trough distribution produced by the
      # default drug and regimen, so the two value boxes land on different
      # sides of the colour thresholds rather than both reading 100%.
      numericInput("target1", "Lower target (mg/L)",
                   value = 8, min = 0, step = 1, width = "100%"),

      numericInput("target2", "Upper target (mg/L)",
                   value = 20, min = 0, step = 1, width = "100%")
    )
  ),

  dashboardBody(
    custom_css,
    tabItems(

      # --- Simulation tab ---------------------------------------------------
      tabItem(
        tabName = "dashboard",

        fluidRow(
          valueBoxOutput("pta_1_box", width = 3),
          valueBoxOutput("pta_2_box", width = 3),
          valueBoxOutput("cl_box", width = 3),
          valueBoxOutput("dose_total_box", width = 3)
        ),

        fluidRow(
          box(
            title = tags$div(
              tags$strong("Concentration-time profile"),
              tags$span(textOutput("plot_subtitle", inline = TRUE),
                        style = "color: #7f8c8d; font-size: 14px; font-weight: normal;")
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            tags$div(class = "plot-container",
                     plotOutput("main_plot", height = "600px"))
          )
        ),

        fluidRow(
          box(
            title = "Target attainment",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            tags$div(
              style = "padding: 15px;",
              uiOutput("target_description"),
              tags$hr(),
              tags$p(
                tags$em("Note: "),
                "Target attainment is the proportion of simulated subjects whose ",
                tags$strong("trough"), " concentration over the final dosing interval ",
                "stays above the target. The time above target reported in the table ",
                "is the share of that interval spent above the lower target.",
                style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;"
              )
            )
          ),

          box(
            title = "Simulation details",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            tags$div(
              style = "padding: 15px;",
              tags$p(tags$strong("Virtual subjects: "),
                     textOutput("n_subjects_text", inline = TRUE)),
              tags$p(tags$strong("Duration: "),
                     textOutput("duration_text", inline = TRUE)),
              tags$p(tags$strong("Doses given: "),
                     textOutput("n_doses_text", inline = TRUE)),
              tags$p(tags$strong("Body weight range: "),
                     textOutput("wt_range_text", inline = TRUE)),
              tags$p(tags$strong("Renal function range: "),
                     textOutput("renal_range_text", inline = TRUE)),
              tags$hr(),
              tags$p(tags$strong("Mean clearance: "),
                     textOutput("cl_value", inline = TRUE), " L/h"),
              tags$p(tags$strong("Assessment window: "),
                     textOutput("window_text", inline = TRUE))
            )
          )
        ),

        fluidRow(
          box(
            title = "Detailed statistics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput("stats_table")
          )
        )
      ),

      # --- Model setup tab --------------------------------------------------
      tabItem(
        tabName = "setup",

        fluidRow(
          box(
            title = "Disposition parameters",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$p("Typical values for a subject at the reference covariates below.",
                   style = "color: #7f8c8d;"),
            fluidRow(
              column(6, numericInput("tvcl", "CL (L/h)", value = 5, min = 0.01, step = 0.5)),
              column(6, numericInput("tvv1", "V1 (L)", value = 15, min = 0.01, step = 1))
            ),
            fluidRow(
              column(6, numericInput("tvq", "Q (L/h)", value = 3, min = 0, step = 0.5)),
              column(6, numericInput("tvv2", "V2 (L)", value = 25, min = 0.01, step = 1))
            )
          ),

          box(
            title = "Covariate model",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$p("Typical values are defined at the reference covariates; these should match whatever the source estimates were normalised to.",
                   style = "color: #7f8c8d;"),
            fluidRow(
              column(6, numericInput("wt_ref", "Reference weight (kg)",
                                     value = 70, min = 1, step = 5)),
              column(6, numericInput("renal_ref", "Reference renal function",
                                     value = 120, min = 1, step = 10))
            ),
            fluidRow(
              column(6, numericInput("renal_exp", "Renal exponent on CL",
                                     value = 0.5, min = 0, max = 2, step = 0.05)),
              column(6, tags$p(tags$em("0 removes the renal effect; 1 makes clearance proportional to the marker."),
                               style = "color: #7f8c8d; font-size: 12px; margin-top: 25px;"))
            ),
            tags$hr(),
            checkboxInput("allometric", "Allometric weight scaling (0.75 on flows, 1 on volumes)",
                          value = TRUE)
          )
        ),

        fluidRow(
          box(
            title = "Variability",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            tags$p("Entered as coefficients of variation; converted to log-scale variances internally.",
                   style = "color: #7f8c8d;"),
            fluidRow(
              column(6, numericInput("iiv_cl", "Between-subject CV on CL (%)",
                                     value = 30, min = 0, max = 200, step = 5)),
              column(6, numericInput("iiv_v1", "Between-subject CV on V1 (%)",
                                     value = 25, min = 0, max = 200, step = 5))
            ),
            fluidRow(
              column(6, numericInput("iov_cl", "Between-occasion CV on CL (%)",
                                     value = 0, min = 0, max = 200, step = 5)),
              column(6, tags$p(tags$em("Set to 0 for a single-occasion design."),
                               style = "color: #7f8c8d; font-size: 12px; margin-top: 25px;"))
            )
          ),

          box(
            title = "Simulation settings",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            fluidRow(
              column(6, numericInput("n_subjects", "Virtual subjects",
                                     value = 1000, min = 10, max = 5000, step = 100)),
              column(6, numericInput("seed", "Random seed",
                                     value = 123, min = 1, step = 1))
            ),
            numericInput("delta", "Output time step (h)",
                         value = 0.1, min = 0.01, max = 1, step = 0.05),
            tags$p(tags$em("A finer step sharpens peak and trough estimates at the cost of runtime."),
                   style = "color: #7f8c8d; font-size: 12px;")
          )
        )
      ),

      # --- About tab --------------------------------------------------------
      tabItem(
        tabName = "about",
        box(
          title = "About this application",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          tags$div(
            style = "padding: 20px;",
            tags$h3("Dosing Simulator"),
            tags$p(
              "An interactive Monte Carlo simulator for intravenous dosing regimens,
              built on a generic two-compartment population pharmacokinetic model.
              The drug is not fixed: every typical parameter, covariate effect and
              variance term is an input, so any compound adequately described by
              two-compartment linear kinetics can be explored by entering its
              estimates on the Model setup tab."
            ),
            tags$h4("What it does"),
            tags$ul(
              tags$li("Simulates a population of virtual subjects with variability in weight and renal function"),
              tags$li("Applies between-subject and between-occasion variability to the PK parameters"),
              tags$li("Plots the median concentration-time profile with 50% and 90% prediction intervals"),
              tags$li("Reports target attainment against two user-defined concentration targets"),
              tags$li("Summarises trough statistics and time above target over the final dosing interval")
            ),
            tags$h4("Model structure"),
            tags$ul(
              tags$li("Two compartments with first-order elimination from the central compartment"),
              tags$li("Allometric scaling of clearance and volume on body weight"),
              tags$li("A power function of a renal function marker on clearance"),
              tags$li("Log-normal between-subject variability on clearance and central volume"),
              tags$li("Between-occasion variability on clearance, for two-occasion designs")
            ),
            tags$h4("Units"),
            tags$p(
              "Units are not enforced. The convention assumed throughout is amount in mg,
              volume in L, clearance in L/h and time in h, giving concentration in mg/L.
              Any self-consistent set works."
            ),
            tags$hr(),
            tags$p(
              tags$strong("This is a simulation tool for research and teaching. "),
              "The default parameters are illustrative round numbers and do not
              describe any real compound. Nothing here is validated for clinical
              use and it must not be used to guide the treatment of a patient.",
              style = "color: #c0392b;"
            )
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # Number of doses that fit in the simulated window, and the window over
  # which attainment is assessed (the final complete dosing interval).
  regimen <- reactive({
    shiny::req(input$interval, input$duration)
    validate(
      need(input$interval > 0, "Dosing interval must be greater than zero."),
      need(input$duration >= input$interval,
           "Simulation duration must be at least one dosing interval."),
      need(!is.na(input$infdur) && input$infdur >= 0,
           "Infusion duration cannot be negative."),
      need(input$infdur < input$interval,
           "Infusion duration must be shorter than the dosing interval.")
    )
    n_doses <- floor(input$duration / input$interval)
    list(
      n_doses = n_doses,
      addl = max(n_doses - 1, 0),
      window_start = input$duration - input$interval,
      window_end = input$duration
    )
  })

  # The current parameter set. Previously this configured a compiled
  # mrgsolve object; now it is a plain list handed to the closed-form engine.
  configured_model <- reactive({
    shiny::req(input$tvcl, input$tvv1, input$tvq, input$tvv2)
    validate(
      need(input$tvcl > 0 && input$tvv1 > 0 && input$tvv2 > 0,
           "Clearance and volumes must be greater than zero.")
    )
    list(
      tvcl = input$tvcl, tvv1 = input$tvv1,
      tvq = input$tvq, tvv2 = input$tvv2,
      wt_ref = input$wt_ref, renal_ref = input$renal_ref,
      renal_exp_cl = input$renal_exp,
      allometric = isTRUE(input$allometric),
      iiv_cl_var = cv_to_var(input$iiv_cl),
      iiv_v1_var = cv_to_var(input$iiv_v1),
      iov_cl_var = cv_to_var(input$iov_cl)
    )
  })

  run_simulation <- reactive({
    reg <- regimen()
    cfg <- configured_model()
    n <- input$n_subjects
    shiny::req(n)

    sim <- simulate_population(
      n_subjects = n,
      wt_range = input$wt_range,
      renal_range = input$renal_range,
      dose = input$dose,
      dose_per_kg = identical(input$dose_basis, "mgkg"),
      inf_dur = input$infdur,
      interval = input$interval,
      n_doses = reg$n_doses,
      duration = input$duration,
      delta = input$delta,
      tvcl = cfg$tvcl, tvv1 = cfg$tvv1, tvq = cfg$tvq, tvv2 = cfg$tvv2,
      wt_ref = cfg$wt_ref, renal_ref = cfg$renal_ref,
      renal_exp_cl = cfg$renal_exp_cl,
      allometric = cfg$allometric,
      iiv_cl_var = cfg$iiv_cl_var,
      iiv_v1_var = cfg$iiv_v1_var,
      iov_cl_var = cfg$iov_cl_var,
      seed = input$seed
    ) %>% as_tibble()

    amount <- if (identical(input$dose_basis, "mgkg")) {
      sim %>% distinct(ID, WT) %>% pull(WT) * input$dose
    } else {
      rep(input$dose, n)
    }

    window <- sim %>% dplyr::filter(TIME >= reg$window_start, TIME <= reg$window_end)

    per_subject <- window %>%
      group_by(ID) %>%
      summarise(
        trough = min(IPRED),
        peak = max(IPRED),
        # Share of the interval spent above the lower target. mean() over an
        # evenly spaced grid is the fraction of sampled time points, which
        # approaches the true fraction as the output step shrinks.
        time_above = mean(IPRED > input$target1) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        above_target_1 = trough > input$target1,
        above_target_2 = trough > input$target2
      )

    list(
      sim = sim,
      per_subject = per_subject,
      pta_1 = mean(per_subject$above_target_1),
      pta_2 = mean(per_subject$above_target_2),
      cl_mean = mean(sim$CL),
      dose_total_mean = mean(amount),
      regimen = reg,
      stats = per_subject %>%
        summarise(
          median_trough = median(trough),
          mean_trough = mean(trough),
          q5_trough = quantile(trough, 0.05),
          q95_trough = quantile(trough, 0.95),
          min_trough = min(trough),
          max_trough = max(trough),
          median_peak = median(peak),
          median_time_above = median(time_above)
        )
    )
  })

  # --- Value boxes ---------------------------------------------------------

  pta_box <- function(pta, label, sublabel, icon_name) {
    value <- pta * 100
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.1f", value),
                  style = "font-size: 36px; font-weight: bold;"),
        tags$span("%", style = "font-size: 24px;")
      ),
      subtitle = tags$div(
        tags$strong(label), tags$br(),
        tags$span(sublabel, style = "font-size: 11px;")
      ),
      icon = icon(icon_name),
      color = if (value >= 90) "green" else if (value >= 70) "yellow" else "red",
      width = NULL
    )
  }

  output$pta_1_box <- renderValueBox({
    sim_data <- run_simulation()
    pta_box(sim_data$pta_1,
            sprintf("Attainment, %g mg/L", input$target1),
            "Trough above lower target", "check-circle")
  })

  output$pta_2_box <- renderValueBox({
    sim_data <- run_simulation()
    pta_box(sim_data$pta_2,
            sprintf("Attainment, %g mg/L", input$target2),
            "Trough above upper target", "star")
  })

  output$cl_box <- renderValueBox({
    sim_data <- run_simulation()
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.2f", sim_data$cl_mean),
                  style = "font-size: 36px; font-weight: bold;"),
        tags$span(" L/h", style = "font-size: 20px;")
      ),
      subtitle = tags$div(
        tags$strong("Mean clearance"), tags$br(),
        tags$span("Population average", style = "font-size: 11px;")
      ),
      icon = icon("tint"), color = "blue", width = NULL
    )
  })

  output$dose_total_box <- renderValueBox({
    sim_data <- run_simulation()
    subtitle <- if (input$dose_basis == "mgkg") {
      sprintf("%g mg/kg, mean weight %.1f kg", input$dose,
              mean(input$wt_range))
    } else {
      sprintf("%g mg flat dose", input$dose)
    }
    valueBox(
      value = tags$div(
        tags$span(sprintf("%.0f", sim_data$dose_total_mean),
                  style = "font-size: 36px; font-weight: bold;"),
        tags$span(" mg", style = "font-size: 20px;")
      ),
      subtitle = tags$div(
        tags$strong("Mean dose per administration"), tags$br(),
        tags$span(subtitle, style = "font-size: 11px;")
      ),
      icon = icon("syringe"), color = "purple", width = NULL
    )
  })

  # --- Text outputs --------------------------------------------------------

  output$plot_subtitle <- renderText({
    sprintf(" (n = %s virtual subjects)", format(input$n_subjects, big.mark = ","))
  })

  output$n_subjects_text <- renderText({
    format(input$n_subjects, big.mark = ",")
  })

  output$duration_text <- renderText({
    sprintf("%g h", input$duration)
  })

  output$n_doses_text <- renderText({
    reg <- regimen()
    sprintf("%d, every %g h over %g h",
            reg$n_doses, input$interval,
            if (input$infdur > 0) input$infdur else 0)
  })

  output$wt_range_text <- renderText({
    sprintf("%.0f - %.0f kg", input$wt_range[1], input$wt_range[2])
  })

  output$renal_range_text <- renderText({
    sprintf("%.0f - %.0f mL/min/1.73m²",
            input$renal_range[1], input$renal_range[2])
  })

  output$cl_value <- renderText({
    sprintf("%.2f", run_simulation()$cl_mean)
  })

  output$window_text <- renderText({
    reg <- regimen()
    sprintf("%g - %g h (final dosing interval)", reg$window_start, reg$window_end)
  })

  output$target_description <- renderUI({
    tags$div(
      tags$p(
        tags$strong(sprintf("Lower target (%g mg/L): ", input$target1)),
        tags$span("the concentration the trough should not fall below.",
                  style = "color: #7f8c8d;")
      ),
      tags$p(
        tags$strong(sprintf("Upper target (%g mg/L): ", input$target2)),
        tags$span("a more demanding threshold, for comparison.",
                  style = "color: #7f8c8d;")
      )
    )
  })

  # --- Plot ----------------------------------------------------------------

  output$main_plot <- renderPlot({
    sim_data <- run_simulation()
    reg <- sim_data$regimen

    d_summary <- sim_data$sim %>%
      group_by(TIME) %>%
      summarise(
        med = median(IPRED, na.rm = TRUE),
        min5 = quantile(IPRED, 0.05, na.rm = TRUE),
        max95 = quantile(IPRED, 0.95, na.rm = TRUE),
        q25 = quantile(IPRED, 0.25, na.rm = TRUE),
        q75 = quantile(IPRED, 0.75, na.rm = TRUE),
        .groups = "drop"
      )

    # Scaled to the data and the targets rather than to a fixed ceiling, so
    # nothing is silently clipped out of the panel.
    y_max <- max(d_summary$max95, input$target1, input$target2, na.rm = TRUE) * 1.1
    label_x <- input$duration * 0.99

    ggplot(d_summary, aes(x = TIME)) +
      geom_ribbon(aes(ymin = min5, ymax = max95),
                  fill = "#3498db", alpha = 0.15, color = NA) +
      geom_ribbon(aes(ymin = q25, ymax = q75),
                  fill = "#2980b9", alpha = 0.25, color = NA) +
      geom_line(aes(y = med), color = "#2c3e50", linewidth = 1.5) +
      annotate("rect",
               xmin = reg$window_start, xmax = reg$window_end,
               ymin = 0, ymax = y_max,
               fill = "#95a5a6", alpha = 0.10) +
      geom_hline(yintercept = input$target1, color = "#e74c3c",
                 linetype = "dashed", linewidth = 1.2, alpha = 0.8) +
      geom_hline(yintercept = input$target2, color = "#c0392b",
                 linetype = "dashed", linewidth = 1.2, alpha = 0.8) +
      annotate("text", x = label_x, y = input$target1 + y_max * 0.02,
               label = sprintf("Lower target (%g mg/L)", input$target1),
               hjust = 1, color = "#e74c3c", size = 4.5, fontface = "bold") +
      annotate("text", x = label_x, y = input$target2 + y_max * 0.02,
               label = sprintf("Upper target (%g mg/L)", input$target2),
               hjust = 1, color = "#c0392b", size = 4.5, fontface = "bold") +
      labs(
        y = "Concentration (mg/L)",
        x = "Time (hours)",
        title = "Population simulation: concentration-time profile",
        subtitle = "Median with 50% (dark) and 90% (light) prediction intervals; shaded band is the assessment window"
      ) +
      scale_y_continuous(limits = c(0, y_max), expand = expansion(c(0, 0.02))) +
      scale_x_continuous(limits = c(0, input$duration),
                         breaks = scales::pretty_breaks(n = 12),
                         expand = expansion(c(0, 0.02))) +
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
  })

  # --- Statistics table ----------------------------------------------------

  output$stats_table <- DT::renderDataTable({
    sim_data <- run_simulation()
    s <- sim_data$stats

    stats_df <- data.frame(
      Metric = c(
        sprintf("Attainment, trough above %g mg/L", input$target1),
        sprintf("Attainment, trough above %g mg/L", input$target2),
        sprintf("Median time above %g mg/L", input$target1),
        "Mean clearance (L/h)",
        "Median trough (mg/L)",
        "Mean trough (mg/L)",
        "5th percentile trough (mg/L)",
        "95th percentile trough (mg/L)",
        "Minimum trough (mg/L)",
        "Maximum trough (mg/L)",
        "Median peak (mg/L)"
      ),
      Value = c(
        sprintf("%.1f%%", sim_data$pta_1 * 100),
        sprintf("%.1f%%", sim_data$pta_2 * 100),
        sprintf("%.1f%% of the interval", s$median_time_above),
        sprintf("%.2f", sim_data$cl_mean),
        sprintf("%.2f", s$median_trough),
        sprintf("%.2f", s$mean_trough),
        sprintf("%.2f", s$q5_trough),
        sprintf("%.2f", s$q95_trough),
        sprintf("%.2f", s$min_trough),
        sprintf("%.2f", s$max_trough),
        sprintf("%.2f", s$median_peak)
      ),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      stats_df,
      options = list(pageLength = 11, dom = "t", ordering = FALSE),
      rownames = FALSE,
      colnames = c("Metric", "Value")
    ) %>%
      DT::formatStyle("Metric", fontWeight = "bold", color = "#2c3e50") %>%
      DT::formatStyle("Value", color = "#34495e")
  })
}

shinyApp(ui, server)
