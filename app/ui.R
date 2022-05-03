library(shiny)
library(glue)
library(shinycssloaders)
options(spinner.color = "#800080")

fluidPage(
  title = "Publication bias",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  includeCSS("www/styles.css"),

  titlePanel("Sensitivity to publication bias in meta-analysis"),
  
  fluidRow(
    column(
      width = 8,
      div(class = "bs-callout bs-callout-info",
          includeMarkdown("docs/header.md"))
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      inputPanel(
        fileInput("meta_data", "Upload meta-analysis data (csv)",
                  accept = ".csv", placeholder = ""),
        uiOutput("y_cols"),
        uiOutput("v_cols"),
        uiOutput("direction"),
        uiOutput("model_type"),
        uiOutput("cluster_cols")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        includeMarkdown("docs/corrected.md"),
        uiOutput("eta_slider"),
        withSpinner(tagList(
          uiOutput("uncorrected"),
          uiOutput("corrected"),
          uiOutput("worst"),
          uiOutput("corrected_summary")
        ))
      )
    ),
    
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        includeMarkdown("docs/svalue.md"),
        uiOutput("q_slider"),
        withSpinner(tagList(
          uiOutput("sval_est"),
          uiOutput("sval_ci"),
          uiOutput("sval_summary")
        ))
      )
    ),
    
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        includeMarkdown("docs/funnel.md"),
        withSpinner(tagList(
          plotOutput("funnel_plot", width = "auto", height = "auto"),
          uiOutput("download_button")
        ))
      )
    )
  )
)
