library(shiny)
library(shinyFeedback)
library(glue)
library(shinycssloaders)
options(spinner.color = "#6f42c1")

fluidPage(
  useShinyFeedback(),
  title = "Publication bias",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  includeCSS("www/styles.css"),
  tags$head(tags$base(target = "_blank")),
  
  titlePanel("Sensitivity to publication bias in meta-analysis"),
  
  fluidRow(
    column(
      width = 8,
      div(class = "bs-callout bs-callout-info",
          div(class = "docs", includeMarkdown("docs/header.md")))
    )
  ),
  
  div(class = "bs-callout bs-callout-input",
      fluidRow(
        column(
          width = 2,
          fileInput("meta_data", "Upload meta-analysis data (csv)",
                    accept = ".csv", placeholder = "")
        ),
        column(width = 2, uiOutput("y_cols")),
        column(width = 2, uiOutput("v_cols")),
        column(width = 2, uiOutput("directions")),
        column(width = 2, uiOutput("model_type")),
        column(width = 2, uiOutput("cluster_cols"))
      ),
      fluidRow(
        column(width = 11, offset = 2, textInput("error", "")))
  ),
  
  fluidRow(
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/corrected.md")),
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
        div(class = "docs", includeMarkdown("docs/svalue.md")),
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
        div(class = "docs", includeMarkdown("docs/funnel.md")),
        withSpinner(tagList(
          plotOutput("funnel", width = "auto", height = "auto"),
          uiOutput("download_funnel_button")
        ))
      )
    )
  )
)
