library(shiny)
library(glue)
library(shinycssloaders)
# library(shinyjs)
# useShinyjs()
options(spinner.color = "#800080")

fluidPage(
  title = "Publication bias",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("Sensitivity to publication bias in meta-analysis"),
  
  fluidRow(
    column(
      width = 7,
      div(
        # MM: not sure how to add line breaks within this class
        # <br> also doesn't work
        # Also, in the citation, could we please link to: https://rss.onlinelibrary.wiley.com/doi/10.1111/rssc.12440
        class = "bs-callout bs-callout-info",
        'This website conducts sensitivity analyses for publication bias in meta-analyses. Please use the following citation:
        
        \n\n Mathur MB & VanderWeele TJ (2020). Sensitivity analysis for publication bias in meta-analyses. Journal of the Royal Statistical Society: Series C, 69(5), 1091-1119 \n\n
        
        
        These analyses consider publication bias that favors affirmative studies (i.e., those with positive estimates and significant p-values) over nonaffirmative studies (i.e., those with negative or nonsignificant estimates). These analyses enable statements such as: "For publication bias to shift the observed point estimate to the null, significant results would need to be at least 30-fold more likely to be published than negative or nonsignificant results." Alternatively, you can consider shifting to a chosen non-null value or shifting the confidence interval to include the null or another value.
        
        \n\nThis website also provides a worst-case meta-analytic point estimate under maximal publication bias obtained simply by conducting a standard meta-analysis of only the negative and nonsignificant studies.'
      )
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
  
  # conditionalPanel(
  # condition = "1 == 1",
  fluidRow(
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        h4("Estimate publication bias correction"),
        p("For a chosen ratio of publication probabilities (η), estimate a
          publication bias-corrected pooled point estimate and 95% confidence
          interval."),
        uiOutput("eta_slider"),
        withSpinner(tagList(
          uiOutput("uncorrected"),
          uiOutput("corrected"),
          uiOutput("worst"),
          uiOutput("corrected_summary")
        ))
      )
    ),
    # column(width = 1),
    column(
      width = 4, #offset = 1,
      div(
        class = "bs-callout bs-callout-output",
        h4('Severity of publication bias needed to "explain away" results'),
        p("Estimate the S-value, defined as the severity of publication bias
          (i.e., the ratio by which affirmative studies are more
          likely to be published than nonaffirmative studies, or vice
          versa) that would be required to shift the pooled point estimate or
          its confidence interval limit to q."),
        uiOutput("q_slider"),
        withSpinner(tagList(
          uiOutput("sval_est"),
          uiOutput("sval_ci"),
          uiOutput("sval_summary")
        ))
      )
    ),
    # ),
    
    # fluidRow(div(style = "margin-bottom: 2em;")),
    
    # fluidRow(
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        h4("Significance funnel plot"),
        p("The estimate among only nonaffirmative studies (gray diamond)
          represents a corrected estimate under worst-case publication bias that favors affirmative results. If
          the gray diamond represents a negligible effect size or if it is much
          smaller than the pooled estimate among all studies (black diamond),
          this suggests that the meta-analysis may not be robust to extreme
          publication bias."),
        withSpinner(tagList(
          plotOutput("funnel_plot", width = "auto", height = "auto"), #, height = "400px", width = "450px"),
          uiOutput("download_button")
        )),
        # downloadButton("download_funnel")
      )
    )
    ,
    # column(width = 1),
    #   column(
    #     width = 5, offset = 1,
    #     h4("p-value plot"),
    #     p('Plot the one-tailed p-values. The leftmost red line indicates the
    #     cutoff for one-tailed p-values less than 0.025 (corresponding to
    #     "affirmative" studies; i.e., those with a positive point estimate and a
    #     two-tailed p-value less than 0.05). The rightmost red line indicates
    #     one-tailed p-values greater than 0.975 (i.e., studies with a negative
    #     point estimate and a two-tailed p-value less than 0.05). If there is a
    #     substantial point mass of p-values to the right of the rightmost red
    #     line, this suggests that selection may be two-tailed rather than
    #     one-tailed.'),
    #     plotOutput("pval_plot", height = "400px", width = "450px")
    #   )
    # )
  )
)
