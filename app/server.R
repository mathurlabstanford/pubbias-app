library(shiny)
library(glue)
library(tidyverse)
library(PublicationBias)

ci_text <- function(estimate, ci_lower, ci_upper, sig = 2) {
  glue("{signif(estimate, sig)} (95% CI [{signif(ci_lower, sig)},
        {signif(ci_upper, sig)}])")
}
sval_print <- function(sval) if (is.numeric(sval)) signif(sval, 2) else sval


shinyServer(function(input, output) {
  
  # ----------------------------------------------------------------------------
  # overall inputs
  # ----------------------------------------------------------------------------
  
  meta_data <- reactive({
    meta_file <- input$meta_data
    ext <- tools::file_ext(meta_file$datapath)
    
    req(meta_file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read_csv(meta_file$datapath) |> mutate("unique_id" = 1:n(), .before = 1)
    
    # example_df <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
    #                               ci = cpos, di = cneg, data = metafor::dat.bcg)
    # example_df$yi <- -example_df$yi
    # example_df
  })
  
  output$y_cols <- renderUI({
    req(meta_data)
    selectInput("y_col", "Column of point estimates",
                choices = c("Select a column" = "", names(meta_data())))
  })
  
  output$v_cols <- renderUI({
    req(meta_data)
    selectInput("v_col", "Column of estimated variances",
                choices = c("Select a column" = "", names(meta_data())))
  })
  
  output$direction <- renderUI({
    req(input$v_col)
    selectInput("direction", "Direction",
                choices = c("favor affirmative", "favor non-affirmative"))
  })
  
  output$model_type <- renderUI({
    req(input$v_col)
    selectInput("model_type", "Model type", choices = c("fixed", "robust"))
  })
  
  output$cluster_cols <- renderUI({
    if (req(input$model_type) == "robust") {
      selectInput("cluster_col", "Column of cluster labels",
                  choices = names(meta_data()))
    }
  })
  
  affirmative <- reactive({
    req(input$direction)
    str_detect(input$direction, "\\saffirmative")
  })
  
  cluster_col <- reactive({
    cc <- input$cluster_col
    if (is.null(cc) || cc == "") "unique_id" else cc
  })
  
  # ----------------------------------------------------------------------------
  # corrected_meta
  # ----------------------------------------------------------------------------
  
  output$eta_slider <- renderUI({
    req(uncorrected_model())
    sliderInput("eta", "η (ratio)", value = 2, min = 1, max = 20, step = 0.1)
  })
  
  uncorrected_model <- reactive({
    req(input$y_col, input$v_col)
    metafor::rma(yi = meta_data()[[input$y_col]],
                 vi = meta_data()[[input$v_col]],
                 method = "FE") |>
      broom::tidy(conf.int = TRUE)
  })
  
  corrected_model <- reactive({
    req(input$eta, input$y_col, input$v_col, cluster_col, affirmative,
        input$model_type)
    corrected_meta(yi = meta_data()[[input$y_col]],
                   vi = meta_data()[[input$v_col]],
                   eta = input$eta,
                   clustervar = meta_data()[[cluster_col()]],
                   model = input$model_type,
                   favor.positive = affirmative())
  })
  
  output$uncorrected <- renderUI({
    req(uncorrected_model)
    um <- uncorrected_model()
    p(strong("Uncorrected estimate (log RR):"), br(),
      ci_text(um$estimate, um$conf.low, um$conf.high))
  })
  
  output$corrected <- renderUI({
    req(corrected_model)
    cm <- corrected_model()
    p(strong("Corrected estimate  (log RR):"), br(),
      ci_text(cm$est, cm$lo, cm$hi))
  })
  
  output$corrected_summary <- renderUI({
    req(input$eta, input$y_col, input$v_col, affirmative)
    more_likely <- if (affirmative()) "affirmative" else "non-affirmative"
    less_likely <- if (affirmative()) "non-affirmative" else "affirmative"
    cm <- corrected_model()
    p(em(glue("If {more_likely} studies were {input$eta} times more likely
              to be published than {less_likely} studies, the meta-analytic
              point estimate corrected for publication bias would be
              {ci_text(cm$est, cm$lo, cm$hi)}.")))
  })
  
  # ----------------------------------------------------------------------------
  # svalue
  # ----------------------------------------------------------------------------
  
  sval <- reactive({
    req(input$q, input$y_col, input$v_col, cluster_col, affirmative,
        input$model_type)
    affirmative <- str_detect(input$direction, "\\saffirmative")
    svalue(yi = meta_data()[[input$y_col]],
           vi = meta_data()[[input$v_col]],
           q = input$q, #log(input$q),
           clustervar = meta_data()[[cluster_col()]],
           favor.positive = affirmative(),
           model = input$model_type)
  })
  
  output$q_slider <- renderUI({
    req(uncorrected_model)
    m0 <- uncorrected_model()$estimate
    q_range <- if (m0 < 0) c(m0, 0) else c(0, m0)
    q_range <- round(q_range, 2)
    sliderInput("q", "q (log RR)", value = 0, min = q_range[1], max = q_range[2],
                step = 0.01)
  })
  
  output$sval_est <- renderUI({
    req(sval)
    p(strong(glue(
      "Publication bias required to shift point estimate to {input$q}:"
    )),
      br(), sval_print(sval()$sval.est))
  })
  
  output$sval_ci <- renderUI({
    req(sval)
    p(strong(glue(
      "Publication bias required to shift CI limit to {input$q}:"
    )),
    br(), sval_print(sval()$sval.ci))
  })
  
  output$sval_summary <- renderUI({
    req(input$q, input$y_col, input$v_col, cluster_col, affirmative,
        input$model_type)
    more_likely <- if (affirmative()) "affirmative" else "non-affirmative"
    less_likely <- if (affirmative()) "non-affirmative" else "affirmative"
    p(em(glue("For the meta-analytic point estimate corrected for publication
              bias to shift to {input$q}, {more_likely} studies would
              need to be {sval_print(sval()$sval.est)} times more likely to get
              published than {less_likely} studies. For its CI bound to shift to
              {input$q}, {more_likely} studies would need to be
              {sval_print(sval()$sval.ci)} times more likely to get published
              than {less_likely} studies.")))
  })
  
  # ----------------------------------------------------------------------------
  # significance_funnel
  # ----------------------------------------------------------------------------
  
  output$funnel_plot <- renderPlot({
    req(input$y_col, input$v_col, affirmative)
    significance_funnel(yi = meta_data()$yi, vi = meta_data()$vi,
                        favor.positive = affirmative()) +
      theme_classic(base_size = 18, base_family = "Lato") +
      theme(legend.position = "top",
            legend.title = element_blank())
  })
  
  # ----------------------------------------------------------------------------
  # pval_plot
  # ----------------------------------------------------------------------------
  
  # output$pval_plot <- renderPlot({
  #   pval_plot(yi = meta_data()$yi, vi = meta_data()$vi) +
  #     theme_classic(base_size = 18, base_family = "Lato") +
  #     theme(legend.position = "top",
  #           legend.title = element_blank())
  # })
  
})
