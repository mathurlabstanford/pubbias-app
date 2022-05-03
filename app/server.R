library(shiny)
library(glue)
library(tidyverse)
library(PublicationBias)
library(markdown)

ci_text <- function(estimate, ci_lower, ci_upper, sig = 2) {
  glue("{signif(estimate, sig)} (95% CI [{signif(ci_lower, sig)},
        {signif(ci_upper, sig)}])")
}
estimate_text <- function(model_label, model_result, sig = 2) {
  if (is.null(model_result)) ci <- ""
  else ci <- ci_text(model_result$estimate, model_result$ci_lower,
                     model_result$ci_upper, sig = sig)
  p(strong(glue("{str_to_sentence(model_label)} estimate:")), br(), ci)
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
    read_csv(meta_file$datapath, show_col_types = FALSE)
      # mutate("unique_id" = 1:n(), .before = 1) #, yi = -yi)
    
    # read_csv("../escalc_example.csv", show_col_types = FALSE) |>
    #   mutate("unique_id" = 1:n(), .before = 1)
    
    # example_df <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
    #                               ci = cpos, di = cneg, data = metafor::dat.bcg)
    # example_df$yi <- -example_df$yi
    # example_df
  })

  output$y_cols <- renderUI({
    req(input$meta_data)
    selectInput("y_col", "Column of point estimates",
                choices = c("Select a column" = "", names(meta_data())))
  })
  
  output$v_cols <- renderUI({
    req(input$meta_data)
    selectInput("v_col", "Column of estimated variances",
                choices = c("Select a column" = "", names(meta_data())))
  })
  
  output$direction <- renderUI({
    # req(input$v_col)
    # req(uncorrected_model, input$model_type)
    req(input$meta_data, input$y_col, input$v_col)
    # est <- uncorrected_model()$estimate
    # dir <- if (est < 0) "favor negative" else "favor positive"
    selectInput("direction", "Direction", #selected = dir,
                choices = c("favor positive", "favor negative"))
  })
  
  output$model_type <- renderUI({
    req(input$meta_data, input$y_col, input$v_col, input$direction)
    selectInput("model_type", "Model type",
                choices = c("fixed-effects" = "fixed",
                            "robust random-effects" = "robust"))
  })
  
  output$cluster_cols <- renderUI({
    req(input$meta_data, input$y_col, input$v_col, input$direction, input$model_type)
    if (input$model_type == "robust") {
      selectInput("cluster_col", "Column of cluster labels",
                  choices = c("[none]", names(meta_data())))
    }
  })
  
  positive <- reactive({
    req(input$direction)
    str_detect(input$direction, "positive")
  })
  
  cluster_col <- reactive({
    req(input$model_type) #, input$cluster_col)
    # cc <- input$cluster_col
    # if (is.null(cc) || str_detect(cc, "none")) 1:nrow(meta_data()) else meta_data()[[cc]]
    if (str_detect(input$model_type, "fixed") || 
        (!is.null(input$cluster_col) && str_detect(input$cluster_col, "none"))) {
      1:nrow(meta_data())
    } else {
      meta_data()[[input$cluster_col]]
    }
  })
  
  # ----------------------------------------------------------------------------
  # corrected_meta
  # ----------------------------------------------------------------------------
  
  output$eta_slider <- renderUI({
    req(uncorrected_model())
    sliderInput("eta", "η (ratio)", value = 2, min = 1, max = 20, step = 0.1)
  })
  
  uncorrected_model <- reactive({
    req(input$y_col, input$v_col, input$model_type) # cluster_col
    if (input$model_type == "fixed") {
      meta_model <- metafor::rma(yi = meta_data()[[input$y_col]],
                   vi = meta_data()[[input$v_col]],
                   method = "FE")
      meta_result <- list(estimate = meta_model$beta,
                          ci_lower = meta_model$ci.lb,
                          ci_upper = meta_model$ci.ub)
    } else if (input$model_type == "robust") {
      robu_formula <- as.formula(glue("{input$y_col} ~ 1"))
      meta_model <- robumeta::robu(robu_formula,
                     # studynum = meta_data()[[cluster_col()]],
                     studynum = cluster_col(),
                     data = meta_data(),
                     var.eff.size = meta_data()[[input$v_col]],
                     small = TRUE)
      meta_result <- list(estimate = meta_model$reg_table$b.r,
                          ci_lower = meta_model$reg_table$CI.L,
                          ci_upper = meta_model$reg_table$CI.U)
    }
    meta_result
  })
  
  # worst_model <- reactive({
    # req(input$y_col, input$v_col, input$direction, input$model_type)
    # y <- sym(input$y_col)
    # v <- sym(input$v_col)
    # nonaffirmative <- meta_data() |>
    #   mutate(pval = 2 * (1 - pnorm(abs(!!y / sqrt(!!v))))) |>
    #   filter(!!y > 0, pval < 0.05)
    # if (nrow(nonaffirmative) == 0) return(NULL)
    # 
    # robu_formula <- as.formula(glue("{input$y_col} ~ 1"))
    # meta_model <- robumeta::robu(robu_formula,
    #                              studynum = nonaffirmative[[cluster_col()]],
    #                              data = nonaffirmative,
    #                              var.eff.size = nonaffirmative[[input$v_col]],
    #                              small = TRUE)
    # meta_result <- list(estimate = meta_model$reg_table$b.r,
    #                     ci_lower = meta_model$reg_table$CI.L,
    #                     ci_upper = meta_model$reg_table$CI.U)
    # meta_result
  # })
  
  corrected_model <- reactive({
    req(input$eta, input$y_col, input$v_col, input$direction, input$model_type, cluster_col())
    meta_model <- corrected_meta(yi = meta_data()[[input$y_col]],
                                 vi = meta_data()[[input$v_col]],
                                 eta = input$eta,
                                 # clustervar = meta_data()[[cluster_col()]],
                                 clustervar = cluster_col(),
                                 model = input$model_type,
                                 favor.positive = positive())
    meta_result <- list(estimate = meta_model$est,
                        ci_lower = meta_model$lo,
                        ci_upper = meta_model$hi)
  })
  
  output$uncorrected <- renderUI({
    req(uncorrected_model)
    estimate_text("uncorrected", uncorrected_model())
  })

  output$corrected <- renderUI({
    req(corrected_model)
    estimate_text("corrected", corrected_model())
  })
  
  output$corrected_summary <- renderUI({
    req(input$eta, input$y_col, input$v_col, positive)
    more_likely <- if (positive()) "positive" else "negative"
    less_likely <- if (positive()) "negative" else "positive"
    cm <- corrected_model()
    p(em(glue("If {more_likely} studies were {input$eta} times more likely
              to be published than {less_likely} studies, the meta-analytic
              point estimate corrected for publication bias would be
              {ci_text(cm$estimate, cm$ci_lower, cm$ci_upper)}.")))
  })
  
  # ----------------------------------------------------------------------------
  # svalue
  # ----------------------------------------------------------------------------
  
  sval_model <- reactive({
    req(input$q, input$y_col, input$v_col, input$direction, input$model_type, cluster_col)
    svalue(yi = meta_data()[[input$y_col]],
           vi = meta_data()[[input$v_col]],
           q = input$q,
           # clustervar = meta_data()[[cluster_col()]],
           clustervar = cluster_col(),
           favor.positive = positive(),
           model = input$model_type,
           return.worst.meta = TRUE)
  })
  
  sval <- reactive({
    req(sval_model)
    sval_model()$stats
  })
  
  worst_model <- reactive({
    req(sval_model)
    meta_model <- sval_model()$meta.worst
    if (input$model_type == "fixed") {
      meta_result <- list(estimate = meta_model$beta,
                          ci_lower = meta_model$ci.lb,
                          ci_upper = meta_model$ci.ub)
    } else if (input$model_type == "robust") {
      meta_result <- list(estimate = meta_model$reg_table$b.r,
                          ci_lower = meta_model$reg_table$CI.L,
                          ci_upper = meta_model$reg_table$CI.U)
    }
    meta_result
  })
  
  output$worst <- renderUI({
    req(worst_model)
    estimate_text("worst-case", worst_model())
  })
  
  output$q_slider <- renderUI({
    req(uncorrected_model)
    m0 <- uncorrected_model()$estimate
    q_range <- if (m0 < 0) c(m0, 0) else c(0, m0)
    q_range <- round(q_range, 2)
    sliderInput("q", "q", value = 0, min = q_range[1], max = q_range[2],
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
    req(sval)
    # req(input$q, input$y_col, input$v_col, input$direction, input$model_type)
    more_likely <- if (positive()) "positive" else "negative"
    less_likely <- if (positive()) "negative" else "positive"
    sval_text <- function(var, val) {
      if (str_detect(var, "estimate") & str_detect(val, "Not possible")) {
        glue("Under this model of publication bias, there is no amount of
             publication bias that would shift the {var} to 0.")
      } else if (str_detect(var, "bound") & str_detect(val, "--")) {
        glue("Since the uncorrected CI already contains {input$q}, it is not
             relevant to consider publication bias to shift the CI to include
             {input$q}.")
      } else {
        glue("For the {var} corrected for publication bias to shift to
             {input$q}, {more_likely} studies would need to be {sval_print(val)}
             times more likely to get published than {less_likely} studies.")
      }
    }
    p(em(paste(sval_text("point estimate", sval()$sval.est),
               sval_text("CI bound", sval()$sval.ci),
               collapse = " ")))
    
    # p(em(glue("For the meta-analytic point estimate corrected for publication
    #           bias to shift to {input$q}, {more_likely} studies would
    #           need to be {sval_print(sval()$sval.est)} times more likely to get
    #           published than {less_likely} studies. For its CI bound to shift to
    #           {input$q}, {more_likely} studies would need to be
    #           {sval_print(sval()$sval.ci)} times more likely to get published
    #           than {less_likely} studies.")))
  })
  
  # ----------------------------------------------------------------------------
  # significance_funnel
  # ----------------------------------------------------------------------------
  
  funnel_plot <- function() {
    significance_funnel(yi = meta_data()$yi, vi = meta_data()$vi,
                        favor.positive = positive(),
                        est.all = uncorrected_model()$estimate,
                        est.N = worst_model()$estimate) +
      # theme_classic(base_size = 18, base_family = "Lato") +
      theme_classic(base_family = "Lato") +
      theme(legend.position = "top",
            legend.title = element_blank())
  }
  
  fp_res <- 300
  fp_width <- 1200
  fp_height <- 1100
  
  output$funnel_plot <- renderPlot(res = fp_res, height = fp_height, width = fp_width, {
    # req(input$y_col, input$v_col, input$direction)
    req(uncorrected_model, worst_model)
    funnel_plot()
  })
  
  output$download_funnel <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$meta_data$name), "_funnel", ".png")
    },
    content = function(file) {
      ggsave(file, plot = funnel_plot(), device = "png", dpi = fp_res,
             height = fp_height, width = fp_width, units = "px")
    }
  )
  
  output$download_button <- renderUI({
    req(funnel_plot())
    downloadButton("download_funnel")
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
