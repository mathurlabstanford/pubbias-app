library(shiny)
library(shinyFeedback)
library(glue)
library(tidyverse)
library(PublicationBias)
library(markdown)

.str <- function(s) {
  paste(strwrap(glue(s, .envir = parent.frame())), collapse = "")
}

ci_text <- function(estimate, ci_lower, ci_upper, sig = 2) {
  .str("{signif(estimate, sig)} (95% CI [{signif(ci_lower, sig)},
        {signif(ci_upper, sig)}])")
}
estimate_text <- function(model_label, model_result, sig = 2) {
  if (is.null(model_result)) ci <- ""
  else ci <- ci_text(model_result$estimate, model_result$ci_lower,
                     model_result$ci_upper, sig = sig)
  p(strong(glue("{str_to_sentence(model_label)} estimate:")), br(), ci)
}
sval_print <- function(sval) if (is.numeric(sval)) signif(sval, 2) else sval

danger <- function(inputId, show, text) {
  feedbackDanger(inputId, show, text, color = "var(--red)", icon = NULL)
}

shinyServer(function(input, output) {
  
  # ----------------------------------------------------------------------------
  # overall input elements
  # ----------------------------------------------------------------------------
  
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
  
  output$directions <- renderUI({
    req(input$v_col)
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
    req(input$meta_data, input$y_col, input$v_col, input$direction,
        input$model_type)
    if (input$model_type == "robust") {
      selectInput("cluster_col", "Column of cluster labels",
                  choices = c("[none]", names(meta_data())))
    }
  })
  
  # ----------------------------------------------------------------------------
  # values based on overall inputs
  # ----------------------------------------------------------------------------
  
  meta_data <- reactive({
    meta_file <- input$meta_data
    ext <- tools::file_ext(meta_file$datapath)
    req(meta_file)
    # validate(need(ext == "csv", "Please upload a csv file"))
    # danger("meta_data", ext == "csv", "please upload a csv file")
    read_csv(meta_file$datapath, show_col_types = FALSE)
    
    # read_csv("../escalc_example.csv", show_col_types = FALSE) |>
    #   mutate("unique_id" = 1:n(), .before = 1)
    
    # example_df <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
    #                               ci = cpos, di = cneg, data = metafor::dat.bcg)
    # example_df$yi <- -example_df$yi
    # example_df
  })
  
  positive <- reactive({
    req(input$direction)
    str_detect(input$direction, "positive")
  })
  
  cluster_col <- reactive({
    req(input$model_type)
    cc <- input$cluster_col
    cluster_none <- !is.null(cc) && str_detect(cc, "none")
    fixed <- str_detect(input$model_type, "fixed")
    if (fixed || cluster_none) 1:nrow(meta_data()) else meta_data()[[cc]]
  })
  
  y_vals <- reactive({
    req(meta_data(), input$y_col)
    # req(input$y_col)
    meta_data()[[input$y_col]]
  })
  
  v_vals <- reactive({
    req(meta_data(), input$v_col)
    # req(input$v_col)
    meta_data()[[input$v_col]]
  })
  
  # ----------------------------------------------------------------------------
  # input validation
  # ----------------------------------------------------------------------------
  
  valid_y <- reactive({
    req(y_vals())
    y_valid <- is.numeric(y_vals())
    danger("y_col", !y_valid, "values must be numeric")
    req(y_valid)
  })
  
  valid_v <- reactive({
    req(v_vals())
    v_valid <- is.numeric(v_vals()) & all(v_vals() > 0)
    danger("v_col", !v_valid, "values must be numeric & positive")
    req(v_valid)
  })
  
  valid_affirm <- reactive({
    req(y_vals(), v_vals(), input$direction)
    
    if (positive()) yi = y_vals() else yi = -y_vals()
    pvals <- 2 * (1 - pnorm(abs(yi) / sqrt(v_vals())))
    alpha <- formals(PublicationBias::corrected_meta)$alpha.select
    affirm <- (pvals < alpha) & (yi > 0)
    no_aff <- sum(affirm) == 0
    no_nonaff <- sum(!affirm) == 0
    no_either <- no_aff | no_nonaff
    no_dir <- if (no_aff) "affirmative" else if (no_nonaff) "nonaffirmative"
    error <- .str("There are zero {no_dir} studies – double check your columns
                  and direction")
    danger("error", no_either, error)
    req(!no_either)
  })
  
  # ----------------------------------------------------------------------------
  # corrected_meta
  # ----------------------------------------------------------------------------
  
  output$eta_slider <- renderUI({
    req(uncorrected_model())
    sliderInput("eta", "η (ratio)", value = 2, min = 1, max = 20, step = 1)
  })
  
  uncorrected_model <- reactive({
    req(valid_y(), valid_v(), input$model_type)
    if (input$model_type == "fixed") {
      meta_model <- metafor::rma(yi = y_vals(), vi = v_vals(), method = "FE")
      meta_result <- list(estimate = meta_model$beta,
                          ci_lower = meta_model$ci.lb,
                          ci_upper = meta_model$ci.ub)
    } else if (input$model_type == "robust") {
      robu_formula <- as.formula(glue("{input$y_col} ~ 1"))
      meta_model <- robumeta::robu(robu_formula,
                                   studynum = cluster_col(),
                                   data = meta_data(),
                                   var.eff.size = v_vals(),
                                   small = TRUE)
      meta_result <- list(estimate = meta_model$reg_table$b.r,
                          ci_lower = meta_model$reg_table$CI.L,
                          ci_upper = meta_model$reg_table$CI.U)
    }
    meta_result
  })
  
  corrected_model <- reactive({
    req(input$eta, valid_y(), valid_v(), valid_affirm(),
        input$model_type, cluster_col())
    meta_model <- corrected_meta(yi = meta_data()[[input$y_col]],
                                 vi = meta_data()[[input$v_col]],
                                 eta = input$eta,
                                 clustervar = cluster_col(),
                                 model = input$model_type,
                                 favor.positive = positive())
    return(list(estimate = meta_model$est,
                ci_lower = meta_model$lo,
                ci_upper = meta_model$hi))
  })
  
  output$uncorrected <- renderUI({
    req(uncorrected_model())
    estimate_text("uncorrected", uncorrected_model())
  })
  
  output$corrected <- renderUI({
    req(corrected_model())
    estimate_text("corrected", corrected_model())
  })
  
  corrected_summary <- reactive({
    req(corrected_model())
    more_likely <- if (positive()) "positive" else "negative"
    less_likely <- if (positive()) "negative" else "positive"
    cm <- corrected_model()
    wm <- worst_model()
    .str("If affirmative (i.e., significant and {more_likely}) studies were
         {input$eta} times more likely to be published than nonaffirmative
         (i.e., nonsignificant or {less_likely}) studies, the meta-analytic
         point estimate corrected for publication bias would be
         {ci_text(cm$estimate, cm$ci_lower, cm$ci_upper)}.<br>
         If there were worst-case publication bias (i.e., that favors
         affirmative results infinitely more than nonaffirmative results), the
         corrected meta-analytic point estimate would be
         {ci_text(wm$estimate, wm$ci_lower, wm$ci_upper)}.")
  })
  
  output$corrected_summary <- renderUI({
    req(corrected_summary())
    p(em(HTML(corrected_summary())))
  })
  
  output$clip_corrected <- renderUI({
    req(corrected_summary())
    rclipButton(
      inputId = "clipbtn_corrected",
      label = "Copy summary",
      clipText = corrected_summary(), 
      icon = icon("clipboard")
    )
  })
  
  # ----------------------------------------------------------------------------
  # svalue
  # ----------------------------------------------------------------------------
  
  sval_model <- reactive({
    req(input$q, valid_y(), valid_v(), valid_affirm(),
        input$model_type, cluster_col())
    sval <- svalue(yi = meta_data()[[input$y_col]],
                   vi = meta_data()[[input$v_col]],
                   q = input$q,
                   clustervar = cluster_col(),
                   favor.positive = positive(),
                   model = input$model_type,
                   return.worst.meta = TRUE)
    return(sval)
  })
  
  sval <- reactive({
    req(sval_model())
    sval_model()$stats
  })
  
  worst_model <- reactive({
    req(sval_model())
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
    req(worst_model())
    estimate_text("worst-case", worst_model())
  })
  
  output$q_slider <- renderUI({
    req(uncorrected_model())
    m0 <- uncorrected_model()$estimate
    q_range <- if (m0 < 0) c(m0, 0) else c(0, m0)
    q_range <- round(q_range, 2)
    sliderInput("q", "q", value = 0, min = q_range[1], max = q_range[2],
                step = 0.01)
  })
  
  output$sval_est <- renderUI({
    req(sval())
    p(strong(glue(
      "Publication bias required to shift point estimate to {input$q}:"
    )),
    br(), sval_print(sval()$sval.est))
  })
  
  output$sval_ci <- renderUI({
    req(sval())
    p(strong(glue(
      "Publication bias required to shift CI limit to {input$q}:"
    )),
    br(), sval_print(sval()$sval.ci))
  })

  sval_summary <- reactive({
    req(sval())
    more_likely <- if (positive()) "positive" else "negative"
    less_likely <- if (positive()) "negative" else "positive"
    sval_text <- function(var, val) {
      if (str_detect(var, "estimate") & str_detect(val, "Not possible")) {
        .str("Under this model of publication bias, there is no amount of
             publication bias that would shift the {var} to 0.")
      } else if (str_detect(var, "bound") & str_detect(val, "--")) {
        .str("Since the uncorrected CI already contains {input$q}, it is not
             relevant to consider publication bias to shift the CI to include
             {input$q}.")
      } else {
        .str("For the {var} corrected for publication bias to shift to
             {input$q}, affirmative (i.e., significant and {more_likely})
             studies would need to be {sval_print(val)} times more likely to be
             published than nonaffirmative (i.e, nonsignificant or
             {less_likely}) studies.")
      }
    }
    paste(sval_text("point estimate", sval()$sval.est),
          sval_text("CI bound", sval()$sval.ci),
          sep = "<br>")
  })
  
  output$sval_summary <- renderUI({
    p(em(HTML(sval_summary())))
  })
  
  output$clip_sval <- renderUI({
    req(sval_summary())
    # message(sval_summary())
    rclipButton(
      inputId = "clipbtn_sval",
      label = "Copy summary",
      clipText = str_replace(sval_summary(), "<br>", "\n"),
      icon = icon("clipboard")
    )
  })
  
  # ----------------------------------------------------------------------------
  # significance_funnel
  # ----------------------------------------------------------------------------
  
  funnel_plot <- function() {
    significance_funnel(yi = meta_data()$yi, vi = meta_data()$vi,
                        favor.positive = positive(),
                        est.all = uncorrected_model()$estimate,
                        est.N = worst_model()$estimate) +
      theme_classic(base_family = "Lato") +
      theme(legend.position = "top",
            legend.title = element_blank())
  }
  
  fp_res <- 300
  fp_width <- 1200
  fp_height <- 1100
  
  output$funnel <- renderPlot(res = fp_res, height = fp_height,
                              width = fp_width, {
                                req(uncorrected_model(), worst_model())
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
  
  output$download_funnel_button <- renderUI({
    req(uncorrected_model(), worst_model())
    downloadButton("download_funnel")
  })
  
})
