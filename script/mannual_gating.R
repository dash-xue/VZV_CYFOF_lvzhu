library(CytoML)
library(flowWorkspace)
library(plotly)
library(shiny)
library(flowCore)

# ==== 路径 ====
FCS_PATH <- "/public/users/xueyupeng/VZV/lvzhu/subtype/B/A004-V0_20260129-1_1-3.fcs"

# ==== 直接读取 FCS ====
fr <- read.FCS(FCS_PATH, transformation = FALSE)

# ==== 参数信息 ====
params <- pData(parameters(fr))

# ==== 表达矩阵转 data.frame ====
df <- as.data.frame(exprs(fr))

# ==== 用 desc 替换列名 ====
old_names <- colnames(df)

idx <- match(old_names, params$name)

new_names <- params$desc[idx]
new_names[is.na(new_names) | new_names == ""] <- old_names[is.na(new_names) | new_names == ""]

new_names <- make.unique(new_names)

colnames(df) <- new_names

library(shiny)
library(plotly)

interactive_gate <- function(df, x_col, y_col, init_threshold = NULL) {
  
  result <- NULL
  
  # ======================
  # JS MUST be OUTSIDE
  # ======================
  js <- "
  document.addEventListener('keydown', function(e){

    if(e.key === 'Enter'){
      Shiny.setInputValue('confirm_key', Math.random());
    }

    if(e.key === 'ArrowLeft'){
      Shiny.setInputValue('key_left', Math.random());
    }

    if(e.key === 'ArrowRight'){
      Shiny.setInputValue('key_right', Math.random());
    }

  });
  "
  
  ui <- fluidPage(
    h3(paste("Gate:", x_col, "vs", y_col)),
    
    plotlyOutput("scatter", height = "650px"),
    
    br(),
    verbatimTextOutput("stats"),
    
    br(),
    actionButton("confirm", "Confirm (Enter)")
  )
  
  server <- function(input, output, session) {
    
    vals <- reactiveValues(threshold = init_threshold)
    
    x_range <- reactiveVal(NULL)
    
    # ======================
    # plot
    # ======================
    output$scatter <- renderPlotly({
      
      plot_ly(
        data = df,
        x = as.formula(paste0("~`", x_col, "`")),
        y = as.formula(paste0("~`", y_col, "`")),
        type = "scattergl",
        mode = "markers",
        marker = list(size = 3, opacity = 0.4)
      ) %>%
        event_register("plotly_click")
    })
    
    # ======================
    # init range
    # ======================
    
    observe({
      x_range(range(df[[x_col]], na.rm = TRUE))
    })
    
    # ======================
    # update line
    # ======================
    update_line <- function() {
      
      req(vals$threshold)
      
      plotlyProxy("scatter", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            shapes = list(
              list(
                type = "line",
                x0 = vals$threshold,
                x1 = vals$threshold,
                y0 = 0,
                y1 = 1,
                xref = "x",
                yref = "paper",
                line = list(color = "red", width = 2)
              )
            )
          )
        )
    }
    
    # ======================
    # click
    # ======================
    observeEvent(event_data("plotly_click"), {
      vals$threshold <- event_data("plotly_click")$x
      update_line()
    })
    
    # ======================
    # keyboard
    # ======================
    observeEvent(input$key_left, {
      
      req(vals$threshold)
      
      step <- diff(x_range()) / 1000
      vals$threshold <- vals$threshold - step
      
      update_line()
    })
    
    observeEvent(input$key_right, {
      
      req(vals$threshold)
      
      step <- diff(x_range()) / 1000
      vals$threshold <- vals$threshold + step
      
      update_line()
    })
    
    # ======================
    # stats
    # ======================
    output$stats <- renderText({
      
      req(vals$threshold)
      
      x <- df[[x_col]]
      
      total_n <- sum(!is.na(x))
      pos_n   <- sum(x > vals$threshold, na.rm = TRUE)
      pos_pct <- round(pos_n / total_n * 100, 2)
      
      paste0(
        "Threshold : ", round(vals$threshold, 4), "\n",
        "Positive  : ", pos_pct, "%\n",
        "Pos Count : ", pos_n, "\n",
        "Total     : ", total_n
      )
    })
    
    # ======================
    # confirm
    # ======================
    observeEvent(input$confirm_key, {
      
      req(vals$threshold)
      
      x <- df[[x_col]]
      
      total_n <- sum(!is.na(x))
      pos_n   <- sum(x > vals$threshold, na.rm = TRUE)
      pos_pct <- round(pos_n / total_n * 100, 4)
      
      result <<- list(
        threshold      = vals$threshold,
        positive_pct   = pos_pct,
        positive_count = pos_n,
        total_count    = total_n
      )
      
      stopApp()
    })
  }
  
  app <- shinyApp(
    ui = tagList(
      tags$script(HTML(js)),   # ✅ now js exists
      ui
    ),
    server = server
  )
  
  runApp(app)
  
  return(result)
}








interactive_gate <- function(df, x_col, y_col, init_threshold = NULL) {
  
  result <- NULL
  
  # ======================
  # JS keyboard control
  # ======================
  js <- "
  document.addEventListener('keydown', function(e){

    if(e.key === 'Enter'){
      Shiny.setInputValue('confirm_key', Math.random());
    }

    if(e.key === 'ArrowLeft'){
      Shiny.setInputValue('key_left', Math.random());
    }

    if(e.key === 'ArrowRight'){
      Shiny.setInputValue('key_right', Math.random());
    }

  });
  "
  
  ui <- fluidPage(
    h3(paste("Gate:", x_col, "vs", y_col)),
    
    plotlyOutput("scatter", height = "650px"),
    
    br(),
    verbatimTextOutput("stats"),
    
    br(),
    actionButton("confirm", "Confirm (Enter)")
  )
  
  server <- function(input, output, session) {
    
    vals <- reactiveValues(
      threshold = init_threshold
    )
    
    x_range <- reactiveVal(NULL)
    
    # ======================
    # plot (only once)
    # ======================
    output$scatter <- renderPlotly({
      
      plot_ly(
        data = df,
        x = as.formula(paste0("~`", x_col, "`")),
        y = as.formula(paste0("~`", y_col, "`")),
        type = "scattergl",
        mode = "markers",
        marker = list(size = 3, opacity = 0.4)
      ) %>%
        event_register("plotly_click")
    })
    
    # ======================
    # init range
    # ======================
    observe({
      x_range(range(df[[x_col]], na.rm = TRUE))
    })
    
    # ======================
    # draw / update line
    # ======================
    update_line <- function() {
      
      req(vals$threshold)
      
      plotlyProxy("scatter", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            shapes = list(
              list(
                type = "line",
                x0 = vals$threshold,
                x1 = vals$threshold,
                y0 = 0,
                y1 = 1,
                xref = "x",
                yref = "paper",
                line = list(color = "red", width = 2)
              )
            )
          )
        )
    }
    
    # ======================
    # ⭐关键修复：初始化自动画线
    # ======================
    observeEvent(vals$threshold, {
      req(vals$threshold)
      update_line()
    })
    
    # ======================
    # click set threshold
    # ======================
    observeEvent(event_data("plotly_click"), {
      
      vals$threshold <- event_data("plotly_click")$x
      update_line()
    })
    
    # ======================
    # keyboard LEFT
    # ======================
    observeEvent(input$key_left, {
      
      req(vals$threshold)
      
      step <- diff(x_range()) / 1000
      vals$threshold <- vals$threshold - step
      
      update_line()
    })
    
    # ======================
    # keyboard RIGHT
    # ======================
    observeEvent(input$key_right, {
      
      req(vals$threshold)
      
      step <- diff(x_range()) / 1000
      vals$threshold <- vals$threshold + step
      
      update_line()
    })
    
    # ======================
    # stats
    # ======================
    output$stats <- renderText({
      
      req(vals$threshold)
      
      x <- df[[x_col]]
      
      total_n <- sum(!is.na(x))
      pos_n   <- sum(x > vals$threshold, na.rm = TRUE)
      pos_pct <- round(pos_n / total_n * 100, 2)
      
      paste0(
        "Threshold : ", round(vals$threshold, 4), "\n",
        "Positive  : ", pos_pct, "%\n",
        "Pos Count : ", pos_n, "\n",
        "Total     : ", total_n
      )
    })
    
    # ======================
    # confirm
    # ======================
    observeEvent(input$confirm_key, {
      
      req(vals$threshold)
      
      x <- df[[x_col]]
      
      total_n <- sum(!is.na(x))
      pos_n   <- sum(x > vals$threshold, na.rm = TRUE)
      pos_pct <- round(pos_n / total_n * 100, 4)
      
      result <<- list(
        threshold      = vals$threshold,
        positive_pct   = pos_pct,
        positive_count = pos_n,
        total_count    = total_n
      )
      
      stopApp()
    })
  }
  
  app <- shinyApp(
    ui = tagList(
      tags$script(HTML(js)),
      ui
    ),
    server = server
  )
  
  runApp(app)
  
  return(result)
}



run_batch_gate <- function(files, x_col, y_col) {
  
  results <- list()
  threshold_prev <- NULL
  
  for (i in seq_along(files)) {
    
    fcs_path <- files[i]
    
    fr <- flowCore::read.FCS(fcs_path, transformation = FALSE)
    
    # ==== 参数信息 ====
    params <- pData(parameters(fr))
    
    # ==== 表达矩阵转 data.frame ====
    df <- as.data.frame(exprs(fr))
    
    # ==== 用 desc 替换列名 ====
    old_names <- colnames(df)
    
    idx <- match(old_names, params$name)
    
    new_names <- params$desc[idx]
    new_names[is.na(new_names) | new_names == ""] <- old_names[is.na(new_names) | new_names == ""]
    
    new_names <- make.unique(new_names)
    
    colnames(df) <- new_names
    
    cat("\nProcessing:", fcs_path, "\n")
    
    res <- interactive_gate(
      df = df,
      x_col = x_col,
      y_col = y_col,
      init_threshold = threshold_prev
    )
    
    results[[fcs_path]] <- res
    
    # 传递 threshold 到下一个文件
    threshold_prev <- res$threshold
  }
  
  return(results)
}

files = list.files('/public/users/xueyupeng/VZV/lvzhu/subtype/B',full.names = T)
resuts = run_batch_gate(files,x_col = 'CXCL9_146Nd',y_col = "CD19_174Yb")

thr <- interactive_gate(
  df,
  x_col = "CD45-1_106Cd",
  y_col = "CD45-2_110Cd"
)

thr

c('IFN-a_144Nd','CXCL9_146Nd','CD86_147Sm','H3K27ac_148Nd',
'IL-1b_149Sm','4E-BP1_150Nd','pS6_151Eu','CCR2_152Sm',
'TNF-a_153Eu','IL-4_154Sm','CASP-8_156Gd','CD38_159Tb',
'CD123_161Dy','Ki-67_162Dy','CXCL10_163Dy','Arginase-1_164Dy',
'IL-6_165Ho','MCP-1_166Er','CD69_167Er','STC1_168Er',
'GZMB_169Tm','HLA-DR_171Yb','IFN-g_172Yb','pSTAT1_175Lu','pSTAT3_176Yb')

common_cytokine = c('CXCL9_146Nd','H3K27ac_148Nd','4E-BP1_150Nd','pS6_151Eu',
                    'CASP-8_156Gd','CD38_159Tb','Ki-67_162Dy','CXCL10_163Dy',
                    'Arginase-1_164Dy','MCP-1_166Er','CD69_167Er','STC1_168Er',
                    'HLA-DR_171Yb','pSTAT1_175Lu','pSTAT3_176Yb')
b_cytokine = c('CD86_147Sm','TNF-a_153Eu','IL-4_154Sm','IL-6_165Ho','IFN-g_172Yb')












# ============================================================
# Required packages
# ============================================================
library(shiny)
library(plotly)
library(flowCore)
library(Biobase)

# ============================================================
# Single-file interactive gate
# ============================================================
interactive_gate <- function(df, x_col, y_col, init_threshold = NULL) {
  
  result <- NULL
  
  # ------------------------------------------------------------
  # keyboard binding
  # Enter      = confirm
  # Left Arrow = move left
  # Right Arrow= move right
  # ------------------------------------------------------------
  js <- "
  document.addEventListener('keydown', function(e){

    if(e.key === 'Enter'){
      Shiny.setInputValue('confirm_key', Math.random(), {priority:'event'});
    }

    if(e.key === 'ArrowLeft'){
      Shiny.setInputValue('key_left', Math.random(), {priority:'event'});
    }

    if(e.key === 'ArrowRight'){
      Shiny.setInputValue('key_right', Math.random(), {priority:'event'});
    }

  });
  "
  
  ui <- fluidPage(
    
    h3(paste0("Gate: ", x_col, " vs ", y_col)),
    
    plotlyOutput("scatter", height = "650px"),
    
    br(),
    
    verbatimTextOutput("stats"),
    
    br(),
    
    actionButton("confirm", "Confirm (Enter)")
  )
  
  server <- function(input, output, session) {
    
    vals <- reactiveValues(
      threshold = init_threshold
    )
    
    x_range <- reactiveVal(NULL)
    
    # ==========================================================
    # initial plot
    # ==========================================================
    output$scatter <- renderPlotly({
      
      xvals <- df[[x_col]]
      yvals <- df[[y_col]]
      
      x_range(range(xvals, na.rm = TRUE))
      
      plot_ly(
        data = df,
        x = xvals,
        y = yvals,
        type = "scattergl",
        mode = "markers",
        marker = list(size = 3, opacity = 0.4)
      ) %>%
        event_register("plotly_click")
    })
    
    # ==========================================================
    # draw line
    # ==========================================================
    update_line <- function() {
      
      req(vals$threshold)
      
      plotlyProxy("scatter", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            shapes = list(
              list(
                type = "line",
                x0 = vals$threshold,
                x1 = vals$threshold,
                y0 = 0,
                y1 = 1,
                xref = "x",
                yref = "paper",
                line = list(
                  color = "red",
                  width = 2
                )
              )
            )
          )
        )
    }
    
    # ==========================================================
    # auto draw initial threshold
    # ==========================================================
    observeEvent(vals$threshold, {
      req(vals$threshold)
      update_line()
    }, ignoreInit = FALSE)
    
    # ==========================================================
    # mouse click set threshold
    # ==========================================================
    observeEvent(event_data("plotly_click"), {
      
      click <- event_data("plotly_click")
      vals$threshold <- click$x
    })
    
    # ==========================================================
    # keyboard left
    # ==========================================================
    observeEvent(input$key_left, {
      
      req(vals$threshold)
      
      step <- diff(x_range()) / 1000
      vals$threshold <- vals$threshold - step
    })
    
    # ==========================================================
    # keyboard right
    # ==========================================================
    observeEvent(input$key_right, {
      
      req(vals$threshold)
      
      step <- diff(x_range()) / 1000
      vals$threshold <- vals$threshold + step
    })
    
    # ==========================================================
    # stats
    # ==========================================================
    output$stats <- renderText({
      
      req(vals$threshold)
      
      x <- df[[x_col]]
      
      total_n <- sum(!is.na(x))
      pos_n   <- sum(x > vals$threshold, na.rm = TRUE)
      pos_pct <- round(pos_n / total_n * 100, 2)
      
      paste0(
        "Threshold : ", round(vals$threshold, 4), "\n",
        "Positive  : ", pos_pct, "%\n",
        "Pos Count : ", pos_n, "\n",
        "Total     : ", total_n
      )
    })
    
    # ==========================================================
    # confirm button
    # ==========================================================
    confirm_fun <- function() {
      
      req(vals$threshold)
      
      x <- df[[x_col]]
      
      total_n <- sum(!is.na(x))
      pos_n   <- sum(x > vals$threshold, na.rm = TRUE)
      pos_pct <- round(pos_n / total_n * 100, 4)
      
      result <<- list(
        threshold      = vals$threshold,
        positive_pct   = pos_pct,
        positive_count = pos_n,
        total_count    = total_n
      )
      
      stopApp()
    }
    
    observeEvent(input$confirm_key, {
      confirm_fun()
    })
    
    observeEvent(input$confirm, {
      confirm_fun()
    })
  }
  
  app <- shinyApp(
    ui = tagList(
      tags$script(HTML(js)),
      ui
    ),
    server = server
  )
  
  runApp(app, launch.browser = FALSE)
  
  return(result)
}


# ============================================================
# Batch gate multiple FCS files
# ============================================================
run_batch_gate <- function(files, x_col, y_col) {
  
  results <- list()
  threshold_prev <- NULL
  
  for (i in seq_along(files)) {
    
    fcs_path <- files[i]
    
    cat("\n=====================================\n")
    cat("Processing:", i, "/", length(files), "\n")
    cat(fcs_path, "\n")
    cat("=====================================\n")
    
    # ----------------------------------------------------------
    # read FCS
    # ----------------------------------------------------------
    fr <- read.FCS(
      filename = fcs_path,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    # ----------------------------------------------------------
    # expression matrix
    # ----------------------------------------------------------
    df <- as.data.frame(exprs(fr))
    
    # ----------------------------------------------------------
    # parameter annotation
    # ----------------------------------------------------------
    params <- pData(parameters(fr))
    
    old_names <- colnames(df)
    idx <- match(old_names, params$name)
    
    new_names <- params$desc[idx]
    
    new_names[is.na(new_names) | new_names == ""] <-
      old_names[is.na(new_names) | new_names == ""]
    
    new_names <- make.unique(new_names)
    
    colnames(df) <- new_names
    
    # ----------------------------------------------------------
    # run gate
    # ----------------------------------------------------------
    res <- interactive_gate(
      df = df,
      x_col = x_col,
      y_col = y_col,
      init_threshold = threshold_prev
    )
    
    # save
    results[[fcs_path]] <- res
    
    # pass threshold to next file
    threshold_prev <- res$threshold
  }
  
  # ==========================================================
  # convert to table
  # ==========================================================
  out <- do.call(
    rbind,
    lapply(names(results), function(nm) {
      
      data.frame(
        file = nm,
        threshold = results[[nm]]$threshold,
        positive_pct = results[[nm]]$positive_pct,
        positive_count = results[[nm]]$positive_count,
        total_count = results[[nm]]$total_count,
        stringsAsFactors = FALSE
      )
    })
  )
  
  rownames(out) <- NULL
  
  return(out)
}

files = list.files('/public/users/xueyupeng/VZV/lvzhu/subtype/B',full.names = T)
options(shiny.launch.browser = FALSE)
resuts = run_batch_gate(files,x_col = 'CXCL9_146Nd',y_col = "CD19_174Yb")
