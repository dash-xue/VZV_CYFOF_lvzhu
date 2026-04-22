library(CytoML)
library(flowWorkspace)
library(plotly)
library(shiny)
library(flowCore)

# ==== 路径 ====
WSP_PATH <- "/public/users/xueyupeng/VZV/lvzhu/20260129/separate/20260129_separate.wsp"

# ==== 读取 FlowJo ====
ws <- open_flowjo_xml(WSP_PATH)
gs <- flowjo_to_gatingset(ws, name = "All Samples")

# ==== 取 root 数据 ====
fr <- gs_pop_get_data(gs, "root")[[1]]   # 取第一个样本
params <- pData(parameters(fr))
# ==== 转 data.frame ====
df <- as.data.frame(exprs(fr))

# 当前 df 列名
old_names <- colnames(df)

# 匹配 name
idx <- match(old_names, params$name)

# 构建新名字
new_names <- params$desc[idx]

# fallback：如果 desc 为空，用原 name
new_names[is.na(new_names) | new_names == ""] <- old_names[is.na(new_names) | new_names == ""]

# 去重（关键！）
new_names <- make.unique(new_names)

# 替换
colnames(df) <- new_names

# ==== UI ====
ui <- fluidPage(
  h3("Interactive gating demo"),
  plotlyOutput("scatter"),
  verbatimTextOutput("threshold"),
  verbatimTextOutput("summary")
)

# ==== Server ====
server <- function(input, output, session,x_col,y_col) {
  
  vals <- reactiveValues(threshold = NULL)
  
  # ===== 初始绘图（只画一次）=====
  output$scatter <- renderPlotly({
    plot_ly(
      data = df,
      x = ~ x_col,
      y = ~ y_col,
      type = "scattergl",
      mode = "markers"
    ) %>%
      event_register("plotly_click")
  })
  
  # ===== 点击事件：只更新竖线 =====
  observeEvent(event_data("plotly_click"), {
    
    click <- event_data("plotly_click")
    vals$threshold <- click$x
    
    # 👇 用 plotlyProxy 更新竖线（不重画图）
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
              yref = "paper",  # 关键：覆盖整个y轴
              line = list(color = "red", width = 2)
            )
          )
        )
      )
    
    # 可选：回写全局
    assign("clicked_threshold", vals$threshold, envir = .GlobalEnv)
  })
  
  # ===== 显示阈值 =====
  output$threshold <- renderPrint({
    vals$threshold
  })
  
  # ===== 分群统计 =====
  output$summary <- renderPrint({
    req(vals$threshold)
    
    tmp <- df
    tmp$pos <- tmp$x_col > vals$threshold
    
    table(tmp$pos)
  })
}

# ==== 启动 ====
shinyApp(ui, server,x_col = 'CD45-1_106Cd', y_col = 'CD45-2_110Cd')

clicked_threshold




interactive_gate <- function(df, x_col, y_col) {
  
  library(shiny)
  library(plotly)
  
  result <- NULL  # 用于保存返回值
  
  ui <- fluidPage(
    h3(paste("Click to set threshold:", x_col)),
    plotlyOutput("scatter"),
    verbatimTextOutput("threshold")
  )
  
  server <- function(input, output, session) {
    
    vals <- reactiveValues(threshold = NULL)
    
    output$scatter <- renderPlotly({
      plot_ly(
        data = df,
        x = as.formula(paste0("~`", x_col, "`")),
        y = as.formula(paste0("~`", y_col, "`")),
        type = "scattergl",
        mode = "markers"
      ) %>%
        event_register("plotly_click")
    })
    
    observeEvent(event_data("plotly_click"), {
      
      click <- event_data("plotly_click")
      vals$threshold <- click$x
      
      # 保存结果
      result <<- vals$threshold
      
      # 关闭 app（关键）
      stopApp()
    })
    
    output$threshold <- renderPrint({
      vals$threshold
    })
  }
  
  # 启动 app（阻塞）
  runApp(shinyApp(ui, server))
  
  return(result)
}

thr <- interactive_gate(
  df,
  x_col = "CD45-1_106Cd",
  y_col = "CD45-2_110Cd"
)

thr
