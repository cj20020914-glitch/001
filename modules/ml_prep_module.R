# ml_prep_module.R - 机器学习准备文件生成模块

load_module_dependency <- get0("safe_library", mode = "function", inherits = TRUE)
if (is.null(load_module_dependency)) {
  load_module_dependency <- function(package, required = TRUE) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
    TRUE
  }
}
invisible(vapply(c("limma", "dplyr"), load_module_dependency, logical(1), required = TRUE))
rm(load_module_dependency)

# ============================================================
# UI
# ============================================================
ml_prep_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # ---- 左侧控制面板 ----
    column(
      width = 3,
      wellPanel(
        h4("⚙️ 参数设置"),
        
        fileInput(ns("exprFile"), "📤 上传表达矩阵 (CSV)",
                  accept = c(".csv")),
        p("格式：第一列为基因名，其余列为样本表达值", 
          style = "font-size: 11px; color: #888;"),
        
        hr(),
        
        fileInput(ns("geneFile"), "📤 上传基因列表 (TXT/CSV)",
                  accept = c(".txt", ".csv")),
        p("格式：每行一个基因名", 
          style = "font-size: 11px; color: #888;"),
        
        hr(),
        
        actionButton(ns("run"), "🚀 提取基因表达数据", 
                     class = "btn-success",
                     style = "width: 100%; font-size: 16px; font-weight: bold;")
      )
    ),
    
    # ---- 右侧展示面板 ----
    column(
      width = 9,
      tabsetPanel(
        id = ns("tabset"),
        
        tabPanel(
          "📊 数据预览",
          br(),
          uiOutput(ns("previewUI")),
          br(),
          downloadButton(ns("downloadResult"), "下载 CSV")
        ),
        
        tabPanel(
          "📋 提取信息",
          br(),
          uiOutput(ns("infoUI"))
        )
      )
    )
  )
}


# ============================================================
# Server
# ============================================================
ml_prep_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储结果 ----
    result_data <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    extract_info <- reactiveVal(NULL)
    
    # ---- 核心分析 ----
    observeEvent(input$run, {
      
      # 检查文件是否上传
      if (is.null(input$exprFile)) {
        showNotification("⚠️ 请上传表达矩阵文件！", type = "error")
        return()
      }
      
      if (is.null(input$geneFile)) {
        showNotification("⚠️ 请上传基因列表文件！", type = "error")
        return()
      }
      
      is_running(TRUE)
      result_data(NULL)
      extract_info(NULL)
      
      withProgress(message = "⏳ 提取基因表达数据...", value = 0, {
        
        incProgress(0.1, detail = "读取表达矩阵...")
        
        tryCatch({
          # ---- 1. 读取表达矩阵 ----
          expr_matrix <- read_expression_matrix(input$exprFile)
          
          # 第一列是基因名
          
          # 转换为数值矩阵
          
          # 过滤低表达基因（行均值 > 0）
          expr_matrix <- expr_matrix[rowMeans(expr_matrix, na.rm = TRUE) > 0, ]
          
          incProgress(0.4, detail = "读取基因列表...")
          
          # ---- 2. 读取基因列表 ----
          gene_list <- read_gene_list_file(input$geneFile)
          
          if (length(gene_list) == 0) {
            showNotification("⚠️ 基因列表为空！", type = "error")
            is_running(FALSE)
            return()
          }
          
          incProgress(0.7, detail = "提取交集基因...")
          
          # ---- 3. 提取共同基因 ----
          common_genes <- intersect(gene_list, rownames(expr_matrix))
          
          if (length(common_genes) == 0) {
            showNotification("⚠️ 未找到共同基因！请检查基因名是否匹配", type = "error")
            is_running(FALSE)
            return()
          }
          
          # 提取表达数据
          gene_expr <- expr_matrix[common_genes, , drop = FALSE]
          
          incProgress(0.9, detail = "生成结果...")
          
          # ---- 4. 构建输出 ----
          # 第一行是样本名（ID）
          output_df <- data.frame(
            ID = rownames(gene_expr),
            gene_expr,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          
          # ---- 5. 存储结果 ----
          result_data(output_df)
          
          # 提取信息
          info <- list(
            total_genes = nrow(expr_matrix),
            total_samples = ncol(expr_matrix),
            gene_list_count = length(gene_list),
            common_count = length(common_genes),
            missing_count = length(gene_list) - length(common_genes)
          )
          extract_info(info)
          
          is_running(FALSE)
          incProgress(1.0, detail = "✅ 完成！")
          
          showNotification(
            paste0("✅ 提取完成！共提取 ", length(common_genes), " 个基因的表达数据"),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          showNotification(paste0("❌ 错误: ", e$message), type = "error", duration = 10)
          is_running(FALSE)
        })
      })
    })
    
    # ---- 数据预览 ----
    output$previewTable <- renderDT({
      df <- result_data()
      if (is.null(df)) {
        return(datatable(data.frame(信息 = "请先运行提取")))
      }
      
      # 预览前10行前10列
      preview_rows <- min(10, nrow(df))
      preview_cols <- min(10, ncol(df))
      preview_df <- df[1:preview_rows, 1:preview_cols]
      
      datatable(preview_df, 
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    })
    
    output$previewUI <- renderUI({
      if (is_running()) {
        div(
          style = "text-align: center; padding: 50px;",
          h4("⏳ 提取中，请稍候...", style = "color: #666;")
        )
      } else {
        tagList(
          p("预览前 10 行 × 10 列："),
          DTOutput(ns("previewTable"))
        )
      }
    })
    
    # ---- 提取信息 ----
    output$infoTable <- renderDT({
      info <- extract_info()
      if (is.null(info)) {
        return(datatable(data.frame(信息 = "请先运行提取")))
      }
      
      info_df <- data.frame(
        项目 = c("原始基因数", "原始样本数", "基因列表基因数", 
               "匹配基因数", "未匹配基因数"),
        数值 = c(info$total_genes, info$total_samples, 
               info$gene_list_count, info$common_count, 
               info$missing_count)
      )
      
      datatable(info_df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })
    
    output$infoUI <- renderUI({
      if (is_running()) {
        div(
          style = "text-align: center; padding: 50px;",
          h4("⏳ 提取中，请稍候...", style = "color: #666;")
        )
      } else {
        tagList(
          h5("📊 提取统计"),
          DTOutput(ns("infoTable"))
        )
      }
    })
    
    # ---- 下载 ----
    output$downloadResult <- downloadHandler(
      filename = function() paste0("geneexp_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- result_data()
        if (is.null(df)) {
          write.csv(data.frame(信息 = "请先运行提取"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
