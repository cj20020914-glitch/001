# =============================================================================
# modules/preprocess_module.R
# GPL (Gene Expression Platform) 文件预处理模块
# 功能：上传、解析、预览Affymetrix GPL注释文件
# 版本：2.1
# =============================================================================

# =============================================================================
# UI 部分
# =============================================================================
preprocess_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; border-radius: 10px; color: white; margin-bottom: 20px;",
          h3(icon("file-code"), " GPL 注释文件预处理", style = "margin: 0;"),
          p("上传并解析 Affymetrix GPL 平台注释文件 (.txt, .gpl)", style = "margin: 5px 0 0 0; opacity: 0.9;")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        wellPanel(
          style = "background-color: #f8f9fa;",
          h4(icon("upload"), " 上传GPL文件"),
          p("支持 .txt, .gpl, .annot 格式", style = "color: #6c757d; font-size: 12px;"),
          fileInput(
            ns("gplFile"),
            label = NULL,
            accept = c(".txt", ".gpl", ".annot", "text/plain"),
            buttonLabel = "浏览文件",
            placeholder = "选择GPL注释文件"
          ),
          div(
            style = "margin-top: 10px;",
            actionButton(
              ns("loadExample"),
              "加载示例数据",
              icon = icon("flask"),
              class = "btn-info btn-sm"
            ),
            actionButton(
              ns("clearData"),
              "清除数据",
              icon = icon("trash"),
              class = "btn-danger btn-sm",
              style = "margin-left: 10px;"
            )
          )
        )
      ),
      
      column(
        width = 6,
        wellPanel(
          style = "background-color: #f8f9fa;",
          h4(icon("info-circle"), " 文件信息"),
          verbatimTextOutput(ns("fileInfo")),
          br(),
          h4(icon("table"), " 数据概览"),
          verbatimTextOutput(ns("dataSummary"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          style = "background-color: white; border: 1px solid #dee2e6;",
          h4(icon("eye"), " 数据预览"),
          p("显示前100行数据，点击表头可排序，使用搜索框可过滤",
            style = "color: #6c757d; font-size: 12px;"),
          br(),
          DTOutput(ns("gplPreview")) %>% withSpinner(color = "#667eea")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h5(icon("list"), " 列名列表"),
          div(
            style = "max-height: 300px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px;",
            verbatimTextOutput(ns("columnNames"))
          )
        )
      ),
      column(
        width = 6,
        wellPanel(
          h5(icon("bar-chart"), " 统计信息"),
          div(
            style = "background: #f8f9fa; padding: 10px; border-radius: 4px;",
            verbatimTextOutput(ns("statistics"))
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "text-align: right; margin-top: 10px;",
          downloadButton(
            ns("downloadData"),
            "下载解析后的数据 (CSV)",
            icon = icon("download"),
            class = "btn-success"
          ),
          actionButton(
            ns("showRaw"),
            "查看原始数据",
            icon = icon("code"),
            class = "btn-warning",
            style = "margin-left: 10px;"
          )
        )
      )
    ),
    
    bsModal(
      id = ns("rawModal"),
      title = "原始数据（前50行）",
      trigger = ns("showRaw"),
      verbatimTextOutput(ns("rawData"))
    )
  )
}


# =============================================================================
# Server 部分
# =============================================================================
preprocess_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 辅助函数 ----
    rep_char <- function(char, n) {
      paste(rep(char, n), collapse = "")
    }
    
    # ---- 响应式变量 ----
    gpl_data <- reactiveVal(NULL)
    gpl_filename <- reactiveVal(NULL)
    parse_status <- reactiveVal(list(success = FALSE, message = ""))
    
    # ---- 核心解析函数 ----
    parse_gpl_file <- function(file_path, file_name = NULL) {
      tryCatch({
        first_lines <- readLines(file_path, n = 50, warn = FALSE)
        
        comment_lines <- grep("^#", first_lines)
        skip_rows <- if (length(comment_lines) > 0) max(comment_lines) else 0
        
        if (skip_rows >= length(first_lines) - 1) {
          header_candidates <- grep("ID\\t|^ID\\s", first_lines, value = TRUE)
          if (length(header_candidates) > 0) {
            skip_rows <- which(first_lines == header_candidates[1]) - 1
          } else {
            skip_rows <- 33
          }
        }
        
        data <- tryCatch({
          read.delim(
            file_path,
            skip = skip_rows,
            header = TRUE,
            sep = "\t",
            quote = "",
            stringsAsFactors = FALSE,
            fill = TRUE,
            check.names = FALSE,
            na.strings = c("", "NA", "null", "NULL")
          )
        }, error = function(e) {
          read.csv(
            file_path,
            skip = skip_rows,
            header = TRUE,
            stringsAsFactors = FALSE,
            na.strings = c("", "NA", "null", "NULL")
          )
        })
        
        if (is.null(data) || nrow(data) == 0) {
          return(list(
            success = FALSE,
            data = NULL,
            message = "文件为空或无法解析"
          ))
        }
        
        colnames(data) <- gsub("^#", "", colnames(data))
        colnames(data) <- trimws(colnames(data))
        
        return(list(
          success = TRUE,
          data = data,
          message = paste("成功解析", nrow(data), "行,", ncol(data), "列"),
          skip_rows = skip_rows,
          file_name = file_name
        ))
        
      }, error = function(e) {
        return(list(
          success = FALSE,
          data = NULL,
          message = paste("解析错误:", e$message)
        ))
      })
    }
    
    # ---- 文件上传 ----
    observeEvent(input$gplFile, {
      req(input$gplFile)
      
      showNotification("正在解析GPL文件...", type = "info", duration = 2)
      
      result <- parse_gpl_file(
        input$gplFile$datapath,
        input$gplFile$name
      )
      
      if (result$success) {
        gpl_data(result$data)
        gpl_filename(result$file_name)
        parse_status(list(
          success = TRUE,
          message = result$message
        ))
        
        showNotification(
          paste("✅ 解析成功!", result$message),
          type = "success",
          duration = 3
        )
      } else {
        gpl_data(NULL)
        parse_status(list(
          success = FALSE,
          message = result$message
        ))
        
        showNotification(
          paste("❌ 解析失败:", result$message),
          type = "error",
          duration = 5
        )
      }
    })
    
    # ---- 加载示例数据 ----
    observeEvent(input$loadExample, {
      showNotification("正在加载示例数据...", type = "info", duration = 2)
      
      example_data <- data.frame(
        ID = paste0("example_", 1:50, "_at"),
        GB_ACC = rep(c("NM_000001", "NM_000002", "NM_000003"), length.out = 50),
        "Gene Symbol" = paste0("GENE", 1:50),
        "Gene Title" = paste0("Gene Title for Gene", 1:50),
        "ENTREZ_GENE_ID" = 1000:1049,
        "GO Biological Process" = rep("GO:0008152 // metabolic process // IEA", 50),
        "GO Cellular Component" = rep("GO:0005737 // cytoplasm // IEA", 50),
        "GO Molecular Function" = rep("GO:0005515 // protein binding // IPI", 50),
        check.names = FALSE
      )
      
      gpl_data(example_data)
      gpl_filename("示例_GPL_data.txt")
      parse_status(list(
        success = TRUE,
        message = "示例数据加载成功"
      ))
      
      showNotification("✅ 示例数据加载成功!", type = "success", duration = 3)
    })
    
    # ---- 清除数据 ----
    observeEvent(input$clearData, {
      gpl_data(NULL)
      gpl_filename(NULL)
      parse_status(list(success = FALSE, message = "数据已清除"))
      reset("gplFile")
      showNotification("数据已清除", type = "warning", duration = 2)
    })
    
    # ---- 文件信息 ----
    output$fileInfo <- renderPrint({
      data <- gpl_data()
      status <- parse_status()
      
      if (is.null(data)) {
        cat("📄 未加载任何数据\n")
        cat("状态: ", ifelse(status$success, "✅ 就绪", "⏳ 等待数据"), "\n")
        cat("提示: 请上传GPL文件或加载示例数据\n")
        return()
      }
      
      cat("📄 文件名:", gpl_filename(), "\n")
      cat("📊 行数:", format(nrow(data), big.mark = ","), "\n")
      cat("📋 列数:", ncol(data), "\n")
      cat("📈 数据大小:", format(object.size(data), units = "auto"), "\n")
      cat("✅ 状态:", status$message, "\n")
    })
    
    # ---- 数据摘要 ----
    output$dataSummary <- renderPrint({
      data <- gpl_data()
      
      if (is.null(data)) {
        cat("暂无数据摘要")
        return()
      }
      
      has_id <- any(grepl("ID$|^ID$", colnames(data), ignore.case = TRUE))
      has_symbol <- any(grepl("Gene.*Symbol|Symbol", colnames(data), ignore.case = TRUE))
      has_title <- any(grepl("Gene.*Title|Title", colnames(data), ignore.case = TRUE))
      has_go <- any(grepl("GO|Ontology", colnames(data), ignore.case = TRUE))
      
      cat("🔍 关键列检测:\n")
      cat("  - 探针ID列:", ifelse(has_id, "✅", "❌"), "\n")
      cat("  - 基因符号列:", ifelse(has_symbol, "✅", "❌"), "\n")
      cat("  - 基因名称列:", ifelse(has_title, "✅", "❌"), "\n")
      cat("  - GO注释列:", ifelse(has_go, "✅", "❌"), "\n")
      cat("\n")
      
      cat("📊 完整性统计:\n")
      for (col in colnames(data)[1:min(5, ncol(data))]) {
        non_na <- sum(!is.na(data[[col]]) & data[[col]] != "")
        pct <- round(non_na / nrow(data) * 100, 1)
        cat("  - ", col, ": ", pct, "% 非空 (", non_na, "/", nrow(data), ")", "\n", sep = "")
      }
    })
    
    # ---- 数据预览 ----
    output$gplPreview <- renderDT({
      data <- gpl_data()
      
      if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
        return(
          datatable(
            data.frame(
              "状态" = "⚠️ 暂无数据",
              "提示" = "请上传GPL文件或点击'加载示例数据'",
              check.names = FALSE
            ),
            options = list(
              dom = "t",
              pageLength = 1,
              ordering = FALSE,
              searching = FALSE
            ),
            rownames = FALSE,
            class = "display"
          ) %>%
            formatStyle(
              columns = 1:2,
              fontSize = "14px",
              color = "#6c757d",
              backgroundColor = "#f8f9fa"
            )
        )
      }
      
      display_data <- data
      total_rows <- nrow(data)
      
      if (total_rows > 100) {
        display_data <- data[1:100, ]
        row_note <- paste0("（显示前100行，共", format(total_rows, big.mark = ","), "行）")
      } else {
        row_note <- paste0("（共", total_rows, "行）")
      }
      
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "500px",
          scroller = TRUE,
          autoWidth = TRUE,
          dom = "lfrtipS",
          language = list(
            search = "搜索:",
            lengthMenu = "显示 _MENU_ 行",
            info = row_note,
            infoEmpty = "无数据",
            infoFiltered = "（从 _MAX_ 行中过滤）"
          ),
          columnDefs = list(
            list(
              targets = "_all",
              render = JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display' && typeof data === 'string' && data.length > 300) {",
                "    return '<span title=\"' + data.replace(/\"/g, '&quot;') + '\">' + data.substr(0, 300) + '...</span>';",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        ),
        class = "display nowrap compact stripe hover",
        rownames = FALSE,
        filter = "top",
        extensions = 'Scroller'
      )
      
      if (ncol(display_data) > 0) {
        all_cols <- names(display_data)
        
        id_cols <- grep("ID$|^ID$|Probe|ProbeSet|Affymetrix", all_cols, ignore.case = TRUE, value = TRUE)
        if (length(id_cols) > 0) {
          dt <- dt %>% formatStyle(columns = id_cols, fontWeight = "bold", color = "#2980b9")
        }
        
        gene_cols <- grep("Gene|Symbol|Name|Title|Description", all_cols, ignore.case = TRUE, value = TRUE)
        if (length(gene_cols) > 0) {
          dt <- dt %>% formatStyle(columns = gene_cols, backgroundColor = "#e8f4f8")
        }
        
        go_cols <- grep("GO|Ontology|Process|Function|Component", all_cols, ignore.case = TRUE, value = TRUE)
        if (length(go_cols) > 0) {
          dt <- dt %>% formatStyle(columns = go_cols, fontSize = "11px", backgroundColor = "#f8f9fa", color = "#2c3e50")
        }
        
        num_cols <- which(sapply(display_data, is.numeric))
        if (length(num_cols) > 0) {
          dt <- dt %>% formatStyle(columns = num_cols, textAlign = "right", fontFamily = "monospace")
        }
        
        acc_cols <- grep("GB_ACC|Accession|RefSeq|ENTREZ", all_cols, ignore.case = TRUE, value = TRUE)
        if (length(acc_cols) > 0) {
          dt <- dt %>% formatStyle(columns = acc_cols, color = "#27ae60", fontFamily = "monospace")
        }
        
        species_cols <- grep("Species|Organism|Scientific", all_cols, ignore.case = TRUE, value = TRUE)
        if (length(species_cols) > 0) {
          dt <- dt %>% formatStyle(columns = species_cols, fontStyle = "italic", color = "#8e44ad")
        }
      }
      
      return(dt)
    })
    
    # ---- 列名输出 ----
    output$columnNames <- renderPrint({
      data <- gpl_data()
      
      if (is.null(data)) {
        cat("暂无数据")
        return()
      }
      
      cols <- names(data)
      for (i in seq_along(cols)) {
        cat(sprintf("%3d: %s\n", i, cols[i]))
      }
    })
    
    # ---- 统计信息 ----
    output$statistics <- renderPrint({
      data <- gpl_data()
      
      if (is.null(data)) {
        cat("暂无统计信息")
        return()
      }
      
      cat("📊 数据统计:\n")
      cat(rep_char("━", 40), "\n")
      cat(sprintf("总行数: %s\n", format(nrow(data), big.mark = ",")))
      cat(sprintf("总列数: %s\n", ncol(data)))
      cat("\n")
      
      col_types <- sapply(data, class)
      type_counts <- table(col_types)
      cat("列类型分布:\n")
      for (type in names(type_counts)) {
        cat(sprintf("  - %s: %d 列\n", type, type_counts[type]))
      }
      cat("\n")
      
      cat("前5列唯一值数量:\n")
      for (col in names(data)[1:min(5, ncol(data))]) {
        unique_count <- length(unique(data[[col]]))
        cat(sprintf("  - %s: %s\n", col, format(unique_count, big.mark = ",")))
      }
    })
    
    # ---- 下载功能 ----
    output$downloadData <- downloadHandler(
      filename = function() {
        base_name <- ifelse(is.null(gpl_filename()), "GPL_data", tools::file_path_sans_ext(gpl_filename()))
        paste0(base_name, "_parsed_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- gpl_data()
        if (!is.null(data)) {
          write.csv(data, file, row.names = FALSE, na = "")
        } else {
          write.csv(data.frame(信息 = "无数据可下载"), file, row.names = FALSE)
        }
      }
    )
    
    # ---- 原始数据查看 ----
    output$rawData <- renderPrint({
      data <- gpl_data()
      
      if (is.null(data)) {
        cat("暂无原始数据")
        return()
      }
      
      cat("原始数据（前50行）:\n")
      cat(rep_char("━", 60), "\n")
      print(head(data, 50))
      cat("\n... 共", nrow(data), "行")
    })
    
    # ---- 返回数据 ----
    return(list(
      data = reactive({ gpl_data() }),
      status = reactive({ parse_status() }),
      filename = reactive({ gpl_filename() })
    ))
    
  })
}