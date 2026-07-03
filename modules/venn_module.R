# venn_module.R - Venn图模块（完整版）
# 功能：绘制韦恩图，支持2-5组数据，输出交集列表
# 参考差异分析模块的布局和风格

# ============================================================
# UI
# ============================================================
venn_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .venn-card,
        .venn-plot-card,
        .venn-result-card {
            border: 1px solid #b0bec5;
            border-radius: 4px;
            padding: 12px 16px;
            background-color: #ffffff;
        }
        .venn-card,
        .venn-plot-card {
            height: 370px;
            overflow-y: auto;
        }
        .venn-card h4,
        .venn-plot-card h4,
        .venn-result-card h4 {
            margin-top: 0;
            margin-bottom: 8px;
            font-size: 14px;
            font-weight: 700;
            color: #263238;
        }
        .venn-card hr,
        .venn-plot-card hr {
            margin: 4px 0 8px 0;
        }
        .venn-group-toolbar {
            display: flex;
            gap: 4px;
            flex-wrap: wrap;
            align-items: center;
            margin-bottom: 8px;
        }
        .venn-group-toolbar .btn-xs {
            font-size: 10px;
            padding: 2px 9px;
            border-radius: 0;
        }
        .venn-group-count {
            font-size: 11px;
            color: #607d8b;
            margin-left: 4px;
        }
        .venn-group-row {
            display: grid;
            grid-template-columns: minmax(240px, 1fr) 110px 34px;
            gap: 6px;
            align-items: center;
            margin-bottom: 6px;
        }
        .venn-upload-box {
            position: relative;
            border: 1px dashed #b0bec5;
            min-height: 46px;
            padding: 5px 8px;
            background: #ffffff;
            cursor: pointer;
            overflow: hidden;
        }
        .venn-upload-box:hover {
            background: #f7fafc;
        }
        .venn-upload-box .shiny-input-container {
            position: absolute;
            inset: 0;
            width: 100% !important;
            height: 100%;
            margin: 0;
            opacity: 0;
            z-index: 2;
            cursor: pointer;
        }
        .venn-upload-placeholder {
            pointer-events: none;
            display: grid;
            grid-template-columns: 22px minmax(0, 1fr);
            gap: 6px;
            align-items: center;
            min-height: 34px;
        }
        .venn-upload-title {
            display: block;
            color: #263238;
            font-size: 11px;
            font-weight: 700;
            line-height: 1.2;
        }
        .venn-upload-status {
            display: block;
            color: #1e88e5;
            font-size: 10px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .venn-group-row .shiny-input-container {
            margin-bottom: 0;
        }
        .venn-group-row .form-control {
            height: 28px;
            font-size: 11px;
            padding: 2px 5px;
            border-radius: 0;
        }
        .venn-group-row .btn-xs {
            width: 28px;
            height: 28px;
            padding: 0;
            border-radius: 0;
            font-weight: 700;
        }
        .venn-group-row .colour-input-container {
            width: 34px !important;
        }
        .venn-compact-section {
            border: 1px solid #d7dee2;
            background: #ffffff;
            padding: 6px 8px;
            margin-top: 8px;
            margin-bottom: 8px;
        }
        .venn-compact-title {
            display: block;
            color: #263238;
            font-size: 11px;
            font-weight: 700;
            margin-bottom: 5px;
        }
        .venn-compact-grid {
            display: grid;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 5px 8px;
            align-items: center;
        }
        .venn-mini-control {
            display: grid;
            grid-template-columns: auto minmax(0, 1fr);
            gap: 5px;
            align-items: center;
            font-size: 10px;
            color: #263238;
        }
        .venn-mini-control .shiny-input-container {
            margin-bottom: 0;
        }
        .venn-mini-control .form-control {
            height: 24px;
            padding: 2px 4px;
            font-size: 11px;
        }
        .venn-radio-inline .shiny-options-group {
            display: flex;
            gap: 10px;
            align-items: center;
        }
        .venn-radio-inline .radio {
            margin: 0;
            font-size: 11px;
        }
        .venn-plot-box {
            min-height: 285px;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .venn-result-panel {
            max-width: 100%;
            overflow-x: hidden;
        }
        .venn-result-panel .nav-tabs {
            border-bottom: 1px solid #d7dee2;
            margin-bottom: 8px;
        }
        .venn-result-panel .nav-tabs > li > a {
            border-radius: 0;
            border: none;
            margin-right: 26px;
            padding: 8px 2px 9px 2px;
            color: #37474f;
            background: transparent;
            font-size: 12px;
        }
        .venn-result-panel .nav-tabs > li.active > a,
        .venn-result-panel .nav-tabs > li.active > a:hover,
        .venn-result-panel .nav-tabs > li.active > a:focus {
            border: none;
            border-bottom: 2px solid #1e88e5;
            color: #1e88e5;
            background: transparent;
            font-weight: 700;
        }
        .venn-result-file-list {
            border: 1px solid #d7dee2;
            background: #ffffff;
        }
        .venn-result-file-row {
            display: grid;
            grid-template-columns: 28px minmax(150px, 1fr) 54px minmax(150px, 1.4fr) 70px;
            gap: 8px;
            align-items: center;
            padding: 6px 8px;
            border-bottom: 1px solid #eef2f4;
            font-size: 11px;
        }
        .venn-result-file-row:last-child {
            border-bottom: none;
        }
        .venn-file-index {
            color: #1e88e5;
            font-weight: 700;
        }
        .venn-result-file-name {
            color: #263238;
            font-weight: 700;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .venn-result-file-type,
        .venn-result-file-desc {
            color: #607d8b;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .venn-result-file-type {
            color: #455a64;
            font-weight: 700;
        }
        .venn-result-file-download .btn {
            font-size: 10px;
            padding: 1px 8px;
            line-height: 1.4;
        }
        .venn-result-data-preview {
            max-height: 190px;
            overflow-y: auto;
        }
        .venn-result-data-preview h5 {
            margin: 4px 0 6px 0;
            font-size: 12px;
            font-weight: 700;
            color: #263238;
        }
        .venn-download-size-controls {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px;
            margin-top: 8px;
        }
        .venn-download-size-controls .shiny-input-container {
            width: 100%;
            margin-bottom: 0;
        }
        .venn-qa {
            font-size: 12px;
            line-height: 1.7;
            color: #455a64;
            max-height: 190px;
            overflow-y: auto;
        }
        .venn-qa dl {
            margin: 0;
        }
        .venn-qa dt {
            margin-top: 8px;
            color: #263238;
        }
        .venn-qa dt:first-child {
            margin-top: 0;
        }
        .venn-qa dd {
            margin-left: 0;
            margin-bottom: 4px;
        }
    ")),
    
    fluidRow(
      style = "margin: 0;",
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "venn-card",
          h4("参数设置"),
          div(
            class = "venn-group-toolbar",
            tags$span("组数:", style = "font-size: 11px; font-weight: 700; color: #263238;"),
            actionButton(ns("set2"), "2组", class = "btn-primary btn-xs"),
            actionButton(ns("set3"), "3组", class = "btn-default btn-xs"),
            actionButton(ns("set4"), "4组", class = "btn-default btn-xs"),
            actionButton(ns("set5"), "5组", class = "btn-default btn-xs"),
            tags$span(id = ns("groupCountLabel"), "当前: 2组", class = "venn-group-count")
          ),
          uiOutput(ns("groupInputs")),
          div(
            class = "venn-compact-section",
            span("图片参数", class = "venn-compact-title"),
            div(
              class = "venn-compact-grid",
              div(class = "venn-mini-control", span("宽度"), numericInput(ns("vennWidth"), NULL, value = 7, min = 3, max = 20, step = 0.5)),
              div(class = "venn-mini-control", span("高度"), numericInput(ns("vennHeight"), NULL, value = 6, min = 3, max = 20, step = 0.5)),
              div(class = "venn-mini-control", span("集合名"), numericInput(ns("vennSetNameSize"), NULL, value = 6, min = 2, max = 20, step = 0.5)),
              div(class = "venn-mini-control", span("数字"), numericInput(ns("vennTextSize"), NULL, value = 4.5, min = 2, max = 20, step = 0.5)),
              div(
                class = "venn-radio-inline",
                style = "grid-column: 1 / -1;",
                radioButtons(ns("vennShowPercent"), "百分比", choices = c("显示" = "yes", "不显示" = "no"), selected = "yes", inline = TRUE)
              )
            )
          ),
          actionButton(ns("runVenn"), "绘制 Venn 图", 
                        class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 5px 0; border-radius: 0;")
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "venn-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "venn-plot-box",
            plotOutput(
              ns("venn_plot"),
              height = "285px",
              width = "100%",
              click = ns("vennPlot_click")
            )
          )
        )
      )
    ),
    
    fluidRow(
      style = "margin: 0;",
      column(
        width = 12,
        style = "padding: 4px;",
        tags$div(
          class = "venn-result-card",
          h4("结果预览"),
          div(
            class = "venn-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel("结果表", uiOutput(ns("resultFileList"))),
              tabPanel(
                "数据预览",
                div(
                  class = "venn-result-data-preview",
                  h5("交集基因"),
                  DTOutput(ns("intersectTable")),
                  h5("基因统计"),
                  DTOutput(ns("statsTable"))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "venn-qa",
                  tags$dl(
                    tags$dt("Q1：Venn 图模块用于什么？"),
                    tags$dd("Venn 图用于展示 2-5 个基因列表之间的重叠关系，常用于差异基因、富集结果、机器学习筛选基因等多个来源的交集分析。"),
                    tags$dt("Q2：输入文件格式有什么要求？"),
                    tags$dd("每个 TXT 文件第一列为基因名，每行一个基因。空行、NA 和重复基因会自动过滤。文件中如果有多列，模块默认读取第一列。"),
                    tags$dt("Q3：集合名称和颜色会影响分析吗？"),
                    tags$dd("集合名称和颜色只影响图形显示及结果表列名，不改变交集计算逻辑。建议使用简短名称，例如 DEG、WGCNA、ML 或 Treat。"),
                    tags$dt("Q4：结果文件包含什么？"),
                    tags$dd("venn.png 是当前 Venn 图；intersection_genes.txt 是所有集合共同交集基因；membership_matrix.csv 显示每个基因属于哪些集合；set_statistics.csv 显示各集合和总交集数量。"),
                    tags$dt("Q5：什么时候使用交集基因？"),
                    tags$dd("所有集合共同交集适合后续富集分析、ROC 验证、机器学习建模或候选基因筛选。若共同交集过少，可减少集合数量或分别查看两两交集。")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}


# ============================================================
# Server
# ============================================================
venn_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 状态变量 ----
    group_count <- reactiveVal(2)
    venn_results <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)

    venn_clean_number <- function(value, default, min_value, max_value) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) != 1 || is.na(value)) {
        value <- default
      }
      max(min_value, min(max_value, value))
    }

    set_group_count <- function(n) {
      group_count(n)
      venn_results(NULL)
      shinyjs::runjs(sprintf(
        "$('#%s,#%s,#%s,#%s').removeClass('btn-primary').addClass('btn-default'); $('#%s').removeClass('btn-default').addClass('btn-primary'); $('#%s').text('当前: %d组');",
        ns("set2"), ns("set3"), ns("set4"), ns("set5"), ns(paste0("set", n)), ns("groupCountLabel"), n
      ))
    }

    draw_venn_plot <- function(res, set_name_size = NULL, text_size = NULL, stroke_size = 0.7) {
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return(invisible(NULL))
      }

      p <- ggvenn::ggvenn(
        res$gene_list,
        show_percentage = identical(input$vennShowPercent %||% "yes", "yes"),
        stroke_color = "white",
        stroke_size = stroke_size,
        fill_color = res$colors,
        set_name_color = res$colors,
        set_name_size = venn_clean_number(set_name_size %||% input$vennSetNameSize, 6, 2, 20),
        text_size = venn_clean_number(text_size %||% input$vennTextSize, 4.5, 2, 20)
      )
      print(p)
    }

    get_venn_download_size <- function() {
      list(
        width = venn_clean_number(input$downloadVennWidth, venn_clean_number(input$vennWidth, 7, 3, 20), 3, 20),
        height = venn_clean_number(input$downloadVennHeight, venn_clean_number(input$vennHeight, 6, 3, 20), 3, 20),
        dpi = as.integer(round(venn_clean_number(input$downloadVennDpi, 300, 72, 600)))
      )
    }

    write_venn_png <- function(file) {
      res <- venn_results()
      size <- get_venn_download_size()
      png(file, width = size$width * size$dpi, height = size$height * size$dpi, res = size$dpi)
      on.exit(dev.off(), add = TRUE)
      if (is.null(res)) {
        plot(1, type = "n", main = "请先运行 Venn 图分析")
        return(invisible(NULL))
      }
      draw_venn_plot(res, set_name_size = input$vennSetNameSize, text_size = input$vennTextSize, stroke_size = 1)
    }

    show_venn_download_modal <- function() {
      showModal(
        modalDialog(
          title = "下载 Venn 图",
          p("设置导出图片尺寸后点击下载。单位为英寸，DPI 用于控制分辨率。",
            style = "color: #607d8b; font-size: 12px; margin: 0 0 8px 0;"),
          div(
            class = "venn-download-size-controls",
            numericInput(ns("downloadVennWidth"), "宽(in)", value = venn_clean_number(input$vennWidth, 7, 3, 20), min = 3, max = 20, step = 0.5),
            numericInput(ns("downloadVennHeight"), "高(in)", value = venn_clean_number(input$vennHeight, 6, 3, 20), min = 3, max = 20, step = 0.5),
            numericInput(ns("downloadVennDpi"), "DPI", value = 300, min = 72, max = 600, step = 50)
          ),
          footer = tagList(
            modalButton("取消"),
            downloadButton(ns("downloadVennModalPNG"), "下载PNG", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    }
    
    # ---- 组数切换 ----
    observeEvent(input$set2, set_group_count(2))
    observeEvent(input$set3, set_group_count(3))
    observeEvent(input$set4, set_group_count(4))
    observeEvent(input$set5, set_group_count(5))
    
    # ---- 动态生成组输入 ----
    output$groupInputs <- renderUI({
      n <- group_count()
      tagList(
        lapply(1:n, function(i) {
          div(
            class = "venn-group-row",
            div(
              class = "venn-upload-box",
              id = ns(paste0("fileBox", i)),
              tags$div(
                class = "venn-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 20px; line-height: 1;"),
                tags$span(
                  tags$span(paste0("基因列表 ", i), class = "venn-upload-title"),
                  tags$span(id = ns(paste0("fileStatus", i)), "Drop file here or click to upload", class = "venn-upload-status")
                )
              ),
              fileInput(ns(paste0("file", i)), NULL,
                        accept = c(".txt", ".csv", ".tsv"),
                        buttonLabel = "浏览",
                        placeholder = "选择基因列表文件")
            ),
            textInput(ns(paste0("name", i)), NULL, 
                      value = paste0("Set", i), 
                      placeholder = paste0("集合", i)),
            colourInput(ns(paste0("color", i)), NULL, 
                        value = c("#F94144", "#577590", "#F9C74F", "#43AA8B", "#9B5DE5")[i],
                        showColour = "background", palette = "limited",
                        width = "34px")
          )
        })
      )
    })
    
    # ---- 文件选择状态更新 ----
    observe({
      n <- group_count()
      for (i in 1:n) {
        local({
          idx <- i
          observeEvent(input[[paste0("file", idx)]], {
            if (!is.null(input[[paste0("file", idx)]])) {
              shinyjs::runjs(paste0('$("#', ns(paste0("fileStatus", idx)), '").text("', 
                                     input[[paste0("file", idx)]]$name, '")'))
            }
          })
        })
      }
    })
    
    # ---- 清除文件 ----
    observe({
      n <- group_count()
      for (i in 1:n) {
        local({
          idx <- i
          observeEvent(input[[paste0("clearFile", idx)]], {
            shinyjs::reset(paste0("file", idx))
            shinyjs::runjs(paste0('$("#', ns(paste0("fileStatus", idx)), '").text("Drop file here or click to upload")'))
            venn_results(NULL)
          })
        })
      }
    })
    
    # ---- 核心分析 ----
    observeEvent(input$runVenn, {
      n <- group_count()

      file_infos <- lapply(seq_len(n), function(i) input[[paste0("file", i)]])
      if (any(vapply(file_infos, is.null, logical(1)))) {
        showNotification("⚠️ 请上传所有基因列表文件！", type = "error")
        return()
      }

      name_inputs <- vapply(seq_len(n), function(i) input[[paste0("name", i)]] %||% paste0("Set", i), character(1))
      color_defaults <- c("#F94144", "#577590", "#F9C74F", "#43AA8B", "#9B5DE5")
      color_inputs <- vapply(seq_len(n), function(i) input[[paste0("color", i)]] %||% color_defaults[i], character(1))

      is_running(TRUE)
      venn_results(NULL)
      task_note <- app_start_task_notification("Venn 图分析正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          gene_list <- list()
          names_vec <- c()
          colors_vec <- c()

          for (i in 1:n) {
            file_info <- file_infos[[i]]
            sep <- if (exists("detect_table_separator", mode = "function")) {
              detect_table_separator(file_info)
            } else {
              "\t"
            }
            genes <- read.table(
              file_info$datapath,
              header = FALSE,
              stringsAsFactors = FALSE,
              sep = sep,
              fill = TRUE,
              quote = "",
              comment.char = ""
            )[, 1]
            genes <- unique(genes[genes != "" & !is.na(genes)])

            if (length(genes) == 0) {
              stop(paste0("基因列表 ", i, " 为空！"), call. = FALSE)
            }

            name <- name_inputs[i]
            if (is.null(name) || !nzchar(trimws(name))) name <- paste0("Set", i)
            name <- make.unique(c(names_vec, trimws(name)), sep = "_")[length(names_vec) + 1]

            gene_list[[name]] <- genes
            names_vec <- c(names_vec, name)
            colors_vec <- c(colors_vec, color_inputs[i])
          }

          all_genes <- unique(unlist(gene_list))
          membership <- data.frame(Gene = all_genes)
          for (name in names_vec) {
            membership[[name]] <- membership$Gene %in% gene_list[[name]]
          }

          intersect_results <- list()
          inter_all <- all_genes
          for (name in names_vec) {
            inter_all <- intersect(inter_all, gene_list[[name]])
          }
          intersect_results[["all"]] <- inter_all

          if (n >= 2) {
            for (i in 1:(n-1)) {
              for (j in (i+1):n) {
                key <- paste0(names_vec[i], "_&_", names_vec[j])
                intersect_results[[key]] <- intersect(gene_list[[names_vec[i]]], 
                                                      gene_list[[names_vec[j]]])
              }
            }
          }

          list(
            gene_list = gene_list,
            names = names_vec,
            colors = colors_vec,
            intersect_results = intersect_results,
            all_genes = all_genes,
            membership = membership,
            n_groups = n
          )
        },
        on_success = function(result) {
          app_clear_task_notification(task_note)
          venn_results(result)
          showNotification(
            paste0("✅ Venn 图绘制完成！共 ", length(result$intersect_results[["all"]]), " 个交集基因"),
            type = "message",
            duration = 5
          )
        },
        on_error = function(error) {
          app_clear_task_notification(task_note)
          showNotification(paste0("❌ 错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          app_clear_task_notification(task_note)
          is_running(FALSE)
        }
      )
    })
    
    # ---- 绘制 Venn 图 ----
    output$venn_plot <- renderPlot({
      res <- venn_results()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        if (is_running()) {
          text(1, 1, "绘制中，请稍候...", col = "#666666", cex = 1.1)
        }
        return()
      }
      draw_venn_plot(res, stroke_size = 0.7)
    },
    width = function() {
      width <- session$clientData[[paste0("output_", ns("venn_plot"), "_width")]]
      width <- suppressWarnings(as.numeric(width))
      if (length(width) != 1 || is.na(width) || width < 10) 520 else width
    },
    height = function() {
      height <- session$clientData[[paste0("output_", ns("venn_plot"), "_height")]]
      height <- suppressWarnings(as.numeric(height))
      if (length(height) != 1 || is.na(height) || height < 10) 285 else height
    },
    execOnResize = TRUE)
    
    # ---- Venn图放大 ----
    observeEvent(input$vennModalBtn, {
      res <- venn_results()
      if (is.null(res)) {
        showNotification("请先运行 Venn 图分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "Venn 图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("vennLarge"), height = "600px")
          )
        )
      )
    })

    observeEvent(input$vennPlot_click, {
      res <- venn_results()
      if (is.null(res)) {
        return()
      }

      showModal(
        modalDialog(
          title = "Venn 图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("vennLarge"), height = "600px")
          )
        )
      )
    })
    
    output$vennLarge <- renderPlot({
      res <- venn_results()
      if (is.null(res)) return()
      draw_venn_plot(res, set_name_size = 10, text_size = 7, stroke_size = 1)
    })

    output$resultFileList <- renderUI({
      has_results <- !is.null(venn_results())
      files <- data.frame(
        文件名 = c("venn.png", "intersection_genes.txt", "membership_matrix.csv", "set_statistics.csv"),
        类型 = c("PNG", "TXT", "CSV", "CSV"),
        说明 = c(
          "Venn 图，点击上方图片可放大",
          "所有集合共同交集基因",
          "所有基因在各集合中的归属矩阵",
          "各集合基因数量和共同交集数量"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (!has_results) {
        return(NULL)
      }

      download_ids <- c("", "downloadIntersect", "downloadMembership", "downloadSetStats")
      tags$div(
        class = "venn-result-file-list",
        lapply(seq_len(nrow(files)), function(i) {
          is_png <- identical(files$类型[i], "PNG")
          download_control <- if (is_png) {
            actionButton(ns("openVennDownload"), "下载", class = "btn-primary btn-xs")
          } else {
            downloadButton(ns(download_ids[i]), "下载", class = "btn-primary btn-xs")
          }

          tags$div(
            class = "venn-result-file-row",
            span(sprintf("%02d", i), class = "venn-file-index"),
            span(files$文件名[i], class = "venn-result-file-name", title = files$文件名[i]),
            span(files$类型[i], class = "venn-result-file-type"),
            span(files$说明[i], class = "venn-result-file-desc", title = files$说明[i]),
            span(class = "venn-result-file-download", download_control)
          )
        })
      )
    })
    
    # ---- 交集基因表格 ----
    output$intersectTable <- renderDT({
      res <- venn_results()
      if (is.null(res)) {
        return(NULL)
      }
      
      inter_genes <- res$intersect_results[["all"]]
      if (length(inter_genes) == 0) {
        return(datatable(data.frame(信息 = "无交集基因"),
                         options = list(dom = 't', pageLength = 5)))
      }
      
      df <- data.frame(基因 = inter_genes)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp'))
    })
    
    # ---- 更新交集计数 ----
    output$intersectCount <- renderUI({
      res <- venn_results()
      if (is.null(res)) {
        tags$span("共 0 个", style = "font-size: 11px; color: #888;")
      } else {
        tags$span(paste0("共 ", length(res$intersect_results[["all"]]), " 个"), 
                  style = "font-size: 11px; color: #888;")
      }
    })
    
    # ---- 基因统计表格 ----
    output$statsTable <- renderDT({
      res <- venn_results()
      if (is.null(res)) {
        return(NULL)
      }
      
      df <- data.frame(
        项目 = c(res$names, "全部并集", "共同交集"),
        基因数 = c(
          as.integer(sapply(res$gene_list, length)),
          length(res$all_genes),
          length(res$intersect_results[["all"]])
        )
      )
      datatable(df, options = list(dom = 't', pageLength = 10))
    })
    
    # ---- 下载功能 ----
    observeEvent(input$openVennDownload, {
      if (is.null(venn_results())) {
        showNotification("请先运行 Venn 图分析", type = "warning")
        return()
      }
      show_venn_download_modal()
    })

    output$downloadVenn <- downloadHandler(
      filename = "venn.png",
      content = function(file) {
        write_venn_png(file)
      }
    )

    output$downloadVennModalPNG <- downloadHandler(
      filename = "venn.png",
      content = function(file) {
        write_venn_png(file)
      }
    )
    
    output$downloadIntersect <- downloadHandler(
      filename = "intersection_genes.txt",
      content = function(file) {
        res <- venn_results()
        if (!is.null(res) && length(res$intersect_results[["all"]]) > 0) {
          writeLines(res$intersect_results[["all"]], file)
        } else {
          writeLines("无交集基因", file)
        }
      }
    )

    output$downloadMembership <- downloadHandler(
      filename = "membership_matrix.csv",
      content = function(file) {
        res <- venn_results()
        if (is.null(res)) {
          write.csv(data.frame(提示 = "请先运行 Venn 图分析"), file, row.names = FALSE)
        } else {
          write.csv(res$membership, file, row.names = FALSE)
        }
      }
    )

    output$downloadSetStats <- downloadHandler(
      filename = "set_statistics.csv",
      content = function(file) {
        res <- venn_results()
        if (is.null(res)) {
          write.csv(data.frame(提示 = "请先运行 Venn 图分析"), file, row.names = FALSE)
          return()
        }

        df <- data.frame(
          项目 = c(res$names, "全部并集", "共同交集"),
          基因数 = c(
            as.integer(sapply(res$gene_list, length)),
            length(res$all_genes),
            length(res$intersect_results[["all"]])
          ),
          check.names = FALSE
        )
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # ---- 说明按钮 ----
    observeEvent(input$helpBtn, {
      showModal(
        modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("Venn 图模块 - 使用说明", style = "font-size: 18px; font-weight: bold;"),
            tags$span("v1.0", style = "font-size: 12px; color: #999;")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "max-height: 70vh; overflow-y: auto; padding-right: 10px; font-size: 14px; line-height: 1.8;",
            tags$hr(style = "margin: 8px 0;"),
            
            tags$h5("1. 功能介绍", style = "color: #8e44ad;"),
            tags$p("Venn 图模块用于展示多个基因列表之间的交集关系。"),
            
            tags$h5("2. 使用步骤", style = "color: #3498db;"),
            tags$ol(
              tags$li("选择组数（2-5组）"),
              tags$li("上传每个组的基因列表（TXT格式，每行一个基因名）"),
              tags$li("可自定义集合名称和颜色"),
              tags$li("点击「绘制 Venn 图」生成图表")
            ),
            
            tags$h5("3. 输出结果", style = "color: #2ecc71;"),
            tags$ul(
              tags$li("Venn 图：展示各集合的重叠关系"),
              tags$li("交集基因：所有集合的共同基因列表"),
              tags$li("基因统计：各集合的基因数量")
            ),
            
            tags$h5("4. 文件格式要求", style = "color: #f39c12;"),
            tags$ul(
              tags$li("TXT 文本文件"),
              tags$li("每行一个基因名"),
              tags$li("空行和重复基因会自动过滤")
            ),
            
            tags$hr(style = "margin: 12px 0;"),
            div(
              style = "display: flex; justify-content: space-between; font-size: 12px; color: #999;",
              tags$span("更新日期: 2026-06-25"),
              tags$span("版本: v1.0")
            )
          )
        )
      )
    })
    
  })
}
