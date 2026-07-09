# deg_module.R - 差异分析模块（完整版）
# 修改内容：
# 1. 结果文件包含所有上调/下调基因（不限数量）
# 2. 火山图无任何线条（无阈值线、无标签连接线）
# 3. 火山图不显示基因名字标签
# 4. 修复热图和PCA图无法放大的问题
# 5. 区域一增加颜色修改功能（colourpicker颜色选择器）
# 6. 修复"说明"按钮功能，添加完整Q&A使用说明
# 7. 删除区域二"点击图片或「放大查看」按钮可放大"提示文字
# 8. 区域一高度调整为280px，与区域三一致
# 9. 上调默认红色(#E41A1C)，下调默认蓝色(#377EB8)，不显著默认黑色(#000000)
# 10. 删除区域一滚动条（overflow: hidden）
# 11. 区域一边框调整为与区域二一致（padding: 8px）
# 12. 删除区域一"等待运行"日志
# 13. 区域一宽度保持与区域二一致（width = 6）

# ============================================================
# UI
# ============================================================
deg_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .plot-container {
            display: flex;
            justify-content: center;
            align-items: center;
            background-color: #fafafa;
            border: 1px solid #eee;
            border-radius: 0px;
            padding: 5px;
            cursor: pointer;
            transition: background-color 0.2s;
        }
        .plot-container:hover {
            background-color: #f0f0f0;
        }
        .color-picker-row {
            display: flex;
            align-items: center;
            gap: 6px;
            margin-bottom: 4px;
        }
        .color-picker-row label {
            font-size: 11px;
            margin: 0;
            min-width: 50px;
        }
        .help-content table {
            width: 100%;
            border-collapse: collapse;
            margin: 8px 0;
            font-size: 13px;
        }
        .help-content table th {
            background-color: #f0f4f8;
            border: 1px solid #dce4ec;
            padding: 6px 10px;
            text-align: left;
            font-weight: bold;
        }
        .help-content table td {
            border: 1px solid #dce4ec;
            padding: 6px 10px;
            text-align: left;
        }
        .help-content table tr:nth-child(even) {
            background-color: #f8fafc;
        }
        .help-content .qa-block {
            margin-bottom: 16px;
            padding-bottom: 12px;
            border-bottom: 1px solid #eee;
        }
        .help-content .qa-block:last-child {
            border-bottom: none;
        }
        .help-content .q {
            font-weight: bold;
            color: #2c3e50;
            font-size: 14px;
            margin-bottom: 4px;
        }
        .help-content .a {
            color: #444;
            font-size: 13px;
            line-height: 1.7;
            padding-left: 20px;
        }
        .help-content .highlight {
            background-color: #fff3cd;
            padding: 1px 4px;
            border-radius: 2px;
        }
        .help-content .tag {
            display: inline-block;
            background-color: #eaf4ea;
            padding: 1px 8px;
            border-radius: 2px;
            font-size: 12px;
            color: #2d6a2d;
            margin: 1px 2px;
        }
        .help-content .tag-blue {
            background-color: #e3f0ff;
            color: #1a5a8a;
        }
        .help-content .tag-purple {
            background-color: #f0e8ff;
            color: #6a3a8a;
        }
        .deg-card,
        .deg-plot-card,
        .deg-result-card {
            border: 1px solid #b0bec5;
            border-radius: 4px;
            padding: 12px 16px;
            background-color: #ffffff;
        }
        .deg-card,
        .deg-plot-card {
            height: 370px;
            overflow-y: auto;
        }
        .deg-card h4,
        .deg-plot-card h4,
        .deg-result-card h4 {
            color: #2c3e50;
            margin-top: 0;
            margin-bottom: 10px;
            font-size: 14px;
            font-weight: 700;
        }
        .deg-card hr,
        .deg-plot-card hr {
            margin: 4px 0 8px 0;
        }
        .deg-upload-toolbar {
            display: flex;
            gap: 4px;
            flex-wrap: wrap;
            align-items: center;
            margin-bottom: 6px;
        }
        .deg-upload-row {
            display: grid;
            grid-template-columns: 1fr;
            gap: 6px;
            align-items: center;
            margin-bottom: 5px;
        }
        .deg-upload-box {
            position: relative;
            border: 1px dashed #b0bec5;
            border-radius: 0;
            min-height: 58px;
            padding: 6px 10px;
            background-color: #ffffff;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            overflow: hidden;
            transition: background-color 0.2s;
        }
        .deg-upload-box:hover {
            background-color: #f7fafc;
        }
        .deg-upload-box .shiny-input-container {
            position: absolute;
            inset: 0;
            width: 100% !important;
            height: 100%;
            margin: 0;
            opacity: 0;
            z-index: 2;
            cursor: pointer;
        }
        .deg-upload-box .input-group,
        .deg-upload-box .input-group-btn,
        .deg-upload-box .btn-file,
        .deg-upload-box input[type='file'] {
            width: 100%;
            height: 100%;
            cursor: pointer;
        }
        .deg-upload-placeholder {
            text-align: center;
            pointer-events: none;
            display: grid;
            gap: 2px;
            justify-items: center;
        }
        .deg-upload-title {
            font-weight: 700;
            font-size: 11px;
            color: #263238;
        }
        .deg-upload-status {
            color: #1e88e5;
            font-size: 11px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
            max-width: 170px;
        }
        .deg-param-grid {
            display: grid;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 6px;
        }
        .deg-compact-section {
            border: 1px solid #d7dee2;
            background: #ffffff;
            padding: 5px 8px;
            margin-bottom: 6px;
        }
        .deg-compact-title {
            display: block;
            color: #263238;
            font-size: 11px;
            font-weight: 700;
            margin-bottom: 4px;
        }
        .deg-compact-grid {
            display: grid;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 4px 8px;
            align-items: center;
        }
        .deg-compact-grid-4 {
            display: grid;
            grid-template-columns: repeat(4, minmax(0, 1fr));
            gap: 4px 8px;
            align-items: center;
        }
        .deg-mini-control {
            display: grid;
            grid-template-columns: auto minmax(0, 1fr);
            gap: 4px;
            align-items: center;
            font-size: 10px;
            color: #263238;
        }
        .deg-mini-control .shiny-input-container {
            margin-bottom: 0;
        }
        .deg-mini-control .form-control {
            height: 24px;
            padding: 2px 4px;
            font-size: 11px;
        }
        .deg-radio-inline .shiny-options-group {
            display: flex;
            gap: 10px;
            align-items: center;
        }
        .deg-radio-inline .radio {
            margin: 0;
            font-size: 11px;
        }
        .deg-card .form-control {
            font-size: 11px;
            padding: 2px 4px;
            height: 26px;
        }
        .deg-card .shiny-input-container {
            margin-bottom: 5px;
        }
        .deg-color-panel {
            border: 1px solid #d7dee2;
            background: #ffffff;
            padding: 5px 8px;
            margin-bottom: 6px;
        }
        .deg-active-plot-box {
            border: none;
            background: transparent;
            min-height: 285px;
            cursor: zoom-in;
        }
        .deg-result-panel {
            max-width: 100%;
            overflow-x: hidden;
        }
        .deg-result-panel .nav-tabs {
            border-bottom: 1px solid #d7dee2;
            margin-bottom: 8px;
        }
        .deg-card .nav-tabs {
            border-bottom: 1px solid #d7dee2;
            margin-bottom: 8px;
        }
        .deg-card .nav-tabs > li > a,
        .deg-result-panel .nav-tabs > li > a {
            border: none;
            border-radius: 0;
            margin-right: 26px;
            padding: 8px 2px 9px 2px;
            color: #37474f;
            background: transparent;
            font-size: 12px;
        }
        .deg-card .nav-tabs > li.active > a,
        .deg-card .nav-tabs > li.active > a:hover,
        .deg-card .nav-tabs > li.active > a:focus,
        .deg-result-panel .nav-tabs > li.active > a,
        .deg-result-panel .nav-tabs > li.active > a:hover,
        .deg-result-panel .nav-tabs > li.active > a:focus {
            border: none;
            border-bottom: 2px solid #1e88e5;
            color: #1e88e5;
            background: transparent;
            font-weight: 700;
        }
        .deg-result-file-list {
            border: 1px solid #d7dee2;
            background: #ffffff;
        }
        .deg-result-file-row {
            display: grid;
            grid-template-columns: 28px minmax(150px, 1fr) 54px minmax(150px, 1.4fr) 70px;
            gap: 8px;
            align-items: center;
            padding: 6px 8px;
            border-bottom: 1px solid #eef2f4;
            font-size: 11px;
        }
        .deg-result-file-row:last-child {
            border-bottom: none;
        }
        .deg-file-index {
            color: #1e88e5;
            font-weight: 700;
        }
        .deg-result-file-action {
            padding: 0;
            border: none;
            background: transparent;
            color: #1e88e5;
            text-align: left;
            font-size: 11px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .deg-result-file-name,
        .deg-result-file-desc {
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .deg-result-file-type {
            color: #455a64;
            font-weight: 700;
        }
        .deg-result-file-desc {
            color: #607d8b;
        }
        .deg-result-file-download .btn {
            font-size: 10px;
            padding: 1px 8px;
            line-height: 1.4;
        }
        .deg-download-size-controls {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px;
            margin-top: 8px;
        }
        .deg-download-size-controls .shiny-input-container {
            width: 100%;
            margin-bottom: 0;
        }
        .deg-download-size-controls label {
            font-size: 10px;
            color: #455a64;
            margin-bottom: 2px;
            font-weight: 500;
        }
        .deg-download-size-controls .form-control {
            height: 24px;
            padding: 2px 4px;
            font-size: 11px;
        }
        .deg-qa {
            font-size: 12px;
            line-height: 1.7;
            color: #455a64;
            max-height: 190px;
            overflow-y: auto;
        }
        .deg-qa dl {
            margin: 0;
        }
        .deg-qa dt {
            margin-top: 8px;
            color: #263238;
        }
        .deg-qa dt:first-child {
            margin-top: 0;
        }
        .deg-qa dd {
            margin-left: 0;
            margin-bottom: 4px;
        }
    ")),
    
    # ---- 第一行：区域一 + 区域二（各占50%） ----
    fluidRow(
      style = "margin: 0;",
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "deg-card",
          h4("参数设置"),
          tabsetPanel(
            id = ns("degTaskTab"),
            type = "tabs",
            tabPanel(
              "样本类型矫正",
              tags$div(
                class = "deg-upload-row",
                tags$div(
                  id = ns("sampleTypeExprFileBox"),
                  class = "deg-upload-box",
                  tags$div(
                    class = "deg-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("表达矩阵", class = "deg-upload-title"),
                    tags$span(id = ns("sampleTypeExprFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
                  ),
                  fileInput(ns("sampleTypeExprFile"), NULL,
                            accept = c(".csv", ".tsv", ".txt"),
                            buttonLabel = "浏览",
                            placeholder = "选择表达矩阵文件")
                )
              ),
              tags$div(
                class = "deg-upload-row",
                tags$div(
                  id = ns("sampleTypeControlFileBox"),
                  class = "deg-upload-box",
                  tags$div(
                    class = "deg-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("对照组列表", class = "deg-upload-title"),
                    tags$span(id = ns("sampleTypeControlFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
                  ),
                  fileInput(ns("sampleTypeControlFile"), NULL,
                            accept = c(".txt", ".csv", ".tsv"),
                            buttonLabel = "浏览",
                            placeholder = "选择对照组列表")
                )
              ),
              tags$div(
                class = "deg-upload-row",
                tags$div(
                  id = ns("sampleTypeTreatFileBox"),
                  class = "deg-upload-box",
                  tags$div(
                    class = "deg-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("实验组列表", class = "deg-upload-title"),
                    tags$span(id = ns("sampleTypeTreatFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
                  ),
                  fileInput(ns("sampleTypeTreatFile"), NULL,
                            accept = c(".txt", ".csv", ".tsv"),
                            buttonLabel = "浏览",
                            placeholder = "选择实验组列表")
                )
              ),
              div(
                class = "deg-compact-section",
                span("处理参数", class = "deg-compact-title"),
                checkboxInput(ns("sampleTypeAutoLog"), "自动判断是否 log2 转换", value = TRUE),
                checkboxInput(ns("sampleTypeNormalize"), "进行 limma 组间标准化", value = TRUE)
              ),
              actionButton(ns("runSampleType"), "生成样本类型矩阵",
                           class = "btn-success btn-sm",
                           style = "width: 100%; font-size: 12px; font-weight: bold; padding: 4px 0; margin-bottom: 3px;")
            ),
            tabPanel(
              "差异分析",
              div(
                class = "deg-upload-toolbar",
                actionButton(ns("clearAllFiles"), "清除全部",
                             class = "btn-danger btn-xs",
                             style = "font-size: 9px; padding: 1px 6px;")
              ),
              tags$div(
                class = "deg-upload-row",
                tags$div(
                  id = ns("countFileBox"),
                  class = "deg-upload-box",
                  tags$div(
                    class = "deg-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("表达矩阵 / Sample Type Matrix", class = "deg-upload-title"),
                    tags$span(id = ns("countFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
                  ),
                  fileInput(ns("countFile"), NULL,
                            accept = c(".csv", ".tsv", ".txt"),
                            buttonLabel = "浏览",
                            placeholder = "选择表达矩阵文件")
                )
              ),
              div(
                class = "deg-param-grid",
                numericInput(ns("logFC"), "logFC", value = 0.5, min = 0, max = 5, step = 0.1),
                numericInput(ns("adjP"), "P值", value = 0.05, min = 0.000001, max = 1, step = 0.01)
              ),
              actionButton(ns("runDiff"), "运行差异分析",
                           class = "btn-success btn-sm",
                           style = "width: 100%; font-size: 12px; font-weight: bold; padding: 4px 0; margin-bottom: 3px;")
            )
          )
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "deg-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "deg-active-plot-box",
            plotOutput(
              ns("activeDegPlot"),
              height = "285px",
              width = "100%",
              click = ns("activeDegPlot_click")
            )
          )
        )
      )
    ),
    
    # ---- 第二行：结果预览 ----
    fluidRow(
      style = "margin: 0;",
      column(
        width = 12,
        style = "padding: 4px;",
        tags$div(
          class = "deg-result-card",
          h4("结果预览"),
          div(
            class = "deg-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel(
                "结果表",
                uiOutput(ns("resultFileList"))
              ),
              tabPanel(
                "数据预览",
                uiOutput(ns("degDataPreview"))
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "deg-qa",
                  tags$dl(
                    tags$dt("Q1：差异分析的目的是什么？"),
                    tags$dd("差异分析用于从表达矩阵中筛选两组样本之间表达水平显著变化的基因，常作为富集分析、WGCNA、机器学习、ROC 和免疫浸润分析的上游步骤。"),
                    tags$dt("Q2：需要上传哪些文件？"),
                    tags$dd("若原始表达矩阵还没有分组后缀，先在“样本类型矫正”上传表达矩阵、对照组列表和实验组列表，生成 Sample Type Matrix；若已经有 _con/_tre 后缀，可直接在“差异分析”上传 1 个表达矩阵文件。"),
                    tags$dt("Q3：表达矩阵格式有什么要求？"),
                    tags$dd("支持 CSV、TSV 和 TXT。第一列应为基因名，后续列为样本；表达值应为数值型。差异分析使用的矩阵中，对照组样本列名需要以 _con 结尾，实验组样本列名需要以 _tre 结尾。"),
                    tags$dt("Q4：logFC 和 P值如何理解？"),
                    tags$dd("logFC 表示表达倍数变化的 log2 值；P值使用 limma 输出的原始 P.Value。当前筛选逻辑固定为 abs(logFC) 大于阈值且 P.Value 小于阈值。"),
                    tags$dt("Q5：火山图三类点分别代表什么？"),
                    tags$dd("红色表示上调基因，蓝色表示下调基因，灰色表示未同时达到 logFC 和 P值阈值的基因。"),
                    tags$dt("Q6：为什么只保留两个参数？"),
                    tags$dd("参考原始差异分析脚本，用户只需要控制 logFC 阈值和 P.Value 阈值，其它绘图参数由后台固定，保证结果格式稳定。"),
                    tags$dt("Q7：火山图中基因标注如何选择？"),
                    tags$dd("当前默认标注 FC 和 P.Value 都显著的基因，并按 P.Value 和 logFC 强度优先展示。若标注过多，可以提高 logFC 或 P值筛选阈值。"),
                    tags$dt("Q8：热图如何解读？"),
                    tags$dd("热图展示显著差异基因在两组样本中的表达模式。颜色代表标准化后的相对表达量，同组样本应呈现相近模式；若样本聚类混乱，建议检查分组和数据质量。"),
                    tags$dt("Q9：PCA 图如何解读？"),
                    tags$dd("PCA 图显示样本整体表达谱的主要变异方向。若对照组与实验组明显分离，说明两组存在较清晰的全局表达差异；若混合严重，可能差异较弱或数据质量需要检查。"),
                    tags$dt("Q10：结果表包含哪些列？"),
                    tags$dd("结果表包含 Gene、logFC、AveExpr、t、P.Value、adj.P.Val、B 等 limma 输出指标。当前模块用 P.Value 作为筛选 P值。"),
                    tags$dt("Q11：没有显著差异基因怎么办？"),
                    tags$dd("可以检查样本分组是否正确、样本量是否过少、表达矩阵是否已标准化，或适当放宽 logFC/P值阈值。若仍无结果，可能说明该比较下整体差异确实较弱。"),
                    tags$dt("Q12：结果文件如何使用？"),
                    tags$dd("Sample Type Matrix 可继续用于差异分析和机器学习提取特征值；diff_results CSV 可用于查看显著差异基因详情；up/down gene TXT 可进入富集分析、Venn 交集和机器学习筛选；volcano、heatmap、pca PNG 可用于结果汇报和论文图初稿。")
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
deg_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储分析结果 ----
    analysisResults <- reactiveVal(NULL)
    sample_type_result <- reactiveVal(NULL)
    sample_type_running <- reactiveVal(FALSE)
    active_deg_plot <- reactiveVal("volcano")
    download_deg_plot <- reactiveVal("volcano")

    deg_set_upload_status <- function(status_id, label) {
      label <- gsub("\\\\", "\\\\\\\\", label)
      label <- gsub('"', '\\"', label, fixed = TRUE)
      shinyjs::runjs(sprintf('$("#%s").text("%s")', ns(status_id), label))
    }

    deg_upload_ref <- function(file) {
      if (is.null(file) || is.null(file$datapath) || is.null(file$name)) {
        stop("未提供文件。", call. = FALSE)
      }
      data.frame(
        datapath = as.character(file$datapath[[1]]),
        name = as.character(file$name[[1]]),
        stringsAsFactors = FALSE
      )
    }

    deg_plot_label <- function(plot_key) {
      labels <- c(
        volcano = "火山图",
        heatmap = "热图",
        pca = "PCA图"
      )
      labels[[plot_key]] %||% "图片"
    }

    deg_plot_filename <- function(plot_key) {
      switch(
        plot_key,
        volcano = "volcano.png",
        heatmap = "heatmap.png",
        pca = "pca.png",
        "diff_plot.png"
      )
    }

    deg_clean_number <- function(value, default, min_value, max_value) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) != 1 || is.na(value)) {
        value <- default
      }
      max(min_value, min(max_value, value))
    }

    deg_p_value_column <- function() {
      "P.Value"
    }

    deg_p_value_label <- function() {
      "P-value"
    }

    deg_p_cutoff <- function() {
      deg_clean_number(input$adjP, 0.05, 0.000001, 1)
    }

    deg_group_labels <- function() {
      c(control = "Control", treatment = "Treatment")
    }

    deg_group_factor <- function(res) {
      labels <- deg_group_labels()
      factor(
        c(rep(labels[["control"]], res$n_ctrl), rep(labels[["treatment"]], res$n_treat)),
        levels = unname(labels)
      )
    }

    deg_group_colors <- function(control_color, treatment_color) {
      labels <- deg_group_labels()
      stats::setNames(c(control_color, treatment_color), unname(labels))
    }

    deg_prepare_expression_matrix <- function(count_file) {
      expr_matrix <- read_expression_matrix(count_file)

      if (any(is.na(expr_matrix))) {
        stop("表达矩阵中含有无法识别的数值，请检查数据", call. = FALSE)
      }

      expr_matrix <- limma::avereps(expr_matrix)

      sample_names <- colnames(expr_matrix)
      ctrl_samples <- sample_names[grepl("_con$", sample_names, ignore.case = TRUE)]
      treat_samples <- sample_names[grepl("_tre$", sample_names, ignore.case = TRUE)]

      if (!length(ctrl_samples) || !length(treat_samples)) {
        stop("表达矩阵列名需要带样本类型后缀：对照组以 _con 结尾，实验组以 _tre 结尾。", call. = FALSE)
      }
      if (length(ctrl_samples) + length(treat_samples) != ncol(expr_matrix)) {
        unknown <- setdiff(sample_names, c(ctrl_samples, treat_samples))
        stop(paste0("存在无法识别分组的样本列：", paste(unknown, collapse = ", "),
                    "。请将列名改为 *_con 或 *_tre。"), call. = FALSE)
      }

      qx <- as.numeric(stats::quantile(expr_matrix, probs = c(0, 0.25, 0.5, 0.75, 0.99, 1), na.rm = TRUE))
      need_log <- (qx[5] > 100) || ((qx[6] - qx[1]) > 50 && qx[2] > 0)
      if (isTRUE(need_log)) {
        expr_matrix[expr_matrix < 0] <- 0
        expr_matrix <- log2(expr_matrix + 1)
      }

      expr_matrix <- suppressWarnings(limma::normalizeBetweenArrays(expr_matrix, method = "quantile"))
      expr_matrix[is.na(expr_matrix)] <- 0

      data_ctrl <- expr_matrix[, ctrl_samples, drop = FALSE]
      data_treat <- expr_matrix[, treat_samples, drop = FALSE]
      combined_expr <- cbind(data_ctrl, data_treat)

      list(
        combined = combined_expr,
        n_ctrl = ncol(data_ctrl),
        n_treat = ncol(data_treat),
        ctrl_samples = ctrl_samples,
        treat_samples = treat_samples,
        log_transformed = need_log
      )
    }

    deg_process_sample_type_matrix <- function(expr_file,
                                               control_file,
                                               treat_file,
                                               auto_log = TRUE,
                                               normalize = TRUE) {
      upload_path <- function(file) {
        if (is.null(file)) {
          stop("未提供文件。", call. = FALSE)
        }
        if ((is.list(file) || is.data.frame(file)) && !is.null(file$datapath)) {
          path <- as.character(file$datapath[[1]])
        } else {
          path <- as.character(file)[[1]]
        }
        if (!file.exists(path)) {
          stop(paste0("文件不存在：", path), call. = FALSE)
        }
        path
      }

      upload_name <- function(file) {
        if ((is.list(file) || is.data.frame(file)) && !is.null(file$name)) {
          return(as.character(file$name[[1]]))
        }
        basename(upload_path(file))
      }

      table_separator <- function(file) {
        ext <- tolower(tools::file_ext(upload_name(file)))
        if (identical(ext, "csv")) "," else "\t"
      }

      read_group_samples <- function(file) {
        sample_data <- utils::read.table(
          upload_path(file),
          sep = table_separator(file),
          header = FALSE,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          comment.char = "",
          quote = "\""
        )
        if (!nrow(sample_data) || !ncol(sample_data)) {
          stop("样本分组文件为空。", call. = FALSE)
        }
        samples <- trimws(as.character(sample_data[[1]]))
        unique(samples[nzchar(samples)])
      }

      clean_group_samples <- function(samples, available_samples) {
        samples <- trimws(as.character(samples))
        samples <- samples[nzchar(samples)]
        if (length(samples) > 1 &&
            !samples[[1]] %in% available_samples &&
            tolower(samples[[1]]) %in% c("sample", "samples", "id", "sampleid", "sample_id", "gsm")) {
          samples <- samples[-1]
        }
        stripped <- sub("_(con|ctrl|control|tre|treat|case|disease)$", "", samples, ignore.case = TRUE)
        if (sum(stripped %in% available_samples) > sum(samples %in% available_samples)) {
          samples <- stripped
        }
        unique(samples)
      }

      validate_inputs <- function(expr_matrix, ctrl_samples, treat_samples) {
        errors <- character()
        if (!is.matrix(expr_matrix) || !is.numeric(expr_matrix)) {
          errors <- c(errors, "表达矩阵必须为数值矩阵。")
        }
        if (nrow(expr_matrix) < 1 || ncol(expr_matrix) < 2) {
          errors <- c(errors, "表达矩阵至少需要1个基因和2个样本。")
        }
        if (!length(ctrl_samples) || !length(treat_samples)) {
          errors <- c(errors, "对照组或实验组样本列表为空。")
        }
        overlap_samples <- intersect(ctrl_samples, treat_samples)
        if (length(overlap_samples)) {
          errors <- c(errors, paste0("对照组和实验组样本重复：", paste(overlap_samples, collapse = ", ")))
        }
        missing_ctrl <- setdiff(ctrl_samples, colnames(expr_matrix))
        missing_treat <- setdiff(treat_samples, colnames(expr_matrix))
        if (length(missing_ctrl)) {
          errors <- c(errors, paste0("对照组样本不在表达矩阵中：", paste(missing_ctrl, collapse = ", ")))
        }
        if (length(missing_treat)) {
          errors <- c(errors, paste0("实验组样本不在表达矩阵中：", paste(missing_treat, collapse = ", ")))
        }
        if (length(errors)) {
          stop(paste(errors, collapse = "\n"), call. = FALSE)
        }
        invisible(TRUE)
      }

      raw_data <- utils::read.table(
        upload_path(expr_file),
        sep = table_separator(expr_file),
        header = TRUE,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        comment.char = "",
        quote = "\""
      )
      if (!nrow(raw_data) || ncol(raw_data) < 3) {
        stop("表达矩阵为空或样本列不足。", call. = FALSE)
      }

      gene_ids <- trimws(as.character(raw_data[[1]]))
      expr_matrix <- as.matrix(raw_data[, -1, drop = FALSE])
      suppressWarnings(storage.mode(expr_matrix) <- "numeric")
      rownames(expr_matrix) <- gene_ids

      if (any(is.na(expr_matrix))) {
        stop("表达矩阵中含有无法识别的数值，请检查输入文件。", call. = FALSE)
      }

      expr_matrix <- limma::avereps(expr_matrix)
      qx <- as.numeric(stats::quantile(expr_matrix, probs = c(0, 0.25, 0.5, 0.75, 0.99, 1), na.rm = TRUE))
      need_log <- isTRUE(auto_log) && ((qx[5] > 100) || ((qx[6] - qx[1]) > 50 && qx[2] > 0))
      if (need_log) {
        expr_matrix[expr_matrix < 0] <- 0
        expr_matrix <- log2(expr_matrix + 1)
      }

      if (isTRUE(normalize)) {
        expr_matrix <- limma::normalizeBetweenArrays(expr_matrix)
      }
      expr_matrix[is.na(expr_matrix)] <- 0

      ctrl_samples <- clean_group_samples(read_group_samples(control_file), colnames(expr_matrix))
      treat_samples <- clean_group_samples(read_group_samples(treat_file), colnames(expr_matrix))
      validate_inputs(expr_matrix, ctrl_samples, treat_samples)

      control_data <- expr_matrix[, ctrl_samples, drop = FALSE]
      treat_data <- expr_matrix[, treat_samples, drop = FALSE]
      combined_data <- cbind(control_data, treat_data)
      num_control <- ncol(control_data)
      num_treat <- ncol(treat_data)
      colnames(combined_data) <- paste0(colnames(combined_data), "_", c(rep("con", num_control), rep("tre", num_treat)))

      final_df <- data.frame(
        GeneName = rownames(combined_data),
        combined_data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      summary_df <- data.frame(
        项目 = c("原始基因数", "去重后基因数", "对照组样本数(con)", "实验组样本数(tre)", "总样本数", "是否执行log2", "是否执行标准化"),
        数值 = c(length(gene_ids), nrow(combined_data), num_control, num_treat, ncol(combined_data), if (need_log) "是" else "否", if (isTRUE(normalize)) "是" else "否"),
        stringsAsFactors = FALSE
      )

      summary_lines <- c(
        paste("Number of control samples (con):", num_control),
        paste("Number of treatment samples (tre):", num_treat),
        paste("Total samples:", ncol(combined_data)),
        paste("Original genes:", length(gene_ids)),
        paste("Genes after avereps:", nrow(combined_data)),
        paste("Log2 transformed:", if (need_log) "yes" else "no"),
        paste("Normalized:", if (isTRUE(normalize)) "yes" else "no")
      )

      list(matrix = final_df, summary = summary_df, summary_lines = summary_lines)
    }
    environment(deg_process_sample_type_matrix) <- globalenv()

    deg_plot_defaults <- function(plot_key) {
      if (identical(plot_key, "volcano")) {
        return(list(
          width = 8,
          height = 8
        ))
      }

      if (identical(plot_key, "heatmap")) {
        return(list(width = 10, height = 8))
      }

      list(width = 7, height = 5)
    }

    get_deg_plot_download_size <- function(default_width = 7, default_height = 5) {
      list(
        width = default_width,
        height = default_height,
        dpi = 300L
      )
    }

    show_deg_plot_download_modal <- function(plot_key) {
      download_deg_plot(plot_key)
      shinyjs::click("downloadDegModalPNG")
    }

    make_volcano_plot <- function(res, point_size = NULL, base_size = 13) {
      point_size <- deg_clean_number(point_size %||% 2, 2, 0.1, 10)
      df <- res$all
      df$Gene <- rownames(df)
      p_col <- deg_p_value_column()
      df$PForPlot <- pmax(df[[p_col]], .Machine$double.xmin)

      sig_degs <- df[with(df, abs(logFC) > input$logFC & PForPlot < deg_p_cutoff()), , drop = FALSE]
      up_top <- rownames(sig_degs)[order(-sig_degs$logFC)]
      down_top <- rownames(sig_degs)[order(sig_degs$logFC)]
      label_genes <- unique(c(utils::head(up_top, 25), utils::head(down_top, 25)))
      df$label <- ifelse(df$Gene %in% label_genes, df$Gene, "")

      df$Group <- "Not Significant"
      df$Group[df$logFC > input$logFC & df$PForPlot < deg_p_cutoff()] <- "Up regulated"
      df$Group[df$logFC < -input$logFC & df$PForPlot < deg_p_cutoff()] <- "Down regulated"
      df$Group <- factor(df$Group, levels = c("Up regulated", "Down regulated", "Not Significant"))

      ggplot(df, aes(x = logFC, y = -log10(PForPlot), color = Group)) +
        geom_point(alpha = 0.7, size = point_size) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          max.overlaps = 50,
          box.padding = 0.3,
          point.padding = 0.2,
          segment.color = "grey50",
          show.legend = FALSE
        ) +
        scale_color_manual(values = c(
          "Up regulated" = "#FF4500",
          "Down regulated" = "#1E90FF",
          "Not Significant" = "#808080"
        )) +
        geom_vline(xintercept = c(-input$logFC, input$logFC), linetype = "dashed", color = "black") +
        geom_hline(yintercept = -log10(deg_p_cutoff()), linetype = "dashed", color = "black") +
        labs(
          title = "Volcano Plot with Top 25 Genes Labeled",
          x = "Log2 Fold Change",
          y = "-Log10 Adjusted P-value"
        ) +
        theme_minimal(base_size = base_size) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#2F4F4F"))
      }

    select_heatmap_genes <- function(res, max_display = 50) {
      ordered_genes <- rownames(res$sig)
      total_count <- length(ordered_genes)
      if (total_count > (max_display * 2)) {
        ordered_genes[c(seq_len(max_display), (total_count - max_display + 1):total_count)]
      } else {
        ordered_genes
      }
    }

    draw_deg_heatmap <- function(res, fontsize_row = 7, fontsize_col = 10) {
      if (is.null(res) || nrow(res$sig) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "无显著差异基因")
        return(invisible(NULL))
      }

      selected <- select_heatmap_genes(res)
      heatmap_expr <- res$combined[selected, , drop = FALSE]

      sample_annotation <- data.frame(
        Group = deg_group_factor(res)
      )
      rownames(sample_annotation) <- colnames(res$combined)

      annotation_colors <- list(
        Group = deg_group_colors("#66C2A5", "#FC8D62")
      )

      color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)

      pheatmap(
        mat = heatmap_expr,
        annotation_col = sample_annotation,
        annotation_colors = annotation_colors,
        color = color_palette,
        cluster_cols = FALSE,
        show_colnames = FALSE,
        scale = "row",
        fontsize_row = fontsize_row,
        fontsize_col = fontsize_col,
        main = paste0("Differential Expression Heatmap (", length(selected), " genes)")
      )
    }

    make_pca_plot <- function(res, point_size = 4, label_size = 3.5, base_size = 14) {
      pca_result <- prcomp(t(res$combined), scale. = TRUE)
      pca_var_perc <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)

      pca_df <- data.frame(
        Sample = colnames(res$combined),
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2],
        Group = deg_group_factor(res)
      )

      ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
        stat_ellipse(level = 0.95, linetype = "dashed", linewidth = 1) +
        geom_point(size = point_size, alpha = 0.85) +
        geom_text_repel(aes(label = Sample), size = label_size, max.overlaps = 15) +
        scale_color_manual(values = deg_group_colors("#0072B2", "#E69F00")) +
        labs(
          title = "PCA Analysis",
          x = paste("PC1 (", pca_var_perc[1], "%)", sep = ""),
          y = paste("PC2 (", pca_var_perc[2], "%)", sep = "")
        ) +
        theme_classic(base_size = base_size) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              legend.position = "top")
    }

    draw_active_deg_plot <- function(plot_key, large = FALSE) {
      res <- analysisResults()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return(invisible(NULL))
      }

      switch(
        plot_key,
        volcano = print(make_volcano_plot(
          res,
          point_size = if (large) 3 else NULL,
          base_size = if (large) 18 else 13
        )),
        heatmap = draw_deg_heatmap(res, fontsize_row = if (large) 8 else 7, fontsize_col = if (large) 10 else 8),
        pca = print(make_pca_plot(res, point_size = if (large) 6 else 4, label_size = if (large) 5 else 3.5, base_size = if (large) 16 else 14)),
        print(make_volcano_plot(res))
      )
    }

    write_deg_plot_png <- function(file, plot_key) {
      defaults <- deg_plot_defaults(plot_key)
      size <- get_deg_plot_download_size(defaults$width, defaults$height)
      res <- analysisResults()

      if (is.null(res)) {
        png(file, width = size$width * size$dpi, height = size$height * size$dpi, res = size$dpi)
        on.exit(dev.off(), add = TRUE)
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return(invisible(NULL))
      }

      if (identical(plot_key, "heatmap")) {
        png(file, width = size$width * size$dpi, height = size$height * size$dpi, res = size$dpi)
        on.exit(dev.off(), add = TRUE)
        draw_deg_heatmap(res, fontsize_row = 10, fontsize_col = 12)
        return(invisible(NULL))
      }

      plot_obj <- if (identical(plot_key, "pca")) {
        make_pca_plot(res, point_size = 6, label_size = 5, base_size = 18)
      } else {
        make_volcano_plot(res, point_size = NULL, base_size = 18)
      }

      ggsave(file, plot_obj, width = size$width, height = size$height, dpi = size$dpi, bg = "white")
    }
    
    # ---- 辅助函数：获取所有上调/下调基因 ----
    get_all_up_genes <- function(res) {
      if (is.null(res) || nrow(res$sig) == 0) return(NULL)
      up_genes <- res$sig[res$sig$logFC > 0, ]
      up_genes <- up_genes[order(up_genes$logFC, decreasing = TRUE), ]
      return(rownames(up_genes))
    }
    
    get_all_down_genes <- function(res) {
      if (is.null(res) || nrow(res$sig) == 0) return(NULL)
      down_genes <- res$sig[res$sig$logFC < 0, ]
      down_genes <- down_genes[order(down_genes$logFC, decreasing = FALSE), ]
      return(rownames(down_genes))
    }
    
    # ============================================================
    # "说明"按钮功能
    # ============================================================
    observeEvent(input$helpBtn, {
      showModal(
        modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("差异分析模块 - 使用说明", style = "font-size: 18px; font-weight: bold;"),
            tags$span("v1.0", style = "font-size: 12px; color: #999;")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            class = "help-content",
            style = "max-height: 70vh; overflow-y: auto; padding-right: 10px;",
            
            tags$hr(style = "margin: 8px 0;"),
            
            # Q1
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "什么是差异分析？"),
              div(class = "a", 
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "差异分析是比较两组样本（如对照组 vs 实验组），通过统计方法找出表达水平有显著差异的基因。本模块使用 ",
                  tags$code("limma"), " 包进行差异表达分析，这是转录组学中最常用的方法之一。",
                  tags$br(),
                  "核心输出：",
                  tags$ul(
                    tags$li(tags$strong("log2FC"), "：基因在两组间的表达变化倍数（以2为底的对数）"),
                    tags$li(tags$strong("P值 / FDR"), "：差异的统计显著性")
                  )
              )
            ),
            
            # Q2
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "logFC 阈值是什么意思？应该设置为多少？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "logFC（log2 Fold Change）是表达变化的倍数。例如：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("logFC"),
                        tags$th("含义")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("1"), tags$td("实验组表达量是对照组的 2 倍（上调）")),
                      tags$tr(tags$td("-1"), tags$td("实验组表达量是对照组的 1/2 倍（下调）")),
                      tags$tr(tags$td("2"), tags$td("实验组表达量是对照组的 4 倍（上调）"))
                    )
                  ),
                  tags$br(),
                  tags$strong("推荐设置："),
                  tags$ul(
                    tags$li("默认值：", tags$span(class = "tag", "1.0"), "（变化超过2倍）"),
                    tags$li("探索性分析：", tags$span(class = "tag", "0.5"), "（变化超过1.4倍，更宽松）"),
                    tags$li("严格筛选：", tags$span(class = "tag", "1.5 或 2.0"), "（变化超过2.8倍或4倍）")
                  )
              )
            ),
            
            # Q3
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "FDR 阈值是什么？为什么不用 P 值？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "FDR（False Discovery Rate，错误发现率）是校正后的 P 值，用于控制假阳性率。",
                  tags$br(),
                  "在差异分析中，我们会同时检验成千上万个基因，如果直接用 P < 0.05 筛选，会有大量假阳性结果。FDR 校正可以有效地控制这种多重检验带来的误差。",
                  tags$br(),
                  tags$br(),
                  tags$strong("推荐设置："),
                  tags$ul(
                    tags$li("默认值：", tags$span(class = "tag", "0.05"), "（5% 的错误发现率）"),
                    tags$li("探索性分析：", tags$span(class = "tag", "0.1"), "（更宽松）"),
                    tags$li("严格筛选：", tags$span(class = "tag", "0.01"), "（更严格）")
                  )
              )
            ),
            
            # Q4
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "如何解读火山图？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "火山图是差异分析最核心的可视化结果：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("区域"),
                        tags$th("含义")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td(tags$span(style="color:#E41A1C;font-weight:bold;", "右侧红色点")), tags$td("上调基因（实验组表达高于对照组）")),
                      tags$tr(tags$td(tags$span(style="color:#377EB8;font-weight:bold;", "左侧蓝色点")), tags$td("下调基因（实验组表达低于对照组）")),
                      tags$tr(tags$td(tags$span(style="color:#000000;", "黑色点")), tags$td("差异不显著的基因")),
                      tags$tr(tags$td("横轴"), tags$td("log2FC（离中心越远，表达变化越大）")),
                      tags$tr(tags$td("纵轴"), tags$td("-log10(FDR)（数值越大，差异越显著）"))
                    )
                  ),
                  tags$br(),
                  tags$strong("判断标准："),
                  tags$ul(
                    tags$li("分布在左右两侧且靠上的点 = 显著差异基因"),
                    tags$li("越靠边缘 = 差异越明显"),
                    tags$li("越靠上方 = 统计显著性越高")
                  )
              )
            ),
            
            # Q5
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "如何解读热图？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "热图展示显著差异基因在两组样本中的表达模式：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("元素"),
                        tags$th("含义")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("行"), tags$td("差异基因（按表达量排序）")),
                      tags$tr(tags$td("列"), tags$td("样本（按分组排列）")),
                      tags$tr(tags$td("颜色"), tags$td("红色 = 高表达，蓝色 = 低表达")),
                      tags$tr(tags$td("颜色深浅"), tags$td("表达量偏离均值的程度"))
                    )
                  ),
                  tags$br(),
                  tags$strong("解读要点："),
                  tags$ul(
                    tags$li("同组样本的表达模式应相似（聚类在一起）"),
                    tags$li("两组之间的颜色差异越大，区分度越好"),
                    tags$li("如果样本聚类混乱，建议检查数据质量或调整阈值")
                  )
              )
            ),
            
            # Q6
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "如何解读 PCA 图？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "PCA 图展示所有样本在二维空间中的分布：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("元素"),
                        tags$th("含义")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("X轴（PC1）"), tags$td("最大变异方向，解释最多的差异")),
                      tags$tr(tags$td("Y轴（PC2）"), tags$td("第二大变异方向")),
                      tags$tr(tags$td("点"), tags$td("每个样本")),
                      tags$tr(tags$td("颜色"), tags$td("分组（对照组/实验组）")),
                      tags$tr(tags$td("椭圆"), tags$td("95% 置信区间"))
                    )
                  ),
                  tags$br(),
                  tags$strong("期望结果："),
                  tags$ul(
                    tags$li(tags$span(style="color:#27ae60;", "✓"), " 两组样本明显分离 → 分组有生物学差异"),
                    tags$li(tags$span(style="color:#f39c12;", "⚠"), " 两组样本混合在一起 → 差异不明显或数据有问题"),
                    tags$li(tags$span(style="color:#27ae60;", "✓"), " 同组样本聚集在一起 → 数据重复性好")
                  )
              )
            ),
            
            # Q7
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "结果表格中包含哪些信息？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "结果表格包含所有显著差异基因的详细信息：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("列名"),
                        tags$th("含义")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("Gene"), tags$td("基因符号")),
                      tags$tr(tags$td("logFC"), tags$td("log2 折叠变化值")),
                      tags$tr(tags$td("AveExpr"), tags$td("所有样本的平均表达值")),
                      tags$tr(tags$td("t"), tags$td("t 统计量")),
                      tags$tr(tags$td("P.Value"), tags$td("原始 P 值")),
                      tags$tr(tags$td("adj.P.Val"), tags$td("FDR 校正后的 P 值")),
                      tags$tr(tags$td("B"), tags$td("差异表达的对数 odds"))
                    )
                  )
              )
            ),
            
            # Q8
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "应该选择哪些阈值？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "阈值选择取决于研究目的：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("研究目的"),
                        tags$th("logFC"),
                        tags$th("FDR"),
                        tags$th("说明")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("探索性分析"), tags$td(tags$span(class="tag", "0.5")), tags$td(tags$span(class="tag", "0.1")), tags$td("不遗漏潜在候选基因")),
                      tags$tr(tags$td("常规筛选"), tags$td(tags$span(class="tag", "1.0")), tags$td(tags$span(class="tag", "0.05")), tags$td("平衡灵敏度和特异性")),
                      tags$tr(tags$td("严格筛选"), tags$td(tags$span(class="tag", "1.5")), tags$td(tags$span(class="tag", "0.01")), tags$td("获得高置信度差异基因")),
                      tags$tr(tags$td("验证性研究"), tags$td(tags$span(class="tag", "2.0")), tags$td(tags$span(class="tag", "0.001")), tags$td("仅保留最显著的基因"))
                    )
                  )
              )
            ),
            
            # Q9
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "差异分析需要什么输入文件？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "只需要以下一个文件：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("文件"),
                        tags$th("格式"),
                        tags$th("说明")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("表达矩阵"), tags$td("CSV/TSV/TXT"), tags$td("第一列为基因名，其余列为样本表达值；对照组列名以 _con 结尾，实验组列名以 _tre 结尾"))
                    )
                  ),
                  tags$br(),
                  tags$span("可直接上传样本列名已带 _con/_tre 后缀的表达矩阵文件。")
              )
            ),
            
            # Q10
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "分析失败怎么办？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "常见问题及解决方案：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("问题"),
                        tags$th("可能原因"),
                        tags$th("解决方法")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("文件上传失败"), tags$td("格式不正确"), tags$td("检查文件是否为 CSV 或 TSV 格式")),
                      tags$tr(tags$td("样本名不匹配"), tags$td("分组文件中的样本名与表达矩阵列名不一致"), tags$td("检查样本名是否完全一致（区分大小写）")),
                      tags$tr(tags$td("无显著基因"), tags$td("阈值过于严格"), tags$td("尝试放宽 logFC 或 FDR 阈值")),
                      tags$tr(tags$td("分析报错"), tags$td("数据包含非数值"), tags$td("检查表达矩阵是否全部为数值（除第一列）"))
                    )
                  )
              )
            ),
            
            # Q11
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "下载的文件是什么格式？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "本模块支持以下格式下载：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("文件"),
                        tags$th("格式"),
                        tags$th("用途")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("火山图"), tags$td("PNG"), tags$td("论文插图")),
                      tags$tr(tags$td("热图"), tags$td("PNG"), tags$td("论文插图")),
                      tags$tr(tags$td("PCA图"), tags$td("PNG"), tags$td("论文插图")),
                      tags$tr(tags$td("结果表格"), tags$td("CSV"), tags$td("Excel 打开，进一步筛选和分析"))
                    )
                  )
              )
            ),
            
            # Q12
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "数据处理过程中需要注意什么？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "以下几点需要特别注意：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("注意事项"),
                        tags$th("说明")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("数据标准化"), tags$td("建议使用 log2(TPM+1) 或 log2(CPM+1) 标准化后的数据")),
                      tags$tr(tags$td("样本数量"), tags$td("每组至少 3 个样本，建议 ≥ 5 个")),
                      tags$tr(tags$td("基因名格式"), tags$td("确保基因名为标准符号（如 TP53、EGFR）")),
                      tags$tr(tags$td("文件编码"), tags$td("使用 UTF-8 或 ANSI 编码")),
                      tags$tr(tags$td("阈值选择"), tags$td("根据研究目的合理选择阈值，不盲目追求显著性"))
                    )
                  )
              )
            ),
            
            # Q13
            div(
              class = "qa-block",
              div(class = "q", tags$span(style = "color: #3498db;", "Q: "), "差异分析的目的是什么？"),
              div(class = "a",
                  tags$span(style = "color: #2ecc71; font-weight: bold;", "A: "),
                  "差异分析的目的是从成千上万个基因中筛选出表达水平发生显著变化的基因，这些基因往往是与实验处理、疾病状态或生物学过程相关的关键分子。",
                  tags$br(),
                  tags$br(),
                  "差异分析是转录组学数据挖掘的",
                  tags$strong("第一步和基础"),
                  "，其结果直接指导后续的：",
                  tags$ul(
                    tags$li("功能富集分析（GO/KEGG）"),
                    tags$li("WGCNA分析"),
                    tags$li("机器学习建模"),
                    tags$li("生物标志物筛选")
                  )
              )
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
    
    observeEvent(input$sampleTypeExprFile, {
      req(input$sampleTypeExprFile$name)
      deg_set_upload_status("sampleTypeExprFileStatus", input$sampleTypeExprFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$sampleTypeControlFile, {
      req(input$sampleTypeControlFile$name)
      deg_set_upload_status("sampleTypeControlFileStatus", input$sampleTypeControlFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$sampleTypeTreatFile, {
      req(input$sampleTypeTreatFile$name)
      deg_set_upload_status("sampleTypeTreatFileStatus", input$sampleTypeTreatFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$runSampleType, {
      if (is.null(input$sampleTypeExprFile) ||
          is.null(input$sampleTypeControlFile) ||
          is.null(input$sampleTypeTreatFile)) {
        showNotification("请上传表达矩阵、对照组列表和实验组列表。", type = "error")
        return()
      }

      expr_file <- deg_upload_ref(input$sampleTypeExprFile)
      control_file <- deg_upload_ref(input$sampleTypeControlFile)
      treat_file <- deg_upload_ref(input$sampleTypeTreatFile)
      auto_log <- input$sampleTypeAutoLog
      normalize <- input$sampleTypeNormalize

      sample_type_running(TRUE)
      sample_type_result(NULL)
      task_note <- app_start_task_notification("样本类型矫正正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          deg_process_sample_type_matrix(
            expr_file = expr_file,
            control_file = control_file,
            treat_file = treat_file,
            auto_log = auto_log,
            normalize = normalize
          )
        },
        on_success = function(result) {
          sample_type_result(result)
          showNotification("样本类型矩阵生成完成。", type = "message", duration = 5)
        },
        on_error = function(error) {
          showNotification(paste0("错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          sample_type_running(FALSE)
          app_clear_task_notification(task_note)
        }
      )
      return()
    })
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$countFile, {
      if (!is.null(input$countFile)) {
        deg_set_upload_status("countFileStatus", input$countFile$name)
      }
    })
    
    # ---- 清除上传文件功能 ----
    observeEvent(input$clearCountFile, {
      shinyjs::reset("countFile")
      runjs(paste0('$("#', ns("countFileStatus"), '").text("Drop file here or click to upload")'))
      analysisResults(NULL)
      showNotification("已清除表达矩阵文件", type = "message")
    })
    
    observeEvent(input$clearAllFiles, {
      shinyjs::reset("sampleTypeExprFile")
      shinyjs::reset("sampleTypeControlFile")
      shinyjs::reset("sampleTypeTreatFile")
      shinyjs::reset("countFile")
      runjs(paste0('$("#', ns("sampleTypeExprFileStatus"), '").text("Drop file here or click to upload")'))
      runjs(paste0('$("#', ns("sampleTypeControlFileStatus"), '").text("Drop file here or click to upload")'))
      runjs(paste0('$("#', ns("sampleTypeTreatFileStatus"), '").text("Drop file here or click to upload")'))
      runjs(paste0('$("#', ns("countFileStatus"), '").text("Drop file here or click to upload")'))
      sample_type_result(NULL)
      analysisResults(NULL)
      showNotification("已清除所有上传文件和分析结果", type = "message")
    })
    
    # ---- 运行日志 ----
    output$diffLog <- renderText({
      "等待运行..."
    })
    
    # ---- 观察运行按钮 ----
    observeEvent(input$runDiff, {
      
      print("runDiff clicked")
      print(paste("countFile:", is.null(input$countFile)))
      
      if (is.null(input$countFile)) {
        showNotification("请上传表达矩阵文件！", type = "error")
        return()
      }

      count_file <- input$countFile
      log_fc_cutoff <- input$logFC
      p_value_cutoff <- deg_p_cutoff()
      p_value_column <- deg_p_value_column()
      task_note <- app_start_task_notification("差异分析正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          prepared <- deg_prepare_expression_matrix(count_file)
          combined_expr <- prepared$combined
          num_ctrl <- prepared$n_ctrl
          num_treat <- prepared$n_treat

          group_labels <- c(rep("Control", num_ctrl), rep("Treatment", num_treat))
          design_mat <- stats::model.matrix(~0 + factor(group_labels))
          colnames(design_mat) <- c("Control", "Treatment")

          fit_initial <- limma::lmFit(combined_expr, design_mat)
          contrast_mat <- limma::makeContrasts(Treatment - Control, levels = design_mat)
          fit_contrasted <- limma::contrasts.fit(fit_initial, contrast_mat)
          fit_contrasted <- limma::eBayes(fit_contrasted)

          all_results <- limma::topTable(fit_contrasted, adjust.method = "fdr", number = Inf)
          sig_genes <- all_results[abs(all_results$logFC) > log_fc_cutoff &
                                     all_results[[p_value_column]] < p_value_cutoff, ]
          sig_genes <- sig_genes[order(sig_genes$logFC), ]

          list(
            all = all_results,
            sig = sig_genes,
            combined = combined_expr,
            n_ctrl = num_ctrl,
            n_treat = num_treat,
            p_value_column = p_value_column,
            p_value_cutoff = p_value_cutoff,
            ctrl_samples = prepared$ctrl_samples,
            treat_samples = prepared$treat_samples,
            log_transformed = prepared$log_transformed
          )
        },
        on_success = function(result) {
          app_clear_task_notification(task_note)
          analysisResults(result)
          active_deg_plot("volcano")
          showNotification(
            paste0("分析完成！显著差异基因: ", nrow(result$sig)),
            type = "message",
            duration = 5
          )
        },
        on_error = function(error) {
          app_clear_task_notification(task_note)
          print(paste("Error:", conditionMessage(error)))
          showNotification(paste0("错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          app_clear_task_notification(task_note)
        }
      )
      return()
    })
    
    # ---- 火山图 ----
    output$volcanoPlot <- renderPlot({
      res <- analysisResults()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "请先运行差异分析")
        return()
      }
      
      color_up <- input$colorUp
      color_down <- input$colorDown
      color_ns <- input$colorNS
      
      df <- res$all
      df$Gene <- rownames(df)
      
      df$Significance <- ifelse(
        df$adj.P.Val < input$adjP & abs(df$logFC) > input$logFC,
        ifelse(df$logFC > input$logFC, "Up regulated", "Down regulated"),
        "Not Significant"
      )
      
      p <- ggplot(df, aes(x = logFC, y = -log10(adj.P.Val), color = Significance)) +
        geom_point(size = 2, alpha = 0.7) +
        scale_color_manual(
          values = c("Down regulated" = color_down, 
                     "Not Significant" = color_ns, 
                     "Up regulated" = color_up)
        ) +
        labs(title = "Volcano Plot",
             x = "Log2 Fold Change",
             y = "-Log10 Adjusted P-value") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      
      print(p)
    })
    
    # ---- 热图 ----
    output$heatmapPlot <- renderPlot({
      res <- analysisResults()
      if (is.null(res) || nrow(res$sig) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "无显著差异基因")
        return()
      }
      
      max_display <- 50
      ordered_genes <- rownames(res$sig)
      total_count <- length(ordered_genes)
      
      if (total_count > (max_display * 2)) {
        selected <- ordered_genes[c(1:max_display, 
                                    (total_count - max_display + 1):total_count)]
      } else {
        selected <- ordered_genes
      }
      
      heatmap_expr <- res$combined[selected, , drop = FALSE]
      
      sample_annotation <- data.frame(
        Group = deg_group_factor(res)
      )
      rownames(sample_annotation) <- colnames(res$combined)
      
      annotation_colors <- list(
        Group = deg_group_colors("#66C2A5", "#FC8D62")
      )
      
      color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)
      
      pheatmap(
        mat = heatmap_expr,
        annotation_col = sample_annotation,
        annotation_colors = annotation_colors,
        color = color_palette,
        cluster_cols = FALSE,
        show_colnames = FALSE,
        scale = "row",
        fontsize_row = 7,
        main = paste0("Differential Expression Heatmap (", length(selected), " genes)")
      )
    })
    
    # ---- PCA图 ----
    output$pcaPlot <- renderPlot({
      res <- analysisResults()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "请先运行差异分析")
        return()
      }
      
      pca_result <- prcomp(t(res$combined), scale. = TRUE)
      pca_var_perc <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
      
      pca_df <- data.frame(
        Sample = colnames(res$combined),
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2],
        Group = deg_group_factor(res)
      )
      
      ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
        stat_ellipse(level = 0.95, linetype = "dashed", linewidth = 1) +
        geom_point(size = 4, alpha = 0.8) +
        geom_text_repel(aes(label = Sample), size = 3.5, max.overlaps = 15) +
        scale_color_manual(values = deg_group_colors("#0072B2", "#E69F00")) +
        labs(title = "PCA Analysis",
             x = paste("PC1 (", pca_var_perc[1], "%)", sep = ""),
             y = paste("PC2 (", pca_var_perc[2], "%)", sep = "")) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              legend.position = "top")
    })
    
    # ---- 结果表格 ----
    output$resultTable <- renderDT({
      res <- analysisResults()
      if (is.null(res)) return(NULL)
      df <- cbind(Gene = rownames(res$sig), res$sig)
      datatable(df, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$sampleTypePreview <- renderDT({
      res <- sample_type_result()
      if (is.null(res)) {
        return(NULL)
      }
      preview_rows <- min(10, nrow(res$matrix))
      preview_cols <- min(10, ncol(res$matrix))
      datatable(
        res$matrix[seq_len(preview_rows), seq_len(preview_cols), drop = FALSE],
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$sampleTypeSummary <- renderDT({
      res <- sample_type_result()
      if (is.null(res)) {
        return(NULL)
      }
      datatable(res$summary, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })

    output$degDataPreview <- renderUI({
      if (identical(input$degTaskTab, "样本类型矫正")) {
        if (sample_type_running()) {
          return(div(style = "padding: 16px; color: #607d8b;", "样本类型矫正正在后台运行，请稍候。"))
        }
        return(tagList(
          DTOutput(ns("sampleTypePreview")),
          br(),
          DTOutput(ns("sampleTypeSummary"))
        ))
      }
      DTOutput(ns("resultTable"))
    })
    
    # ---- 结果文件列表 ----
    output$resultFileList <- renderUI({
      if (identical(input$degTaskTab, "样本类型矫正")) {
        if (is.null(sample_type_result())) {
          message <- if (sample_type_running()) {
            "样本类型矫正正在后台运行，请稍候。"
          } else {
            "暂无结果文件。请先运行样本类型矫正。"
          }
          return(
            div(
              class = "deg-result-file-list",
              div(style = "padding: 10px 8px; color: #78909c; font-size: 11px;", message)
            )
          )
        }

        rows <- list(
          list(file = "Sample Type Matrix.csv", type = "CSV", desc = "样本类型矩阵", download = "downloadSampleTypeCSV"),
          list(file = "Sample Type Matrix.txt", type = "TXT", desc = "样本类型矩阵文本格式", download = "downloadSampleTypeTXT"),
          list(file = "Sample_Summary.txt", type = "TXT", desc = "样本类型矫正统计", download = "downloadSampleTypeSummary")
        )

        return(div(
          class = "deg-result-file-list",
          lapply(seq_along(rows), function(i) {
            row <- rows[[i]]
            div(
              class = "deg-result-file-row",
              span(sprintf("%02d", i), class = "deg-file-index"),
              span(row$file, class = "deg-result-file-name", title = row$file),
              span(row$type, class = "deg-result-file-type"),
              span(row$desc, class = "deg-result-file-desc", title = row$desc),
              span(
                class = "deg-result-file-download",
                downloadButton(
                  ns(row$download),
                  "下载",
                  style = "font-size: 10px; padding: 1px 8px;"
                )
              )
            )
          })
        ))
      }

      res <- analysisResults()
      if (is.null(res)) {
        return(
          div(
            class = "deg-result-file-list",
            div(style = "padding: 10px 8px; color: #78909c; font-size: 11px;", "暂无结果文件。请先运行差异分析。")
          )
        )
      }
      
      all_up_genes <- get_all_up_genes(res)
      all_down_genes <- get_all_down_genes(res)

      rows <- list(
        list(file = paste0("diff_results_", Sys.Date(), ".csv"), type = "CSV", desc = "显著差异基因结果表", download = "downloadResult", plot = ""),
        list(file = paste0("up_genes_all.txt (", length(all_up_genes), " 个)"), type = "TXT", desc = "全部上调基因", download = "downloadUpGenesAll", plot = ""),
        list(file = paste0("down_genes_all.txt (", length(all_down_genes), " 个)"), type = "TXT", desc = "全部下调基因", download = "downloadDownGenesAll", plot = ""),
        list(file = "volcano.png", type = "PNG", desc = "火山图，点击文件名可在上方预览", download = "downloadDegPlot_volcano", plot = "volcano"),
        list(file = "heatmap.png", type = "PNG", desc = "差异基因热图，点击文件名可在上方预览", download = "downloadDegPlot_heatmap", plot = "heatmap"),
        list(file = "pca.png", type = "PNG", desc = "PCA 图，点击文件名可在上方预览", download = "downloadDegPlot_pca", plot = "pca")
      )

      div(
        class = "deg-result-file-list",
        lapply(seq_along(rows), function(i) {
          row <- rows[[i]]
          file_cell <- if (nzchar(row$plot)) {
            actionButton(
              ns(paste0("showDegPlot_", row$plot)),
              row$file,
              class = "deg-result-file-action",
              title = "点击后在上方图片区预览"
            )
          } else {
            span(row$file, class = "deg-result-file-name", title = row$file)
          }

          download_control <- if (nzchar(row$plot)) {
            downloadButton(
              ns(row$download),
              "下载",
              style = "font-size: 10px; padding: 1px 8px;"
            )
          } else {
            downloadButton(
              ns(row$download),
              "下载",
              style = "font-size: 10px; padding: 1px 8px;"
            )
          }

          div(
            class = "deg-result-file-row",
            span(sprintf("%02d", i), class = "deg-file-index"),
            file_cell,
            span(row$type, class = "deg-result-file-type"),
            span(row$desc, class = "deg-result-file-desc", title = row$desc),
            span(class = "deg-result-file-download", download_control)
          )
        })
      )
    })

    observeEvent(input$showDegPlot_volcano, {
      if (is.null(analysisResults())) {
        return()
      }
      active_deg_plot("volcano")
    }, ignoreInit = TRUE)

    observeEvent(input$showDegPlot_heatmap, {
      if (is.null(analysisResults())) {
        return()
      }
      active_deg_plot("heatmap")
    }, ignoreInit = TRUE)

    observeEvent(input$showDegPlot_pca, {
      if (is.null(analysisResults())) {
        return()
      }
      active_deg_plot("pca")
    }, ignoreInit = TRUE)

    observeEvent(input$openDegPlotDownload_volcano, {
      if (is.null(analysisResults())) {
        return()
      }
      show_deg_plot_download_modal("volcano")
    }, ignoreInit = TRUE)

    observeEvent(input$openDegPlotDownload_heatmap, {
      if (is.null(analysisResults())) {
        return()
      }
      show_deg_plot_download_modal("heatmap")
    }, ignoreInit = TRUE)

    observeEvent(input$openDegPlotDownload_pca, {
      if (is.null(analysisResults())) {
        return()
      }
      show_deg_plot_download_modal("pca")
    }, ignoreInit = TRUE)

    output$activeDegPlot <- renderPlot({
      draw_active_deg_plot(active_deg_plot())
    })

    output$activeDegPlotLarge <- renderPlot({
      draw_active_deg_plot(active_deg_plot(), large = TRUE)
    })

    output$downloadDegModalPNG <- downloadHandler(
      filename = function() {
        deg_plot_filename(download_deg_plot())
      },
      content = function(file) {
        write_deg_plot_png(file, download_deg_plot())
      }
    )

    output$downloadDegPlot_volcano <- downloadHandler(
      filename = function() "volcano.png",
      content = function(file) write_deg_plot_png(file, "volcano")
    )

    output$downloadDegPlot_heatmap <- downloadHandler(
      filename = function() "heatmap.png",
      content = function(file) write_deg_plot_png(file, "heatmap")
    )

    output$downloadDegPlot_pca <- downloadHandler(
      filename = function() "pca.png",
      content = function(file) write_deg_plot_png(file, "pca")
    )

    observeEvent(input$activeDegPlot_click, {
      if (is.null(analysisResults())) {
        return()
      }

      showModal(
        modalDialog(
          title = paste0(deg_plot_label(active_deg_plot()), " - 放大预览"),
          plotOutput(ns("activeDegPlotLarge"), height = "70vh", width = "100%"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭")
        )
      )
    })

    output$downloadSampleTypeCSV <- downloadHandler(
      filename = function() "Sample Type Matrix.csv",
      content = function(file) {
        req(sample_type_result())
        utils::write.csv(sample_type_result()$matrix, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$downloadSampleTypeTXT <- downloadHandler(
      filename = function() "Sample Type Matrix.txt",
      content = function(file) {
        req(sample_type_result())
        utils::write.table(sample_type_result()$matrix, file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    )

    output$downloadSampleTypeSummary <- downloadHandler(
      filename = function() "Sample_Summary.txt",
      content = function(file) {
        req(sample_type_result())
        writeLines(sample_type_result()$summary_lines, con = file, useBytes = TRUE)
      }
    )
    
    # ============================================================
    # 放大查看功能
    # ============================================================
    
    # ---- 火山图放大 ----
    observeEvent(input$volcanoModalBtn, {
      res <- analysisResults()
      if (is.null(res)) {
        showNotification("请先运行差异分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "火山图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            imageOutput(ns("volcanoLarge"), height = "600px")
          )
        )
      )
    })
    
    observeEvent(input$volcanoClick, {
      res <- analysisResults()
      if (is.null(res)) {
        showNotification("请先运行差异分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "火山图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            imageOutput(ns("volcanoLarge"), height = "600px")
          )
        )
      )
    })
    
    output$volcanoLarge <- renderImage({
      outfile <- tempfile(fileext = ".png")
      res <- analysisResults()
      
      if (!is.null(res)) {
        color_up <- input$colorUp
        color_down <- input$colorDown
        color_ns <- input$colorNS
        
        df <- res$all
        df$Gene <- rownames(df)
        df$Significance <- ifelse(
          df$adj.P.Val < input$adjP & abs(df$logFC) > input$logFC,
          ifelse(df$logFC > input$logFC, "Up regulated", "Down regulated"),
          "Not Significant"
        )
        
        p <- ggplot(df, aes(x = logFC, y = -log10(adj.P.Val), color = Significance)) +
          geom_point(size = 3, alpha = 0.8) +
          scale_color_manual(
            values = c("Down regulated" = color_down, 
                       "Not Significant" = color_ns, 
                       "Up regulated" = color_up)
          ) +
          labs(title = "Volcano Plot",
               x = "Log2 Fold Change",
               y = "-Log10 Adjusted P-value") +
          theme_minimal(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14),
                legend.title = element_blank())
        
        ggsave(outfile, p, width = 10, height = 8, dpi = 150)
      } else {
        png(outfile, width = 800, height = 600)
        plot(1, type = "n", main = "请先运行差异分析")
        dev.off()
      }
      
      list(src = outfile, contentType = "image/png", width = "100%", height = "600px")
    }, deleteFile = TRUE)
    
    # ---- 热图放大 ----
    observeEvent(input$heatmapModalBtn, {
      res <- analysisResults()
      if (is.null(res)) {
        showNotification("请先运行差异分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "热图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("heatmapLarge"), height = "600px")
          )
        )
      )
    })
    
    observeEvent(input$heatmapClick, {
      res <- analysisResults()
      if (is.null(res)) {
        showNotification("请先运行差异分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "热图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("heatmapLarge"), height = "600px")
          )
        )
      )
    })
    
    output$heatmapLarge <- renderPlot({
      res <- analysisResults()
      
      if (is.null(res) || nrow(res$sig) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "无显著差异基因")
        return()
      }
      
      max_display <- 50
      ordered_genes <- rownames(res$sig)
      total_count <- length(ordered_genes)
      
      if (total_count > (max_display * 2)) {
        selected <- ordered_genes[c(1:max_display, 
                                    (total_count - max_display + 1):total_count)]
      } else {
        selected <- ordered_genes
      }
      
      heatmap_expr <- res$combined[selected, , drop = FALSE]
      
      sample_annotation <- data.frame(
        Group = deg_group_factor(res)
      )
      rownames(sample_annotation) <- colnames(res$combined)
      
      annotation_colors <- list(
        Group = deg_group_colors("#66C2A5", "#FC8D62")
      )
      
      color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)
      
      pheatmap(
        mat = heatmap_expr,
        annotation_col = sample_annotation,
        annotation_colors = annotation_colors,
        color = color_palette,
        cluster_cols = FALSE,
        show_colnames = FALSE,
        scale = "row",
        fontsize_row = 8,
        fontsize_col = 10,
        main = paste0("Differential Expression Heatmap (", length(selected), " genes)")
      )
    })
    
    # ---- PCA图放大 ----
    observeEvent(input$pcaModalBtn, {
      res <- analysisResults()
      if (is.null(res)) {
        showNotification("请先运行差异分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "PCA图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("pcaLarge"), height = "600px")
          )
        )
      )
    })
    
    observeEvent(input$pcaClick, {
      res <- analysisResults()
      if (is.null(res)) {
        showNotification("请先运行差异分析", type = "warning")
        return()
      }
      
      showModal(
        modalDialog(
          title = "PCA图",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("pcaLarge"), height = "600px")
          )
        )
      )
    })
    
    output$pcaLarge <- renderPlot({
      res <- analysisResults()
      
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "请先运行差异分析")
        return()
      }
      
      pca_result <- prcomp(t(res$combined), scale. = TRUE)
      pca_var_perc <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
      
      pca_df <- data.frame(
        Sample = colnames(res$combined),
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2],
        Group = deg_group_factor(res)
      )
      
      ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
        stat_ellipse(level = 0.95, linetype = "dashed", linewidth = 1.2) +
        geom_point(size = 6, alpha = 0.9) +
        geom_text_repel(aes(label = Sample), size = 5, max.overlaps = 20) +
        scale_color_manual(values = deg_group_colors("#0072B2", "#E69F00")) +
        labs(title = "PCA Analysis",
             x = paste("PC1 (", pca_var_perc[1], "%)", sep = ""),
             y = paste("PC2 (", pca_var_perc[2], "%)", sep = "")) +
        theme_classic(base_size = 16) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              legend.position = "top")
    })
    
    # ---- 下载功能 ----
    output$downloadResult <- downloadHandler(
      filename = function() paste0("diff_results_", Sys.Date(), ".csv"),
      content = function(file) {
        res <- analysisResults()
        if (!is.null(res)) {
          df <- cbind(Gene = rownames(res$sig), res$sig)
          write.csv(df, file, row.names = FALSE)
        }
      }
    )
    
    output$downloadUpGenesAll <- downloadHandler(
      filename = function() paste0("up_genes_all_", Sys.Date(), ".txt"),
      content = function(file) {
        res <- analysisResults()
        if (is.null(res)) return()
        up_genes <- get_all_up_genes(res)
        if (!is.null(up_genes) && length(up_genes) > 0) {
          writeLines(up_genes, file)
        } else {
          writeLines("没有上调基因", file)
        }
      }
    )
    
    output$downloadDownGenesAll <- downloadHandler(
      filename = function() paste0("down_genes_all_", Sys.Date(), ".txt"),
      content = function(file) {
        res <- analysisResults()
        if (is.null(res)) return()
        down_genes <- get_all_down_genes(res)
        if (!is.null(down_genes) && length(down_genes) > 0) {
          writeLines(down_genes, file)
        } else {
          writeLines("没有下调基因", file)
        }
      }
    )
    
    output$downloadVolcano <- downloadHandler(
      filename = "volcano.png",
      content = function(file) {
        res <- analysisResults()
        if (is.null(res)) {
          png(file, width = 4000, height = 3200, res = 300)
          plot(1, type = "n", main = "请先运行差异分析")
          dev.off()
          return()
        }
        
        color_up <- input$colorUp
        color_down <- input$colorDown
        color_ns <- input$colorNS
        
        df <- res$all
        df$Gene <- rownames(df)
        df$Significance <- ifelse(
          df$adj.P.Val < input$adjP & abs(df$logFC) > input$logFC,
          ifelse(df$logFC > input$logFC, "Up regulated", "Down regulated"),
          "Not Significant"
        )
        
        p <- ggplot(df, aes(x = logFC, y = -log10(adj.P.Val), color = Significance)) +
          geom_point(size = 3, alpha = 0.8) +
          scale_color_manual(
            values = c("Down regulated" = color_down, 
                       "Not Significant" = color_ns, 
                       "Up regulated" = color_up)
          ) +
          labs(title = "Volcano Plot",
               x = "Log2 Fold Change",
               y = "-Log10 Adjusted P-value") +
          theme_minimal(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14),
                legend.title = element_blank())
        
        ggsave(file, p, width = 16, height = 12, dpi = 300, bg = "white")
      }
    )
    
    output$downloadHeatmap <- downloadHandler(
      filename = "heatmap.png",
      content = function(file) {
        res <- analysisResults()
        if (is.null(res) || nrow(res$sig) == 0) {
          png(file, width = 4000, height = 3200, res = 300)
          plot(1, type = "n", main = "无显著差异基因")
          dev.off()
          return()
        }
        
        max_display <- 50
        ordered_genes <- rownames(res$sig)
        total_count <- length(ordered_genes)
        
        if (total_count > (max_display * 2)) {
          selected <- ordered_genes[c(1:max_display, 
                                      (total_count - max_display + 1):total_count)]
        } else {
          selected <- ordered_genes
        }
        
        heatmap_expr <- res$combined[selected, , drop = FALSE]
        
        sample_annotation <- data.frame(
          Group = deg_group_factor(res)
        )
        rownames(sample_annotation) <- colnames(res$combined)
        
        annotation_colors <- list(
          Group = deg_group_colors("#66C2A5", "#FC8D62")
        )
        
        color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)
        
        png(file, width = 4000, height = 3200, res = 300)
        pheatmap(
          mat = heatmap_expr,
          annotation_col = sample_annotation,
          annotation_colors = annotation_colors,
          color = color_palette,
          cluster_cols = FALSE,
          show_colnames = FALSE,
          scale = "row",
          fontsize_row = 10,
          fontsize_col = 12,
          main = paste0("Differential Expression Heatmap (", length(selected), " genes)")
        )
        dev.off()
      }
    )
    
    output$downloadPCA <- downloadHandler(
      filename = "pca.png",
      content = function(file) {
        res <- analysisResults()
        if (is.null(res)) {
          png(file, width = 4000, height = 3200, res = 300)
          plot(1, type = "n", main = "请先运行差异分析")
          dev.off()
          return()
        }
        
        pca_result <- prcomp(t(res$combined), scale. = TRUE)
        pca_var_perc <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
        
        pca_df <- data.frame(
          Sample = colnames(res$combined),
          PC1 = pca_result$x[, 1],
          PC2 = pca_result$x[, 2],
          Group = deg_group_factor(res)
        )
        
        p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
          stat_ellipse(level = 0.95, linetype = "dashed", linewidth = 1.2) +
          geom_point(size = 6, alpha = 0.9) +
          geom_text_repel(aes(label = Sample), size = 6, max.overlaps = 20) +
          scale_color_manual(values = deg_group_colors("#0072B2", "#E69F00")) +
          labs(title = "PCA Analysis",
               x = paste("PC1 (", pca_var_perc[1], "%)", sep = ""),
               y = paste("PC2 (", pca_var_perc[2], "%)", sep = "")) +
          theme_classic(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 16),
                legend.position = "top")
        
        ggsave(file, p, width = 16, height = 12, dpi = 300, bg = "white")
      }
    )
    
    # ---- 返回分析结果 ----
    return(analysisResults)
    
  })
}
