# roc_module.R - ROC曲线分析模块（完整版）
# 功能：对指定基因进行差异分析 + ROC曲线绘制
# 参考：GEO数据库的指定基因的差异分析.R
# 修改：批量基因箱线图 + 合并ROC曲线

# ============================================================
# UI
# ============================================================
roc_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .roc-container {
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
        .roc-container:hover {
            background-color: #f0f0f0;
        }
        .param-box {
            border: 2px solid #3498db;
            border-radius: 0px;
            padding: 8px;
            height: 280px;
            overflow: hidden;
            background-color: #f7fbff;
        }
        .param-box h4 {
            font-size: 13px;
            margin: 0;
            color: #2c3e50;
        }
        .param-box .help-text {
            font-size: 10px;
            color: #666;
            margin-top: 1px;
            margin-bottom: 2px;
        }
        .param-box hr {
            margin: 3px 0;
        }
        .param-box .btn-sm {
            padding: 2px 8px;
            font-size: 12px;
        }
        .param-box .btn-xs {
            font-size: 9px;
            padding: 0px 6px;
            height: 20px;
        }
        .param-box .form-control {
            font-size: 11px;
            padding: 2px 4px;
            height: 26px;
        }
        .param-box .shiny-input-container {
            margin-bottom: 2px;
        }
        .param-box .file-input-box {
            border: 1.5px dashed #ccc;
            border-radius: 0px;
            padding: 2px 8px;
            background-color: #fafafa;
            flex: 1;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: space-between;
            font-size: 10px;
            height: 24px;
        }
        .param-box .file-input-box:hover {
            background-color: #f0f0f0;
        }
        .param-box .file-input-box .file-status {
            color: #3498db;
        }
        .param-row {
            display: flex;
            align-items: center;
            gap: 6px;
            margin-bottom: 2px;
        }
        .param-row label {
            font-size: 10px;
            margin: 0;
            min-width: 40px;
            font-weight: bold;
        }
        .param-row .form-control {
            font-size: 10px;
            padding: 1px 4px;
            height: 22px;
            width: 60px;
        }
        .status-item {
            display: flex;
            justify-content: space-between;
            padding: 4px 0;
            border-bottom: 1px solid #eee;
            font-size: 12px;
        }
        .status-item:last-child {
            border-bottom: none;
        }
        .status-item .label {
            font-weight: bold;
        }
        .file-item {
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 3px 0;
            border-bottom: 1px solid #eee;
            font-size: 12px;
        }
        .file-item:last-child {
            border-bottom: none;
        }
    ")),
    
    # ---- 第一行：区域一 + 区域二（各占50%） ----
    fluidRow(
      style = "margin: 0;",
      
      # ============================================================
      # 区域一：参数设置与运行（左上）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "param-box",
          
          h4("参数设置与运行"),
          p("上传表达矩阵和基因列表，运行ROC分析", class = "help-text"),
          hr(style = "margin: 3px 0;"),
          
          # ---- 文件上传 ----
          div(
            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 3px;",
            div(
              class = "file-input-box",
              id = ns("exprFileBox"),
              onclick = sprintf("document.getElementById('%s').click();", ns("exprFile")),
              tags$span("表达矩阵 (CSV)"),
              tags$span(id = ns("exprFileStatus"), "点击选择", class = "file-status"),
              tags$div(
                style = "display: none;",
                fileInput(ns("exprFile"), NULL,
                          accept = c(".csv"),
                          buttonLabel = "",
                          placeholder = NULL)
              )
            ),
            actionButton(ns("clearExprFile"), "×", 
                         class = "btn-danger btn-xs",
                         style = "font-size: 12px; font-weight: bold; width: 28px; height: 28px; padding: 0; line-height: 1; border-radius: 0px;")
          ),
          
          div(
            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 6px;",
            div(
              class = "file-input-box",
              id = ns("geneFileBox"),
              onclick = sprintf("document.getElementById('%s').click();", ns("geneFile")),
              tags$span("基因列表 (CSV/TXT)"),
              tags$span(id = ns("geneFileStatus"), "点击选择", class = "file-status"),
              tags$div(
                style = "display: none;",
                fileInput(ns("geneFile"), NULL,
                          accept = c(".csv", ".txt"),
                          buttonLabel = "",
                          placeholder = NULL)
              )
            ),
            actionButton(ns("clearGeneFile"), "×", 
                         class = "btn-danger btn-xs",
                         style = "font-size: 12px; font-weight: bold; width: 28px; height: 28px; padding: 0; line-height: 1; border-radius: 0px;")
          ),
          
          hr(style = "margin: 3px 0;"),
          
          # ---- 参数 ----
          div(
            class = "param-row",
            tags$label("分组标签:", style = "font-size: 10px;"),
            textInput(ns("groupCon"), NULL, value = "con", placeholder = "对照组", width = "50px"),
            textInput(ns("groupTreat"), NULL, value = "treat", placeholder = "实验组", width = "50px"),
            tags$span("样本名格式: Sample_Group", style = "font-size: 9px; color: #888;")
          ),
          
          div(
            class = "param-row",
            tags$label("颜色:", style = "font-size: 10px;"),
            tags$span("对照组", style = "font-size: 9px; color: #2E8B57;"),
            colourInput(ns("colorCon"), NULL, value = "#2E8B57", 
                        showColour = "background", palette = "limited",
                        width = "35px"),
            tags$span("实验组", style = "font-size: 9px; color: #8B008B;"),
            colourInput(ns("colorTreat"), NULL, value = "#8B008B", 
                        showColour = "background", palette = "limited",
                        width = "35px")
          ),
          
          hr(style = "margin: 3px 0;"),
          
          # ---- 运行按钮 ----
          actionButton(ns("runRoc"), "运行ROC分析", 
                       class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 4px 0; margin-bottom: 3px;")
        )
      ),
      
      # ============================================================
      # 区域二：图片显示（右上）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          style = "border: 2px solid #f39c12; border-radius: 0px; padding: 8px; height: 370px; overflow-y: auto; background-color: #fefcf6;",
          
          h4("图片显示", style = "color: #2c3e50; margin-top: 0; margin-bottom: 0; font-size: 14px;"),
          hr(style = "margin: 4px 0;"),
          
          # ---- 图片标签页 ----
          tabsetPanel(
            id = ns("imageTabs"),
            tabPanel(
              "箱线图",
              br(),
              div(
                class = "roc-container",
                style = "height: 230px;",
                plotOutput(ns("boxplotAll"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadBoxplotAll"), "下载 PDF", 
                               style = "font-size: 9px; padding: 1px 8px;"),
                downloadButton(ns("downloadBoxplotAllPNG"), "下载 PNG", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            ),
            tabPanel(
              "合并ROC曲线",
              br(),
              div(
                class = "roc-container",
                style = "height: 230px;",
                plotOutput(ns("combinedRocPlot"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadCombinedRocPDF"), "下载 PDF", 
                               style = "font-size: 9px; padding: 1px 8px;"),
                downloadButton(ns("downloadCombinedRocPNG"), "下载 PNG", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            ),
            tabPanel(
              "AUC条形图",
              br(),
              div(
                class = "roc-container",
                style = "height: 230px;",
                plotOutput(ns("aucBarplot"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadAucBarplot"), "下载 PNG", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            )
          )
        )
      )
    ),
    
    # ---- 第二行：区域三 + 区域四（各占50%） ----
    fluidRow(
      style = "margin: 0;",
      
      # ============================================================
      # 区域三：AUC条形图（左下）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          style = "border: 2px solid #2ecc71; border-radius: 0px; padding: 8px; height: 280px; background-color: #f6fef9;",
          
          h4("AUC条形图", style = "color: #2c3e50; margin-top: 0; margin-bottom: 0; font-size: 14px;"),
          hr(style = "margin: 4px 0;"),
          
          div(
            class = "roc-container",
            style = "height: 190px;",
            plotOutput(ns("aucBarplotBottom"), height = "100%", width = "100%")
          ),
          
          br(),
          div(
            style = "display: flex; gap: 4px; flex-wrap: wrap;",
            downloadButton(ns("downloadAucBarplotBottom"), "下载 PNG", 
                           style = "font-size: 9px; padding: 1px 8px;")
          )
        )
      ),
      
      # ============================================================
      # 区域四：结果表格（右下）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          style = "border: 2px solid #9b59b6; border-radius: 0px; padding: 8px; height: 280px; background-color: #faf5fc;",
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            h4("结果表格", style = "color: #2c3e50; margin-top: 0; margin-bottom: 0; font-size: 14px;"),
            tags$span(id = ns("resultCount"), "共 0 个基因", style = "font-size: 11px; color: #888;")
          ),
          hr(style = "margin: 4px 0;"),
          
          DTOutput(ns("resultTable")),
          
          br(),
          div(
            style = "display: flex; gap: 4px; flex-wrap: wrap;",
            downloadButton(ns("downloadResult"), "下载 CSV", 
                           style = "font-size: 9px; padding: 1px 8px;")
          )
        )
      )
    )
  )
}


# ============================================================
# UI - 差异分析同款紧凑布局
# ============================================================
roc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .roc-card,
      .roc-plot-card,
      .roc-result-card {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        padding: 12px 16px;
        background-color: #ffffff;
      }
      .roc-card,
      .roc-plot-card {
        height: 370px;
        overflow-y: auto;
      }
      .roc-card h4,
      .roc-plot-card h4,
      .roc-result-card h4 {
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 14px;
        font-weight: 700;
      }
      .roc-card hr,
      .roc-plot-card hr {
        margin: 4px 0 8px 0;
      }
      .roc-upload-toolbar {
        display: flex;
        gap: 4px;
        flex-wrap: wrap;
        align-items: center;
        margin-bottom: 6px;
      }
      .roc-upload-row {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        align-items: center;
        margin-bottom: 6px;
      }
      .roc-upload-box {
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
      .roc-upload-box:hover {
        background-color: #f7fafc;
      }
      .roc-upload-box .shiny-input-container {
        position: absolute;
        inset: 0;
        width: 100% !important;
        height: 100%;
        margin: 0;
        opacity: 0;
        z-index: 2;
        cursor: pointer;
      }
      .roc-upload-box .input-group,
      .roc-upload-box .input-group-btn,
      .roc-upload-box .btn-file,
      .roc-upload-box input[type='file'] {
        width: 100%;
        height: 100%;
        cursor: pointer;
      }
      .roc-upload-placeholder {
        text-align: center;
        pointer-events: none;
        display: grid;
        gap: 2px;
        justify-items: center;
      }
      .roc-upload-title {
        font-weight: 700;
        font-size: 11px;
        color: #263238;
        max-width: 100%;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .roc-upload-status {
        color: #1e88e5;
        font-size: 11px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 170px;
      }
      .roc-compact-section {
        border: 1px solid #d7dee2;
        background: #ffffff;
        padding: 6px 8px;
        margin-bottom: 7px;
      }
      .roc-compact-title {
        display: block;
        color: #263238;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 5px;
      }
      .roc-param-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 6px 8px;
      }
      .roc-param-grid .shiny-input-container {
        width: 100%;
        margin-bottom: 0;
      }
      .roc-card .form-control {
        font-size: 11px;
        padding: 2px 4px;
        height: 26px;
      }
      .roc-card .shiny-input-container {
        margin-bottom: 6px;
      }
      .roc-card label {
        font-size: 11px;
        color: #263238;
        margin-bottom: 3px;
      }
      .roc-color-row {
        display: flex;
        gap: 8px;
        align-items: center;
        flex-wrap: wrap;
      }
      .roc-color-control {
        display: flex;
        gap: 4px;
        align-items: center;
        font-size: 11px;
        color: #455a64;
      }
      .roc-run-btn {
        width: 100%;
        font-size: 12px;
        font-weight: 700;
        padding: 4px 0;
        margin-top: 4px;
      }
      .roc-active-plot-box {
        border: none;
        background: transparent;
        min-height: 285px;
        cursor: zoom-in;
      }
      .roc-result-panel {
        max-width: 100%;
        overflow-x: hidden;
      }
      .roc-result-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .roc-result-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 26px;
        padding: 8px 2px 9px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .roc-result-panel .nav-tabs > li.active > a,
      .roc-result-panel .nav-tabs > li.active > a:hover,
      .roc-result-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .roc-result-slot {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        min-height: 120px;
      }
      .roc-result-file-list {
        border: 1px solid #d7dee2;
        background: #ffffff;
      }
      .roc-result-file-row {
        display: grid;
        grid-template-columns: 28px minmax(150px, 1fr) 54px minmax(150px, 1.4fr) 70px;
        gap: 8px;
        align-items: center;
        padding: 6px 8px;
        border-bottom: 1px solid #eef2f4;
        font-size: 11px;
      }
      .roc-result-file-row:last-child {
        border-bottom: none;
      }
      .roc-file-index {
        color: #1e88e5;
        font-weight: 700;
      }
      .roc-result-file-action {
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
      .roc-result-file-name,
      .roc-result-file-desc {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .roc-result-file-type {
        color: #455a64;
        font-weight: 700;
      }
      .roc-result-file-desc {
        color: #607d8b;
      }
      .roc-result-file-download .btn {
        font-size: 10px;
        padding: 1px 8px;
        line-height: 1.4;
      }
      .roc-status-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 8px;
        margin-bottom: 8px;
      }
      .roc-status-item {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        font-size: 12px;
      }
      .roc-status-item b {
        display: block;
        color: #263238;
        font-size: 12px;
      }
      .roc-qa {
        font-size: 12px;
        line-height: 1.7;
        color: #455a64;
        max-height: 190px;
        overflow-y: auto;
      }
      .roc-qa dl { margin: 0; }
      .roc-qa dt {
        margin-top: 8px;
        color: #263238;
      }
      .roc-qa dt:first-child { margin-top: 0; }
      .roc-qa dd {
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
          class = "roc-card",
          h4("参数设置"),
          hr(),
          tags$div(
            class = "roc-upload-row",
            tags$div(
              id = ns("exprFileBox"),
              class = "roc-upload-box",
              tags$div(
                class = "roc-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(id = ns("exprFileTitle"), "表达矩阵/Sample Type Matrix.csv", class = "roc-upload-title"),
                tags$span(id = ns("exprFileStatus"), "Drop file here or click to upload", class = "roc-upload-status")
              ),
              fileInput(ns("exprFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择表达矩阵文件")
            )
          ),
          tags$div(
            class = "roc-upload-row",
            tags$div(
              id = ns("geneFileBox"),
              class = "roc-upload-box",
              tags$div(
                class = "roc-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span("基因列表（可选）", class = "roc-upload-title"),
                tags$span(id = ns("geneFileStatus"), "Drop file here or click to upload", class = "roc-upload-status")
              ),
              fileInput(ns("geneFile"), NULL,
                        accept = c(".csv", ".txt", ".tsv"),
                        buttonLabel = "浏览",
                        placeholder = "选择基因列表")
            )
          ),
          div(
            class = "roc-compact-section",
            span("分组标签", class = "roc-compact-title"),
            div(
              class = "roc-param-grid",
              textInput(ns("groupCon"), "对照组", value = "con"),
              textInput(ns("groupTreat"), "实验组", value = "treat")
            )
          ),
          div(
            class = "roc-compact-section",
            span("颜色", class = "roc-compact-title"),
            div(
              class = "roc-color-row",
              div(class = "roc-color-control", span("对照组"), colourpicker::colourInput(ns("colorCon"), NULL, value = "#2E8B57", showColour = "background", palette = "limited", width = "28px")),
              div(class = "roc-color-control", span("实验组"), colourpicker::colourInput(ns("colorTreat"), NULL, value = "#8B008B", showColour = "background", palette = "limited", width = "28px"))
            )
          ),
          actionButton(ns("runRoc"), "运行ROC分析", class = "btn-success btn-sm roc-run-btn")
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "roc-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "roc-active-plot-box",
            plotOutput(
              ns("activeRocPlot"),
              height = "285px",
              width = "100%",
              click = ns("activeRocPlot_click")
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
          class = "roc-result-card",
          h4("结果预览"),
          div(
            class = "roc-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel(
                "结果表",
                div(class = "roc-result-slot", uiOutput(ns("resultFileList")))
              ),
              tabPanel(
                "数据预览",
                div(
                  class = "roc-result-slot",
                  uiOutput(ns("rocStatus")),
                  DTOutput(ns("resultTable"))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "roc-qa",
                  tags$dl(
                    tags$dt("Q1：ROC模块需要什么输入？"),
                    tags$dd("需要表达矩阵，基因列表可选。表达矩阵第一列为基因名，后续列为样本表达值；样本名末尾下划线后的内容作为分组标签。"),
                    tags$dt("Q2：基因列表不上传会怎样？"),
                    tags$dd("不上传基因列表时会分析表达矩阵中的全部基因；基因很多时建议先筛选后再运行。"),
                    tags$dt("Q3：图片如何查看？"),
                    tags$dd("运行完成后点击结果表中的箱线图、合并ROC或AUC条形图文件名，图片会显示在右侧图片区域。"),
                    tags$dt("Q4：AUC如何理解？"),
                    tags$dd("AUC 越接近 1，说明该基因区分两组样本的能力越强；接近 0.5 表示区分能力较弱。"),
                    tags$dt("Q5：结果文件怎么使用？"),
                    tags$dd("ROC_results.csv 可查看每个基因的 P 值、AUC 和置信区间；PNG/PDF 图片可用于报告和后续整理。")
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
roc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储结果 ----
    roc_results <- reactiveVal(NULL)
    result_df <- reactiveVal(NULL)
    gene_list <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    active_roc_plot <- reactiveVal("combined")
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$exprFile, {
      if (!is.null(input$exprFile)) {
        label <- paste0("表达矩阵/", input$exprFile$name)
        label <- gsub("\\\\", "\\\\\\\\", label)
        label <- gsub('"', '\\"', label, fixed = TRUE)
        runjs(paste0('$("#', ns("exprFileTitle"), '").text("', label, '")'))
        runjs(paste0('$("#', ns("exprFileStatus"), '").text("Drop file here or click to upload")'))
      }
    })
    
    observeEvent(input$geneFile, {
      if (!is.null(input$geneFile)) {
        runjs(paste0('$("#', ns("geneFileStatus"), '").text("', input$geneFile$name, '")'))
      }
    })
    
    # ---- 清除文件 ----
    observeEvent(input$clearExprFile, {
      shinyjs::reset("exprFile")
      runjs(paste0('$("#', ns("exprFileTitle"), '").text("表达矩阵/Sample Type Matrix.csv")'))
      runjs(paste0('$("#', ns("exprFileStatus"), '").text("Drop file here or click to upload")'))
      roc_results(NULL)
      result_df(NULL)
      gene_list(NULL)
    })
    
    observeEvent(input$clearGeneFile, {
      shinyjs::reset("geneFile")
      runjs(paste0('$("#', ns("geneFileStatus"), '").text("点击选择")'))
    })
    
    # ============================================================
    # 使用说明弹窗
    # ============================================================
    observeEvent(input$helpBtn, {
      showModal(
        modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("ROC曲线分析 - 使用说明", style = "font-size: 18px; font-weight: bold;"),
            tags$span("v1.0", style = "font-size: 12px; color: #999;")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "max-height: 70vh; overflow-y: auto; padding-right: 10px; font-size: 14px; line-height: 1.8;",
            tags$hr(style = "margin: 8px 0;"),
            
            tags$h5("1. 功能目的", style = "color: #3498db;"),
            tags$p("对指定基因进行差异表达分析和ROC曲线评估，评估单个基因区分两组样本的能力。"),
            
            tags$h5("2. 输入文件", style = "color: #2ecc71;"),
            tags$ul(
              tags$li(tags$strong("表达矩阵"), "：CSV格式，行=基因，列=样本，样本名格式为 ", tags$code("Sample_Group")),
              tags$li(tags$strong("基因列表"), "：CSV或TXT格式（可选），第一列为基因名")
            ),
            
            tags$h5("3. 输出结果", style = "color: #f39c12;"),
            tags$ul(
              tags$li("批量箱线图：所有基因合并展示，含P值标签"),
              tags$li("合并ROC曲线：所有基因叠加展示"),
              tags$li("AUC条形图：基因排名对比"),
              tags$li("结果表格：P值、AUC等")
            ),
            
            tags$h5("4. 注意事项", style = "color: #e74c3c;"),
            tags$ul(
              tags$li("样本名格式：", tags$code("Sample1_con"), " 或 ", tags$code("Sample1_treat")),
              tags$li("分组标签需与样本名后缀一致"),
              tags$li("基因数过多时建议先筛选（如差异分析结果）")
            ),
            
            tags$hr(style = "margin: 12px 0;"),
            div(
              style = "display: flex; justify-content: space-between; font-size: 12px; color: #999;",
              tags$span("更新日期: 2026-06-26"),
              tags$span("版本: v1.0")
            )
          )
        )
      )
    })
    
    # ---- 核心分析 ----
    observeEvent(input$runRoc, {
      
      if (is.null(input$exprFile)) {
        showNotification("请上传表达矩阵文件！", type = "error")
        return()
      }

      expr_file <- input$exprFile
      gene_file <- input$geneFile
      con_label <- input$groupCon
      treat_label <- input$groupTreat
      
      is_running(TRUE)
      roc_results(NULL)
      result_df(NULL)

      showNotification("ROC 分析已在后台启动，可以切换到其它模块继续操作。", type = "message", duration = APP_RUNNING_NOTIFICATION_DURATION)

      run_async_task(
        task = function() {
          rt <- read_expression_matrix(expr_file)

          y <- gsub("(.*)\\_(.*)", "\\2", colnames(rt))
          y <- ifelse(y == con_label, 0, 1)

          if (sum(y == 0) == 0 || sum(y == 1) == 0) {
            stop(paste0("未找到分组标签 ", con_label, " 和 ", treat_label), call. = FALSE)
          }

          conNum <- sum(y == 0)
          treatNum <- sum(y == 1)
          Type <- c(rep(con_label, conNum), rep(treat_label, treatNum))

          if (!is.null(gene_file)) {
            selectedGenes <- read_gene_list_file(gene_file)
            found_genes <- selectedGenes[selectedGenes %in% rownames(rt)]
            if (length(found_genes) == 0) {
              stop("基因列表中的基因在表达矩阵中未找到", call. = FALSE)
            }
            rt_filtered <- rt[found_genes, , drop = FALSE]
          } else {
            rt_filtered <- rt
          }

          rt_filtered_df <- data.frame(Gene = rownames(rt_filtered), rt_filtered, check.names = FALSE)
          rt_filtered_long <- reshape2::melt(
            rt_filtered_df,
            id.vars = "Gene",
            variable.name = "Sample",
            value.name = "Expression"
          )

          sample_type <- data.frame(Sample = colnames(rt_filtered), Type = Type, stringsAsFactors = FALSE)
          rt_filtered_long <- merge(rt_filtered_long, sample_type, by = "Sample")

          gene_names <- rownames(rt_filtered)
          pvals <- vapply(gene_names, function(g) {
            dat <- rt_filtered_long[rt_filtered_long$Gene == g, ]
            stats::t.test(Expression ~ Type, data = dat)$p.value
          }, numeric(1))
          pvals_df <- data.frame(Gene = names(pvals), P_Value = pvals, stringsAsFactors = FALSE)

          mean_con <- vapply(gene_names, function(g) {
            mean(rt_filtered_long[rt_filtered_long$Gene == g & rt_filtered_long$Type == con_label, "Expression"])
          }, numeric(1))
          mean_treat <- vapply(gene_names, function(g) {
            mean(rt_filtered_long[rt_filtered_long$Gene == g & rt_filtered_long$Type == treat_label, "Expression"])
          }, numeric(1))

          roc_list <- list()
          auc_values <- c()
          auc_lower <- c()
          auc_upper <- c()

          for (gene in gene_names) {
            expr_values <- as.numeric(rt_filtered[gene, ])
            roc_obj <- pROC::roc(response = y, predictor = expr_values, quiet = TRUE)
            ci1 <- pROC::ci.auc(roc_obj, method = "bootstrap", quiet = TRUE)
            ciVec <- as.numeric(ci1)

            roc_list[[gene]] <- roc_obj
            auc_values[gene] <- ciVec[2]
            auc_lower[gene] <- ciVec[1]
            auc_upper[gene] <- ciVec[3]
          }

          result_df_val <- data.frame(
            Gene = gene_names,
            P_Value = pvals[gene_names],
            AUC = auc_values[gene_names],
            AUC_Lower_CI = auc_lower[gene_names],
            AUC_Upper_CI = auc_upper[gene_names],
            Mean_Con = mean_con[gene_names],
            Mean_Treat = mean_treat[gene_names],
            stringsAsFactors = FALSE
          )

          result_df_val <- result_df_val[order(result_df_val$AUC, decreasing = TRUE), ]
          rownames(result_df_val) <- NULL

          list(
            results = list(
              roc_list = roc_list,
              auc_values = auc_values,
              rt_filtered = rt_filtered,
              rt_filtered_long = rt_filtered_long,
              Type = Type,
              con_label = con_label,
              treat_label = treat_label,
              conNum = conNum,
              treatNum = treatNum,
              total_genes = length(gene_names),
              pvals_df = pvals_df,
              gene_names = gene_names
            ),
            result_df = result_df_val,
            gene_names = gene_names
          )
        },
        on_success = function(result) {
          roc_results(result$results)
          result_df(result$result_df)
          gene_list(result$gene_names)
          active_roc_plot("combined")
          showNotification(
            paste0("ROC 分析完成！共分析 ", length(result$gene_names), " 个基因"),
            type = "message",
            duration = 5
          )
        },
        on_error = function(error) {
          showNotification(paste0("错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          is_running(FALSE)
        }
      )
      return()
      
      withProgress(message = "ROC分析运行中...", value = 0, {
        
        tryCatch({
          
          incProgress(0.1, detail = "读取表达矩阵...")
          
          # ---- 1. 读取数据 ----
          rt <- read_expression_matrix(input$exprFile)
          
          # ---- 2. 提取分组信息 ----
          con_label <- input$groupCon
          treat_label <- input$groupTreat
          
          y <- gsub("(.*)\\_(.*)", "\\2", colnames(rt))
          y <- ifelse(y == con_label, 0, 1)
          
          if (sum(y == 0) == 0 || sum(y == 1) == 0) {
            showNotification(paste0("未找到分组标签: ", con_label, " 和 ", treat_label), type = "error")
            is_running(FALSE)
            return()
          }
          
          conNum <- sum(y == 0)
          treatNum <- sum(y == 1)
          Type <- c(rep(con_label, conNum), rep(treat_label, treatNum))
          
          incProgress(0.2, detail = "筛选基因...")
          
          # ---- 3. 读取基因列表 ----
          if (!is.null(input$geneFile)) {
            selectedGenes <- read_gene_list_file(input$geneFile)
            
            found_genes <- selectedGenes[selectedGenes %in% rownames(rt)]
            if (length(found_genes) == 0) {
              showNotification("基因列表中的基因在表达矩阵中未找到", type = "error")
              is_running(FALSE)
              return()
            }
            rt_filtered <- rt[found_genes, , drop = FALSE]
            showNotification(paste0("找到 ", length(found_genes), " 个基因"), type = "message")
          } else {
            rt_filtered <- rt
            showNotification(paste0("分析所有基因: ", nrow(rt_filtered), " 个"), type = "message")
          }
          
          incProgress(0.3, detail = "计算差异和ROC...")
          
          # ---- 4. 转换长格式 ----
          rt_filtered_df <- data.frame(Gene = rownames(rt_filtered), rt_filtered, check.names = FALSE)
          rt_filtered_long <- reshape2::melt(rt_filtered_df, id.vars = "Gene",
                                             variable.name = "Sample",
                                             value.name = "Expression")
          
          sample_type <- data.frame(Sample = colnames(rt_filtered), Type = Type, stringsAsFactors = FALSE)
          rt_filtered_long <- merge(rt_filtered_long, sample_type, by = "Sample")
          
          # ---- 5. 计算P值 ----
          gene_names <- rownames(rt_filtered)
          pvals <- sapply(gene_names, function(g) {
            dat <- rt_filtered_long[rt_filtered_long$Gene == g, ]
            t.test(Expression ~ Type, data = dat)$p.value
          })
          pvals_df <- data.frame(Gene = names(pvals), P_Value = pvals, stringsAsFactors = FALSE)
          
          # 计算每个基因的均值
          mean_con <- sapply(gene_names, function(g) {
            mean(rt_filtered_long[rt_filtered_long$Gene == g & rt_filtered_long$Type == con_label, "Expression"])
          })
          mean_treat <- sapply(gene_names, function(g) {
            mean(rt_filtered_long[rt_filtered_long$Gene == g & rt_filtered_long$Type == treat_label, "Expression"])
          })
          
          # ---- 6. 计算ROC ----
          roc_list <- list()
          auc_values <- c()
          auc_lower <- c()
          auc_upper <- c()
          
          for (gene in gene_names) {
            expr_values <- as.numeric(rt_filtered[gene, ])
            roc_obj <- roc(response = y, predictor = expr_values, quiet = TRUE)
            ci1 <- ci.auc(roc_obj, method = "bootstrap", quiet = TRUE)
            ciVec <- as.numeric(ci1)
            
            roc_list[[gene]] <- roc_obj
            auc_values[gene] <- ciVec[2]
            auc_lower[gene] <- ciVec[1]
            auc_upper[gene] <- ciVec[3]
          }
          
          # ---- 7. 创建结果表 ----
          result_df_val <- data.frame(
            Gene = gene_names,
            P_Value = pvals[gene_names],
            AUC = auc_values[gene_names],
            AUC_Lower_CI = auc_lower[gene_names],
            AUC_Upper_CI = auc_upper[gene_names],
            Mean_Con = mean_con[gene_names],
            Mean_Treat = mean_treat[gene_names],
            stringsAsFactors = FALSE
          )
          
          result_df_val <- result_df_val[order(result_df_val$AUC, decreasing = TRUE), ]
          rownames(result_df_val) <- NULL
          
          incProgress(0.8, detail = "整理结果...")
          
          # ---- 8. 存储结果 ----
          roc_results(list(
            roc_list = roc_list,
            auc_values = auc_values,
            rt_filtered = rt_filtered,
            rt_filtered_long = rt_filtered_long,
            Type = Type,
            con_label = con_label,
            treat_label = treat_label,
            conNum = conNum,
            treatNum = treatNum,
            total_genes = length(gene_names),
            pvals_df = pvals_df,
            gene_names = gene_names
          ))
          
          result_df(result_df_val)
          gene_list(gene_names)
          
          is_running(FALSE)
          incProgress(1.0, detail = "完成！")
          
          showNotification(
            paste0("ROC分析完成！共分析 ", length(gene_names), " 个基因"),
            type = "message", duration = 5
          )
          
        }, error = function(e) {
          showNotification(paste0("错误: ", e$message), type = "error", duration = 10)
          is_running(FALSE)
        })
      })
    })
    
    # ============================================================
    # 批量箱线图（所有基因合并）
    # ============================================================
    plot_empty_roc <- function(message = "请先运行ROC分析") {
      plot.new()
      invisible(NULL)
    }

    roc_plot_label <- function(plot_key) {
      switch(
        plot_key,
        boxplot = "所有基因箱线图",
        combined = "合并ROC曲线",
        auc = "AUC条形图",
        "ROC图片"
      )
    }

    draw_active_roc_plot <- function(plot_key = active_roc_plot(), large = FALSE) {
      if (is.null(plot_key) || !nzchar(plot_key)) {
        plot_key <- "combined"
      }

      res <- roc_results()
      df <- result_df()
      if (is.null(res) || is.null(df) || nrow(df) == 0) {
        plot_empty_roc()
        return(invisible(NULL))
      }

      if (identical(plot_key, "boxplot")) {
        color_con <- input$colorCon
        color_treat <- input$colorTreat

        fill_colors <- c(color_con, color_treat)
        names(fill_colors) <- c(res$con_label, res$treat_label)

        color_colors <- c(color_con, color_treat)
        names(color_colors) <- c(res$con_label, res$treat_label)

        label_offset <- 0.15 * stats::sd(res$rt_filtered_long$Expression, na.rm = TRUE)
        if (!is.finite(label_offset) || label_offset <= 0) {
          label_offset <- 0.1
        }

        p <- ggplot(res$rt_filtered_long, aes(x = Gene, y = Expression)) +
          geom_boxplot(
            aes(fill = Type),
            position = position_dodge(width = 0.8),
            outlier.colour = "red",
            outlier.size = 1.5,
            width = 0.6,
            alpha = 0.7
          ) +
          geom_jitter(
            aes(color = Type),
            position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
            size = if (large) 1.7 else 1.3
          ) +
          scale_fill_manual(values = fill_colors) +
          scale_color_manual(values = color_colors) +
          theme_minimal(base_size = if (large) 12 else 10) +
          labs(x = "", y = "Expression") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = if (large) 9 else 8),
            legend.title = element_blank()
          )

        max_y <- aggregate(Expression ~ Gene, data = res$rt_filtered_long, max)
        pvals_df <- merge(res$pvals_df, max_y, by = "Gene")
        pvals_df$label_y <- pvals_df$Expression + label_offset

        p <- p + geom_text(
          data = pvals_df,
          mapping = aes(
            x = Gene,
            y = label_y,
            label = paste0("p = ", format(P_Value, scientific = TRUE, digits = 3))
          ),
          inherit.aes = FALSE,
          color = "black",
          size = if (large) 2.8 else 2.4
        )

        print(p)
        return(invisible(NULL))
      }

      if (identical(plot_key, "auc")) {
        top_n <- min(20, nrow(df))
        plot_df <- df[seq_len(top_n), ]

        p <- ggplot(plot_df, aes(x = reorder(Gene, AUC), y = AUC, fill = AUC)) +
          geom_bar(stat = "identity", width = 0.7) +
          geom_errorbar(
            aes(ymin = AUC_Lower_CI, ymax = AUC_Upper_CI),
            width = 0.2,
            color = "gray40"
          ) +
          coord_flip() +
          scale_fill_gradient(low = "#FFB266", high = "#E41A1C") +
          labs(title = "AUC Ranking", x = "", y = "AUC") +
          theme_minimal(base_size = if (large) 12 else 10) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = if (large) 13 else 11),
            axis.text.y = element_text(size = if (large) 9 else 8)
          )

        print(p)
        return(invisible(NULL))
      }

      roc_list <- res$roc_list
      auc_values <- res$auc_values
      gene_names <- res$gene_names
      if (length(gene_names) == 0) {
        plot_empty_roc("没有可绘制的基因")
        return(invisible(NULL))
      }

      colors <- grDevices::rainbow(length(gene_names))
      for (i in seq_along(gene_names)) {
        gene <- gene_names[i]
        roc_obj <- roc_list[[gene]]

        if (i == 1) {
          plot(
            roc_obj,
            col = colors[i],
            lwd = if (large) 2.2 else 1.8,
            main = "ROC Curves for All Genes",
            legacy.axes = TRUE
          )
        } else {
          lines(roc_obj, col = colors[i], lwd = if (large) 2.2 else 1.8)
        }
      }

      abline(a = 0, b = 1, lty = 2, col = "gray60", lwd = 1.5)
      legend_text <- paste0(gene_names, " (AUC=", sprintf("%.3f", auc_values[gene_names]), ")")
      legend(
        "bottomright",
        legend = legend_text,
        col = colors[seq_along(gene_names)],
        lwd = 2,
        cex = if (large) 0.75 else 0.58,
        bty = "n"
      )
    }

    output$activeRocPlot <- renderPlot({
      draw_active_roc_plot(active_roc_plot())
    })

    output$activeRocPlotLarge <- renderPlot({
      draw_active_roc_plot(active_roc_plot(), large = TRUE)
    })

    observeEvent(input$activeRocPlot_click, {
      if (is.null(roc_results())) {
        return()
      }

      showModal(
        modalDialog(
          title = roc_plot_label(active_roc_plot()),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          plotOutput(ns("activeRocPlotLarge"), height = "70vh", width = "100%")
        )
      )
    })

    make_roc_file_row <- function(index, name, type, desc, download_id, plot_key = NULL) {
      tags$div(
        class = "roc-result-file-row",
        tags$span(sprintf("%02d", index), class = "roc-file-index"),
        if (!is.null(plot_key)) {
          actionButton(
            ns(paste0("showRocPlot_", plot_key)),
            name,
            class = "roc-result-file-action",
            title = "点击预览图片"
          )
        } else {
          tags$span(name, class = "roc-result-file-name")
        },
        tags$span(type, class = "roc-result-file-type"),
        tags$span(desc, class = "roc-result-file-desc"),
        tags$span(
          class = "roc-result-file-download",
          downloadButton(ns(download_id), "下载", class = "btn-xs")
        )
      )
    }

    output$resultFileList <- renderUI({
      if (is.null(roc_results()) || is.null(result_df())) {
        return(NULL)
      }

      div(
        class = "roc-result-file-list",
        make_roc_file_row(1, "boxplot_all_genes.png", "PNG", "所有基因箱线图，点击文件名可在上方预览", "downloadBoxplotAllPNG", "boxplot"),
        make_roc_file_row(2, "boxplot_all_genes.pdf", "PDF", "所有基因箱线图PDF", "downloadBoxplotAll"),
        make_roc_file_row(3, "ROC_curves_all_genes_combined.png", "PNG", "合并ROC曲线，点击文件名可在上方预览", "downloadCombinedRocPNG", "combined"),
        make_roc_file_row(4, "ROC_curves_all_genes_combined.pdf", "PDF", "合并ROC曲线PDF", "downloadCombinedRocPDF"),
        make_roc_file_row(5, "AUC_barplot_top.png", "PNG", "AUC排序条形图，点击文件名可在上方预览", "downloadAucBarplot", "auc"),
        make_roc_file_row(6, "ROC_results.csv", "CSV", "每个基因的P值、AUC和置信区间", "downloadResult")
      )
    })

    output$rocStatus <- renderUI({
      res <- roc_results()
      df <- result_df()
      if (is.null(res) || is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      best_gene <- as.character(df$Gene[1])
      best_auc <- sprintf("%.3f", df$AUC[1])

      div(
        class = "roc-status-grid",
        div(class = "roc-status-item", tags$b("基因数"), tags$span(res$total_genes)),
        div(class = "roc-status-item", tags$b("样本数"), tags$span(res$conNum + res$treatNum)),
        div(class = "roc-status-item", tags$b("分组"), tags$span(paste0(res$con_label, "=", res$conNum, "；", res$treat_label, "=", res$treatNum))),
        div(class = "roc-status-item", tags$b("最高AUC"), tags$span(paste0(best_gene, "：", best_auc)))
      )
    })

    observeEvent(input$showRocPlot_boxplot, {
      active_roc_plot("boxplot")
    })

    observeEvent(input$showRocPlot_combined, {
      active_roc_plot("combined")
    })

    observeEvent(input$showRocPlot_auc, {
      active_roc_plot("auc")
    })

    output$boxplotAll <- renderPlot({
      res <- roc_results()
      if (is.null(res)) {
        plot_empty_roc()
        return()
      }
      
      color_con <- input$colorCon
      color_treat <- input$colorTreat
      
      fill_colors <- c(color_con, color_treat)
      names(fill_colors) <- c(res$con_label, res$treat_label)
      
      color_colors <- c(color_con, color_treat)
      names(color_colors) <- c(res$con_label, res$treat_label)
      
      p <- ggplot(res$rt_filtered_long, aes(x = Gene, y = Expression)) +
        geom_boxplot(aes(fill = Type), 
                     position = position_dodge(width = 0.8), 
                     outlier.colour = "red", outlier.size = 1.5, width = 0.6, alpha = 0.7) +
        geom_jitter(aes(color = Type), 
                    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
                    size = 1.5) +
        scale_fill_manual(values = fill_colors) +
        scale_color_manual(values = color_colors) +
        theme_minimal(base_size = 12) +
        labs(x = "", y = "Expression") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
              legend.title = element_blank())
      
      # 添加P值标签
      max_y <- aggregate(Expression ~ Gene, data = res$rt_filtered_long, max)
      pvals_df <- merge(res$pvals_df, max_y, by = "Gene")
      pvals_df$label_y <- pvals_df$Expression + 0.15 * sd(res$rt_filtered_long$Expression, na.rm = TRUE)
      
      p <- p + geom_text(data = pvals_df, 
                         mapping = aes(x = Gene, y = label_y, label = paste0("p = ", format(P_Value, scientific = TRUE, digits = 3))),
                         inherit.aes = FALSE,
                         color = "black", size = 2.8)
      
      print(p)
    })
    
    # ============================================================
    # 合并ROC曲线（所有基因叠加）
    # ============================================================
    output$combinedRocPlot <- renderPlot({
      res <- roc_results()
      if (is.null(res)) {
        plot_empty_roc()
        return()
      }
      
      roc_list <- res$roc_list
      auc_values <- res$auc_values
      gene_names <- res$gene_names
      
      colors <- rainbow(length(gene_names))
      
      first <- TRUE
      
      for (i in seq_along(gene_names)) {
        gene <- gene_names[i]
        roc_obj <- roc_list[[gene]]
        
        if (first) {
          plot(roc_obj, col = colors[i], lwd = 2, 
               main = "ROC Curves for All Genes", legacy.axes = TRUE)
          first <- FALSE
        } else {
          lines(roc_obj, col = colors[i], lwd = 2)
        }
      }
      
      abline(a = 0, b = 1, lty = 2, col = "gray60", lwd = 1.5)
      
      legend_text <- paste0(gene_names, " (AUC=", sprintf("%.3f", auc_values[gene_names]), ")")
      legend("bottomright", legend = legend_text, 
             col = colors[1:length(gene_names)], lwd = 2, cex = 0.7, bty = "n")
    })
    
    # ============================================================
    # AUC条形图（区域二）
    # ============================================================
    output$aucBarplot <- renderPlot({
      df <- result_df()
      if (is.null(df) || nrow(df) == 0) {
        plot_empty_roc()
        return()
      }
      
      top_n <- min(20, nrow(df))
      plot_df <- df[1:top_n, ]
      
      p <- ggplot(plot_df, aes(x = reorder(Gene, AUC), y = AUC, fill = AUC)) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        geom_errorbar(aes(ymin = AUC_Lower_CI, ymax = AUC_Upper_CI), 
                      width = 0.2, color = "gray40") +
        scale_fill_gradient(low = "#FFB266", high = "#E41A1C") +
        labs(title = "AUC Ranking", x = "", y = "AUC") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
              axis.text.y = element_text(size = 9))
      
      print(p)
    })
    
    # ============================================================
    # AUC条形图（区域三）
    # ============================================================
    output$aucBarplotBottom <- renderPlot({
      df <- result_df()
      if (is.null(df) || nrow(df) == 0) {
        plot_empty_roc()
        return()
      }
      
      top_n <- min(20, nrow(df))
      plot_df <- df[1:top_n, ]
      
      p <- ggplot(plot_df, aes(x = reorder(Gene, AUC), y = AUC, fill = AUC)) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        geom_errorbar(aes(ymin = AUC_Lower_CI, ymax = AUC_Upper_CI), 
                      width = 0.2, color = "gray40") +
        scale_fill_gradient(low = "#FFB266", high = "#E41A1C") +
        labs(title = "AUC Ranking", x = "", y = "AUC") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
              axis.text.y = element_text(size = 9))
      
      print(p)
    })
    
    # ============================================================
    # 结果表格
    # ============================================================
    output$resultTable <- renderDT({
      df <- result_df()
      if (is.null(df)) {
        return(NULL)
      }
      
      datatable(df, options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp'),
                rownames = FALSE) %>%
        formatRound(columns = c("P_Value", "AUC", "AUC_Lower_CI", "AUC_Upper_CI"), digits = 4)
    })
    
    # ---- 更新结果计数 ----
    output$resultCount <- renderUI({
      df <- result_df()
      if (is.null(df)) {
        tags$span("共 0 个基因", style = "font-size: 11px; color: #888;")
      } else {
        tags$span(paste0("共 ", nrow(df), " 个基因"), style = "font-size: 11px; color: #888;")
      }
    })
    
    # ============================================================
    # 下载功能
    # ============================================================
    
    output$downloadBoxplotAll <- downloadHandler(
      filename = "boxplot_all_genes.pdf",
      content = function(file) {
        res <- roc_results()
        if (is.null(res)) {
          pdf(file)
          plot(1, type = "n", main = "请先运行ROC分析")
          dev.off()
          return()
        }
        
        color_con <- input$colorCon
        color_treat <- input$colorTreat
        
        fill_colors <- c(color_con, color_treat)
        names(fill_colors) <- c(res$con_label, res$treat_label)
        
        color_colors <- c(color_con, color_treat)
        names(color_colors) <- c(res$con_label, res$treat_label)
        
        p <- ggplot(res$rt_filtered_long, aes(x = Gene, y = Expression)) +
          geom_boxplot(aes(fill = Type), 
                       position = position_dodge(width = 0.8), 
                       outlier.colour = "red", outlier.size = 1.5, width = 0.6, alpha = 0.7) +
          geom_jitter(aes(color = Type), 
                      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
                      size = 1.5) +
          scale_fill_manual(values = fill_colors) +
          scale_color_manual(values = color_colors) +
          theme_minimal(base_size = 12) +
          labs(x = "", y = "Expression") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
                legend.title = element_blank())
        
        max_y <- aggregate(Expression ~ Gene, data = res$rt_filtered_long, max)
        pvals_df <- merge(res$pvals_df, max_y, by = "Gene")
        pvals_df$label_y <- pvals_df$Expression + 0.15 * sd(res$rt_filtered_long$Expression, na.rm = TRUE)
        
        p <- p + geom_text(data = pvals_df, 
                           mapping = aes(x = Gene, y = label_y, label = paste0("p = ", format(P_Value, scientific = TRUE, digits = 3))),
                           inherit.aes = FALSE,
                           color = "black", size = 2.8)
        
        ggsave(file, p, width = 10, height = 6)
      }
    )
    
    output$downloadBoxplotAllPNG <- downloadHandler(
      filename = "boxplot_all_genes.png",
      content = function(file) {
        res <- roc_results()
        if (is.null(res)) {
          png(file, width = 4000, height = 3000, res = 300)
          plot(1, type = "n", main = "请先运行ROC分析")
          dev.off()
          return()
        }
        
        color_con <- input$colorCon
        color_treat <- input$colorTreat
        
        fill_colors <- c(color_con, color_treat)
        names(fill_colors) <- c(res$con_label, res$treat_label)
        
        color_colors <- c(color_con, color_treat)
        names(color_colors) <- c(res$con_label, res$treat_label)
        
        p <- ggplot(res$rt_filtered_long, aes(x = Gene, y = Expression)) +
          geom_boxplot(aes(fill = Type), 
                       position = position_dodge(width = 0.8), 
                       outlier.colour = "red", outlier.size = 1.5, width = 0.6, alpha = 0.7) +
          geom_jitter(aes(color = Type), 
                      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
                      size = 1.5) +
          scale_fill_manual(values = fill_colors) +
          scale_color_manual(values = color_colors) +
          theme_minimal(base_size = 12) +
          labs(x = "", y = "Expression") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
                legend.title = element_blank())
        
        max_y <- aggregate(Expression ~ Gene, data = res$rt_filtered_long, max)
        pvals_df <- merge(res$pvals_df, max_y, by = "Gene")
        pvals_df$label_y <- pvals_df$Expression + 0.15 * sd(res$rt_filtered_long$Expression, na.rm = TRUE)
        
        p <- p + geom_text(data = pvals_df, 
                           mapping = aes(x = Gene, y = label_y, label = paste0("p = ", format(P_Value, scientific = TRUE, digits = 3))),
                           inherit.aes = FALSE,
                           color = "black", size = 2.8)
        
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    output$downloadCombinedRocPDF <- downloadHandler(
      filename = "ROC_curves_all_genes_combined.pdf",
      content = function(file) {
        res <- roc_results()
        if (is.null(res)) {
          pdf(file)
          plot(1, type = "n", main = "请先运行ROC分析")
          dev.off()
          return()
        }
        
        pdf(file, width = 8, height = 8)
        
        roc_list <- res$roc_list
        auc_values <- res$auc_values
        gene_names <- res$gene_names
        
        colors <- rainbow(length(gene_names))
        first <- TRUE
        
        for (i in seq_along(gene_names)) {
          gene <- gene_names[i]
          roc_obj <- roc_list[[gene]]
          
          if (first) {
            plot(roc_obj, col = colors[i], lwd = 2, 
                 main = "ROC Curves for All Genes", legacy.axes = TRUE)
            first <- FALSE
          } else {
            lines(roc_obj, col = colors[i], lwd = 2)
          }
        }
        
        abline(a = 0, b = 1, lty = 2, col = "gray60", lwd = 1.5)
        
        legend_text <- paste0(gene_names, " (AUC=", sprintf("%.3f", auc_values[gene_names]), ")")
        legend("bottomright", legend = legend_text, 
               col = colors[1:length(gene_names)], lwd = 2, cex = 0.8, bty = "n")
        
        dev.off()
      }
    )
    
    output$downloadCombinedRocPNG <- downloadHandler(
      filename = "ROC_curves_all_genes_combined.png",
      content = function(file) {
        res <- roc_results()
        if (is.null(res)) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行ROC分析")
          dev.off()
          return()
        }
        
        png(file, width = 4000, height = 4000, res = 300)
        
        roc_list <- res$roc_list
        auc_values <- res$auc_values
        gene_names <- res$gene_names
        
        colors <- rainbow(length(gene_names))
        first <- TRUE
        
        for (i in seq_along(gene_names)) {
          gene <- gene_names[i]
          roc_obj <- roc_list[[gene]]
          
          if (first) {
            plot(roc_obj, col = colors[i], lwd = 2, 
                 main = "ROC Curves for All Genes", legacy.axes = TRUE)
            first <- FALSE
          } else {
            lines(roc_obj, col = colors[i], lwd = 2)
          }
        }
        
        abline(a = 0, b = 1, lty = 2, col = "gray60", lwd = 1.5)
        
        legend_text <- paste0(gene_names, " (AUC=", sprintf("%.3f", auc_values[gene_names]), ")")
        legend("bottomright", legend = legend_text, 
               col = colors[1:length(gene_names)], lwd = 2, cex = 0.8, bty = "n")
        
        dev.off()
      }
    )
    
    output$downloadAucBarplot <- downloadHandler(
      filename = "AUC_barplot_top.png",
      content = function(file) {
        df <- result_df()
        if (is.null(df) || nrow(df) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行ROC分析")
          dev.off()
          return()
        }
        
        top_n <- min(20, nrow(df))
        plot_df <- df[1:top_n, ]
        
        p <- ggplot(plot_df, aes(x = reorder(Gene, AUC), y = AUC, fill = AUC)) +
          geom_bar(stat = "identity", width = 0.7) +
          coord_flip() +
          geom_errorbar(aes(ymin = AUC_Lower_CI, ymax = AUC_Upper_CI), 
                        width = 0.2, color = "gray40") +
          scale_fill_gradient(low = "#FFB266", high = "#E41A1C") +
          labs(title = "AUC Ranking (Top 20)", x = "", y = "AUC") +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5))
        
        ggsave(file, p, width = 8, height = 6, dpi = 300)
      }
    )
    
    output$downloadAucBarplotBottom <- downloadHandler(
      filename = "AUC_barplot_bottom.png",
      content = function(file) {
        df <- result_df()
        if (is.null(df) || nrow(df) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行ROC分析")
          dev.off()
          return()
        }
        
        top_n <- min(20, nrow(df))
        plot_df <- df[1:top_n, ]
        
        p <- ggplot(plot_df, aes(x = reorder(Gene, AUC), y = AUC, fill = AUC)) +
          geom_bar(stat = "identity", width = 0.7) +
          coord_flip() +
          geom_errorbar(aes(ymin = AUC_Lower_CI, ymax = AUC_Upper_CI), 
                        width = 0.2, color = "gray40") +
          scale_fill_gradient(low = "#FFB266", high = "#E41A1C") +
          labs(title = "AUC Ranking (Top 20)", x = "", y = "AUC") +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5))
        
        ggsave(file, p, width = 8, height = 6, dpi = 300)
      }
    )
    
    output$downloadResult <- downloadHandler(
      filename = "ROC_results.csv",
      content = function(file) {
        df <- result_df()
        if (is.null(df)) {
          write.csv(data.frame(信息 = "请先运行ROC分析"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
