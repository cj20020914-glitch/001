# nomogram_module.R - 列线图及决策曲线模块（修复版 v2）
# 修复：datadist 设置问题 - 使用 dd 对象

# ============================================================
# UI
# ============================================================
nomogram_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .nomogram-container {
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
        .nomogram-container:hover {
            background-color: #f0f0f0;
        }
        .param-box {
            border: 2px solid #e74c3c;
            border-radius: 0px;
            padding: 8px;
            height: 370px;
            overflow: hidden;
            background-color: #fdf2f2;
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
        .info-box {
            background-color: #fdf2f2;
            padding: 6px 10px;
            border-radius: 0px;
            font-size: 11px;
            color: #c0392b;
            border-left: 3px solid #e74c3c;
            margin-bottom: 4px;
        }
        .info-box .icon {
            font-weight: bold;
            margin-right: 4px;
        }
    ")),
    
    # ---- 第一行：区域一 + 区域二（各占50%） ----
    fluidRow(
      style = "margin: 0;",
      
      # ============================================================
      # 区域一：方法说明（左上）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          style = "border: 2px solid #e74c3c; border-radius: 0px; padding: 8px; height: 370px; overflow-y: auto; background-color: #fdf2f2;",
          
          h4("方法说明", style = "color: #2c3e50; margin-top: 0; margin-bottom: 0; font-size: 13px;"),
          p("基于筛选基因构建列线图预测模型", style = "color: #666; font-size: 10px; margin-top: 1px; margin-bottom: 2px;"),
          hr(style = "margin: 3px 0;"),
          
          div(class = "info-box",
              tags$span(class = "icon", "📌"),
              "构建列线图预测疾病风险"
          ),
          
          h5("输入文件", style = "color: #2c3e50; font-weight: bold; font-size: 12px; margin-top: 4px; margin-bottom: 2px;"),
          tags$ul(style = "font-size: 11px; padding-left: 18px; margin-top: 2px; margin-bottom: 4px;",
                  tags$li(tags$strong("表达矩阵"), "：基因表达数据（CSV格式）"),
                  tags$li(tags$strong("基因列表"), "：筛选后的特征基因（CSV格式）")
          ),
          
          h5("输出结果", style = "color: #2c3e50; font-weight: bold; font-size: 12px; margin-top: 4px; margin-bottom: 2px;"),
          tags$ul(style = "font-size: 11px; padding-left: 18px; margin-top: 2px; margin-bottom: 4px;",
                  tags$li("📊 列线图 (Nomogram)"),
                  tags$li("📊 校准曲线 (Calibration Curve)"),
                  tags$li("📊 决策曲线 (DCA)"),
                  tags$li("📊 主模型ROC曲线"),
                  tags$li("📋 模型系数表 + 预测概率")
          ),
          
          h5("核心功能", style = "color: #2c3e50; font-weight: bold; font-size: 12px; margin-top: 4px; margin-bottom: 2px;"),
          tags$ul(style = "font-size: 11px; padding-left: 18px; margin-top: 2px; margin-bottom: 2px;",
                  tags$li("逻辑回归建模"),
                  tags$li("列线图可视化"),
                  tags$li("校准曲线(Bootstrap)"),
                  tags$li("决策曲线分析(DCA)"),
                  tags$li("模型ROC评估")
          )
        )
      ),
      
      # ============================================================
      # 区域二：参数设置与运行（右上）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "param-box",
          
          h4("参数设置与运行"),
          p("上传文件并运行列线图分析", class = "help-text"),
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
            actionButton(ns("clearExprFile"), "✕", 
                         class = "btn-danger btn-xs",
                         style = "font-size: 8px; width: 18px; height: 18px; padding: 0; line-height: 1; border-radius: 0px;")
          ),
          
          div(
            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 6px;",
            div(
              class = "file-input-box",
              id = ns("geneFileBox"),
              onclick = sprintf("document.getElementById('%s').click();", ns("geneFile")),
              tags$span("基因列表 (CSV)"),
              tags$span(id = ns("geneFileStatus"), "点击选择", class = "file-status"),
              tags$div(
                style = "display: none;",
                fileInput(ns("geneFile"), NULL,
                          accept = c(".csv"),
                          buttonLabel = "",
                          placeholder = NULL)
              )
            ),
            actionButton(ns("clearGeneFile"), "✕", 
                         class = "btn-danger btn-xs",
                         style = "font-size: 8px; width: 18px; height: 18px; padding: 0; line-height: 1; border-radius: 0px;")
          ),
          
          hr(style = "margin: 3px 0;"),
          
          # ---- 参数 ----
          div(
            class = "param-row",
            tags$label("Bootstrap:"),
            numericInput(ns("bootN"), NULL, value = 800, min = 100, max = 2000, step = 100,
                         width = "70px"),
            tags$span("校准曲线重采样次数", style = "font-size: 9px; color: #888;")
          ),
          
          div(
            class = "param-row",
            tags$label("DCA阈值:"),
            numericInput(ns("thresholdStep"), NULL, value = 0.01, min = 0.005, max = 0.05, step = 0.005,
                         width = "70px"),
            tags$span("决策曲线步长", style = "font-size: 9px; color: #888;")
          ),
          
          hr(style = "margin: 3px 0;"),
          
          # ---- 运行按钮 ----
          actionButton(ns("runNomogram"), "运行列线图分析", 
                       class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 4px 0; margin-bottom: 3px;"),
          
          # ---- 运行日志 ----
          tags$div(
            style = "border: 1px solid #ddd; border-radius: 0px; padding: 2px 6px; background-color: #f8f8f8; max-height: 40px; overflow-y: auto; font-size: 10px;",
            verbatimTextOutput(ns("nomogramLog"))
          )
        )
      )
    ),
    
    # ---- 第二行：区域三 + 区域四（各占50%） ----
    fluidRow(
      style = "margin: 0;",
      
      # ============================================================
      # 区域三：图片显示（左下）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          style = "border: 2px solid #f39c12; border-radius: 0px; padding: 8px; height: 280px; overflow-y: auto; background-color: #fefcf6;",
          
          h4("图片预览", style = "color: #2c3e50; margin-top: 0; margin-bottom: 0; font-size: 14px;"),
          hr(style = "margin: 4px 0;"),
          
          # ---- 图片标签页 ----
          tabsetPanel(
            id = ns("imageTabs"),
            tabPanel(
              "列线图",
              br(),
              div(
                class = "nomogram-container",
                style = "height: 160px;",
                plotOutput(ns("nomogramPlot"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadNomogram"), "下载 PDF", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            ),
            tabPanel(
              "校准曲线",
              br(),
              div(
                class = "nomogram-container",
                style = "height: 160px;",
                plotOutput(ns("calibrationPlot"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadCalibration"), "下载 PDF", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            ),
            tabPanel(
              "决策曲线",
              br(),
              div(
                class = "nomogram-container",
                style = "height: 160px;",
                plotOutput(ns("dcaPlot"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadDCA"), "下载 PDF", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            ),
            tabPanel(
              "模型ROC",
              br(),
              div(
                class = "nomogram-container",
                style = "height: 160px;",
                plotOutput(ns("rocPlot"), height = "100%", width = "100%")
              ),
              br(),
              div(
                style = "display: flex; gap: 4px; flex-wrap: wrap;",
                downloadButton(ns("downloadROC"), "下载 PDF", 
                               style = "font-size: 9px; padding: 1px 8px;")
              )
            )
          )
        )
      ),
      
      # ============================================================
      # 区域四：结果文件（右下）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          style = "border: 2px solid #9b59b6; border-radius: 0px; padding: 8px; height: 280px; background-color: #faf5fc;",
          
          h4("结果文件", style = "color: #2c3e50; margin-top: 0; margin-bottom: 0; font-size: 14px;"),
          p("分析完成后点击下载", style = "color: #666; font-size: 11px; margin-top: 2px; margin-bottom: 4px;"),
          hr(style = "margin: 4px 0;"),
          
          # ---- 数据状态 ----
          div(
            style = "padding: 4px 0; margin-bottom: 4px;",
            uiOutput(ns("nomogramStatus"))
          ),
          
          # ---- 文件列表 ----
          div(
            style = "padding: 4px 0;",
            uiOutput(ns("resultFileList"))
          ),
          
          br(),
          div(
            style = "display: flex; gap: 4px; flex-wrap: wrap;",
            downloadButton(ns("downloadCoef"), "模型系数 CSV", 
                           style = "font-size: 9px; padding: 1px 8px;"),
            downloadButton(ns("downloadPred"), "预测概率 CSV", 
                           style = "font-size: 9px; padding: 1px 8px;"),
            downloadButton(ns("downloadAUC"), "AUC结果 CSV", 
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
nomogram_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .nomo-card,
      .nomo-plot-card,
      .nomo-result-card {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        padding: 12px 16px;
        background-color: #ffffff;
      }
      .nomo-card,
      .nomo-plot-card {
        height: 370px;
        overflow-y: auto;
      }
      .nomo-card h4,
      .nomo-plot-card h4,
      .nomo-result-card h4 {
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 14px;
        font-weight: 700;
      }
      .nomo-card hr,
      .nomo-plot-card hr {
        margin: 4px 0 8px 0;
      }
      .nomo-upload-toolbar {
        display: flex;
        gap: 4px;
        flex-wrap: wrap;
        align-items: center;
        margin-bottom: 6px;
      }
      .nomo-upload-row {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        align-items: center;
        margin-bottom: 6px;
      }
      .nomo-upload-box {
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
      .nomo-upload-box:hover {
        background-color: #f7fafc;
      }
      .nomo-upload-box .shiny-input-container {
        position: absolute;
        inset: 0;
        width: 100% !important;
        height: 100%;
        margin: 0;
        opacity: 0;
        z-index: 2;
        cursor: pointer;
      }
      .nomo-upload-box .input-group,
      .nomo-upload-box .input-group-btn,
      .nomo-upload-box .btn-file,
      .nomo-upload-box input[type='file'] {
        width: 100%;
        height: 100%;
        cursor: pointer;
      }
      .nomo-upload-placeholder {
        text-align: center;
        pointer-events: none;
        display: grid;
        gap: 2px;
        justify-items: center;
      }
      .nomo-upload-title {
        font-weight: 700;
        font-size: 11px;
        color: #263238;
        max-width: 100%;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .nomo-upload-status {
        color: #1e88e5;
        font-size: 11px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 170px;
      }
      .nomo-compact-section {
        border: 1px solid #d7dee2;
        background: #ffffff;
        padding: 6px 8px;
        margin-bottom: 7px;
      }
      .nomo-compact-title {
        display: block;
        color: #263238;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 5px;
      }
      .nomo-param-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 6px 8px;
      }
      .nomo-param-grid .shiny-input-container {
        width: 100%;
        margin-bottom: 0;
      }
      .nomo-card .form-control {
        font-size: 11px;
        padding: 2px 4px;
        height: 26px;
      }
      .nomo-card .shiny-input-container {
        margin-bottom: 6px;
      }
      .nomo-card label {
        font-size: 11px;
        color: #263238;
        margin-bottom: 3px;
      }
      .nomo-run-btn {
        width: 100%;
        font-size: 12px;
        font-weight: 700;
        padding: 4px 0;
        margin-top: 4px;
      }
      .nomo-active-plot-box {
        border: none;
        background: transparent;
        min-height: 285px;
        cursor: zoom-in;
      }
      .nomo-result-panel {
        max-width: 100%;
        overflow-x: hidden;
      }
      .nomo-result-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .nomo-result-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 26px;
        padding: 8px 2px 9px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .nomo-result-panel .nav-tabs > li.active > a,
      .nomo-result-panel .nav-tabs > li.active > a:hover,
      .nomo-result-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .nomo-result-slot {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        min-height: 120px;
      }
      .nomo-result-file-list {
        border: 1px solid #d7dee2;
        background: #ffffff;
      }
      .nomo-result-file-row {
        display: grid;
        grid-template-columns: 28px minmax(150px, 1fr) 54px minmax(150px, 1.4fr) 70px;
        gap: 8px;
        align-items: center;
        padding: 6px 8px;
        border-bottom: 1px solid #eef2f4;
        font-size: 11px;
      }
      .nomo-result-file-row:last-child {
        border-bottom: none;
      }
      .nomo-file-index {
        color: #1e88e5;
        font-weight: 700;
      }
      .nomo-result-file-action {
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
      .nomo-result-file-name,
      .nomo-result-file-desc {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .nomo-result-file-type {
        color: #455a64;
        font-weight: 700;
      }
      .nomo-result-file-desc {
        color: #607d8b;
      }
      .nomo-result-file-download .btn {
        font-size: 10px;
        padding: 1px 8px;
        line-height: 1.4;
      }
      .nomo-status-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 8px;
        margin-bottom: 8px;
      }
      .nomo-status-item {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        font-size: 12px;
      }
      .nomo-status-item b {
        display: block;
        color: #263238;
        font-size: 12px;
      }
      .nomo-qa {
        font-size: 12px;
        line-height: 1.7;
        color: #455a64;
        max-height: 190px;
        overflow-y: auto;
      }
      .nomo-qa dl { margin: 0; }
      .nomo-qa dt {
        margin-top: 8px;
        color: #263238;
      }
      .nomo-qa dt:first-child { margin-top: 0; }
      .nomo-qa dd {
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
          class = "nomo-card",
          h4("参数设置"),
          hr(),
          tags$div(
            class = "nomo-upload-row",
            tags$div(
              id = ns("exprFileBox"),
              class = "nomo-upload-box",
              tags$div(
                class = "nomo-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(id = ns("exprFileTitle"), "geneexp", class = "nomo-upload-title"),
                tags$span(id = ns("exprFileStatus"), "Drop file here or click to upload", class = "nomo-upload-status")
              ),
              fileInput(ns("exprFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择表达矩阵文件")
            )
          ),
          tags$div(
            class = "nomo-upload-row",
            tags$div(
              id = ns("geneFileBox"),
              class = "nomo-upload-box",
              tags$div(
                class = "nomo-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span("特征基因列表", class = "nomo-upload-title"),
                tags$span(id = ns("geneFileStatus"), "Drop file here or click to upload", class = "nomo-upload-status")
              ),
              fileInput(ns("geneFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择基因列表")
            )
          ),
          div(
            class = "nomo-compact-section",
            span("运行参数", class = "nomo-compact-title"),
            div(
              class = "nomo-param-grid",
              numericInput(ns("bootN"), "Bootstrap", value = 800, min = 100, max = 2000, step = 100),
              numericInput(ns("thresholdStep"), "DCA阈值步长", value = 0.01, min = 0.005, max = 0.05, step = 0.005)
            )
          ),
          actionButton(ns("runNomogram"), "运行列线图分析", class = "btn-success btn-sm nomo-run-btn")
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "nomo-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "nomo-active-plot-box",
            plotOutput(
              ns("activeNomogramPlot"),
              height = "285px",
              width = "100%",
              click = ns("activeNomogramPlot_click")
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
          class = "nomo-result-card",
          h4("结果预览"),
          div(
            class = "nomo-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel(
                "结果表",
                div(class = "nomo-result-slot", uiOutput(ns("resultFileList")))
              ),
              tabPanel(
                "数据预览",
                div(
                  class = "nomo-result-slot",
                  uiOutput(ns("nomogramStatus")),
                  DT::DTOutput(ns("coefPreview")),
                  br(),
                  DT::DTOutput(ns("predPreview"))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "nomo-qa",
                  tags$dl(
                    tags$dt("Q1：列线图模块需要什么输入？"),
                    tags$dd("需要表达矩阵和特征基因列表。表达矩阵第一列为基因名，后续列为样本表达值；样本名末尾下划线后的内容会作为分组标签。"),
                    tags$dt("Q2：基因列表有什么要求？"),
                    tags$dd("第一列为特征基因名，建议 2-10 个基因；基因名需要能在表达矩阵行名中匹配。"),
                    tags$dt("Q3：图片如何查看？"),
                    tags$dd("运行完成后，点击结果表中的列线图、校准曲线、DCA 或 ROC 文件名，图片会显示在右侧图片区域。"),
                    tags$dt("Q4：Bootstrap 参数是什么？"),
                    tags$dd("Bootstrap 用于校准曲线重采样，数值越大越稳定但运行越慢。"),
                    tags$dt("Q5：结果文件怎么使用？"),
                    tags$dd("PDF 可用于报告图片；模型系数、预测概率和 AUC CSV 可用于后续统计、汇总或外部绘图。")
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
nomogram_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储结果 ----
    nomogram_results <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    active_nomogram_plot <- reactiveVal("nomogram")
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$exprFile, {
      if (!is.null(input$exprFile)) {
        label <- input$exprFile$name
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
      runjs(paste0('$("#', ns("exprFileTitle"), '").text("geneexp")'))
      runjs(paste0('$("#', ns("exprFileStatus"), '").text("Drop file here or click to upload")'))
      nomogram_results(NULL)
    })
    
    observeEvent(input$clearGeneFile, {
      shinyjs::reset("geneFile")
      runjs(paste0('$("#', ns("geneFileStatus"), '").text("点击选择")'))
      nomogram_results(NULL)
    })
    
    # ---- 运行日志 ----
    output$nomogramLog <- renderText({
      "等待运行..."
    })
    
    # ============================================================
    # 详细说明弹窗
    # ============================================================
    observeEvent(input$detailBtn, {
      showModal(
        modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("列线图与决策曲线 - 详细说明", style = "font-size: 18px; font-weight: bold;"),
            tags$span("v1.0", style = "font-size: 12px; color: #999;")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "max-height: 70vh; overflow-y: auto; padding-right: 10px; font-size: 14px; line-height: 1.8;",
            tags$hr(style = "margin: 8px 0;"),
            
            tags$h5("1. 功能目的", style = "color: #e74c3c;"),
            tags$p("基于筛选出的特征基因构建", tags$strong("列线图预测模型"), 
                   "，用于评估疾病风险，并通过校准曲线、决策曲线和ROC曲线评估模型性能。"),
            
            tags$h5("2. 输入文件", style = "color: #3498db;"),
            tags$ul(
              tags$li(tags$strong("表达矩阵"), "：CSV格式，第一列为基因名，后续列为样本表达值"),
              tags$li(tags$strong("基因列表"), "：CSV格式，第一列为筛选后的特征基因名")
            ),
            
            tags$h5("3. 输出结果", style = "color: #2ecc71;"),
            tags$ul(
              tags$li(tags$strong("列线图"), "：可视化预测模型，展示各基因对风险的贡献"),
              tags$li(tags$strong("校准曲线"), "：评估模型预测概率与实际概率的一致性"),
              tags$li(tags$strong("决策曲线"), "：评估模型在不同阈值下的临床净收益"),
              tags$li(tags$strong("模型ROC"), "：评估模型的整体区分能力"),
              tags$li(tags$strong("模型系数"), "：各变量的回归系数、OR值及置信区间"),
              tags$li(tags$strong("预测概率"), "：每个样本的预测风险概率")
            ),
            
            tags$h5("4. 注意事项", style = "color: #f39c12;"),
            tags$ul(
              tags$li("样本命名格式：", tags$code("样本名_分组"), "（如 ", tags$code("Sample1_Normal"), "）"),
              tags$li("分组变量：", tags$code("Normal"), "为对照，", tags$code("Disease"), "为疾病"),
              tags$li("基因数建议：2-10个特征基因"),
              tags$li("样本数建议：≥ 30 个样本")
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
    
    # ---- 使用说明 ----
    observeEvent(input$helpBtn, {
      showModal(
        modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("列线图与决策曲线 - 快速说明", style = "font-size: 18px; font-weight: bold;")
          ),
          size = "m",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "font-size: 14px; line-height: 1.8;",
            tags$h5("操作步骤", style = "color: #e74c3c;"),
            tags$ol(
              tags$li("上传表达矩阵（CSV格式）"),
              tags$li("上传基因列表（CSV格式）"),
              tags$li("点击「运行列线图分析」"),
              tags$li("查看列线图、校准曲线、决策曲线、ROC"),
              tags$li("下载结果文件")
            ),
            tags$hr(),
            tags$h5("输出文件说明", style = "color: #2ecc71;"),
            tags$ul(
              tags$li("列线图、校准曲线、决策曲线、ROC → PDF"),
              tags$li("模型系数表 → CSV"),
              tags$li("预测概率 → CSV"),
              tags$li("AUC结果 → CSV")
            ),
            tags$hr(),
            tags$p("💡 点击「详细说明」查看完整文档", style = "color: #999;")
          )
        )
      )
    })
    
    # ---- 核心分析 ----
    observeEvent(input$runNomogram, {
      
      if (is.null(input$exprFile)) {
        showNotification("请上传表达矩阵文件！", type = "error")
        return()
      }
      if (is.null(input$geneFile)) {
        showNotification("请上传基因列表文件！", type = "error")
        return()
      }

      expr_file <- input$exprFile
      gene_file <- input$geneFile
      boot_n <- input$bootN
      threshold_step <- input$thresholdStep
      
      is_running(TRUE)
      nomogram_results(NULL)

      showNotification("列线图分析已在后台启动，可以切换到其它模块继续操作。", type = "message", duration = APP_RUNNING_NOTIFICATION_DURATION)

      run_async_task(
        task = function() {
          old_datadist <- getOption("datadist")
          dd_name <- paste0("nomogram_dd_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%S"))
          on.exit({
            options(datadist = old_datadist)
            if (exists(dd_name, envir = .GlobalEnv, inherits = FALSE)) {
              rm(list = dd_name, envir = .GlobalEnv)
            }
          }, add = TRUE)

          dat_expr <- read_expression_matrix(expr_file)
          feature_genes <- read_gene_list_file(gene_file, header = TRUE)

          if (length(feature_genes) < 2) {
            stop("特征基因数过少（至少需要 2 个）", call. = FALSE)
          }

          rownames(dat_expr) <- gsub("-", "_", rownames(dat_expr))
          found_genes <- feature_genes %in% rownames(dat_expr)

          if (sum(found_genes) == 0) {
            stop("基因列表中无基因在表达矩阵中找到", call. = FALSE)
          }

          dat_expr_filt <- dat_expr[feature_genes[found_genes], , drop = FALSE]
          df_expr <- as.data.frame(t(dat_expr_filt))
          sample_names <- rownames(df_expr)

          groups <- gsub(".*_([A-Za-z0-9]+)$", "\\1", sample_names)
          df_expr$GroupType <- groups

          if (length(unique(groups)) < 2) {
            stop("分组数不足，请检查样本命名", call. = FALSE)
          }

          df_expr$GroupType <- as.numeric(factor(df_expr$GroupType)) - 1

          if (!all(df_expr$GroupType %in% c(0, 1))) {
            stop("分组转换后不是严格的 0/1", call. = FALSE)
          }

          model_vars <- setdiff(colnames(df_expr), "GroupType")
          reg_formula <- stats::as.formula(paste("GroupType ~", paste(model_vars, collapse = " + ")))

          dd <- rms::datadist(df_expr)
          assign(dd_name, dd, envir = .GlobalEnv)
          options(datadist = dd_name)

          lrm_fit <- rms::lrm(reg_formula, data = df_expr, x = TRUE, y = TRUE)
          nomo_obj <- rms::nomogram(
            lrm_fit,
            fun = plogis,
            fun.at = c(0.001, 0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99),
            lp = FALSE,
            funlabel = "Disease Risk"
          )

          calibrate_obj <- rms::calibrate(lrm_fit, method = "boot", B = boot_n)

          set.seed(123)
          dca_obj <- rmda::decision_curve(
            formula = reg_formula,
            data = df_expr,
            thresholds = seq(0, 1, by = threshold_step),
            family = binomial(link = "logit"),
            bootstraps = 100
          )

          pred_probs <- stats::predict(lrm_fit, newdata = df_expr, type = "fitted")
          roc_obj <- pROC::roc(df_expr$GroupType, pred_probs, levels = c(0, 1), direction = "<")
          auc_val <- as.numeric(pROC::auc(roc_obj))

          list(
            df_expr = df_expr,
            lrm_fit = lrm_fit,
            nomo_obj = nomo_obj,
            calibrate_obj = calibrate_obj,
            dca_obj = dca_obj,
            roc_obj = roc_obj,
            auc_val = auc_val,
            pred_probs = pred_probs,
            datadist = dd,
            datadist_name = dd_name,
            model_vars = model_vars,
            n_genes = length(model_vars),
            n_samples = nrow(df_expr)
          )
        },
        on_success = function(result) {
          nomogram_results(result)
          active_nomogram_plot("nomogram")
          showNotification(
            paste0("列线图分析完成！共 ", result$n_genes, " 个基因，",
                   result$n_samples, " 个样本，AUC = ", round(result$auc_val, 3)),
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
      
      withProgress(message = "列线图分析运行中...", value = 0, {
        
        incProgress(0.05, detail = "读取数据...")
        
        tryCatch({
          
          # ---- 1. 读取表达数据 ----
          dat_expr <- read_expression_matrix(input$exprFile)
          
          incProgress(0.15, detail = "读取基因列表...")
          
          # ---- 2. 读取基因列表 ----
          feature_genes <- read_gene_list_file(input$geneFile, header = TRUE)
          
          if (length(feature_genes) < 2) {
            showNotification("特征基因数过少（至少需要2个）", type = "error")
            is_running(FALSE)
            return()
          }
          
          incProgress(0.25, detail = "筛选基因...")
          
          # ---- 3. 筛选基因 ----
          rownames(dat_expr) <- gsub("-", "_", rownames(dat_expr))
          found_genes <- feature_genes %in% rownames(dat_expr)
          
          if (sum(found_genes) == 0) {
            showNotification("基因列表中无基因在表达矩阵中找到", type = "error")
            is_running(FALSE)
            return()
          }
          
          if (any(!found_genes)) {
            showNotification(paste0("以下基因未找到: ", 
                                    paste(feature_genes[!found_genes], collapse=", ")), 
                             type = "warning")
          }
          
          dat_expr_filt <- dat_expr[feature_genes[found_genes], , drop = FALSE]
          
          incProgress(0.35, detail = "构建样本分组...")
          
          # ---- 4. 构建样本数据 ----
          df_expr <- as.data.frame(t(dat_expr_filt))
          sample_names <- rownames(df_expr)
          
          # 提取分组信息
          groups <- gsub(".*_([A-Za-z0-9]+)$", "\\1", sample_names)
          df_expr$GroupType <- groups
          
          if (length(unique(groups)) < 2) {
            showNotification("分组数不足2，请检查样本命名", type = "error")
            is_running(FALSE)
            return()
          }
          
          # 确保分组为0/1
          if (!all(unique(groups) %in% c("Normal", "Disease", "0", "1"))) {
            showNotification("分组标签应为 Normal/Disease 或 0/1", type = "warning")
          }
          
          df_expr$GroupType <- as.numeric(factor(df_expr$GroupType)) - 1
          
          if (!all(df_expr$GroupType %in% c(0, 1))) {
            showNotification("分组转换后不是严格的0/1", type = "error")
            is_running(FALSE)
            return()
          }
          
          incProgress(0.5, detail = "建模准备...")
          
          # ---- 5. 建模环境 ----
          model_vars <- setdiff(colnames(df_expr), "GroupType")
          reg_formula <- as.formula(paste("GroupType ~", paste(model_vars, collapse=" + ")))
          
          # 修复：正确设置 datadist
          dd <- datadist(df_expr)
          options(datadist = "dd")
          
          incProgress(0.6, detail = "拟合模型...")
          
          # ---- 6. 逻辑回归 ----
          lrm_fit <- lrm(reg_formula, data = df_expr, x = TRUE, y = TRUE)
          
          incProgress(0.7, detail = "生成列线图...")
          
          # ---- 7. 列线图 ----
          nomo_obj <- nomogram(
            lrm_fit, 
            fun = plogis,
            fun.at = c(0.001, 0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99),
            lp = FALSE, 
            funlabel = "Disease Risk"
          )
          
          incProgress(0.75, detail = "生成校准曲线...")
          
          # ---- 8. 校准曲线 ----
          calibrate_obj <- calibrate(lrm_fit, method = "boot", B = input$bootN)
          
          incProgress(0.8, detail = "生成决策曲线...")
          
          # ---- 9. 决策曲线 ----
          set.seed(123)
          dca_obj <- decision_curve(
            formula = reg_formula,
            data = df_expr,
            thresholds = seq(0, 1, by = input$thresholdStep),
            family = binomial(link = "logit"),
            bootstraps = 100
          )
          
          incProgress(0.85, detail = "计算ROC...")
          
          # ---- 10. ROC曲线 ----
          pred_probs <- predict(lrm_fit, newdata = df_expr, type = "fitted")
          roc_obj <- roc(df_expr$GroupType, pred_probs, levels = c(0, 1), direction = "<")
          auc_val <- as.numeric(auc(roc_obj))
          
          incProgress(0.95, detail = "保存结果...")
          
          # ---- 11. 存储结果 ----
          nomogram_results(list(
            df_expr = df_expr,
            lrm_fit = lrm_fit,
            nomo_obj = nomo_obj,
            calibrate_obj = calibrate_obj,
            dca_obj = dca_obj,
            roc_obj = roc_obj,
            auc_val = auc_val,
            pred_probs = pred_probs,
            model_vars = model_vars,
            n_genes = length(model_vars),
            n_samples = nrow(df_expr)
          ))
          
          # 清理 datadist 选项
          options(datadist = NULL)
          
          is_running(FALSE)
          incProgress(1.0, detail = "完成！")
          
          showNotification(
            paste0("列线图分析完成！共 ", length(model_vars), " 个基因，", 
                   nrow(df_expr), " 个样本，AUC = ", round(auc_val, 3)),
            type = "message", duration = 5
          )
          
        }, error = function(e) {
          # 清理 datadist 选项
          options(datadist = NULL)
          showNotification(paste0("错误: ", e$message), type = "error", duration = 10)
          is_running(FALSE)
        })
      })
    })
    
    # ---- 列线图 ----
    output$nomogramPlot <- renderPlot({
      res <- nomogram_results()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return()
      }
      
      with_nomogram_datadist(res, {
        plot(res$nomo_obj)
      })
    })
    
    # ---- 校准曲线 ----
    output$calibrationPlot <- renderPlot({
      res <- nomogram_results()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return()
      }
      
      with_nomogram_datadist(res, {
        plot(res$calibrate_obj, xlab = "Predicted", ylab = "Observed", sub = FALSE)
      })
    })
    
    # ---- 决策曲线 ----
    output$dcaPlot <- renderPlot({
      res <- nomogram_results()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return()
      }
      
      plot_decision_curve(
        res$dca_obj,
        xlab = "Threshold Probability",
        col = "orange",
        confidence.intervals = TRUE,
        standardize = TRUE,
        cost.benefit.axis = TRUE
      )
    })
    
    # ---- 主模型ROC ----
    output$rocPlot <- renderPlot({
      res <- nomogram_results()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return()
      }
      
      roc_obj <- res$roc_obj
      auc_val <- res$auc_val
      
      par(mar = c(5, 6, 4, 2) + 0.1, cex = 1.3)
      plot(1 - roc_obj$specificities, roc_obj$sensitivities, 
           type = "l", col = "red", lwd = 4, lty = 1,
           xlab = expression("1 - Specificity"), 
           ylab = "Sensitivity",
           main = "Model ROC Curve", 
           cex.lab = 1.4, cex.axis = 1.15, cex.main = 1.45,
           xlim = c(0, 1), ylim = c(0, 1))
      abline(0, 1, lty = 2, col = "gray70", lwd = 2)
      legend("bottomright", 
             legend = sprintf("AUC = %.3f", auc_val), 
             col = "red", lwd = 4, lty = 1, bty = "n", cex = 1.2)
    })
    
    # ---- 数据状态 ----
    output$nomogramStatus <- renderUI({
      res <- nomogram_results()
      if (is.null(res)) {
        return(NULL)
      }
      
      div(
        class = "nomo-status-grid",
        div(class = "nomo-status-item",
            tags$b("基因数"),
            span(res$n_genes)
        ),
        div(class = "nomo-status-item",
            tags$b("样本数"),
            span(res$n_samples)
        ),
        div(class = "nomo-status-item",
            tags$b("模型AUC"),
            span(round(res$auc_val, 4))
        )
      )
    })
    
    nomogram_plot_label <- function(plot_key) {
      labels <- c(
        nomogram = "列线图",
        calibration = "校准曲线",
        dca = "决策曲线",
        roc = "模型ROC"
      )
      labels[[plot_key]] %||% "图片"
    }

    with_nomogram_datadist <- function(res, expr) {
      if (is.null(res) || is.null(res$datadist)) {
        return(force(expr))
      }
      old_datadist <- getOption("datadist")
      dd_name <- res$datadist_name %||% "nomogram_dd_active"
      assign(dd_name, res$datadist, envir = .GlobalEnv)
      options(datadist = dd_name)
      on.exit({
        options(datadist = old_datadist)
        if (exists(dd_name, envir = .GlobalEnv, inherits = FALSE)) {
          rm(list = dd_name, envir = .GlobalEnv)
        }
      }, add = TRUE)
      force(expr)
    }

    draw_nomogram_plot <- function(plot_key = active_nomogram_plot(), large = FALSE) {
      res <- nomogram_results()
      if (is.null(res)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        return(invisible(NULL))
      }

      plot_key <- plot_key %||% "nomogram"
      with_nomogram_datadist(res, {
        if (identical(plot_key, "nomogram")) {
          plot(res$nomo_obj)
        } else if (identical(plot_key, "calibration")) {
          plot(res$calibrate_obj, xlab = "Predicted", ylab = "Observed", sub = FALSE)
        } else if (identical(plot_key, "dca")) {
          plot_decision_curve(
            res$dca_obj,
            xlab = "Threshold Probability",
            col = "orange",
            confidence.intervals = TRUE,
            standardize = TRUE,
            cost.benefit.axis = TRUE
          )
        } else if (identical(plot_key, "roc")) {
          roc_obj <- res$roc_obj
          auc_val <- res$auc_val
          par(mar = c(5, 6, 4, 2) + 0.1, cex = if (isTRUE(large)) 1.35 else 1.15)
          plot(1 - roc_obj$specificities, roc_obj$sensitivities,
               type = "l", col = "red", lwd = 4, lty = 1,
               xlab = expression("1 - Specificity"),
               ylab = "Sensitivity",
               main = "Model ROC Curve",
               cex.lab = 1.25, cex.axis = 1.05, cex.main = 1.25,
               xlim = c(0, 1), ylim = c(0, 1))
          abline(0, 1, lty = 2, col = "gray70", lwd = 2)
          legend("bottomright",
                 legend = sprintf("AUC = %.3f", auc_val),
                 col = "red", lwd = 4, lty = 1, bty = "n", cex = 1.05)
        } else {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        }
      })
      invisible(NULL)
    }

    output$activeNomogramPlot <- renderPlot({
      draw_nomogram_plot(active_nomogram_plot())
    })

    output$activeNomogramPlotLarge <- renderPlot({
      draw_nomogram_plot(active_nomogram_plot(), large = TRUE)
    })

    observeEvent(input$activeNomogramPlot_click, {
      if (is.null(nomogram_results())) {
        return()
      }
      showModal(
        modalDialog(
          title = paste0(nomogram_plot_label(active_nomogram_plot()), " - 放大预览"),
          plotOutput(ns("activeNomogramPlotLarge"), height = "70vh", width = "100%"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭")
        )
      )
    }, ignoreInit = TRUE)

    # ---- 结果文件列表 ----
    output$resultFileList <- renderUI({
      res <- nomogram_results()
      if (is.null(res)) {
        return(NULL)
      }

      rows <- list(
        list(file = "Nomogram_Plot.pdf", type = "PDF", desc = "列线图，点击文件名可在上方预览", download = "downloadNomogram", plot = "nomogram"),
        list(file = "Calibration_Curve.pdf", type = "PDF", desc = "校准曲线，点击文件名可在上方预览", download = "downloadCalibration", plot = "calibration"),
        list(file = "DCA.pdf", type = "PDF", desc = "决策曲线，点击文件名可在上方预览", download = "downloadDCA", plot = "dca"),
        list(file = "Model_ROC.pdf", type = "PDF", desc = "模型ROC，点击文件名可在上方预览", download = "downloadROC", plot = "roc"),
        list(file = "Model_Coefficients.csv", type = "CSV", desc = "模型系数", download = "downloadCoef", plot = ""),
        list(file = "Predicted_Probabilities.csv", type = "CSV", desc = "预测概率", download = "downloadPred", plot = ""),
        list(file = "Model_AUC.csv", type = "CSV", desc = "AUC结果", download = "downloadAUC", plot = "")
      )

      div(
        class = "nomo-result-file-list",
        lapply(seq_along(rows), function(i) {
          row <- rows[[i]]
          file_cell <- if (nzchar(row$plot)) {
            actionButton(
              ns(paste0("showNomogramPlot_", row$plot)),
              row$file,
              class = "nomo-result-file-action",
              title = "点击后在上方图片区预览"
            )
          } else {
            span(row$file, class = "nomo-result-file-name", title = row$file)
          }

          div(
            class = "nomo-result-file-row",
            span(sprintf("%02d", i), class = "nomo-file-index"),
            file_cell,
            span(row$type, class = "nomo-result-file-type"),
            span(row$desc, class = "nomo-result-file-desc", title = row$desc),
            span(
              class = "nomo-result-file-download",
              downloadButton(
                ns(row$download),
                "下载",
                style = "font-size: 10px; padding: 1px 8px;"
              )
            )
          )
        })
      )
    })

    for (plot_key in c("nomogram", "calibration", "dca", "roc")) {
      local({
        key <- plot_key
        observeEvent(input[[paste0("showNomogramPlot_", key)]], {
          if (is.null(nomogram_results())) {
            return()
          }
          active_nomogram_plot(key)
        }, ignoreInit = TRUE)
      })
    }

    output$coefPreview <- DT::renderDT({
      res <- nomogram_results()
      if (is.null(res)) {
        return(NULL)
      }
      coef_summary <- with_nomogram_datadist(res, summary(res$lrm_fit))
      coef_df <- as.data.frame(coef_summary)
      coef_df$OR <- exp(coef_df$Effect)
      coef_df$OR_low <- exp(coef_df$Effect - 1.96 * coef_df$`S.E.`)
      coef_df$OR_high <- exp(coef_df$Effect + 1.96 * coef_df$`S.E.`)
      DT::datatable(coef_df, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$predPreview <- DT::renderDT({
      res <- nomogram_results()
      if (is.null(res)) {
        return(NULL)
      }
      pred_df <- data.frame(
        Sample = rownames(res$df_expr),
        GroupType = res$df_expr$GroupType,
        Predicted_Prob = res$pred_probs,
        stringsAsFactors = FALSE
      )
      DT::datatable(pred_df, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })
    
    # ============================================================
    # 下载功能
    # ============================================================
    
    # ---- 列线图 ----
    output$downloadNomogram <- downloadHandler(
      filename = "Nomogram_Plot.pdf",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          pdf(file)
          plot(1, type = "n", main = "请先运行列线图分析")
          dev.off()
          return()
        }
        pdf(file, width = 11, height = 6)
        with_nomogram_datadist(res, {
          plot(res$nomo_obj)
        })
        dev.off()
      }
    )
    
    # ---- 校准曲线 ----
    output$downloadCalibration <- downloadHandler(
      filename = "Calibration_Curve.pdf",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          pdf(file)
          plot(1, type = "n", main = "请先运行列线图分析")
          dev.off()
          return()
        }
        pdf(file, width = 5.5, height = 5.5)
        with_nomogram_datadist(res, {
          plot(res$calibrate_obj, xlab = "Predicted", ylab = "Observed", sub = FALSE)
        })
        dev.off()
      }
    )
    
    # ---- 决策曲线 ----
    output$downloadDCA <- downloadHandler(
      filename = "DCA.pdf",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          pdf(file)
          plot(1, type = "n", main = "请先运行列线图分析")
          dev.off()
          return()
        }
        pdf(file, width = 5.5, height = 5.5)
        plot_decision_curve(
          res$dca_obj,
          xlab = "Threshold Probability",
          col = "orange",
          confidence.intervals = TRUE,
          standardize = TRUE,
          cost.benefit.axis = TRUE
        )
        dev.off()
      }
    )
    
    # ---- 模型ROC ----
    output$downloadROC <- downloadHandler(
      filename = "Model_ROC.pdf",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          pdf(file)
          plot(1, type = "n", main = "请先运行列线图分析")
          dev.off()
          return()
        }
        
        roc_obj <- res$roc_obj
        auc_val <- res$auc_val
        
        pdf(file, width = 6, height = 6)
        par(mar = c(5, 6, 4, 2) + 0.1, cex = 1.3)
        plot(1 - roc_obj$specificities, roc_obj$sensitivities, 
             type = "l", col = "red", lwd = 4, lty = 1,
             xlab = expression("1 - Specificity"), 
             ylab = "Sensitivity",
             main = "Model ROC Curve", 
             cex.lab = 1.4, cex.axis = 1.15, cex.main = 1.45,
             xlim = c(0, 1), ylim = c(0, 1))
        abline(0, 1, lty = 2, col = "gray70", lwd = 2)
        legend("bottomright", 
               legend = sprintf("AUC = %.3f", auc_val), 
               col = "red", lwd = 4, lty = 1, bty = "n", cex = 1.2)
        dev.off()
      }
    )
    
    # ---- 模型系数 ----
    output$downloadCoef <- downloadHandler(
      filename = "Model_Coefficients.csv",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          write.csv(data.frame(信息 = "请先运行列线图分析"), file, row.names = FALSE)
          return()
        }
        
        coef_summary <- with_nomogram_datadist(res, summary(res$lrm_fit))
        coef_df <- as.data.frame(coef_summary)
        coef_df$OR <- exp(coef_df$Effect)
        coef_df$OR_low <- exp(coef_df$Effect - 1.96 * coef_df$`S.E.`)
        coef_df$OR_high <- exp(coef_df$Effect + 1.96 * coef_df$`S.E.`)
        write.csv(coef_df, file, row.names = FALSE)
      }
    )
    
    # ---- 预测概率 ----
    output$downloadPred <- downloadHandler(
      filename = "Predicted_Probabilities.csv",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          write.csv(data.frame(信息 = "请先运行列线图分析"), file, row.names = FALSE)
          return()
        }
        
        df_out <- data.frame(
          Sample = rownames(res$df_expr),
          GroupType = res$df_expr$GroupType,
          Predicted_Prob = res$pred_probs
        )
        write.csv(df_out, file, row.names = FALSE)
      }
    )
    
    # ---- AUC结果 ----
    output$downloadAUC <- downloadHandler(
      filename = "Model_AUC.csv",
      content = function(file) {
        res <- nomogram_results()
        if (is.null(res)) {
          write.csv(data.frame(信息 = "请先运行列线图分析"), file, row.names = FALSE)
          return()
        }
        
        auc_df <- data.frame(
          Model = "Combined Model",
          AUC = res$auc_val
        )
        write.csv(auc_df, file, row.names = FALSE)
      }
    )
    
  })
}
