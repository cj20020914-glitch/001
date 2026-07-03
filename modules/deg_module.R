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
        .deg-result-panel .nav-tabs > li > a {
            border: none;
            border-radius: 0;
            margin-right: 26px;
            padding: 8px 2px 9px 2px;
            color: #37474f;
            background: transparent;
            font-size: 12px;
        }
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
          hr(),
          div(
            class = "deg-upload-toolbar",
            actionButton(ns("exampleBtn"), "示例数据",
                         class = "btn-primary btn-xs",
                         style = "font-size: 10px; padding: 2px 10px;"),
            downloadButton(ns("downloadExampleCounts"), "表达矩阵",
                           class = "btn-xs",
                           style = "font-size: 9px; padding: 2px 8px;"),
            downloadButton(ns("downloadExampleCtrl"), "对照组",
                           class = "btn-xs",
                           style = "font-size: 9px; padding: 2px 8px;"),
            downloadButton(ns("downloadExampleTreat"), "实验组",
                           class = "btn-xs",
                           style = "font-size: 9px; padding: 2px 8px;"),
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
                tags$span("表达矩阵", class = "deg-upload-title"),
                tags$span(id = ns("countFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
              ),
              fileInput(ns("countFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择表达矩阵文件")
            )
          ),
          tags$div(
            class = "deg-upload-row",
            tags$div(
              id = ns("ctrlFileBox"),
              class = "deg-upload-box",
              tags$div(
                class = "deg-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span("对照组列表", class = "deg-upload-title"),
                tags$span(id = ns("ctrlFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
              ),
              fileInput(ns("ctrlFile"), NULL,
                        accept = c(".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择对照组列表")
            )
          ),
          tags$div(
            class = "deg-upload-row",
            tags$div(
              id = ns("treatFileBox"),
              class = "deg-upload-box",
              tags$div(
                class = "deg-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span("实验组列表", class = "deg-upload-title"),
                tags$span(id = ns("treatFileStatus"), "Drop file here or click to upload", class = "deg-upload-status")
              ),
              fileInput(ns("treatFile"), NULL,
                        accept = c(".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择实验组列表")
            )
          ),
          div(
            class = "deg-param-grid",
            numericInput(ns("logFC"), "logFC", value = 1, min = 0, max = 5, step = 0.5),
            selectInput(
              ns("pValueType"),
              "P值类型",
              choices = c("FDR(adj.P.Val)" = "adj.P.Val", "P.Value" = "P.Value"),
              selected = "adj.P.Val"
            ),
            numericInput(ns("adjP"), "阈值", value = 0.05, min = 0.000001, max = 1, step = 0.01)
          ),
          div(
            class = "deg-compact-section",
            span("热图/PCA分组名称", class = "deg-compact-title"),
            div(
              class = "deg-compact-grid",
              div(class = "deg-mini-control", span("对照组"), textInput(ns("controlGroupName"), NULL, value = "Control")),
              div(class = "deg-mini-control", span("实验组"), textInput(ns("treatGroupName"), NULL, value = "Treatment"))
            )
          ),
          div(
            class = "deg-compact-section",
            span("火山图参数", class = "deg-compact-title"),
            div(
              class = "deg-compact-grid",
              div(class = "deg-mini-control", span("图片宽度"), numericInput(ns("volcanoWidth"), NULL, value = 6.0, min = 2, max = 20, step = 0.5)),
              div(class = "deg-mini-control", span("图片高度"), numericInput(ns("volcanoHeight"), NULL, value = 8.0, min = 2, max = 20, step = 0.5))
            )
          ),
          div(
            class = "deg-compact-section",
            span("字体大小", class = "deg-compact-title"),
            div(
              class = "deg-compact-grid",
              div(class = "deg-mini-control", span("点大小"), numericInput(ns("volcanoPointSize"), NULL, value = 2.0, min = 0.1, max = 10, step = 0.1)),
              div(class = "deg-mini-control", span("标注"), numericInput(ns("volcanoLabelSize"), NULL, value = 3.0, min = 0.1, max = 10, step = 0.1)),
              div(class = "deg-mini-control", span("legend字"), numericInput(ns("volcanoLegendTextSize"), NULL, value = 10.0, min = 4, max = 30, step = 0.5)),
              div(class = "deg-mini-control", span("legend标题"), numericInput(ns("volcanoLegendTitleSize"), NULL, value = 6.0, min = 4, max = 30, step = 0.5))
            )
          ),
          div(
            class = "deg-color-panel",
            div(
              style = "display: flex; align-items: center; gap: 4px; flex-wrap: wrap;",
              tags$span("颜色:", style = "font-size: 11px; font-weight: bold; color: #2c3e50;"),
              div(
                style = "display: flex; align-items: center; gap: 2px;",
                tags$span("不显著", style = "font-size: 10px; color: #808080; font-weight: bold;"),
                colourInput(ns("colorNS"), NULL, value = "#808080",
                            showColour = "background", palette = "limited",
                            width = "25px")
              ),
              div(
                style = "display: flex; align-items: center; gap: 2px;",
                tags$span("FC显著", style = "font-size: 10px; color: #00A65A; font-weight: bold;"),
                colourInput(ns("colorFC"), NULL, value = "#00A65A",
                            showColour = "background", palette = "limited",
                            width = "25px")
              ),
              div(
                style = "display: flex; align-items: center; gap: 2px;",
                tags$span("P显著", style = "font-size: 10px; color: #1E88E5; font-weight: bold;"),
                colourInput(ns("colorP"), NULL, value = "#1E88E5",
                            showColour = "background", palette = "limited",
                            width = "25px")
              ),
              div(
                style = "display: flex; align-items: center; gap: 2px;",
                tags$span("FC和P都显著", style = "font-size: 10px; color: #F44336; font-weight: bold;"),
                colourInput(ns("colorBoth"), NULL, value = "#F44336",
                            showColour = "background", palette = "limited",
                            width = "25px")
              )
            )
          ),
          div(
            class = "deg-compact-section",
            span("形状", class = "deg-compact-title"),
            div(
              class = "deg-compact-grid-4",
              div(class = "deg-mini-control", span("不显著"), numericInput(ns("shapeNS"), NULL, value = 16, min = 0, max = 25, step = 1)),
              div(class = "deg-mini-control", span("FC"), numericInput(ns("shapeFC"), NULL, value = 16, min = 0, max = 25, step = 1)),
              div(class = "deg-mini-control", span("P"), numericInput(ns("shapeP"), NULL, value = 16, min = 0, max = 25, step = 1)),
              div(class = "deg-mini-control", span("FC和P都显著"), numericInput(ns("shapeBoth"), NULL, value = 16, min = 0, max = 25, step = 1))
            )
          ),
          div(
            class = "deg-compact-section",
            div(
              class = "deg-compact-grid",
              div(
                class = "deg-radio-inline",
                radioButtons(ns("volcanoShowGene"), "基因名", choices = c("显示" = "yes", "不显示" = "no"), selected = "yes", inline = TRUE)
              ),
              div(
                class = "deg-radio-inline",
                radioButtons(ns("volcanoLabelBox"), "标注box", choices = c("带" = "yes", "不带" = "no"), selected = "yes", inline = TRUE)
              ),
              div(
                class = "deg-radio-inline",
                radioButtons(ns("volcanoLabelLine"), "标注连线", choices = c("带" = "yes", "不带" = "no"), selected = "no", inline = TRUE)
              ),
              div(
                class = "deg-radio-inline",
                style = "grid-column: 1 / -1;",
                radioButtons(ns("volcanoFontFamily"), "字体", choices = c("Times New Roman", "Arial"), selected = "Arial", inline = TRUE)
              )
            )
          ),
          actionButton(ns("runDiff"), "运行差异分析",
                       class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 4px 0; margin-bottom: 3px;")
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
                DTOutput(ns("resultTable"))
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "deg-qa",
                  tags$dl(
                    tags$dt("Q1：差异分析的目的是什么？"),
                    tags$dd("差异分析用于从表达矩阵中筛选两组样本之间表达水平显著变化的基因，常作为富集分析、WGCNA、机器学习、ROC 和免疫浸润分析的上游步骤。"),
                    tags$dt("Q2：需要上传哪些文件？"),
                    tags$dd("需要 3 个文件：表达矩阵、对照组样本列表、实验组样本列表。表达矩阵第一列为 gene symbol，后续列为样本表达值；分组列表每行一个样本名，必须与表达矩阵列名一致。"),
                    tags$dt("Q3：表达矩阵格式有什么要求？"),
                    tags$dd("支持 CSV、TSV 和 TXT。第一列应为基因名，后续列为样本；表达值应为数值型。若样本名不匹配或存在无法识别的数值，系统会提示错误。"),
                    tags$dt("Q4：logFC、P.Value 和 FDR 如何理解？"),
                    tags$dd("logFC 表示表达倍数变化的 log2 值；P.Value 是原始 P 值；FDR 是多重检验校正后的 P 值。P < 0.05 和 FDR < 0.05 不是等价关系，FDR 通常更严格。参数区可选择使用 P.Value 或 FDR，并修改对应阈值。"),
                    tags$dt("Q5：火山图四类点分别代表什么？"),
                    tags$dd("不显著表示 FC 和 FDR 都未达阈值；FC显著表示仅表达变化幅度达阈值；P显著表示仅统计显著；FC和P都显著表示同时满足两类阈值，是重点关注基因。"),
                    tags$dt("Q6：火山图参数怎么调整？"),
                    tags$dd("区域一可以调整图片宽高、点大小、标注大小、legend 字体、四类颜色、四类点形状、标注 box、标注连线和字体。区域二预览、放大图和下载图会共用这些设置。"),
                    tags$dt("Q7：火山图中基因标注如何选择？"),
                    tags$dd("当前默认标注 FC 和所选 P 值类型都显著的基因，并按所选 P 值和 logFC 强度优先展示。若标注过多，可以减小标注字号、关闭标注 box 或提高筛选阈值。"),
                    tags$dt("Q8：热图如何解读？"),
                    tags$dd("热图展示显著差异基因在两组样本中的表达模式。颜色代表标准化后的相对表达量，同组样本应呈现相近模式；若样本聚类混乱，建议检查分组和数据质量。"),
                    tags$dt("Q9：PCA 图如何解读？"),
                    tags$dd("PCA 图显示样本整体表达谱的主要变异方向。若对照组与实验组明显分离，说明两组存在较清晰的全局表达差异；若混合严重，可能差异较弱或数据质量需要检查。"),
                    tags$dt("Q10：结果表包含哪些列？"),
                    tags$dd("结果表包含 Gene、logFC、AveExpr、t、P.Value、adj.P.Val、B 等 limma 输出指标。logFC 表示变化方向和幅度，adj.P.Val 越小代表校正后显著性越强。"),
                    tags$dt("Q11：没有显著差异基因怎么办？"),
                    tags$dd("可以检查样本分组是否正确、样本量是否过少、表达矩阵是否已标准化，或适当放宽 logFC/FDR 阈值。若仍无结果，可能说明该比较下整体差异确实较弱。"),
                    tags$dt("Q12：结果文件如何使用？"),
                    tags$dd("diff_results CSV 可用于查看显著差异基因详情；up/down gene TXT 可进入富集分析、Venn 交集和机器学习筛选；volcano、heatmap、pca PNG 可用于结果汇报和论文图初稿。")
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
    active_deg_plot <- reactiveVal("volcano")
    download_deg_plot <- reactiveVal("volcano")

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
      selected <- input$pValueType %||% "adj.P.Val"
      if (selected %in% c("P.Value", "adj.P.Val")) selected else "adj.P.Val"
    }

    deg_p_value_label <- function() {
      if (identical(deg_p_value_column(), "P.Value")) "P-value" else "FDR"
    }

    deg_p_cutoff <- function() {
      deg_clean_number(input$adjP, 0.05, 0.000001, 1)
    }

    deg_group_labels <- function() {
      control_label <- trimws(input$controlGroupName %||% "")
      treat_label <- trimws(input$treatGroupName %||% "")

      if (!nzchar(control_label)) {
        control_label <- "Control"
      }
      if (!nzchar(treat_label)) {
        treat_label <- "Treatment"
      }
      if (identical(control_label, treat_label)) {
        treat_label <- paste0(treat_label, "_2")
      }

      c(control = control_label, treatment = treat_label)
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

    deg_plot_defaults <- function(plot_key) {
      if (identical(plot_key, "volcano")) {
        return(list(
          width = deg_clean_number(input$volcanoWidth, 6, 2, 20),
          height = deg_clean_number(input$volcanoHeight, 8, 2, 20)
        ))
      }

      if (identical(plot_key, "heatmap")) {
        return(list(width = 10, height = 8))
      }

      list(width = 7, height = 5)
    }

    get_deg_plot_download_size <- function(default_width = 7, default_height = 5) {
      list(
        width = deg_clean_number(input$downloadDegPlotWidth, default_width, 2, 20),
        height = deg_clean_number(input$downloadDegPlotHeight, default_height, 2, 20),
        dpi = as.integer(round(deg_clean_number(input$downloadDegPlotDpi, 300, 72, 600)))
      )
    }

    show_deg_plot_download_modal <- function(plot_key) {
      download_deg_plot(plot_key)
      defaults <- deg_plot_defaults(plot_key)

      showModal(
        modalDialog(
          title = paste0("下载", deg_plot_label(plot_key)),
          p("设置导出图片尺寸后点击下载。单位为英寸，DPI 用于控制分辨率。",
            style = "color: #607d8b; font-size: 12px; margin: 0 0 8px 0;"),
          div(
            class = "deg-download-size-controls",
            numericInput(ns("downloadDegPlotWidth"), "宽(in)", value = defaults$width, min = 2, max = 20, step = 0.5),
            numericInput(ns("downloadDegPlotHeight"), "高(in)", value = defaults$height, min = 2, max = 20, step = 0.5),
            numericInput(ns("downloadDegPlotDpi"), "DPI", value = 300, min = 72, max = 600, step = 50)
          ),
          footer = tagList(
            modalButton("取消"),
            downloadButton(ns("downloadDegModalPNG"), "下载PNG", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    }

    make_volcano_plot <- function(res, point_size = NULL, base_size = 13) {
      point_size <- deg_clean_number(point_size %||% input$volcanoPointSize, 2, 0.1, 10)
      label_size <- deg_clean_number(input$volcanoLabelSize, 3, 0.1, 10)
      legend_text_size <- deg_clean_number(input$volcanoLegendTextSize, 10, 4, 30)
      legend_title_size <- deg_clean_number(input$volcanoLegendTitleSize, 6, 4, 30)
      font_family <- input$volcanoFontFamily %||% "Arial"

      category_levels <- c("不显著", "FC显著", "P显著", "FC和P都显著")
      category_colors <- c(
        "不显著" = input$colorNS %||% "#808080",
        "FC显著" = input$colorFC %||% "#00A65A",
        "P显著" = input$colorP %||% "#1E88E5",
        "FC和P都显著" = input$colorBoth %||% "#F44336"
      )
      category_shapes <- c(
        "不显著" = as.integer(round(deg_clean_number(input$shapeNS, 16, 0, 25))),
        "FC显著" = as.integer(round(deg_clean_number(input$shapeFC, 16, 0, 25))),
        "P显著" = as.integer(round(deg_clean_number(input$shapeP, 16, 0, 25))),
        "FC和P都显著" = as.integer(round(deg_clean_number(input$shapeBoth, 16, 0, 25)))
      )

      df <- res$all
      df$Gene <- rownames(df)
      p_col <- deg_p_value_column()
      p_values <- pmax(df[[p_col]], .Machine$double.xmin)
      df$PForPlot <- p_values
      fc_sig <- abs(df$logFC) > input$logFC
      p_sig <- df[[p_col]] < deg_p_cutoff()
      df$Significance <- factor(
        ifelse(
          fc_sig & p_sig, "FC和P都显著",
          ifelse(fc_sig, "FC显著", ifelse(p_sig, "P显著", "不显著"))
        ),
        levels = category_levels
      )

      p <- ggplot(df, aes(x = logFC, y = -log10(PForPlot), color = Significance, shape = Significance)) +
        geom_point(size = point_size, alpha = 0.75) +
        scale_color_manual(values = category_colors, drop = FALSE, name = "Significant") +
        scale_shape_manual(values = category_shapes, drop = FALSE, name = "Significant") +
        labs(
          title = "Volcano Plot",
          x = "Log2 Fold Change",
          y = paste0("-Log10 ", deg_p_value_label())
        ) +
        theme_minimal(base_size = base_size) +
        theme(
          text = element_text(family = font_family),
          plot.title = element_text(face = "bold", hjust = 0.5),
          legend.text = element_text(size = legend_text_size),
          legend.title = element_text(size = legend_title_size)
        )

      label_df <- df[df$Significance == "FC和P都显著", , drop = FALSE]
      if (identical(input$volcanoShowGene %||% "yes", "yes") && nrow(label_df) > 0) {
        label_df <- label_df[order(label_df[[p_col]], -abs(label_df$logFC)), , drop = FALSE]
        label_df <- utils::head(label_df, 50)
        segment_color <- if (identical(input$volcanoLabelLine, "yes")) "grey50" else NA

        label_layer <- if (identical(input$volcanoLabelBox, "yes")) {
          ggrepel::geom_label_repel(
            data = label_df,
            aes(label = Gene),
            size = label_size,
            family = font_family,
            segment.color = segment_color,
            box.padding = 0.35,
            point.padding = 0.2,
            label.size = 0.15,
            fill = "white",
            max.overlaps = Inf,
            show.legend = FALSE
          )
        } else {
          ggrepel::geom_text_repel(
            data = label_df,
            aes(label = Gene),
            size = label_size,
            family = font_family,
            segment.color = segment_color,
            box.padding = 0.35,
            point.padding = 0.2,
            max.overlaps = Inf,
            show.legend = FALSE
          )
        }

        p <- p + label_layer
      }

      p
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
          point_size = if (large) deg_clean_number(input$volcanoPointSize, 2, 0.1, 10) * 1.5 else NULL,
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
                  "需要以下三个文件：",
                  tags$table(
                    tags$thead(
                      tags$tr(
                        tags$th("文件"),
                        tags$th("格式"),
                        tags$th("说明")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("表达矩阵"), tags$td("CSV/TSV"), tags$td("第一列为基因名，其余列为样本表达值")),
                      tags$tr(tags$td("对照组列表"), tags$td("TXT"), tags$td("每行一个样本名，必须与表达矩阵列名一致")),
                      tags$tr(tags$td("实验组列表"), tags$td("TXT"), tags$td("每行一个样本名，必须与表达矩阵列名一致"))
                    )
                  ),
                  tags$br(),
                  tags$span("点击 ", tags$span(class = "tag tag-blue", "示例数据"), " 按钮可下载测试文件")
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
    
    # ---- 示例数据下载 ----
    output$downloadExampleCounts <- downloadHandler(
      filename = "geneMatrix.txt",
      content = function(file) {
        if (file.exists("data/geneMatrix.txt")) {
          file.copy("data/geneMatrix.txt", file)
        } else {
          set.seed(123)
          genes <- paste0("Gene", 1:50)
          samples <- paste0("Sample", 1:12)
          data <- matrix(rnorm(50 * 12, mean = 10, sd = 3), nrow = 50, ncol = 12)
          colnames(data) <- samples
          rownames(data) <- genes
          data[1:20, 7:12] <- data[1:20, 7:12] + 2
          write.table(data, file, sep = "\t", quote = FALSE, row.names = TRUE, col.names = NA)
        }
      }
    )
    
    output$downloadExampleCtrl <- downloadHandler(
      filename = "control.txt",
      content = function(file) {
        writeLines(paste0("Sample", 1:6), file)
      }
    )
    
    output$downloadExampleTreat <- downloadHandler(
      filename = "treat.txt",
      content = function(file) {
        writeLines(paste0("Sample", 7:12), file)
      }
    )
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$countFile, {
      if (!is.null(input$countFile)) {
        runjs(paste0('$("#', ns("countFileStatus"), '").text("', input$countFile$name, '")'))
      }
    })
    
    observeEvent(input$ctrlFile, {
      if (!is.null(input$ctrlFile)) {
        runjs(paste0('$("#', ns("ctrlFileStatus"), '").text("', input$ctrlFile$name, '")'))
      }
    })
    
    observeEvent(input$treatFile, {
      if (!is.null(input$treatFile)) {
        runjs(paste0('$("#', ns("treatFileStatus"), '").text("', input$treatFile$name, '")'))
      }
    })
    
    # ---- 清除上传文件功能 ----
    observeEvent(input$clearCountFile, {
      shinyjs::reset("countFile")
      runjs(paste0('$("#', ns("countFileStatus"), '").text("Drop file here or click to upload")'))
      analysisResults(NULL)
      showNotification("已清除表达矩阵文件", type = "message")
    })
    
    observeEvent(input$clearCtrlFile, {
      shinyjs::reset("ctrlFile")
      runjs(paste0('$("#', ns("ctrlFileStatus"), '").text("Drop file here or click to upload")'))
      analysisResults(NULL)
      showNotification("已清除对照组列表文件", type = "message")
    })
    
    observeEvent(input$clearTreatFile, {
      shinyjs::reset("treatFile")
      runjs(paste0('$("#', ns("treatFileStatus"), '").text("Drop file here or click to upload")'))
      analysisResults(NULL)
      showNotification("已清除实验组列表文件", type = "message")
    })
    
    observeEvent(input$clearAllFiles, {
      shinyjs::reset("countFile")
      shinyjs::reset("ctrlFile")
      shinyjs::reset("treatFile")
      runjs(paste0('$("#', ns("countFileStatus"), '").text("Drop file here or click to upload")'))
      runjs(paste0('$("#', ns("ctrlFileStatus"), '").text("Drop file here or click to upload")'))
      runjs(paste0('$("#', ns("treatFileStatus"), '").text("Drop file here or click to upload")'))
      analysisResults(NULL)
      showNotification("已清除所有上传文件和分析结果", type = "message")
    })
    
    # ---- 示例数据说明 ----
    observeEvent(input$exampleBtn, {
      showNotification("示例数据已准备好，点击下方按钮下载", type = "message")
    })
    
    # ---- 运行日志 ----
    output$diffLog <- renderText({
      "等待运行..."
    })
    
    # ---- 观察运行按钮 ----
    observeEvent(input$runDiff, {
      
      print("runDiff clicked")
      print(paste("countFile:", is.null(input$countFile)))
      print(paste("ctrlFile:", is.null(input$ctrlFile)))
      print(paste("treatFile:", is.null(input$treatFile)))
      
      if (is.null(input$countFile) || is.null(input$ctrlFile) || is.null(input$treatFile)) {
        showNotification("请上传所有必需的文件！", type = "error")
        return()
      }

      count_file <- input$countFile
      ctrl_file <- input$ctrlFile
      treat_file <- input$treatFile
      log_fc_cutoff <- input$logFC
      p_value_cutoff <- deg_p_cutoff()
      p_value_column <- deg_p_value_column()
      task_note <- app_start_task_notification("差异分析正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          expr_matrix <- read_expression_matrix(count_file)

          if (any(is.na(expr_matrix))) {
            stop("表达矩阵中含有无法识别的数值，请检查数据", call. = FALSE)
          }

          expr_matrix <- suppressWarnings(limma::normalizeBetweenArrays(expr_matrix, method = "quantile"))

          ctrl_samples <- read_sample_list(ctrl_file)
          treat_samples <- read_sample_list(treat_file)
          validate_expression_inputs(expr_matrix, ctrl_samples, treat_samples)

          missing_ctrl <- ctrl_samples[!ctrl_samples %in% colnames(expr_matrix)]
          missing_treat <- treat_samples[!treat_samples %in% colnames(expr_matrix)]

          if (length(missing_ctrl) > 0 || length(missing_treat) > 0) {
            msg <- ""
            if (length(missing_ctrl) > 0) {
              msg <- paste0(msg, "对照组缺失: ", paste(missing_ctrl, collapse = ", "))
            }
            if (length(missing_treat) > 0) {
              msg <- paste0(msg, " | 实验组缺失: ", paste(missing_treat, collapse = ", "))
            }
            stop(paste0("样本名不匹配: ", msg), call. = FALSE)
          }

          data_ctrl <- expr_matrix[, ctrl_samples, drop = FALSE]
          data_treat <- expr_matrix[, treat_samples, drop = FALSE]
          combined_expr <- cbind(data_ctrl, data_treat)

          num_ctrl <- ncol(data_ctrl)
          num_treat <- ncol(data_treat)

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
            p_value_cutoff = p_value_cutoff
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
      
      showNotification("正在分析，请稍候...", type = "message", duration = 10)
      
      tryCatch({
        expr_matrix <- read_expression_matrix(input$countFile)
        
        if (any(is.na(expr_matrix))) {
          showNotification("表达矩阵中含有无法识别的数值，请检查数据", type = "error")
          return()
        }
        
        expr_matrix <- suppressWarnings(normalizeBetweenArrays(expr_matrix, method = "quantile"))
        
        ctrl_samples <- read_sample_list(input$ctrlFile)
        treat_samples <- read_sample_list(input$treatFile)
        validate_expression_inputs(expr_matrix, ctrl_samples, treat_samples)
        
        print(paste("Control samples:", length(ctrl_samples)))
        print(paste("Treatment samples:", length(treat_samples)))
        
        missing_ctrl <- ctrl_samples[!ctrl_samples %in% colnames(expr_matrix)]
        missing_treat <- treat_samples[!treat_samples %in% colnames(expr_matrix)]
        
        if (length(missing_ctrl) > 0 || length(missing_treat) > 0) {
          msg <- ""
          if (length(missing_ctrl) > 0) msg <- paste0(msg, "对照组缺失: ", paste(missing_ctrl, collapse=", "))
          if (length(missing_treat) > 0) msg <- paste0(msg, " | 实验组缺失: ", paste(missing_treat, collapse=", "))
          showNotification(paste0("样本名不匹配: ", msg), type = "error")
          return()
        }
        
        data_ctrl <- expr_matrix[, ctrl_samples, drop = FALSE]
        data_treat <- expr_matrix[, treat_samples, drop = FALSE]
        combined_expr <- cbind(data_ctrl, data_treat)
        
        num_ctrl <- ncol(data_ctrl)
        num_treat <- ncol(data_treat)
        
        group_labels <- c(rep("Control", num_ctrl), rep("Treatment", num_treat))
        design_mat <- model.matrix(~0 + factor(group_labels))
        colnames(design_mat) <- c("Control", "Treatment")
        
        fit_initial <- lmFit(combined_expr, design_mat)
        contrast_mat <- makeContrasts(Treatment - Control, levels = design_mat)
        fit_contrasted <- contrasts.fit(fit_initial, contrast_mat)
        fit_contrasted <- eBayes(fit_contrasted)
        
        all_results <- topTable(fit_contrasted, adjust.method = "fdr", number = Inf)
        
        sig_genes <- all_results[abs(all_results$logFC) > input$logFC & 
                                   all_results$adj.P.Val < input$adjP, ]
        sig_genes <- sig_genes[order(sig_genes$logFC), ]
        
        analysisResults(list(
          all = all_results,
          sig = sig_genes,
          combined = combined_expr,
          n_ctrl = num_ctrl,
          n_treat = num_treat
        ))
        active_deg_plot("volcano")
        
        showNotification(paste0("分析完成！显著差异基因: ", nrow(sig_genes)), 
                         type = "message", duration = 5)
        
      }, error = function(e) {
        print(paste("Error:", e$message))
        showNotification(paste0("错误: ", e$message), type = "error", duration = 10)
      })
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
    
    # ---- 结果文件列表 ----
    output$resultFileList <- renderUI({
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
        list(file = "volcano.png", type = "PNG", desc = "火山图，点击文件名可在上方预览", download = "", plot = "volcano"),
        list(file = "heatmap.png", type = "PNG", desc = "差异基因热图，点击文件名可在上方预览", download = "", plot = "heatmap"),
        list(file = "pca.png", type = "PNG", desc = "PCA 图，点击文件名可在上方预览", download = "", plot = "pca")
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
            actionButton(
              ns(paste0("openDegPlotDownload_", row$plot)),
              "下载",
              class = "btn btn-default btn-xs",
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
