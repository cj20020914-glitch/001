# ml_module.R - 机器学习模块（提取特征值 + 11种算法）

# ============================================================
# UI - 差异分析同款紧凑布局
# ============================================================
ml_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .ml-card,
      .ml-plot-card,
      .ml-result-card {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        padding: 12px 16px;
        background-color: #ffffff;
      }
      .ml-card,
      .ml-plot-card {
        height: 370px;
        overflow-y: auto;
      }
      .ml-result-card {
        height: 300px;
        overflow: hidden;
      }
      .ml-card h4,
      .ml-plot-card h4,
      .ml-result-card h4 {
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 14px;
        font-weight: 700;
      }
      .ml-card .nav-tabs,
      .ml-result-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .ml-card .nav-tabs > li > a,
      .ml-result-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 18px;
        padding: 8px 2px 9px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .ml-card .nav-tabs > li.active > a,
      .ml-card .nav-tabs > li.active > a:hover,
      .ml-card .nav-tabs > li.active > a:focus,
      .ml-result-panel .nav-tabs > li.active > a,
      .ml-result-panel .nav-tabs > li.active > a:hover,
      .ml-result-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .ml-card .form-control {
        font-size: 11px;
        padding: 2px 4px;
        height: 26px;
      }
      .ml-card .shiny-input-container {
        margin-bottom: 6px;
      }
      .ml-card label {
        font-size: 11px;
        color: #263238;
        margin-bottom: 3px;
      }
      .ml-upload-row {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        align-items: center;
        margin-bottom: 5px;
      }
      .ml-upload-box {
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
      .ml-upload-box:hover {
        background-color: #f7fafc;
      }
      .ml-upload-box .shiny-input-container {
        position: absolute;
        inset: 0;
        width: 100% !important;
        height: 100%;
        margin: 0;
        opacity: 0;
        z-index: 2;
        cursor: pointer;
      }
      .ml-upload-box .input-group,
      .ml-upload-box .input-group-btn,
      .ml-upload-box .btn-file,
      .ml-upload-box input[type='file'] {
        width: 100%;
        height: 100%;
        cursor: pointer;
      }
      .ml-upload-placeholder {
        text-align: center;
        pointer-events: none;
        display: grid;
        gap: 2px;
        justify-items: center;
      }
      .ml-upload-title {
        font-weight: 700;
        font-size: 11px;
        color: #263238;
      }
      .ml-upload-status {
        color: #1e88e5;
        font-size: 11px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 170px;
      }
      .ml-compact-section {
        border: 1px solid #d7dee2;
        background: #ffffff;
        padding: 6px 8px;
        margin-bottom: 7px;
      }
      .ml-compact-title {
        display: block;
        color: #263238;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 5px;
      }
      .ml-param-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 6px 8px;
      }
      .ml-param-grid .shiny-input-container {
        width: 100%;
        margin-bottom: 0;
      }
      .ml-method-grid .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 2px 8px;
        margin-top: 2px;
      }
      .ml-method-grid .checkbox {
        margin: 0;
        font-size: 11px;
      }
      .ml-method-grid label {
        font-size: 11px;
        white-space: nowrap;
      }
      .ml-run-btn {
        width: 100%;
        font-size: 12px;
        font-weight: 700;
        padding: 4px 0;
        margin-top: 4px;
      }
      .ml-hint {
        font-size: 11px;
        color: #78909c;
        line-height: 1.5;
        margin: 0 0 6px 0;
      }
      .ml-active-summary {
        border: none;
        background: transparent;
        min-height: 285px;
        font-size: 12px;
        color: #455a64;
        line-height: 1.7;
      }
      .ml-active-summary .metric-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 8px;
        margin-top: 8px;
      }
      .ml-active-summary .metric {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
      }
      .ml-active-summary .metric b {
        display: block;
        color: #263238;
        font-size: 12px;
      }
      .ml-plot-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .ml-plot-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 14px;
        padding: 6px 2px 7px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .ml-plot-panel .nav-tabs > li.active > a,
      .ml-plot-panel .nav-tabs > li.active > a:hover,
      .ml-plot-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .ml-plot-slot {
        min-height: 285px;
        overflow: auto;
      }
      .ml-active-plot-box {
        border: none;
        background: transparent;
        min-height: 285px;
        cursor: zoom-in;
      }
      .ml-active-plot-box img,
      .ml-modal-plot-box img {
        width: 100%;
        height: 100%;
        object-fit: contain;
        background: #ffffff;
      }
      .ml-modal-plot-box {
        height: 70vh;
        width: 100%;
      }
      .ml-result-panel {
        max-width: 100%;
        height: 240px;
        overflow-y: auto;
        overflow-x: hidden;
      }
      .ml-download-row {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        align-items: center;
        margin-bottom: 8px;
      }
      .ml-download-row .btn {
        font-size: 10px;
        padding: 2px 8px;
      }
      .ml-result-slot {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        min-height: 120px;
        max-height: 190px;
        overflow-y: auto;
      }
      .ml-result-file-list {
        border: 1px solid #d7dee2;
        background: #ffffff;
        max-height: 190px;
        overflow-y: auto;
      }
      .ml-result-file-row {
        display: grid;
        grid-template-columns: 34px minmax(160px, 1fr) 58px minmax(220px, 2fr) 80px;
        gap: 8px;
        align-items: center;
        padding: 7px 8px;
        border-bottom: 1px solid #eef2f4;
        font-size: 11px;
      }
      .ml-result-file-row:last-child {
        border-bottom: none;
      }
      .ml-file-index {
        color: #90a4ae;
        font-weight: 700;
      }
      .ml-result-file-action {
        border: none;
        background: transparent;
        color: #1e88e5;
        font-size: 11px;
        font-weight: 700;
        padding: 0;
        text-align: left;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .ml-result-file-name {
        color: #263238;
        font-weight: 700;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .ml-result-file-type {
        color: #607d8b;
        font-weight: 700;
      }
      .ml-result-file-desc {
        color: #607d8b;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .ml-result-file-download .btn {
        font-size: 10px;
        padding: 1px 8px;
      }
      .ml-qa {
        font-size: 12px;
        line-height: 1.7;
        color: #455a64;
        max-height: 190px;
        overflow-y: auto;
      }
      .ml-qa dl { margin: 0; }
      .ml-qa dt {
        margin-top: 8px;
        color: #263238;
      }
      .ml-qa dt:first-child { margin-top: 0; }
      .ml-qa dd {
        margin-left: 0;
        margin-bottom: 4px;
      }
    ")),

    fluidRow(
      style = "margin: 0;",
      column(
        width = 6,
        style = "padding: 4px;",
        div(
          class = "ml-card",
          h4("参数设置"),
          tabsetPanel(
            id = ns("mlTabset"),
            type = "tabs",
            tabPanel(
              "样本类型矫正",
              value = "sample_type",
              div(
                class = "ml-download-row",
                downloadButton(ns("downloadExampleCounts"), "示例表达矩阵", class = "btn-xs"),
                downloadButton(ns("downloadExampleCtrl"), "control.txt", class = "btn-xs"),
                downloadButton(ns("downloadExampleTreat"), "treat.txt", class = "btn-xs")
              ),
              tags$div(
                class = "ml-upload-row",
                tags$div(
                  id = ns("sampleTypeExprFileBox"),
                  class = "ml-upload-box",
                  tags$div(
                    class = "ml-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("表达矩阵", class = "ml-upload-title"),
                    tags$span(id = ns("sampleTypeExprFileStatus"), "Drop file here or click to upload", class = "ml-upload-status")
                  ),
                  fileInput(
                    ns("sampleTypeExprFile"),
                    NULL,
                    accept = c(".csv", ".txt", ".tsv"),
                    buttonLabel = "浏览",
                    placeholder = "选择表达矩阵文件"
                  )
                )
              ),
              tags$div(
                class = "ml-upload-row",
                tags$div(
                  id = ns("sampleTypeControlFileBox"),
                  class = "ml-upload-box",
                  tags$div(
                    class = "ml-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("对照组列表", class = "ml-upload-title"),
                    tags$span(id = ns("sampleTypeControlFileStatus"), "Drop file here or click to upload", class = "ml-upload-status")
                  ),
                  fileInput(
                    ns("sampleTypeControlFile"),
                    NULL,
                    accept = c(".txt", ".csv", ".tsv"),
                    buttonLabel = "浏览",
                    placeholder = "选择对照组列表"
                  )
                )
              ),
              tags$div(
                class = "ml-upload-row",
                tags$div(
                  id = ns("sampleTypeTreatFileBox"),
                  class = "ml-upload-box",
                  tags$div(
                    class = "ml-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("实验组列表", class = "ml-upload-title"),
                    tags$span(id = ns("sampleTypeTreatFileStatus"), "Drop file here or click to upload", class = "ml-upload-status")
                  ),
                  fileInput(
                    ns("sampleTypeTreatFile"),
                    NULL,
                    accept = c(".txt", ".csv", ".tsv"),
                    buttonLabel = "浏览",
                    placeholder = "选择实验组列表"
                  )
                )
              ),
              div(
                class = "ml-compact-section",
                span("处理参数", class = "ml-compact-title"),
                checkboxInput(ns("sampleTypeAutoLog"), "自动判断是否 log2 转换", value = TRUE),
                checkboxInput(ns("sampleTypeNormalize"), "进行 limma 组间标准化", value = TRUE)
              ),
              actionButton(ns("runSampleType"), "生成样本类型矩阵", class = "btn-success btn-sm ml-run-btn")
            ),
            tabPanel(
              "提取特征值",
              value = "提取特征值",
              tags$div(
                class = "ml-upload-row",
                tags$div(
                  id = ns("prepExprFileBox"),
                  class = "ml-upload-box",
                  tags$div(
                    class = "ml-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("表达矩阵 / Sample Type Matrix.csv", class = "ml-upload-title"),
                    tags$span(id = ns("prepExprFileStatus"), "Drop file here or click to upload", class = "ml-upload-status")
                  ),
                  fileInput(
                    ns("prepExprFile"),
                    NULL,
                    accept = c(".csv", ".txt", ".tsv"),
                    buttonLabel = "浏览",
                    placeholder = "选择表达矩阵文件"
                  )
                )
              ),
              tags$div(
                class = "ml-upload-row",
                tags$div(
                  id = ns("prepGeneFileBox"),
                  class = "ml-upload-box",
                  tags$div(
                    class = "ml-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("基因列表", class = "ml-upload-title"),
                    tags$span(id = ns("prepGeneFileStatus"), "Drop file here or click to upload", class = "ml-upload-status")
                  ),
                  fileInput(
                    ns("prepGeneFile"),
                    NULL,
                    accept = c(".txt", ".csv", ".tsv"),
                    buttonLabel = "浏览",
                    placeholder = "选择基因列表"
                  )
                )
              ),
              div(
                class = "ml-compact-section",
                p("用于提取研究方向基因，生成 geneexp.csv 供 11 种算法使用。", class = "ml-hint")
              ),
              actionButton(ns("runPrep"), "提取特征值", class = "btn-success btn-sm ml-run-btn")
            ),
            tabPanel(
              "11种算法",
              value = "11种算法",
              tags$div(
                class = "ml-upload-row",
                tags$div(
                  id = ns("allMlFileBox"),
                  class = "ml-upload-box",
                  tags$div(
                    class = "ml-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                    tags$span("geneexp.csv", class = "ml-upload-title"),
                    tags$span(id = ns("allMlFileStatus"), "Drop file here or click to upload", class = "ml-upload-status")
                  ),
                  fileInput(
                    ns("allMlFile"),
                    NULL,
                    accept = c(".csv"),
                    buttonLabel = "浏览",
                    placeholder = "选择 geneexp.csv"
                  )
                )
              ),
              div(
                class = "ml-compact-section",
                span("模型选择", class = "ml-compact-title"),
                div(
                  class = "ml-method-grid",
                  checkboxGroupInput(
                    ns("allMlMethods"),
                    NULL,
                    choices = c(
                      "RF" = "rf",
                      "SVM" = "svm",
                      "GLM" = "glm",
                      "GBM" = "gbm",
                      "KNN" = "knn",
                      "NNET" = "nnet",
                      "LASSO" = "lasso",
                      "DT" = "dt",
                      "NB" = "nb",
                      "ADA" = "ada",
                      "BAG" = "bag"
                    ),
                    selected = c("rf", "svm", "glm", "gbm", "knn", "nnet", "lasso", "dt", "nb", "ada", "bag"),
                    inline = FALSE
                  )
                )
              ),
              div(
                class = "ml-compact-section",
                span("运行参数", class = "ml-compact-title"),
                div(
                  class = "ml-param-grid",
                  sliderInput(ns("allMlSplitRatio"), "训练集比例", min = 0.5, max = 0.9, value = 0.7, step = 0.05),
                  numericInput(ns("allMlCvFolds"), "交叉验证折数", value = 5, min = 3, max = 10, step = 1),
                  numericInput(ns("allMlTopGenes"), "Top基因数", value = 5, min = 1, max = 50, step = 1),
                  numericInput(ns("allMlRandomSeed"), "随机种子", value = 888, min = 1, max = 999999, step = 1),
                  textInput(ns("allMlReferenceGroup"), "对照组标签", value = "con")
                )
              ),
              actionButton(ns("runAllMl"), "运行11种算法", class = "btn-success btn-sm ml-run-btn")
            )
          )
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        div(
          class = "ml-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "ml-plot-panel",
            conditionalPanel(
              ns = ns,
              condition = "input.mlTabset != '11种算法'",
              div(class = "ml-active-summary", uiOutput(ns("mlActivePanel")))
            ),
            conditionalPanel(
              ns = ns,
              condition = "input.mlTabset == '11种算法'",
              div(
                class = "ml-active-plot-box",
                imageOutput(
                  ns("activeAllMlPlot"),
                  height = "285px",
                  width = "100%",
                  click = ns("activeAllMlPlot_click")
                )
              )
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
        div(
          class = "ml-result-card",
          h4("结果预览"),
          div(
            class = "ml-result-panel",
            tabsetPanel(
              id = ns("mlResultTabs"),
              type = "tabs",
              tabPanel(
                "结果表",
                div(
                  class = "ml-result-slot",
                  conditionalPanel(ns = ns, condition = "input.mlTabset == 'sample_type'", uiOutput(ns("sampleTypeFileList"))),
                  conditionalPanel(ns = ns, condition = "input.mlTabset == '提取特征值'", uiOutput(ns("prepFeatureFileList"))),
                  conditionalPanel(ns = ns, condition = "input.mlTabset == '11种算法'", uiOutput(ns("allMlResultFileList")))
                )
              ),
              tabPanel(
                "数据预览",
                div(
                  class = "ml-result-slot",
                  conditionalPanel(ns = ns, condition = "input.mlTabset == 'sample_type'", uiOutput(ns("sampleTypeUI"))),
                  conditionalPanel(ns = ns, condition = "input.mlTabset == '提取特征值'", DTOutput(ns("prepTable")), br(), DTOutput(ns("prepInfo"))),
                  conditionalPanel(ns = ns, condition = "input.mlTabset == '11种算法'", DTOutput(ns("allMlPerformanceTable")), br(), DTOutput(ns("allMlTopGenesTable")), br(), DTOutput(ns("allMlMessagesTable")))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "ml-qa",
                  tags$dl(
                    tags$dt("Q1：机器学习模块推荐流程是什么？"),
                    tags$dd("先在差异分析模块运行样本类型矫正生成 Sample Type Matrix，再用提取特征值生成 geneexp.csv，最后进入 11种算法。"),
                    tags$dt("Q2：样本名为什么要加 _con 和 _tre？"),
                    tags$dd("后续机器学习脚本会从样本名后缀识别分组。_con 表示对照组，_tre 表示实验组，分组错误会直接影响训练标签。"),
                    tags$dt("Q3：geneexp.csv 的格式是什么？"),
                    tags$dd("第一列为基因名，其余列为带分组后缀的样本表达值。该格式会在 11种算法中转置为样本 x 基因的训练矩阵。"),
                    tags$dt("Q4：11种算法里有模型失败怎么办？"),
                    tags$dd("少数模型可能因为依赖包缺失、样本量过少或参数不适合而失败。系统会记录失败原因，其它可用模型会继续运行。"),
                    tags$dt("Q5：结果文件怎么使用？"),
                    tags$dd("Top基因 CSV 可进入后续 ROC、列线图或交集分析；ROC、重要性和残差图用于比较模型表现；Sample Type Matrix 可作为机器学习前置矩阵保存。")
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
ml_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    all_ml_results <- reactiveVal(NULL)
    all_ml_running <- reactiveVal(FALSE)
    active_all_ml_plot <- reactiveVal(NULL)

    run_all_ml_models <- function(file,
                                  split_ratio = 0.7,
                                  cv_folds = 5,
                                  top_n = 5,
                                  reference_group = "con",
                                  random_seed = 888,
                                  selected_models = NULL) {
      required_packages <- c(
        "caret", "DALEX", "ggplot2", "pROC", "PRROC", "randomForest", "kernlab",
        "gbm", "nnet", "glmnet", "rpart",
        "naivebayes", "ada", "ipred"
      )
      missing_packages <- required_packages[
        !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
      ]
      if (length(missing_packages)) {
        stop(paste("缺少依赖包：", paste(missing_packages, collapse = ", ")), call. = FALSE)
      }

      random_seed <- as.integer(random_seed)
      if (is.na(random_seed)) random_seed <- 888
      set.seed(random_seed)

      output_dir <- file.path(
        "tmp",
        "ml_runs",
        paste0("ml_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample.int(9999, 1)))
      )
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      raw_matrix <- utils::read.csv(
        file$datapath,
        header = TRUE,
        check.names = FALSE,
        row.names = 1
      )
      if (is.null(dim(raw_matrix)) || nrow(raw_matrix) == 0 || ncol(raw_matrix) < 2) {
        stop("geneexp.csv 为空或样本列不足。", call. = FALSE)
      }

      trans_mat <- t(raw_matrix)
      storage.mode(trans_mat) <- "numeric"
      colnames(trans_mat) <- make.names(colnames(trans_mat), unique = TRUE)
      sample_ids <- rownames(trans_mat)
      sample_type_raw <- gsub(".*_(\\w+)$", "\\1", sample_ids)
      sample_type <- sample_type_raw
      if (length(unique(sample_type)) < 2) {
        stop("当前仅支持二分类；请确认样本名最后一个下划线后的内容为分组标签。", call. = FALSE)
      }

      reference_label <- reference_group
      if (!reference_label %in% unique(sample_type)) {
        reference_label <- sort(unique(sample_type))[1]
      }
      positive_label <- setdiff(sort(unique(sample_type)), reference_label)[1]

      main_df <- as.data.frame(trans_mat, check.names = FALSE)
      main_df$Label <- factor(sample_type, levels = c(reference_label, positive_label))

      set.seed(random_seed)
      split_idx <- caret::createDataPartition(main_df$Label, p = split_ratio, list = FALSE)
      training_set <- main_df[split_idx, , drop = FALSE]
      testing_set <- main_df[-split_idx, , drop = FALSE]

      if (length(unique(training_set$Label)) < 2 || length(unique(testing_set$Label)) < 2) {
        stop("训练集或测试集没有同时包含两组样本，请调整训练集比例或检查样本数量。", call. = FALSE)
      }
      colnames(training_set) <- make.names(colnames(training_set), unique = TRUE)
      colnames(testing_set) <- make.names(colnames(testing_set), unique = TRUE)
      training_set$Label <- as.factor(training_set$Label)
      testing_set$Label <- as.factor(testing_set$Label)

      min_class_count <- min(table(training_set$Label))
      if (min_class_count < 2) {
        stop("训练集中某一组样本少于2个，无法进行交叉验证。请调整训练集比例或检查分组。", call. = FALSE)
      }
      cv_number <- max(2, min(as.integer(cv_folds), as.integer(min_class_count)))

      fit_ctrl <- caret::trainControl(
        method = "repeatedcv",
        number = cv_number,
        verboseIter = FALSE,
        classProbs = TRUE,
        savePredictions = TRUE
      )

      model_list <- c(
        rf = "rf",
        svm = "svmRadial",
        glm = "glm",
        gbm = "gbm",
        knn = "knn",
        nnet = "nnet",
        lasso = "glmnet",
        dt = "rpart",
        nb = "naive_bayes",
        ada = "ada",
        bag = "treebag"
      )
      if (is.null(selected_models) || !length(selected_models)) {
        selected_models <- names(model_list)
      }
      selected_models <- intersect(selected_models, names(model_list))
      if (!length(selected_models)) {
        stop("请至少选择一种机器学习方法。", call. = FALSE)
      }
      model_list <- model_list[selected_models]

      models <- list()
      failures <- data.frame(Model = character(), Error = character(), stringsAsFactors = FALSE)
      gbm_grid <- expand.grid(
        n.trees = c(50, 100),
        interaction.depth = c(1, 2),
        shrinkage = 0.05,
        n.minobsinnode = 3
      )

      for (mn in names(model_list)) {
        models[[mn]] <- tryCatch({
          if (mn == "glm") {
            caret::train(
              Label ~ .,
              data = training_set,
              method = model_list[[mn]],
              family = stats::binomial(),
              trControl = fit_ctrl
            )
          } else if (mn == "nnet") {
            caret::train(
              Label ~ .,
              data = training_set,
              method = model_list[[mn]],
              trControl = fit_ctrl,
              trace = FALSE
            )
          } else if (mn == "gbm") {
            caret::train(
              Label ~ .,
              data = training_set,
              method = model_list[[mn]],
              trControl = fit_ctrl,
              tuneGrid = gbm_grid,
              bag.fraction = 0.8,
              verbose = FALSE
            )
          } else {
            caret::train(
              Label ~ .,
              data = training_set,
              method = model_list[[mn]],
              trControl = fit_ctrl
            )
          }
        }, error = function(error) {
          failures <<- rbind(
            failures,
            data.frame(
              Model = toupper(mn),
              Error = conditionMessage(error),
              stringsAsFactors = FALSE
            )
          )
          NULL
        })
      }

      models <- models[!vapply(models, is.null, logical(1))]
      if (!length(models)) {
        stop("11种模型均训练失败，请查看依赖包和输入数据格式。", call. = FALSE)
      }

      label_vec <- ifelse(testing_set$Label == reference_label, 0, 1)
      performance <- data.frame()
      top_gene_tables <- list()
      roc_curves <- list()
      residuals <- list()
      confusion_tables <- list()
      prediction_tables <- list()

      my_pred <- function(model_obj, newdata) {
        probs <- stats::predict(model_obj, newdata = newdata, type = "prob")
        if (ncol(probs) == 1) {
          return(probs[, 1])
        }
        as.numeric(probs[, 2])
      }

      exp_obj <- list()
      for (mn in names(models)) {
        exp_obj[[mn]] <- DALEX::explain(
          models[[mn]],
          label = toupper(mn),
          data = testing_set,
          y = label_vec,
          predict_function = my_pred,
          verbose = FALSE
        )
      }

      res_list <- lapply(exp_obj, DALEX::model_performance)
      varimp_lst <- lapply(exp_obj, DALEX::variable_importance, loss_function = DALEX::loss_root_mean_square)

      write_png <- function(path, width, height, expr) {
        grDevices::png(path, width = width, height = height, units = "in", res = 300)
        on.exit(grDevices::dev.off(), add = TRUE)
        plot_obj <- force(expr)
        if (inherits(plot_obj, c("ggplot", "gg", "patchwork", "gtable", "grob"))) {
          print(plot_obj)
        }
        invisible(plot_obj)
      }

      plot_many <- function(items, ...) {
        do.call(plot, c(unname(items), list(...)))
      }

      write_png(file.path(output_dir, "residual.png"), 7, 7, {
        plot_many(res_list)
      })
      write_png(file.path(output_dir, "boxplot.png"), 7, 7, {
        plot_many(res_list, geom = "boxplot")
      })

      for (mn in names(models)) {
        model <- models[[mn]]
        prob <- tryCatch({
          pred_prob <- stats::predict(model, newdata = testing_set, type = "prob")
          if (positive_label %in% colnames(pred_prob)) {
            as.numeric(pred_prob[, positive_label])
          } else {
            as.numeric(pred_prob[, min(2, ncol(pred_prob))])
          }
        }, error = function(error) NULL)

        pred_class <- tryCatch(stats::predict(model, newdata = testing_set), error = function(error) NULL)
        if (is.null(prob) || is.null(pred_class) || length(unique(prob)) < 2) {
          failures <- rbind(
            failures,
            data.frame(Model = toupper(mn), Error = "预测概率无效，跳过评估。", stringsAsFactors = FALSE)
          )
          next
        }

        roc_obj <- pROC::roc(label_vec, prob, quiet = TRUE)
        cm <- caret::confusionMatrix(pred_class, testing_set$Label, positive = positive_label)

        auc_val <- as.numeric(pROC::auc(roc_obj))
        performance <- rbind(
          performance,
          data.frame(
            Model = toupper(mn),
            Method = model_list[[mn]],
            AUC = round(auc_val, 4),
            Accuracy = round(unname(cm$overall["Accuracy"]), 4),
            Sensitivity = round(unname(cm$byClass["Sensitivity"]), 4),
            Specificity = round(unname(cm$byClass["Specificity"]), 4),
            stringsAsFactors = FALSE
          )
        )

        roc_curves[[mn]] <- roc_obj
        residuals[[mn]] <- data.frame(
          Model = toupper(mn),
          Residual = label_vec - prob,
          AbsResidual = abs(label_vec - prob),
          stringsAsFactors = FALSE
        )
        confusion_tables[[mn]] <- as.data.frame(cm$table)
        prediction_tables[[mn]] <- data.frame(
          Sample = rownames(testing_set),
          TrueLabel = testing_set$Label,
          PredictedLabel = pred_class,
          Probability = prob,
          stringsAsFactors = FALSE
        )
      }

      if (!nrow(performance)) {
        stop("模型已训练但均无法完成有效评估，请检查分组、样本量和预测概率。", call. = FALSE)
      }

      cols <- c("red", "blue", "green", "yellow", "orange", "purple", "black",
                "pink", "brown", "magenta", "darkgreen", "deepskyblue")
      write_png(file.path(output_dir, "ROC.png"), 7, 7, {
        plotted <- 0
        legend_labels <- c()
        valid_col_index <- 1

        for (i in seq_along(names(models))) {
          mn <- names(models)[i]
          pred_probs <- tryCatch(
            stats::predict(models[[mn]], newdata = testing_set, type = "prob"),
            error = function(error) NULL
          )
          if (!is.data.frame(pred_probs) && !is.matrix(pred_probs)) next
          if (length(unique(label_vec)) < 2) next
          if (ncol(pred_probs) < 2 || length(unique(as.numeric(pred_probs[, 2]))) < 2) next

          rc <- tryCatch(
            pROC::roc(label_vec, as.numeric(pred_probs[, 2]), ci = TRUE, quiet = TRUE),
            error = function(error) NULL
          )
          if (is.null(rc) || is.na(rc$auc) || all(is.na(rc$ci))) {
            rc <- tryCatch(
              pROC::roc(
                label_vec,
                as.numeric(pred_probs[, 2]),
                ci = TRUE,
                boot.n = 500,
                boot.stratified = TRUE,
                quiet = TRUE
              ),
              error = function(error) NULL
            )
          }
          if (is.null(rc) || is.na(rc$auc) || all(is.na(rc$ci))) next

          auc_val <- as.numeric(rc$auc)
          auc_ci <- as.numeric(rc$ci)
          legend_labels <- c(
            legend_labels,
            sprintf("%s: AUC=%.3f [%.3f-%.3f]", toupper(mn), auc_val, auc_ci[1], auc_ci[3])
          )

          col_use <- cols[valid_col_index]
          if (plotted == 0) {
            plot(rc, legacy.axes = TRUE, main = "ROC curve with 95% CI",
                 col = col_use, lwd = 2.5, print.auc = FALSE)
            ciobj <- try(pROC::ci.se(rc, specificities = seq(0, 1, length.out = 25)), silent = TRUE)
            if (!inherits(ciobj, "try-error")) {
              plot(ciobj, type = "shape", col = grDevices::adjustcolor(col_use, 0.22), no.roc = TRUE, add = TRUE)
            }
            plotted <- 1
          } else {
            plot(rc, col = col_use, legacy.axes = TRUE, lwd = 2.5, add = TRUE, print.auc = FALSE)
            ciobj <- try(pROC::ci.se(rc, specificities = seq(0, 1, length.out = 25)), silent = TRUE)
            if (!inherits(ciobj, "try-error")) {
              plot(ciobj, type = "shape", col = grDevices::adjustcolor(col_use, 0.22), no.roc = TRUE, add = TRUE)
            }
          }
          valid_col_index <- valid_col_index + 1
        }

        if (plotted > 0) {
          legend(
            "bottomright",
            legend = legend_labels,
            col = cols[seq_len(valid_col_index - 1)],
            lwd = 2.5,
            bty = "n",
            cex = 0.95
          )
        } else {
          plot.new()
          title(main = "No valid ROC to plot!")
        }
      })

      write_png(file.path(output_dir, "importance.png"), 9, 26, {
        plot_many(varimp_lst)
      })

      for (mn in names(varimp_lst)) {
        tab <- varimp_lst[[mn]]
        if (!is.null(tab) && nrow(tab) > 0) {
          if ("variable" %in% colnames(tab)) {
            tab <- tab[!is.na(tab$variable) & tab$variable != "_full_model_", , drop = FALSE]
          }
          if (!nrow(tab)) {
            next
          }
          gene_to_write <- tab[1:min(top_n, nrow(tab)), ]
          gene_to_write$Model <- toupper(mn)
          top_gene_tables[[mn]] <- gene_to_write
          utils::write.csv(
            gene_to_write,
            file = file.path(output_dir, paste0("importanceGene_", toupper(mn), ".csv")),
            row.names = FALSE,
            fileEncoding = "UTF-8"
          )
          gene_col <- if ("variable" %in% colnames(gene_to_write)) {
            "variable"
          } else if ("Gene" %in% colnames(gene_to_write)) {
            "Gene"
          } else {
            colnames(gene_to_write)[[1]]
          }
          gene_names <- unique(as.character(gene_to_write[[gene_col]]))
          gene_names <- gene_names[nzchar(gene_names) & !is.na(gene_names) & gene_names != "_full_model_"]
          utils::write.table(
            data.frame(Gene = gene_names),
            file = file.path(output_dir, paste0(toupper(mn), ".txt")),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8"
          )
        }
      }

      top_genes <- if (length(top_gene_tables)) {
        do.call(rbind, top_gene_tables)
      } else {
        data.frame(Model = character(), Gene = character(), Importance = numeric())
      }
      if (nrow(top_genes) && "Model" %in% colnames(top_genes)) {
        top_genes <- top_genes[, c("Model", setdiff(colnames(top_genes), "Model")), drop = FALSE]
      }
      rownames(top_genes) <- NULL
      utils::write.csv(
        top_genes,
        file = file.path(output_dir, "importanceGenes_summary.csv"),
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )

      for (mn in names(models)) {
        predicted <- stats::predict(models[[mn]], newdata = testing_set)
        cm <- table(Predicted = predicted, Actual = testing_set$Label)
        cm_df <- as.data.frame(cm)
        p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
          geom_tile(color = "white") +
          geom_text(aes(label = Freq), vjust = 1) +
          scale_fill_gradient(low = "white", high = "red") +
          ggtitle(paste("Confusion Matrix:", toupper(mn)))
        ggplot2::ggsave(
          file.path(output_dir, paste0("ConfMat_", toupper(mn), ".png")),
          p,
          width = 4,
          height = 4,
          dpi = 300
        )
      }

      for (mn in names(models)) {
        pred_probs <- stats::predict(models[[mn]], newdata = testing_set, type = "prob")
        if (ncol(pred_probs) == 1) next
        pr <- PRROC::pr.curve(
          scores.class0 = as.numeric(pred_probs[, 2])[label_vec == 1],
          scores.class1 = as.numeric(pred_probs[, 2])[label_vec == 0],
          curve = TRUE
        )
        write_png(file.path(output_dir, paste0("PRcurve_", toupper(mn), ".png")), 6, 6, {
          plot(pr)
        })
      }

      for (mn in names(models)) {
        probs <- stats::predict(models[[mn]], newdata = testing_set, type = "prob")
        df <- data.frame(prob = as.numeric(probs[, 2]), TrueLabel = testing_set$Label)
        p <- ggplot(df, aes(x = prob, fill = TrueLabel)) +
          geom_histogram(binwidth = 0.05, position = "identity", alpha = 0.6) +
          ggtitle(paste("Predicted Probabilities:", toupper(mn)))
        ggplot2::ggsave(
          file.path(output_dir, paste0("PredProbHist_", toupper(mn), ".png")),
          p,
          width = 5,
          height = 4,
          dpi = 300
        )
      }

      performance <- performance[order(performance$AUC, decreasing = TRUE), ]
      rownames(performance) <- NULL

      image_rows <- list(
        data.frame(Key = "residual", File = "residual.png", Type = "PNG", Desc = "DALEX residual 分析图", Path = file.path(output_dir, "residual.png"), stringsAsFactors = FALSE),
        data.frame(Key = "boxplot", File = "boxplot.png", Type = "PNG", Desc = "DALEX residual 箱线图", Path = file.path(output_dir, "boxplot.png"), stringsAsFactors = FALSE),
        data.frame(Key = "ROC", File = "ROC.png", Type = "PNG", Desc = "ROC 曲线和 95% CI", Path = file.path(output_dir, "ROC.png"), stringsAsFactors = FALSE),
        data.frame(Key = "importance", File = "importance.png", Type = "PNG", Desc = "DALEX 变量重要性图", Path = file.path(output_dir, "importance.png"), stringsAsFactors = FALSE)
      )
      for (mn in names(models)) {
        model_label <- toupper(mn)
        image_rows <- c(
          image_rows,
          list(
            data.frame(Key = paste0("ConfMat_", model_label), File = paste0("ConfMat_", model_label, ".png"), Type = "PNG", Desc = paste0(model_label, " 混淆矩阵"), Path = file.path(output_dir, paste0("ConfMat_", model_label, ".png")), stringsAsFactors = FALSE),
            data.frame(Key = paste0("PRcurve_", model_label), File = paste0("PRcurve_", model_label, ".png"), Type = "PNG", Desc = paste0(model_label, " PR 曲线"), Path = file.path(output_dir, paste0("PRcurve_", model_label, ".png")), stringsAsFactors = FALSE),
            data.frame(Key = paste0("PredProbHist_", model_label), File = paste0("PredProbHist_", model_label, ".png"), Type = "PNG", Desc = paste0(model_label, " 预测概率直方图"), Path = file.path(output_dir, paste0("PredProbHist_", model_label, ".png")), stringsAsFactors = FALSE)
          )
        )
      }
      image_files <- do.call(rbind, image_rows)
      image_files <- image_files[file.exists(image_files$Path), , drop = FALSE]
      rownames(image_files) <- NULL

      csv_rows <- list(
        data.frame(Key = "importanceGenes_summary", File = "importanceGenes_summary.csv", Type = "CSV", Desc = "各模型 Top 重要基因汇总", Path = file.path(output_dir, "importanceGenes_summary.csv"), stringsAsFactors = FALSE)
      )
      for (mn in names(models)) {
        model_label <- toupper(mn)
        csv_rows <- c(
          csv_rows,
          list(data.frame(Key = paste0("importanceGene_", model_label), File = paste0("importanceGene_", model_label, ".csv"), Type = "CSV", Desc = paste0(model_label, " Top 重要基因"), Path = file.path(output_dir, paste0("importanceGene_", model_label, ".csv")), stringsAsFactors = FALSE))
        )
        csv_rows <- c(
          csv_rows,
          list(data.frame(Key = paste0("geneList_", model_label), File = paste0(model_label, ".txt"), Type = "TXT", Desc = paste0(model_label, " Top 基因列表"), Path = file.path(output_dir, paste0(model_label, ".txt")), stringsAsFactors = FALSE))
        )
      }
      csv_files <- do.call(rbind, csv_rows)
      csv_files <- csv_files[file.exists(csv_files$Path), , drop = FALSE]
      rownames(csv_files) <- NULL

      list(
        performance = performance,
        top_genes = top_genes,
        roc_curves = roc_curves,
        residuals = if (length(residuals)) do.call(rbind, residuals) else data.frame(),
        confusion_tables = confusion_tables,
        prediction_tables = prediction_tables,
        failures = failures,
        missing_packages = missing_packages,
        image_files = image_files,
        csv_files = csv_files,
        output_dir = normalizePath(output_dir, winslash = "/", mustWork = TRUE),
        n_samples = nrow(main_df),
        n_genes = ncol(main_df) - 1,
        n_train = nrow(training_set),
        n_test = nrow(testing_set),
        reference_label = reference_label,
        positive_label = positive_label,
        random_seed = random_seed
      )
    }
    environment(run_all_ml_models) <- globalenv()

    observeEvent(input$runAllMl, {
      if (is.null(input$allMlFile)) {
        showNotification("请上传 geneexp.csv 文件。", type = "error")
        return()
      }

      all_ml_file <- input$allMlFile
      split_ratio <- input$allMlSplitRatio
      cv_folds <- input$allMlCvFolds
      top_n <- input$allMlTopGenes
      random_seed <- input$allMlRandomSeed
      reference_group <- input$allMlReferenceGroup
      selected_models <- input$allMlMethods
      if (is.null(selected_models) || !length(selected_models)) {
        showNotification("请至少选择一种机器学习方法。", type = "error")
        return()
      }

      all_ml_running(TRUE)
      all_ml_results(NULL)
      active_all_ml_plot(NULL)
      showNotification("11种机器学习算法已在后台启动，可以切换到其它模块继续操作。", type = "message", duration = APP_RUNNING_NOTIFICATION_DURATION)

      run_async_task(
        task = function() {
          run_all_ml_models(
            file = all_ml_file,
            split_ratio = split_ratio,
            cv_folds = cv_folds,
            top_n = top_n,
            reference_group = reference_group,
            random_seed = random_seed,
            selected_models = selected_models
          )
        },
        on_success = function(result) {
          all_ml_results(result)
          if (nrow(result$image_files)) {
            default_plot <- if ("ROC" %in% result$image_files$Key) "ROC" else result$image_files$Key[[1]]
            active_all_ml_plot(default_plot)
          }
          failed_count <- nrow(result$failures)
          showNotification(
            paste0("11种算法运行完成：成功 ", nrow(result$performance),
                   " 个模型",
                   if (failed_count > 0) paste0("，失败 ", failed_count, " 个模型") else "",
                   "，训练集 ", result$n_train,
                   " 个样本，测试集 ", result$n_test, " 个样本。"),
            type = "message",
            duration = 6
          )
        },
        on_error = function(error) {
          showNotification(paste0("错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          all_ml_running(FALSE)
        }
      )
    })

    output$allMlStatusUI <- renderUI({
      if (all_ml_running()) {
        return(div(style = "padding: 12px; color: #607d8b;", "11种算法正在后台运行..."))
      }
      res <- all_ml_results()
      if (is.null(res)) {
        return(div(style = "padding: 12px; color: #607d8b;", "请上传 geneexp.csv 并运行11种算法。"))
      }
      div(
        style = "padding: 12px; border: 1px solid #b0bec5; border-radius: 4px;",
        tags$strong("运行完成"),
        tags$span(
          sprintf(
            " 样本数：%s；基因数：%s；成功模型：%s；对照组：%s；实验组：%s",
            res$n_samples,
            res$n_genes,
            nrow(res$performance),
            res$reference_label,
            res$positive_label
          )
        )
      )
    })

    output$allMlPerformanceTable <- renderDT({
      res <- all_ml_results()
      if (is.null(res) || !nrow(res$performance)) {
        return(datatable(data.frame(信息 = "请先运行11种算法")))
      }
      datatable(res$performance, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
    })

    output$allMlTopGenesTable <- renderDT({
      res <- all_ml_results()
      if (is.null(res) || !nrow(res$top_genes)) {
        return(datatable(data.frame(信息 = "暂无Top基因结果")))
      }
      datatable(res$top_genes, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$allMlMessagesTable <- renderDT({
      res <- all_ml_results()
      if (is.null(res)) {
        return(datatable(data.frame(信息 = "请先运行11种算法")))
      }
      messages <- res$failures
      if (length(res$missing_packages)) {
        messages <- rbind(
          data.frame(
            Model = "依赖包",
            Error = paste("可能缺失：", paste(res$missing_packages, collapse = ", ")),
            stringsAsFactors = FALSE
          ),
          messages
        )
      }
      if (!nrow(messages)) {
        messages <- data.frame(Model = "全部", Error = "可用模型均已完成。", stringsAsFactors = FALSE)
      }
      datatable(messages, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
    })

    ml_download_file_list <- function(empty_text, rows) {
      if (!length(rows)) {
        return(
          div(
            class = "ml-result-file-list",
            div(style = "padding: 10px 8px; color: #78909c; font-size: 11px;", empty_text)
          )
        )
      }
      div(
        class = "ml-result-file-list",
        lapply(seq_along(rows), function(i) {
          row <- rows[[i]]
          div(
            class = "ml-result-file-row",
            span(sprintf("%02d", i), class = "ml-file-index"),
            span(row$file, class = "ml-result-file-name", title = row$file),
            span(row$type, class = "ml-result-file-type"),
            span(row$desc, class = "ml-result-file-desc", title = row$desc),
            span(
              class = "ml-result-file-download",
              downloadButton(
                ns(row$download),
                "下载",
                style = "font-size: 10px; padding: 1px 8px;"
              )
            )
          )
        })
      )
    }

    output$sampleTypeFileList <- renderUI({
      if (is.null(sample_type_result())) {
        return(NULL)
      }
      ml_download_file_list(
        "",
        list(
          list(file = "Sample Type Matrix.csv", type = "CSV", desc = "样本类型矩阵", download = "downloadSampleTypeCSV"),
          list(file = "Sample Type Matrix.txt", type = "TXT", desc = "样本类型矩阵文本格式", download = "downloadSampleTypeTXT"),
          list(file = "Sample_Summary.txt", type = "TXT", desc = "样本类型矫正统计", download = "downloadSampleTypeSummary")
        )
      )
    })

    output$prepFeatureFileList <- renderUI({
      if (is.null(prep_result())) {
        return(NULL)
      }
      ml_download_file_list(
        "",
        list(
          list(file = paste0("geneexp_", Sys.Date(), ".csv"), type = "CSV", desc = "提取后的特征值矩阵", download = "downloadPrepResult")
        )
      )
    })

    get_all_ml_image_path <- function(key = active_all_ml_plot()) {
      res <- all_ml_results()
      if (is.null(res)) {
        return(NULL)
      }
      if (is.null(key) || !key %in% res$image_files$Key) {
        return(NULL)
      }
      row <- res$image_files[res$image_files$Key == key, , drop = FALSE]
      if (!nrow(row) || !file.exists(row$Path[[1]])) {
        return(NULL)
      }
      normalizePath(row$Path[[1]], winslash = "/", mustWork = TRUE)
    }

    render_all_ml_image <- function(key) {
      path <- get_all_ml_image_path(key)
      req(!is.null(path), file.exists(path))

      display_path <- tempfile(fileext = ".png")
      copied <- file.copy(path, display_path, overwrite = TRUE)
      req(isTRUE(copied), file.exists(display_path))

      list(
        src = normalizePath(display_path, winslash = "/", mustWork = TRUE),
        contentType = "image/png",
        alt = basename(path)
      )
    }

    output$activeAllMlPlot <- renderImage({
      render_all_ml_image(active_all_ml_plot())
    }, deleteFile = TRUE)

    output$activeAllMlPlotLarge <- renderImage({
      render_all_ml_image(active_all_ml_plot())
    }, deleteFile = TRUE)

    output$allMlRocPlot <- renderImage({
      render_all_ml_image("ROC")
    }, deleteFile = TRUE)

    output$allMlImportancePlot <- renderImage({
      render_all_ml_image("importance")
    }, deleteFile = TRUE)

    output$allMlResidualPlot <- renderImage({
      render_all_ml_image("residual")
    }, deleteFile = TRUE)

    observeEvent(input$activeAllMlPlot_click, {
      res <- all_ml_results()
      key <- active_all_ml_plot()
      if (is.null(res) || is.null(key) || !key %in% res$image_files$Key) {
        return()
      }
      row <- res$image_files[res$image_files$Key == key, , drop = FALSE]
      showModal(
        modalDialog(
          title = paste0(row$File[[1]], " - 放大预览"),
          div(
            class = "ml-modal-plot-box",
            imageOutput(ns("activeAllMlPlotLarge"), height = "100%", width = "100%")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭")
        )
      )
    }, ignoreInit = TRUE)

    output$allMlResultFileList <- renderUI({
      res <- all_ml_results()
      if (is.null(res)) {
        return(NULL)
      }

      image_files <- res$image_files
      image_files$IsImage <- TRUE
      csv_files <- res$csv_files
      csv_files$IsImage <- FALSE
      rows <- rbind(image_files, csv_files)

      div(
        class = "ml-result-file-list",
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, , drop = FALSE]
          file_cell <- if (isTRUE(row$IsImage[[1]])) {
            actionButton(
              ns(paste0("showAllMlPlot_", row$Key[[1]])),
              row$File[[1]],
              class = "ml-result-file-action",
              title = "点击后在上方图片区预览"
            )
          } else {
            span(row$File[[1]], class = "ml-result-file-name", title = row$File[[1]])
          }

          div(
            class = "ml-result-file-row",
            span(sprintf("%02d", i), class = "ml-file-index"),
            file_cell,
            span(row$Type[[1]], class = "ml-result-file-type"),
            span(row$Desc[[1]], class = "ml-result-file-desc", title = row$Desc[[1]]),
            span(
              class = "ml-result-file-download",
              downloadButton(
                ns(paste0("downloadAllMlFile_", row$Key[[1]])),
                "下载",
                style = "font-size: 10px; padding: 1px 8px;"
              )
            )
          )
        })
      )
    })

    all_ml_model_keys <- c("RF", "SVM", "GLM", "GBM", "KNN", "NNET", "LASSO", "DT", "NB", "ADA", "BAG")
    all_ml_image_keys <- c(
      "residual",
      "boxplot",
      "ROC",
      "importance",
      as.vector(outer(c("ConfMat", "PRcurve", "PredProbHist"), all_ml_model_keys, paste, sep = "_"))
    )
    all_ml_csv_keys <- c(
      "importanceGenes_summary",
      paste0("importanceGene_", all_ml_model_keys),
      paste0("geneList_", all_ml_model_keys)
    )
    all_ml_file_keys <- c(all_ml_image_keys, all_ml_csv_keys)

    for (plot_key in all_ml_image_keys) {
      local({
        key <- plot_key
        observeEvent(input[[paste0("showAllMlPlot_", key)]], {
          res <- all_ml_results()
          if (is.null(res) || !key %in% res$image_files$Key) {
            return()
          }
          active_all_ml_plot(key)
        }, ignoreInit = TRUE)
      })
    }

    for (file_key in all_ml_file_keys) {
      local({
        key <- file_key
        output[[paste0("downloadAllMlFile_", key)]] <- downloadHandler(
          filename = function() {
            res <- all_ml_results()
            if (is.null(res)) {
              return(paste0(key, ".txt"))
            }
            files <- rbind(res$image_files, res$csv_files)
            row <- files[files$Key == key, , drop = FALSE]
            if (!nrow(row)) {
              return(paste0(key, ".txt"))
            }
            row$File[[1]]
          },
          content = function(file) {
            res <- all_ml_results()
            req(res)
            files <- rbind(res$image_files, res$csv_files)
            row <- files[files$Key == key, , drop = FALSE]
            req(nrow(row) > 0, file.exists(row$Path[[1]]))
            file.copy(row$Path[[1]], file, overwrite = TRUE)
          }
        )
      })
    }

    # ============================================================
    # 样本类型矫正：生成 Sample Type Matrix
    # ============================================================

    sample_type_result <- reactiveVal(NULL)
    sample_type_running <- reactiveVal(FALSE)

    ml_set_upload_status <- function(status_id, label) {
      label <- gsub("\\\\", "\\\\\\\\", label)
      label <- gsub('"', '\\"', label, fixed = TRUE)
      shinyjs::runjs(sprintf('$("#%s").text("%s")', ns(status_id), label))
    }

    ml_upload_ref <- function(file) {
      if (is.null(file) || is.null(file$datapath) || is.null(file$name)) {
        stop("未提供文件。", call. = FALSE)
      }
      data.frame(
        datapath = as.character(file$datapath[[1]]),
        name = as.character(file$name[[1]]),
        stringsAsFactors = FALSE
      )
    }

    output$downloadExampleCounts <- downloadHandler(
      filename = "geneMatrix.txt",
      content = function(file) {
        if (exists("EXAMPLE_DATA", inherits = TRUE) && file.exists(EXAMPLE_DATA$counts)) {
          file.copy(EXAMPLE_DATA$counts, file, overwrite = TRUE)
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
        if (exists("EXAMPLE_DATA", inherits = TRUE) && file.exists(EXAMPLE_DATA$control)) {
          file.copy(EXAMPLE_DATA$control, file, overwrite = TRUE)
        } else {
          writeLines(paste0("Sample", 1:6), file)
        }
      }
    )

    output$downloadExampleTreat <- downloadHandler(
      filename = "treat.txt",
      content = function(file) {
        if (exists("EXAMPLE_DATA", inherits = TRUE) && file.exists(EXAMPLE_DATA$treat)) {
          file.copy(EXAMPLE_DATA$treat, file, overwrite = TRUE)
        } else {
          writeLines(paste0("Sample", 7:12), file)
        }
      }
    )

    observeEvent(input$sampleTypeExprFile, {
      req(input$sampleTypeExprFile$name)
      ml_set_upload_status("sampleTypeExprFileStatus", input$sampleTypeExprFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$sampleTypeControlFile, {
      req(input$sampleTypeControlFile$name)
      ml_set_upload_status("sampleTypeControlFileStatus", input$sampleTypeControlFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$sampleTypeTreatFile, {
      req(input$sampleTypeTreatFile$name)
      ml_set_upload_status("sampleTypeTreatFileStatus", input$sampleTypeTreatFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$prepExprFile, {
      req(input$prepExprFile$name)
      ml_set_upload_status("prepExprFileStatus", input$prepExprFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$prepGeneFile, {
      req(input$prepGeneFile$name)
      ml_set_upload_status("prepGeneFileStatus", input$prepGeneFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$allMlFile, {
      req(input$allMlFile$name)
      ml_set_upload_status("allMlFileStatus", input$allMlFile$name)
    }, ignoreInit = TRUE)

    process_sample_type_matrix <- function(expr_file,
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
        sample_path <- upload_path(file)
        sample_sep <- table_separator(file)
        sample_data <- utils::read.table(
          sample_path,
          sep = sample_sep,
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

      validate_sample_type_inputs <- function(expr_matrix, ctrl_samples, treat_samples) {
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

      path <- upload_path(expr_file)
      sep <- table_separator(expr_file)
      raw_data <- utils::read.table(
        path,
        sep = sep,
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
      expr_data <- raw_data[, -1, drop = FALSE]
      expr_matrix <- as.matrix(expr_data)
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

      ctrl_samples <- read_group_samples(control_file)
      treat_samples <- read_group_samples(treat_file)
      clean_group_samples <- function(samples, available_samples) {
        samples <- trimws(as.character(samples))
        samples <- samples[nzchar(samples)]
        if (length(samples) > 1 && !samples[[1]] %in% available_samples &&
            tolower(samples[[1]]) %in% c("sample", "samples", "id", "sampleid", "sample_id", "gsm")) {
          samples <- samples[-1]
        }
        stripped <- sub("_(con|ctrl|control|tre|treat|case|disease)$", "", samples, ignore.case = TRUE)
        if (sum(stripped %in% available_samples) > sum(samples %in% available_samples)) {
          samples <- stripped
        }
        unique(samples)
      }
      ctrl_samples <- clean_group_samples(ctrl_samples, colnames(expr_matrix))
      treat_samples <- clean_group_samples(treat_samples, colnames(expr_matrix))
      validate_sample_type_inputs(expr_matrix, ctrl_samples, treat_samples)

      missing_ctrl <- ctrl_samples[!ctrl_samples %in% colnames(expr_matrix)]
      missing_treat <- treat_samples[!treat_samples %in% colnames(expr_matrix)]
      if (length(missing_ctrl) || length(missing_treat)) {
        msg <- c(
          if (length(missing_ctrl)) paste0("对照组缺失: ", paste(missing_ctrl, collapse = ", ")) else NULL,
          if (length(missing_treat)) paste0("实验组缺失: ", paste(missing_treat, collapse = ", ")) else NULL
        )
        stop(paste(msg, collapse = "；"), call. = FALSE)
      }

      control_data <- expr_matrix[, ctrl_samples, drop = FALSE]
      treat_data <- expr_matrix[, treat_samples, drop = FALSE]
      combined_data <- cbind(control_data, treat_data)
      num_control <- ncol(control_data)
      num_treat <- ncol(treat_data)

      sample_types <- c(rep("con", num_control), rep("tre", num_treat))
      colnames(combined_data) <- paste0(colnames(combined_data), "_", sample_types)

      final_df <- data.frame(
        GeneName = rownames(combined_data),
        combined_data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      summary_df <- data.frame(
        项目 = c(
          "原始基因数",
          "去重后基因数",
          "对照组样本数(con)",
          "实验组样本数(tre)",
          "总样本数",
          "是否执行log2",
          "是否执行标准化"
        ),
        数值 = c(
          length(gene_ids),
          nrow(combined_data),
          num_control,
          num_treat,
          ncol(combined_data),
          if (need_log) "是" else "否",
          if (isTRUE(normalize)) "是" else "否"
        ),
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

      list(
        matrix = final_df,
        summary = summary_df,
        summary_lines = summary_lines
      )
    }
    environment(process_sample_type_matrix) <- globalenv()

    observeEvent(input$runSampleType, {
      if (is.null(input$sampleTypeExprFile) ||
          is.null(input$sampleTypeControlFile) ||
          is.null(input$sampleTypeTreatFile)) {
        showNotification("请上传表达矩阵、control.txt 和 treat.txt。", type = "error")
        return()
      }

      expr_file <- ml_upload_ref(input$sampleTypeExprFile)
      control_file <- ml_upload_ref(input$sampleTypeControlFile)
      treat_file <- ml_upload_ref(input$sampleTypeTreatFile)
      auto_log <- input$sampleTypeAutoLog
      normalize <- input$sampleTypeNormalize

      sample_type_running(TRUE)
      sample_type_result(NULL)
      showNotification("样本类型矫正已在后台启动，可以切换到其它模块继续操作。", type = "message", duration = APP_RUNNING_NOTIFICATION_DURATION)

      run_async_task(
        task = function() {
          process_sample_type_matrix(
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
        }
      )
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

    output$sampleTypeUI <- renderUI({
      if (sample_type_running()) {
        return(div(style = "text-align: center; padding: 50px;",
                   h4("样本类型矫正中，请稍候...", style = "color: #666;")))
      }
      tagList(
        h5("结果预览（前 10 行 x 10 列）"),
        DTOutput(ns("sampleTypePreview")),
        br(),
        h5("样本统计"),
        DTOutput(ns("sampleTypeSummary"))
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
    # 第一部分：提取特征值
    # ============================================================
    
    prep_result <- reactiveVal(NULL)
    prep_running <- reactiveVal(FALSE)
    prep_info <- reactiveVal(NULL)
    
    observeEvent(input$runPrep, {
      
      # 检查文件是否上传
      if (is.null(input$prepExprFile)) {
        showNotification("⚠️ 请上传表达矩阵文件！", type = "error")
        return()
      }
      if (is.null(input$prepGeneFile)) {
        showNotification("⚠️ 请上传基因列表文件！", type = "error")
        return()
      }
      
      prep_running(TRUE)
      prep_result(NULL)
      prep_info(NULL)

      prep_expr_file <- input$prepExprFile
      prep_gene_file <- input$prepGeneFile
      showNotification("特征值提取已在后台启动，可以切换到其它模块继续操作。", type = "message", duration = APP_RUNNING_NOTIFICATION_DURATION)

      run_async_task(
        task = function() {
          expr_matrix <- read_expression_matrix(prep_expr_file)
          expr_matrix <- expr_matrix[rowMeans(expr_matrix, na.rm = TRUE) > 0, ]

          gene_list <- read_gene_list_file(prep_gene_file)
          if (length(gene_list) == 0) {
            stop("基因列表为空！", call. = FALSE)
          }

          common_genes <- intersect(gene_list, rownames(expr_matrix))
          if (length(common_genes) == 0) {
            stop("未找到共同基因！请检查基因名是否匹配", call. = FALSE)
          }

          gene_expr <- expr_matrix[common_genes, , drop = FALSE]
          output_df <- data.frame(
            ID = rownames(gene_expr),
            gene_expr,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )

          list(
            output_df = output_df,
            info = list(
              total_genes = nrow(expr_matrix),
              total_samples = ncol(expr_matrix),
              gene_list_count = length(gene_list),
              common_count = length(common_genes),
              missing_count = length(gene_list) - length(common_genes)
            )
          )
        },
        on_success = function(result) {
          prep_result(result$output_df)
          prep_info(result$info)
          showNotification(
            paste0("提取完成！共提取 ", result$info$common_count, " 个基因的表达数据"),
            type = "message",
            duration = 5
          )
        },
        on_error = function(error) {
          showNotification(paste0("错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          prep_running(FALSE)
        }
      )
      return()
      
      withProgress(message = "⏳ 提取特征值...", value = 0, {
        
        incProgress(0.1, detail = "读取表达矩阵...")
        
        tryCatch({
          # ---- 1. 读取表达矩阵 ----
          expr_matrix <- read_expression_matrix(input$prepExprFile)
          
          # 第一列是基因名
          
          # 转换为数值矩阵
          
          # 过滤低表达基因（行均值 > 0）
          expr_matrix <- expr_matrix[rowMeans(expr_matrix, na.rm = TRUE) > 0, ]
          
          incProgress(0.4, detail = "读取基因列表...")
          
          # ---- 2. 读取基因列表 ----
          gene_list <- read_gene_list_file(input$prepGeneFile)
          
          if (length(gene_list) == 0) {
            showNotification("⚠️ 基因列表为空！", type = "error")
            prep_running(FALSE)
            return()
          }
          
          incProgress(0.7, detail = "提取交集基因...")
          
          # ---- 3. 提取共同基因 ----
          common_genes <- intersect(gene_list, rownames(expr_matrix))
          
          if (length(common_genes) == 0) {
            showNotification("⚠️ 未找到共同基因！请检查基因名是否匹配", type = "error")
            prep_running(FALSE)
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
          
          prep_result(output_df)
          
          # 提取信息
          prep_info(list(
            total_genes = nrow(expr_matrix),
            total_samples = ncol(expr_matrix),
            gene_list_count = length(gene_list),
            common_count = length(common_genes),
            missing_count = length(gene_list) - length(common_genes)
          ))
          
          prep_running(FALSE)
          incProgress(1.0, detail = "✅ 完成！")
          
          showNotification(
            paste0("✅ 提取完成！共提取 ", length(common_genes), " 个基因的表达数据"),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          showNotification(paste0("❌ 错误: ", e$message), type = "error", duration = 10)
          prep_running(FALSE)
        })
      })
    })
    
    # ---- 提取特征值预览 ----
    output$prepTable <- renderDT({
      df <- prep_result()
      if (is.null(df)) {
        return(NULL)
      }
      
      # 预览前10行前10列
      preview_rows <- min(10, nrow(df))
      preview_cols <- min(10, ncol(df))
      preview_df <- df[1:preview_rows, 1:preview_cols]
      
      datatable(preview_df, 
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    })
    
    output$prepInfo <- renderDT({
      info <- prep_info()
      if (is.null(info)) {
        return(NULL)
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
    
    output$prepUI <- renderUI({
      if (prep_running()) {
        div(
          style = "text-align: center; padding: 50px;",
          h4("⏳ 提取中，请稍候...", style = "color: #666;")
        )
      } else {
        tagList(
          h5("📊 数据预览（前 10 行 × 10 列）"),
          DTOutput(ns("prepTable")),
          br(),
          h5("📊 特征值提取统计"),
          DTOutput(ns("prepInfo"))
        )
      }
    })
    
    # ---- 下载 ----
    output$downloadPrepResult <- downloadHandler(
      filename = function() paste0("geneexp_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- prep_result()
        if (is.null(df)) {
          write.csv(data.frame(信息 = "请先运行提取特征值"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE)
      }
    )
    

    output$mlActivePanel <- renderUI({
      current_tab <- input$mlTabset %||% "提取特征值"

      metric_grid <- function(items) {
        div(
          class = "metric-grid",
          lapply(items, function(item) {
            div(class = "metric", tags$b(item[[1]]), tags$span(item[[2]]))
          })
        )
      }

      if (identical(current_tab, "提取特征值")) {
        info <- prep_info()
        if (prep_running()) {
          return(tagList(tags$b("提取特征值"), tags$p("正在后台提取研究方向基因表达矩阵。")))
        }
        if (is.null(info)) {
          return(NULL)
        }
        return(tagList(
          tags$b("提取特征值已完成"),
          metric_grid(list(
            list("原始基因数", info$total_genes),
            list("样本数", info$total_samples),
            list("匹配基因数", info$common_count),
            list("未匹配基因数", info$missing_count)
          ))
        ))
      }

      res <- all_ml_results()
      if (all_ml_running()) {
        return(tagList(tags$b("11种算法"), tags$p("正在后台训练多模型，可切换到其它模块继续操作。")))
      }
      if (is.null(res)) {
        return(tagList(tags$b("11种算法"), tags$p("上传 geneexp.csv 后并行比较 11 种机器学习模型，输出性能、Top基因和图形。")))
      }
      tagList(
        tags$b("11种算法已完成"),
        metric_grid(list(
          list("成功模型", nrow(res$performance)),
          list("训练样本", res$n_train),
          list("测试样本", res$n_test),
          list("最佳模型", res$performance$Model[[1]])
        ))
      )
    })
    
  })
}
