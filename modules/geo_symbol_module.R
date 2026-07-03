# GEO expression matrix processing for platforms that already contain gene symbols.

geo_symbol_process_files <- function(expr_file,
                                     platform_file = NULL,
                                     gene_col = 6,
                                     filter_whitespace = TRUE) {
  gene_col <- as.integer(gene_col)
  if (is.na(gene_col) || gene_col < 1) {
    stop("GPL Gene Symbol 列序号必须是大于 0 的整数。", call. = FALSE)
  }

  expr_data <- geo_symbol_read_geo_table(expr_file, file_type = "gse")
  if (ncol(expr_data) < 2) {
    stop("GSE 表达矩阵至少需要 1 列探针 ID 和 1 列样本表达值。", call. = FALSE)
  }

  first_col <- names(expr_data)[1]
  first_col_lower <- tolower(trimws(first_col))
  is_gene_matrix <- first_col_lower %in% c("genesymbol", "gene_symbol", "gene symbol", "symbol", "gene", "genename")
  if (is_gene_matrix) {
    colnames(expr_data)[1] <- "geneSymbol"
    sample_cols <- setdiff(names(expr_data), "geneSymbol")
    expr_dt <- data.table::as.data.table(expr_data)
    expr_dt[, geneSymbol := trimws(as.character(geneSymbol))]
    if (isTRUE(filter_whitespace)) {
      expr_dt <- expr_dt[!grepl(".+\\s+.+", geneSymbol)]
    }
    expr_dt <- expr_dt[!is.na(geneSymbol) & nzchar(geneSymbol) & grepl("^[A-Za-z]", geneSymbol)]
    expr_dt[, (sample_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = sample_cols]
    result_dt <- expr_dt[
      ,
      lapply(.SD, mean, na.rm = TRUE),
      by = geneSymbol,
      .SDcols = sample_cols
    ]
    data.table::setorder(result_dt, geneSymbol)
    result_df <- as.data.frame(result_dt, stringsAsFactors = FALSE)
    expr_matrix <- as.matrix(result_df[, -1, drop = FALSE])
    rownames(expr_matrix) <- result_df$geneSymbol
    storage.mode(expr_matrix) <- "numeric"

    return(list(
      table = result_df,
      expression_matrix = expr_matrix,
      stats = list(
        mode = "gene_matrix",
        probe_count = nrow(expr_data),
        sample_count = ncol(expr_data) - 1,
        platform_rows = NA_integer_,
        mapping_count = NA_integer_,
        matched_probe_count = NA_integer_,
        gene_count = nrow(result_df)
      )
    ))
  }

  if (is.null(platform_file)) {
    stop("上传的是探针表达矩阵时，需要同时上传 GPL 平台注释文件。", call. = FALSE)
  }
  colnames(expr_data)[1] <- "ProbeID"

  platform_data <- geo_symbol_read_geo_table(platform_file, file_type = "gpl", header = TRUE)
  if (ncol(platform_data) < gene_col) {
    stop(
      sprintf("GPL 文件只有 %s 列，无法读取第 %s 列。", ncol(platform_data), gene_col),
      call. = FALSE
    )
  }

  platform_dt <- data.table::as.data.table(platform_data)
  probe_col <- names(platform_dt)[1]
  gene_col_name <- names(platform_dt)[gene_col]

  row_has_value <- rowSums(!is.na(platform_dt) & platform_dt != "") > 0
  probe_ids <- as.character(platform_dt[[probe_col]])
  raw_genes <- as.character(platform_dt[[gene_col_name]])

  valid <- row_has_value &
    !grepl("^(ID|\\!)", probe_ids) &
    !is.na(raw_genes) &
    nzchar(raw_genes)

  if (isTRUE(filter_whitespace)) {
    valid <- valid & !grepl(".+\\s+.+", raw_genes)
  }

  clean_genes <- sub("(.+?)///(.+)", "\\1", raw_genes[valid])
  clean_genes <- trimws(gsub('"', "", clean_genes, fixed = TRUE))

  mapping_dt <- data.table::data.table(
    ProbeID = as.character(probe_ids[valid]),
    geneSymbol = clean_genes
  )
  mapping_dt <- mapping_dt[
    !is.na(ProbeID) & nzchar(ProbeID) &
      !is.na(geneSymbol) & nzchar(geneSymbol) &
      grepl("^[A-Za-z]", geneSymbol)
  ]
  mapping_dt <- mapping_dt[!duplicated(ProbeID, fromLast = TRUE)]

  if (!nrow(mapping_dt)) {
    stop("未从 GPL 文件中解析到有效的 ProbeID 到 Gene Symbol 映射。", call. = FALSE)
  }

  expr_dt <- data.table::as.data.table(expr_data)
  merged_dt <- merge(expr_dt, mapping_dt, by = "ProbeID")
  if (!nrow(merged_dt)) {
    stop("GSE 表达矩阵中的探针 ID 与 GPL 注释没有匹配项。", call. = FALSE)
  }

  sample_cols <- setdiff(names(merged_dt), c("ProbeID", "geneSymbol"))
  merged_dt[, (sample_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = sample_cols]

  agg_dt <- merged_dt[
    ,
    lapply(.SD, mean, na.rm = TRUE),
    by = geneSymbol,
    .SDcols = sample_cols
  ]
  data.table::setorder(agg_dt, geneSymbol)

  result_df <- as.data.frame(agg_dt, stringsAsFactors = FALSE)
  expr_matrix <- as.matrix(result_df[, -1, drop = FALSE])
  rownames(expr_matrix) <- result_df$geneSymbol
  storage.mode(expr_matrix) <- "numeric"

  list(
    table = result_df,
    expression_matrix = expr_matrix,
    stats = list(
      mode = "probe_mapping",
      probe_count = nrow(expr_data),
      sample_count = ncol(expr_data) - 1,
      platform_rows = nrow(platform_data),
      mapping_count = nrow(mapping_dt),
      matched_probe_count = nrow(merged_dt),
      gene_count = nrow(result_df)
    )
  )
}

geo_symbol_stats_table <- function(stats) {
  platform_rows <- if (is.na(stats$platform_rows)) "未使用" else stats$platform_rows
  mapping_count <- if (is.na(stats$mapping_count)) "未使用" else stats$mapping_count
  matched_probe_count <- if (is.na(stats$matched_probe_count)) "未使用" else stats$matched_probe_count
  data.frame(
    指标 = c(
      "处理模式",
      "GSE 探针数",
      "样本数",
      "GPL 行数",
      "有效映射数",
      "匹配探针数",
      "输出基因数"
    ),
    数值 = c(
      if (identical(stats$mode, "gene_matrix")) "已是 geneSymbol 矩阵" else "探针 ID + GPL 映射",
      stats$probe_count,
      stats$sample_count,
      platform_rows,
      mapping_count,
      matched_probe_count,
      stats$gene_count
    ),
    check.names = FALSE
  )
}

geo_symbol_file_path <- function(file) {
  if (is.null(file)) {
    return(NULL)
  }
  if (is.character(file)) {
    return(file[[1]])
  }
  if (!is.null(file$datapath)) {
    return(as.character(file$datapath[[1]]))
  }
  as.character(file)[[1]]
}

geo_symbol_find_table_bounds <- function(path, file_type = c("gse", "gpl")) {
  file_type <- match.arg(file_type)
  con <- base::file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  if (!length(lines)) {
    stop("上传文件为空。", call. = FALSE)
  }

  begin_pattern <- if (identical(file_type, "gse")) {
    "^!.*series_matrix_table_begin"
  } else {
    "^!.*platform_table_begin"
  }
  end_pattern <- if (identical(file_type, "gse")) {
    "^!.*series_matrix_table_end"
  } else {
    "^!.*platform_table_end"
  }

  begin <- grep(begin_pattern, lines, ignore.case = TRUE)
  if (length(begin)) {
    start <- begin[[1]] + 1L
  } else {
    start <- 1L
  }

  is_candidate <- nzchar(trimws(lines)) & grepl("\t", lines, fixed = TRUE)
  if (identical(file_type, "gpl")) {
    is_candidate <- is_candidate & !grepl("^[#!]", lines)
  } else {
    is_candidate <- is_candidate & !grepl("^!", lines)
  }
  header_candidates <- which(seq_along(lines) >= start & is_candidate)
  if (!length(header_candidates)) {
    stop("未在上传文件中找到可读取的数据表头。", call. = FALSE)
  }
  header <- header_candidates[[1]]

  end <- grep(end_pattern, lines, ignore.case = TRUE)
  end <- end[end > header]
  nrows <- if (length(end)) {
    max(0L, end[[1]] - header - 1L)
  } else {
    -1L
  }

  list(skip = header - 1L, nrows = nrows)
}

geo_symbol_read_geo_table <- function(file,
                                      file_type = c("gse", "gpl"),
                                      header = TRUE,
                                      nrows = -1L) {
  file_type <- match.arg(file_type)
  path <- geo_symbol_file_path(file)
  if (is.null(path) || !file.exists(path)) {
    stop("上传文件不存在。", call. = FALSE)
  }
  bounds <- geo_symbol_find_table_bounds(path, file_type = file_type)
  read_nrows <- if (identical(as.integer(nrows), 0L)) {
    0L
  } else if (nrows > 0) {
    if (bounds$nrows > 0) min(nrows, bounds$nrows) else nrows
  } else {
    bounds$nrows
  }
  args <- list(
    file = path,
    header = header,
    sep = "\t",
    quote = "\"",
    comment.char = "",
    skip = bounds$skip,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE
  )
  if (read_nrows >= 0) {
    args$nrows <- read_nrows
  }
  do.call(utils::read.delim, args)
}

geo_symbol_read_preview <- function(file,
                                    n = 30,
                                    header = TRUE,
                                    file_type = c("gse", "gpl"),
                                    number_headers = FALSE) {
  if (is.null(file)) {
    return(data.frame(提示 = "请先上传文件。", check.names = FALSE))
  }

  file_type <- match.arg(file_type)
  preview_data <- geo_symbol_read_geo_table(file, file_type = file_type, header = header, nrows = n)

  if (isTRUE(number_headers)) {
    names(preview_data) <- sprintf("%02d_%s", seq_along(names(preview_data)), names(preview_data))
  }

  preview_data
}

geo_symbol_gpl_columns <- function(file) {
  if (is.null(file)) {
    return(data.frame(列序号 = integer(), 列名 = character(), check.names = FALSE))
  }

  preview <- geo_symbol_read_geo_table(file, file_type = "gpl", header = TRUE, nrows = 0)
  columns <- names(preview)
  data.frame(
    列序号 = seq_along(columns),
    列名 = columns,
    check.names = FALSE
  )
}

geo_symbol_detect_gene_col <- function(file) {
  columns <- geo_symbol_gpl_columns(file)
  if (!nrow(columns)) {
    return(NA_integer_)
  }

  names_lower <- tolower(trimws(columns$列名))
  priority_patterns <- c(
    "^symbol$",
    "^gene[ _.-]*symbol$",
    "gene[ _.-]*symbol",
    "^ilmn_gene$"
  )

  for (pattern in priority_patterns) {
    hit <- grep(pattern, names_lower)
    if (length(hit)) {
      return(columns$列序号[hit[[1]]])
    }
  }

  NA_integer_
}

geo_symbol_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .geo-symbol-page {
        color: #263238;
        max-width: 100%;
        overflow-x: hidden;
      }
      .geo-symbol-title {
        margin-bottom: 12px;
      }
      .geo-symbol-title h3 {
        margin: 0 0 4px 0;
        font-size: 21px;
        font-weight: 700;
        color: #263238;
      }
      .geo-symbol-title p {
        margin: 0;
        color: #607d8b;
        font-size: 13px;
      }
      .geo-symbol-card {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        padding: 12px 14px;
        background: #ffffff;
        margin-bottom: 12px;
      }
      .geo-symbol-card h4 {
        margin: 0 0 10px 0;
        font-size: 15px;
        font-weight: 700;
        color: #37474f;
      }
      .geo-symbol-card .form-group {
        margin-bottom: 8px;
      }
      .geo-symbol-card label {
        font-size: 12px;
        margin-bottom: 3px;
        color: #37474f;
      }
      .geo-symbol-card .help-block {
        margin: 2px 0 0 0;
        font-size: 11px;
        color: #78909c;
      }
      .geo-symbol-tabs {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        background: #ffffff;
        margin-bottom: 12px;
        max-width: 100%;
        box-sizing: border-box;
      }
      .geo-symbol-tabs .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        padding: 0 14px;
      }
      .geo-symbol-tabs .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        color: #546e7a;
        padding: 11px 2px 10px 2px;
        margin-right: 28px;
        font-size: 13px;
        background: transparent;
      }
      .geo-symbol-tabs .nav-tabs > li.active > a,
      .geo-symbol-tabs .nav-tabs > li.active > a:focus,
      .geo-symbol-tabs .nav-tabs > li.active > a:hover {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        font-weight: 700;
        background: transparent;
      }
      .geo-symbol-tabs .tab-content {
        padding: 14px;
      }
      .geo-symbol-tabs .form-group {
        margin-bottom: 8px;
      }
      .geo-symbol-tabs label {
        font-size: 12px;
        margin-bottom: 3px;
        color: #37474f;
      }
      .geo-symbol-tabs .help-block {
        margin: 2px 0 0 0;
        font-size: 11px;
        color: #78909c;
      }
      .geo-symbol-upload-row {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        align-items: center;
        margin-bottom: 5px;
      }
      .geo-symbol-upload-box {
        position: relative;
        border: 1px dashed #b0bec5;
        border-radius: 0;
        min-height: 74px;
        padding: 6px 10px;
        background-color: #ffffff;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
        transition: background-color 0.2s;
      }
      .geo-symbol-upload-box:hover {
        background-color: #f7fafc;
      }
      .geo-symbol-upload-box .shiny-input-container {
        position: absolute;
        inset: 0;
        width: 100% !important;
        height: 100%;
        margin: 0;
        opacity: 0;
        z-index: 2;
        cursor: pointer;
      }
      .geo-symbol-upload-box .input-group,
      .geo-symbol-upload-box .input-group-btn,
      .geo-symbol-upload-box .btn-file,
      .geo-symbol-upload-box input[type='file'] {
        width: 100%;
        height: 100%;
        cursor: pointer;
      }
      .geo-symbol-upload-placeholder {
        text-align: center;
        pointer-events: none;
        display: grid;
        gap: 2px;
        justify-items: center;
      }
      .geo-symbol-upload-title {
        font-weight: 700;
        font-size: 11px;
        color: #263238;
      }
      .geo-symbol-upload-status {
        color: #1e88e5;
        font-size: 11px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 260px;
      }
      .geo-symbol-param-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 8px 10px;
        align-items: start;
      }
      .geo-symbol-param-grid .shiny-input-container {
        width: 100%;
      }
      .geo-symbol-actions {
        display: flex;
        gap: 8px;
        align-items: center;
        justify-content: flex-start;
        margin-top: 4px;
      }
      .geo-symbol-actions .btn {
        min-width: 132px;
      }
      .geo-symbol-preview {
        margin-top: 2px;
        max-width: 100%;
        box-sizing: border-box;
        overflow-x: hidden;
      }
      .geo-symbol-preview .dataTables_wrapper {
        max-width: 100%;
        overflow-x: auto;
      }
      .geo-symbol-result-tabs .nav-tabs {
        border-bottom: 1px solid #d7dee2;
      }
      .geo-symbol-result-tabs .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        color: #546e7a;
        padding: 8px 2px 9px 2px;
        margin-right: 24px;
        font-size: 13px;
        background: transparent;
      }
      .geo-symbol-result-tabs .nav-tabs > li.active > a,
      .geo-symbol-result-tabs .nav-tabs > li.active > a:focus,
      .geo-symbol-result-tabs .nav-tabs > li.active > a:hover {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        font-weight: 700;
        background: transparent;
      }
      .geo-symbol-result-tabs .tab-content {
        padding-top: 12px;
      }
      .geo-symbol-qa {
        color: #263238;
        font-size: 13px;
        line-height: 1.7;
      }
      .geo-symbol-qa dl {
        margin: 0;
      }
      .geo-symbol-qa dt {
        margin-top: 10px;
        font-weight: 700;
      }
      .geo-symbol-qa dt:first-child {
        margin-top: 0;
      }
      .geo-symbol-qa dd {
        margin-left: 0;
        color: #455a64;
      }
      .geo-symbol-file-note {
        color: #607d8b;
        font-size: 12px;
        margin-bottom: 8px;
      }
      .geo-symbol-col-hint {
        color: #37474f;
        font-size: 13px;
        font-weight: 700;
        margin-bottom: 8px;
      }
    ")),

    div(
      class = "geo-symbol-page",
      div(
        class = "geo-symbol-title",
        h3("GEO 数据整理（已有 Gene Symbol）"),
        p("上传 GEO 原始 GSE/GPL 文件，自动整理为 geneMatrix。")
      ),

      div(
        class = "geo-symbol-tabs",
        tabsetPanel(
          id = ns("middleTabs"),
          type = "tabs",
          tabPanel(
            "数据上传",
            fluidRow(
              column(
                width = 6,
                tags$div(
                  class = "geo-symbol-upload-row",
                  tags$div(
                    id = ns("gseFileBox"),
                    class = "geo-symbol-upload-box",
                    tags$div(
                      class = "geo-symbol-upload-placeholder",
                      span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                      tags$span("GSE 表达矩阵", class = "geo-symbol-upload-title"),
                      tags$span(id = ns("gseFileStatus"), "Drop file here or click to upload", class = "geo-symbol-upload-status")
                    ),
                    fileInput(
                      ns("gseFile"),
                      NULL,
                      accept = c(".txt", ".tsv", "text/plain"),
                      buttonLabel = "浏览",
                      placeholder = "选择 GSE 文件"
                    )
                  )
                ),
                tags$div(
                  class = "geo-symbol-upload-row",
                  tags$div(
                    id = ns("gplFileBox"),
                    class = "geo-symbol-upload-box",
                    tags$div(
                      class = "geo-symbol-upload-placeholder",
                      span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                      tags$span("GPL 平台注释文件（ID_REF 时需要）", class = "geo-symbol-upload-title"),
                      tags$span(id = ns("gplFileStatus"), "Drop file here or click to upload", class = "geo-symbol-upload-status")
                    ),
                    fileInput(
                      ns("gplFile"),
                      NULL,
                      accept = c(".txt", ".tsv", "text/plain"),
                      buttonLabel = "浏览",
                      placeholder = "选择 GPL 文件"
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "geo-symbol-param-grid",
                  numericInput(
                    ns("geneCol"),
                    "Gene Symbol 列序号",
                    value = 6,
                    min = 1,
                    step = 1
                  ),
                  textInput(
                    ns("downloadName"),
                    "输出文件名",
                    value = "geneMatrix.txt"
                  )
                ),
                checkboxInput(
                  ns("filterWhitespace"),
                  "过滤含空格 symbol",
                  value = TRUE
                ),
                div(
                  class = "geo-symbol-file-note",
                  "可直接上传 GEO 下载的原始 GSE/GPL 文件。若 GSE 第一列已是 geneSymbol，可不依赖 GPL 直接输出；若第一列是 ID_REF，会自动用 GPL 映射。"
                )
              )
            ),
            div(
              class = "geo-symbol-actions",
              actionButton(
                ns("run"),
                "开始整理",
                class = "btn-primary"
              ),
              downloadButton(
                ns("downloadResult"),
                "下载 .txt 文件"
              )
            )
          ),
          tabPanel(
            "GSE",
            div(
              class = "geo-symbol-file-note",
              "预览 GSE 表达矩阵前 30 行。支持第一列为 geneSymbol 的矩阵，也支持第一列为 ID_REF 的探针矩阵。"
            ),
            DTOutput(ns("gsePreview"))
          ),
          tabPanel(
            "GPL",
            div(
              class = "geo-symbol-file-note",
              "预览 GPL 平台注释前 50 行。支持含 # 注释或 GEO platform table 标记的原始下载文件；表头前的数字就是列序号。"
            ),
            div(class = "geo-symbol-col-hint", textOutput(ns("gplGeneColHint"))),
            DTOutput(ns("gplPreview"))
          )
        )
      ),

      div(
        class = "geo-symbol-card geo-symbol-preview",
        h4("结果预览"),
        div(
          class = "geo-symbol-result-tabs",
          tabsetPanel(
            id = ns("resultTabs"),
            type = "tabs",
            tabPanel(
              "结果表",
              DTOutput(ns("preview"))
            ),
            tabPanel(
              "Q&A",
              div(
                class = "geo-symbol-qa",
                tags$dl(
                  tags$dt("Q1：这个模块适合处理什么数据？"),
                  tags$dd(
                    "适合 GPL 平台注释文件中已经包含 Gene Symbol 的 GEO 表达矩阵。GSE 可以是第一列为 ID_REF 的探针矩阵，也可以是第一列已为 geneSymbol 的表达矩阵。"
                  ),
                  tags$dt("Q2：Gene Symbol 列序号应该怎么填？"),
                  tags$dd(
                    "先上传 GPL 文件并切换到 GPL 预览页。表头会显示类似 13_Symbol 的格式，前面的数字就是列序号。如果自动识别到 Symbol 列，系统会自动回填；如果没有识别到，需要根据 GPL 预览手动填写。"
                  ),
                  tags$dt("Q3：为什么有时第 6 列不是 Gene Symbol？"),
                  tags$dd(
                    "不同平台的 GPL 注释列顺序不一致。比如 Illumina 平台常见第 6 列可能是 ILMN_Gene，而真正的基因符号列可能叫 Symbol、Gene Symbol 或 Gene.symbol，需要以 GPL 预览中的实际列名为准。"
                  ),
                  tags$dt("Q4：过滤含空格 symbol 是什么意思？"),
                  tags$dd(
                    "开启后会去掉包含空格的 gene symbol，减少复合注释、异常注释或不规范符号进入结果。若某些平台的合法基因名确实包含空格，可以临时关闭后对比结果。"
                  ),
                  tags$dt("Q5：多个探针对应同一个基因时怎么处理？"),
                  tags$dd(
                    "模块会先用 GPL 建立 ProbeID 到 Gene Symbol 的映射，再把 GSE 表达矩阵和映射表合并。若多个探针对应同一个 Gene Symbol，会按样本逐列取平均，最终每个基因只保留一行。"
                  ),
                  tags$dt("Q6：结果表每一行和每一列代表什么？"),
                  tags$dd(
                    "结果表第一列是 geneSymbol，后续每一列对应一个样本。每个单元格是该基因在对应样本中的表达值，可直接下载为 geneMatrix.txt。"
                  ),
                  tags$dt("Q7：整理失败或结果为空通常是什么原因？"),
                  tags$dd(
                    "常见原因包括：GSE 第一列探针 ID 与 GPL 第一列探针 ID 不一致、Gene Symbol 列序号填错、输入文件不是制表符分隔文本，或表达矩阵中探针 ID 格式被改动。"
                  ),
                  tags$dt("Q8：geneMatrix.txt 后续可以接哪些分析？"),
                  tags$dd(
                    "整理后的 geneMatrix.txt 可以作为统一表达矩阵继续进入差异分析、WGCNA、机器学习建模、ROC、列线图、免疫浸润和富集分析等后续模块。"
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

geo_symbol_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)

    geo_symbol_set_upload_status <- function(status_id, label) {
      label <- gsub("\\\\", "\\\\\\\\", label)
      label <- gsub('"', '\\"', label, fixed = TRUE)
      shinyjs::runjs(sprintf('$("#%s").text("%s")', session$ns(status_id), label))
    }

    observeEvent(input$gseFile, {
      req(input$gseFile$name)
      geo_symbol_set_upload_status("gseFileStatus", input$gseFile$name)
    }, ignoreInit = TRUE)

    observeEvent(input$gplFile, {
      req(input$gplFile$name)
      geo_symbol_set_upload_status("gplFileStatus", input$gplFile$name)
      detected_col <- geo_symbol_detect_gene_col(input$gplFile)
      if (!is.na(detected_col)) {
        updateNumericInput(session, "geneCol", value = detected_col)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$run, {
      req(input$gseFile)

      gse_path <- input$gseFile$datapath
      gpl_path <- if (is.null(input$gplFile)) NULL else input$gplFile$datapath
      gene_col <- input$geneCol
      filter_whitespace <- input$filterWhitespace

      result(NULL)
      task_note <- app_start_task_notification("GEO 数据整理正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          geo_symbol_process_files(
            expr_file = gse_path,
            platform_file = gpl_path,
            gene_col = gene_col,
            filter_whitespace = filter_whitespace
          )
        },
        on_success = function(processed) {
          app_clear_task_notification(task_note)
          shared_data <- session$userData$shared_data
          if (!is.null(shared_data)) {
            update_shared_matrix_state(
              shared_data,
              processed$expression_matrix,
              source = "geo_symbol"
            )
          }

          result(processed)
          showNotification("GEO 数据整理完成。", type = "message")
        },
        on_error = function(error) {
          app_clear_task_notification(task_note)
          showNotification(paste0("整理失败：", conditionMessage(error)), type = "error")
        },
        on_finally = function() {
          app_clear_task_notification(task_note)
        }
      )
      return()

      withProgress(message = "GEO 数据整理中...", value = 0, {
        tryCatch(
          {
            incProgress(0.15, detail = "读取表达矩阵和平台注释")
            processed <- geo_symbol_process_files(
              expr_file = input$gseFile$datapath,
              platform_file = if (is.null(input$gplFile)) NULL else input$gplFile$datapath,
              gene_col = input$geneCol,
              filter_whitespace = input$filterWhitespace
            )

            incProgress(0.75, detail = "保存结果到共享状态")
            shared_data <- session$userData$shared_data
            if (!is.null(shared_data)) {
              update_shared_matrix_state(
                shared_data,
                processed$expression_matrix,
                source = "geo_symbol"
              )
            }

            result(processed)
            showNotification("GEO 数据整理完成。", type = "message")
          },
          error = function(e) {
            showNotification(paste0("整理失败：", conditionMessage(e)), type = "error")
          }
        )
      })
    })

    output$gsePreview <- renderDT({
      preview_data <- geo_symbol_read_preview(
        input$gseFile,
        n = 30,
        header = TRUE,
        file_type = "gse"
      )
      app_preview_datatable(
        preview_data,
        rownames = FALSE,
        options = list(dom = "tip")
      )
    })

    output$gplPreview <- renderDT({
      preview_data <- geo_symbol_read_preview(
        input$gplFile,
        n = 50,
        header = TRUE,
        file_type = "gpl",
        number_headers = TRUE
      )
      app_preview_datatable(
        preview_data,
        rownames = FALSE,
        options = list(dom = "tip")
      )
    })

    output$gplGeneColHint <- renderText({
      detected_col <- geo_symbol_detect_gene_col(input$gplFile)
      if (is.na(detected_col)) {
        "未自动识别到 Gene Symbol 列。"
      } else {
        sprintf("自动识别建议：Gene Symbol 列序号 = %s", detected_col)
      }
    })

    output$preview <- renderDT({
      req(result())
      preview_data <- utils::head(result()$table, 20)
      app_preview_datatable(
        preview_data,
        rownames = FALSE
      )
    })

    output$downloadResult <- downloadHandler(
      filename = function() {
        name <- trimws(input$downloadName %||% "geneMatrix.txt")
        if (!nzchar(name)) {
          name <- "geneMatrix.txt"
        }
        name
      },
      content = function(file) {
        req(result())
        utils::write.table(
          result()$table,
          file = file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )
  })
}
