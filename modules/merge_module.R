# merge_module.R - 数据集合并与批次校正模块（完整版）
# 功能：合并多个表达矩阵，去除批次效应
# 参考差异分析模块的布局和风格

merge_empty_table <- function(message) {
  data.frame(提示 = message, check.names = FALSE)
}

merge_format_file_size <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes))
  if (!length(bytes) || is.na(bytes)) {
    return("")
  }
  if (bytes >= 1024^2) {
    return(sprintf("%.1f MB", bytes / 1024^2))
  }
  sprintf("%.1f KB", bytes / 1024)
}

merge_uploaded_files_table <- function(files) {
  if (is.null(files) || !nrow(files)) {
    return(merge_empty_table("请先上传至少两个表达矩阵文件。"))
  }

  data.frame(
    文件名 = files$name,
    大小KB = round(files$size / 1024, 2),
    批次名 = make.unique(tools::file_path_sans_ext(files$name), sep = "_"),
    check.names = FALSE
  )
}

merge_read_expression_file <- function(path, name) {
  matrix <- read_expression_matrix(
    list(datapath = path, name = name),
    check_names = FALSE
  )
  validation <- validate_expression_inputs(
    matrix,
    min_genes = 2,
    min_samples = 1,
    allow_na = FALSE,
    stop_on_error = TRUE
  )
  validation$expr_matrix
}

merge_batch_palette <- function(n) {
  if (n <= 0) {
    return("#607D8B")
  }
  if (n <= 8) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set1")[seq_len(n)])
  }
  grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n)
}

merge_plot_label <- function(plot_key) {
  labels <- c(
    boxplot = "箱线图",
    pca = "PCA图",
    combined = "组合图"
  )
  labels[[plot_key]] %||% "图片"
}

merge_blank_plot <- function(message = "") {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = message)
}

merge_batch_plot_theme <- function(base_size = 12, bold_axis = FALSE) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = if (bold_axis) "bold" else "plain"),
      axis.text = element_text(color = "#424242"),
      panel.grid.major = element_line(color = "#e5e5e5", linetype = "dashed", linewidth = 0.35),
      panel.grid.minor = element_line(color = "#f0f0f0", linewidth = 0.25),
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = "#666666", linewidth = 0.35),
      legend.key = element_rect(fill = "white", color = NA)
    )
}

merge_add_batch_ellipses <- function(plot, pca_df) {
  batch_counts <- table(pca_df$Batch)
  ellipse_batches <- names(batch_counts[batch_counts >= 3])

  if (!length(ellipse_batches)) {
    return(plot)
  }

  plot +
    stat_ellipse(
      data = pca_df[pca_df$Batch %in% ellipse_batches, , drop = FALSE],
      aes(fill = Batch),
      geom = "polygon",
      alpha = 0.14,
      linetype = 2,
      show.legend = FALSE
    )
}

merge_tagged_layout <- function(layout, tag_levels = "A") {
  tagged <- layout + patchwork::plot_annotation(tag_levels = tag_levels)
  tagged & theme(plot.tag = element_text(face = "bold", size = 13))
}

merge_boxplot_figure <- function(res) {
  n_batch <- length(unique(res$batch_info))
  mycol <- merge_batch_palette(n_batch)

  p_before <- ggplot(res$expr_melt, aes(x = Sample, y = Expression, fill = Batch)) +
    geom_boxplot(outlier.size = 0.1, linewidth = 0.25) +
    scale_fill_manual(values = mycol) +
    labs(title = "Before Batch Effect Removal", x = "Sample", y = "Expression", fill = "Batch") +
    merge_batch_plot_theme(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  p_after <- ggplot(res$corr_expr_melt, aes(x = Sample, y = Expression, fill = Batch)) +
    geom_boxplot(outlier.size = 0.1, linewidth = 0.25) +
    scale_fill_manual(values = mycol) +
    labs(title = "After Batch Effect Removal", x = "Sample", y = "Expression", fill = "Batch") +
    merge_batch_plot_theme(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  merge_tagged_layout(patchwork::wrap_plots(p_before, p_after, ncol = 2))
}

merge_pca_figure <- function(res) {
  n_batch <- length(unique(res$batch_info))
  mycol <- merge_batch_palette(n_batch)

  pc1_lab_before <- paste0("PC1 (", round(res$var_before[1] * 100, 1), "%)")
  pc2_lab_before <- paste0("PC2 (", round(res$var_before[2] * 100, 1), "%)")
  pc1_lab_after <- paste0("PC1 (", round(res$var_after[1] * 100, 1), "%)")
  pc2_lab_after <- paste0("PC2 (", round(res$var_after[2] * 100, 1), "%)")

  p_before <- merge_add_batch_ellipses(
    ggplot(res$pca_before_df, aes(x = PC1, y = PC2, color = Batch)) +
      geom_point(size = 3, alpha = 0.9),
    res$pca_before_df
  ) +
    scale_color_manual(values = mycol) +
    scale_fill_manual(values = mycol) +
    labs(title = "PCA Before Batch Effect Removal", x = pc1_lab_before, y = pc2_lab_before, color = "Batch") +
    merge_batch_plot_theme(base_size = 12, bold_axis = TRUE) +
    theme(plot.title = element_text(face = "bold", hjust = 0))

  p_after <- merge_add_batch_ellipses(
    ggplot(res$pca_after_df, aes(x = PC1, y = PC2, color = Batch)) +
      geom_point(size = 3, alpha = 0.9),
    res$pca_after_df
  ) +
    scale_color_manual(values = mycol) +
    scale_fill_manual(values = mycol) +
    labs(title = "PCA After Batch Effect Removal", x = pc1_lab_after, y = pc2_lab_after, color = "Batch") +
    merge_batch_plot_theme(base_size = 12, bold_axis = TRUE) +
    theme(plot.title = element_text(face = "bold", hjust = 0))

  merge_tagged_layout(patchwork::wrap_plots(p_before, p_after, ncol = 2))
}

merge_combined_figure <- function(res) {
  n_batch <- length(unique(res$batch_info))
  mycol <- merge_batch_palette(n_batch)

  p_before_box <- ggplot(res$expr_melt, aes(x = Sample, y = Expression, fill = Batch)) +
    geom_boxplot(outlier.size = 0.1, linewidth = 0.25) +
    scale_fill_manual(values = mycol) +
    labs(title = "Before Batch Effect Removal", x = "Sample", y = "Expression", fill = "Batch") +
    merge_batch_plot_theme(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  p_after_box <- ggplot(res$corr_expr_melt, aes(x = Sample, y = Expression, fill = Batch)) +
    geom_boxplot(outlier.size = 0.1, linewidth = 0.25) +
    scale_fill_manual(values = mycol) +
    labs(title = "After Batch Effect Removal", x = "Sample", y = "Expression", fill = "Batch") +
    merge_batch_plot_theme(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  pc1_lab_before <- paste0("PC1 (", round(res$var_before[1] * 100, 1), "%)")
  pc2_lab_before <- paste0("PC2 (", round(res$var_before[2] * 100, 1), "%)")
  pc1_lab_after <- paste0("PC1 (", round(res$var_after[1] * 100, 1), "%)")
  pc2_lab_after <- paste0("PC2 (", round(res$var_after[2] * 100, 1), "%)")

  p_before_pca <- merge_add_batch_ellipses(
    ggplot(res$pca_before_df, aes(x = PC1, y = PC2, color = Batch)) +
      geom_point(size = 3, alpha = 0.9),
    res$pca_before_df
  ) +
    scale_color_manual(values = mycol) +
    scale_fill_manual(values = mycol) +
    labs(title = "PCA Before Batch Effect Removal", x = pc1_lab_before, y = pc2_lab_before, color = "Batch") +
    merge_batch_plot_theme(base_size = 12, bold_axis = TRUE) +
    theme(plot.title = element_text(face = "bold", hjust = 0))

  p_after_pca <- merge_add_batch_ellipses(
    ggplot(res$pca_after_df, aes(x = PC1, y = PC2, color = Batch)) +
      geom_point(size = 3, alpha = 0.9),
    res$pca_after_df
  ) +
    scale_color_manual(values = mycol) +
    scale_fill_manual(values = mycol) +
    labs(title = "PCA After Batch Effect Removal", x = pc1_lab_after, y = pc2_lab_after, color = "Batch") +
    merge_batch_plot_theme(base_size = 12, bold_axis = TRUE) +
    theme(plot.title = element_text(face = "bold", hjust = 0))

  merge_tagged_layout(
    patchwork::wrap_plots(p_before_box, p_after_box, p_before_pca, p_after_pca, ncol = 2)
  )
}

merge_figure_by_key <- function(res, plot_key) {
  switch(
    plot_key,
    boxplot = merge_boxplot_figure(res),
    pca = merge_pca_figure(res),
    combined = merge_combined_figure(res),
    merge_boxplot_figure(res)
  )
}

merge_result_files_table <- function(has_results = FALSE) {
  if (!isTRUE(has_results)) {
    return(data.frame(
      文件名 = "暂无结果文件",
      类型 = "",
      说明 = "请先运行数据集合并。",
      plot_key = "",
      check.names = FALSE
    ))
  }

  data.frame(
    文件名 = c(
      "geneMatrix.txt",
      "geneMatrix.csv",
      "sample_batch_info.txt",
      "boxplot_comparison.png",
      "pca_comparison.png",
      "combined_figure.png"
    ),
    类型 = c("TXT", "CSV", "TXT", "PNG", "PNG", "PNG"),
    说明 = c(
      "批次校正后的表达矩阵",
      "批次校正后的表达矩阵",
      "样本与批次对应关系",
      "校正前后箱线图对比，点击行可在上方显示",
      "校正前后PCA对比，点击行可在上方显示",
      "箱线图与PCA组合图，点击行可在上方显示"
    ),
    plot_key = c("", "", "", "boxplot", "pca", "combined"),
    check.names = FALSE
  )
}

merge_prcomp <- function(expr_matrix) {
  pca_data <- t(expr_matrix)
  variable_cols <- apply(pca_data, 2, stats::sd, na.rm = TRUE) > 0
  pca_data <- pca_data[, variable_cols, drop = FALSE]

  if (ncol(pca_data) < 2 || nrow(pca_data) < 2) {
    stop("PCA 至少需要 2 个样本和 2 个有变异的基因。", call. = FALSE)
  }

  stats::prcomp(pca_data, scale. = TRUE)
}

merge_pca_frame <- function(pca, expr_matrix, batch_info) {
  pc2 <- if (ncol(pca$x) >= 2) pca$x[, 2] else rep(0, nrow(pca$x))
  data.frame(
    Sample = colnames(expr_matrix),
    PC1 = pca$x[, 1],
    PC2 = pc2,
    Batch = factor(batch_info, levels = unique(batch_info)),
    stringsAsFactors = FALSE
  )
}

merge_process_files <- function(file_paths, file_names, batch_names_override = NULL) {
  if (length(file_paths) < 2) {
    stop("数据集合并至少需要上传两个表达矩阵文件。", call. = FALSE)
  }

  ext_list <- tolower(tools::file_ext(file_names))
  if (any(!ext_list %in% c("txt", "csv", "tsv"))) {
    stop("请上传 TXT、TSV 或 CSV 格式的表达矩阵文件。", call. = FALSE)
  }

  matrix_list <- Map(merge_read_expression_file, file_paths, file_names)
  common_genes <- Reduce(intersect, lapply(matrix_list, rownames))
  if (length(common_genes) < 2) {
    stop("多个表达矩阵之间可合并的共同基因少于 2 个。", call. = FALSE)
  }

  matrix_list <- lapply(matrix_list, function(matrix) matrix[common_genes, , drop = FALSE])
  sample_counts <- vapply(matrix_list, ncol, integer(1))
  if (!is.null(batch_names_override) &&
      length(batch_names_override) == length(file_names) &&
      all(nzchar(trimws(batch_names_override)))) {
    batch_names <- make.unique(trimws(batch_names_override), sep = "_")
  } else {
    batch_names <- make.unique(tools::file_path_sans_ext(file_names), sep = "_")
  }
  batch_info <- rep(batch_names, sample_counts)

  expr_matrix <- do.call(cbind, matrix_list)
  if (anyDuplicated(colnames(expr_matrix))) {
    colnames(expr_matrix) <- make.unique(
      paste(batch_info, colnames(expr_matrix), sep = "_"),
      sep = "_"
    )
  }

  validation <- validate_expression_inputs(
    expr_matrix,
    min_genes = 2,
    min_samples = 2,
    allow_na = FALSE,
    stop_on_error = TRUE
  )
  expr_matrix <- validation$expr_matrix

  corrected_expr <- suppressMessages(
    limma::removeBatchEffect(expr_matrix, batch = batch_info)
  )
  corrected_data <- data.frame(
    geneSymbol = rownames(corrected_expr),
    corrected_expr,
    check.names = FALSE
  )

  expr_df <- as.data.frame(expr_matrix)
  expr_df$gene <- rownames(expr_matrix)
  expr_melt <- reshape2::melt(
    expr_df,
    id.vars = "gene",
    variable.name = "Sample",
    value.name = "Expression"
  )
  expr_melt$Batch <- batch_info[match(expr_melt$Sample, colnames(expr_matrix))]

  corr_expr_df <- as.data.frame(corrected_expr)
  corr_expr_df$gene <- rownames(corrected_expr)
  corr_expr_melt <- reshape2::melt(
    corr_expr_df,
    id.vars = "gene",
    variable.name = "Sample",
    value.name = "Expression"
  )
  corr_expr_melt$Batch <- batch_info[match(corr_expr_melt$Sample, colnames(corrected_expr))]

  pca_before <- merge_prcomp(expr_matrix)
  pca_after <- merge_prcomp(corrected_expr)
  var_before <- pca_before$sdev^2 / sum(pca_before$sdev^2)
  var_after <- pca_after$sdev^2 / sum(pca_after$sdev^2)

  list(
    expr_melt = expr_melt,
    corr_expr_melt = corr_expr_melt,
    pca_before_df = merge_pca_frame(pca_before, expr_matrix, batch_info),
    pca_after_df = merge_pca_frame(pca_after, corrected_expr, batch_info),
    var_before = var_before,
    var_after = var_after,
    corrected_data = corrected_data,
    corrected_expr = corrected_expr,
    expr_matrix = expr_matrix,
    batch_info = batch_info,
    batch_table = data.frame(
      Sample = colnames(expr_matrix),
      Batch = batch_info,
      stringsAsFactors = FALSE
    ),
    n_genes = nrow(expr_matrix),
    n_samples = ncol(expr_matrix),
    n_batches = length(unique(batch_info)),
    batch_names = unique(batch_info),
    common_gene_count = length(common_genes),
    validation = validation
  )
}

# ============================================================
# UI
# ============================================================
merge_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .merge-container {
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
        .merge-container:hover {
            background-color: #f0f0f0;
        }
        .merge-param-card {
            border: 1px solid #b0bec5;
            border-radius: 4px;
            padding: 12px 16px;
            height: 370px;
            overflow-y: auto;
            background-color: #ffffff;
        }
        .merge-param-card h4 {
            color: #2c3e50;
            margin-top: 0;
            margin-bottom: 10px;
            font-size: 14px;
            font-weight: 700;
        }
        .merge-param-card .help-text {
            font-size: 10px;
            color: #666;
            margin-top: 1px;
            margin-bottom: 2px;
        }
        .merge-param-card hr {
            margin: 4px 0 8px 0;
        }
        .merge-param-card .btn-sm {
            padding: 2px 8px;
            font-size: 12px;
        }
        .merge-param-card .btn-xs {
            font-size: 9px;
            padding: 0px 6px;
            height: 20px;
        }
        .merge-param-card .form-control {
            font-size: 11px;
            padding: 2px 4px;
            height: 26px;
        }
        .merge-param-card .shiny-input-container {
            margin-bottom: 4px;
        }
        .merge-param-card .file-input-box {
            border: 1.5px dashed #ccc;
            border-radius: 0px;
            padding: 4px 10px;
            background-color: #fafafa;
            flex: 1;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: space-between;
            font-size: 10px;
        }
        .merge-param-card .file-input-box:hover {
            background-color: #f0f0f0;
        }
        .merge-param-card .file-input-box .file-status {
            color: #3498db;
        }
        .merge-upload-field {
            display: grid;
            grid-template-columns: 1fr;
            gap: 0;
            align-items: start;
        }
        .merge-upload-label {
            font-size: 12px;
            color: #263238;
            padding-top: 4px;
            white-space: nowrap;
        }
        .merge-required {
            color: #e53935;
            margin-right: 3px;
        }
        .merge-upload-main {
            min-width: 0;
        }
        .merge-file-upload {
            position: relative;
            min-height: 58px;
            border: 1px dashed #b0bec5;
            background: #ffffff;
            display: flex;
            align-items: center;
            justify-content: center;
            overflow: hidden;
            cursor: pointer;
            transition: background-color 0.2s;
        }
        .merge-file-upload:hover {
            background-color: #f7fafc;
        }
        .merge-file-upload .shiny-input-container {
            position: absolute;
            inset: 0;
            width: 100% !important;
            height: 100%;
            margin: 0;
            opacity: 0;
            z-index: 2;
            cursor: pointer;
        }
        .merge-file-upload .input-group,
        .merge-file-upload .input-group-btn,
        .merge-file-upload .btn-file,
        .merge-file-upload input[type='file'] {
            width: 100%;
            height: 100%;
            cursor: pointer;
        }
        .merge-upload-placeholder {
            text-align: center;
            color: #263238;
            pointer-events: none;
            display: grid;
            gap: 2px;
            justify-items: center;
        }
        .merge-upload-icon {
            color: #a7adb7;
            font-size: 22px;
            line-height: 1;
        }
        .merge-upload-title {
            font-weight: 700;
            font-size: 11px;
            color: #263238;
            max-width: 100%;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .merge-upload-status {
            color: #1e88e5;
            font-size: 11px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
            max-width: 170px;
        }
        .merge-upload-help {
            margin: 6px 0 0 0;
            color: #455a64;
            font-size: 11px;
        }
        .merge-file-name-list {
            margin-top: 6px;
            max-height: 128px;
            overflow-y: auto;
            border: 1px solid #d7dee2;
            background: #ffffff;
        }
        .merge-file-name-row {
            display: grid;
            grid-template-columns: 28px minmax(0, 1fr) 70px 48px;
            gap: 8px;
            align-items: center;
            padding: 5px 8px;
            border-bottom: 1px solid #eef2f4;
            font-size: 11px;
        }
        .merge-file-name-row:last-child {
            border-bottom: none;
        }
        .merge-file-index {
            color: #1e88e5;
            font-weight: 700;
        }
        .merge-file-name {
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
            color: #263238;
        }
        .merge-file-size {
            color: #607d8b;
            text-align: right;
            white-space: nowrap;
        }
        .merge-remove-file-btn {
            padding: 1px 6px;
            font-size: 10px;
            line-height: 1.4;
            color: #455a64;
            border: 1px solid #cfd8dc;
            background: #ffffff;
        }
        .merge-remove-file-btn:hover,
        .merge-remove-file-btn:focus {
            color: #c62828;
            border-color: #ef9a9a;
            background: #fff5f5;
        }
        .merge-file-empty {
            padding: 10px 8px;
            color: #78909c;
            font-size: 11px;
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
        .merge-result-panel {
            max-width: 100%;
            overflow-x: hidden;
        }
        .merge-result-panel .nav-tabs {
            border-bottom: 1px solid #d7dee2;
            margin-bottom: 8px;
        }
        .merge-result-panel .nav-tabs > li > a {
            border: none;
            border-radius: 0;
            margin-right: 26px;
            padding: 8px 2px 9px 2px;
            color: #37474f;
            background: transparent;
            font-size: 12px;
        }
        .merge-result-panel .nav-tabs > li.active > a,
        .merge-result-panel .nav-tabs > li.active > a:hover,
        .merge-result-panel .nav-tabs > li.active > a:focus {
            border: none;
            border-bottom: 2px solid #1e88e5;
            color: #1e88e5;
            background: transparent;
            font-weight: 700;
        }
        .merge-result-panel .nav-tabs > li > a:hover {
            border-color: transparent;
            color: #1e88e5;
            background: transparent;
        }
        .merge-result-panel .tab-content {
            padding-top: 0;
        }
        .merge-result-panel .dataTables_wrapper {
            max-width: 100%;
            overflow-x: auto;
        }
        .merge-plot-card {
            border: 1px solid #b0bec5;
            border-radius: 4px;
            padding: 12px 16px;
            height: 370px;
            overflow-y: auto;
            background-color: #ffffff;
        }
        .merge-plot-card h4 {
            color: #2c3e50;
            margin-top: 0;
            margin-bottom: 10px;
            font-size: 14px;
            font-weight: 700;
        }
        .merge-plot-card hr {
            margin: 4px 0 8px 0;
        }
        .merge-active-plot-box {
            border: none;
            border-radius: 0;
            background-color: transparent;
            min-height: 285px;
            cursor: zoom-in;
        }
        .merge-run-state {
            border: 1px solid #d7dee2;
            background: #ffffff;
            color: #455a64;
            font-size: 11px;
            padding: 6px 8px;
            margin-top: 6px;
        }
        .merge-run-state-active {
            border-color: #90caf9;
            background: #eef7ff;
            color: #1565c0;
            font-weight: 700;
        }
        .merge-run-state-ready {
            border-color: #a5d6a7;
            background: #f1fbf2;
            color: #2e7d32;
        }
        .merge-active-plot-note {
            color: #666;
            font-size: 11px;
            margin: 4px 0 6px 0;
        }
        .merge-summary-inline {
            display: grid;
            grid-template-columns: repeat(4, minmax(0, 1fr));
            gap: 8px;
            margin-bottom: 8px;
        }
        .merge-summary-inline .status-item {
            border: 1px solid #e0e6ea;
            background: #ffffff;
            padding: 6px 8px;
        }
        .merge-summary-inline .label {
            color: #455a64;
            background: transparent;
            padding: 0;
        }
        .merge-result-card {
            border: 1px solid #d0d7de;
            border-radius: 4px;
            padding: 12px 16px;
            min-height: 280px;
            background-color: #ffffff;
        }
        .merge-result-card h4 {
            color: #2c3e50;
            margin-top: 0;
            margin-bottom: 10px;
            font-size: 14px;
            font-weight: 700;
        }
        .merge-result-file-list {
            border: 1px solid #d7dee2;
            background: #ffffff;
        }
        .merge-result-file-row {
            display: grid;
            grid-template-columns: 28px minmax(160px, 1fr) 54px minmax(160px, 1.5fr) 70px;
            gap: 8px;
            align-items: center;
            padding: 6px 8px;
            border-bottom: 1px solid #eef2f4;
            font-size: 11px;
        }
        .merge-result-file-row:last-child {
            border-bottom: none;
        }
        .merge-result-file-action {
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
        .merge-result-file-action:hover,
        .merge-result-file-action:focus {
            color: #0d47a1;
            text-decoration: underline;
            background: transparent;
            box-shadow: none;
        }
        .merge-result-file-name {
            color: #263238;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .merge-result-file-type {
            color: #455a64;
            font-weight: 700;
        }
        .merge-result-file-desc {
            color: #607d8b;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .merge-result-file-download .btn {
            font-size: 10px;
            padding: 1px 8px;
            line-height: 1.4;
        }
        @media (max-width: 992px) {
            .merge-summary-inline {
                grid-template-columns: repeat(2, minmax(0, 1fr));
            }
            .merge-result-file-row {
                grid-template-columns: 28px minmax(120px, 1fr) 44px 64px;
            }
            .merge-result-file-desc {
                display: none;
            }
        }
        .merge-download-size-controls {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px;
            margin-top: 8px;
        }
        .merge-download-size-controls .shiny-input-container {
            width: 100%;
            margin-bottom: 0;
        }
        .merge-download-size-controls label {
            font-size: 10px;
            color: #455a64;
            margin-bottom: 2px;
            font-weight: 500;
        }
        .merge-download-size-controls .form-control {
            height: 24px;
            padding: 2px 4px;
            font-size: 11px;
        }
        .merge-download-modal-note {
            color: #607d8b;
            font-size: 12px;
            margin: 0 0 8px 0;
        }
        .merge-qa {
            font-size: 12px;
            line-height: 1.7;
            color: #455a64;
            max-height: 190px;
            overflow-y: auto;
        }
        .merge-qa dl {
            margin: 0;
        }
        .merge-qa dt {
            margin-top: 8px;
            color: #263238;
        }
        .merge-qa dt:first-child {
            margin-top: 0;
        }
        .merge-qa dd {
            margin-left: 0;
            margin-bottom: 4px;
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
          class = "merge-param-card",
          
          h4("参数设置"),
          hr(),
          
          div(
            class = "merge-upload-field",
            div(
              class = "merge-upload-main",
              div(
                class = "merge-file-upload",
                div(
                  class = "merge-upload-placeholder",
                  span(class = "glyphicon glyphicon-cloud-upload merge-upload-icon"),
                  span("表达矩阵", class = "merge-upload-title"),
                  span("Drop file here or click to upload", class = "merge-upload-status")
                ),
                fileInput(ns("mergeFiles"), NULL,
                          accept = c(".txt", ".tsv", ".csv"),
                          multiple = TRUE,
                          buttonLabel = "浏览",
                          placeholder = "选择表达矩阵文件")
              ),
              p("支持 TXT、TSV、CSV 格式表达矩阵文件", class = "merge-upload-help")
            )
          ),
          uiOutput(ns("mergeUploadedFileNames")),
          p("每次上传会追加到下方列表；文件名将作为批次标识，可点击删除移除单个文件。",
            style = "font-size: 9px; color: #888; margin-top: 2px; margin-bottom: 4px;"),
          
          hr(style = "margin: 3px 0;"),
          
          # ---- 运行按钮 ----
          actionButton(ns("runMerge"), "运行数据集合并与批次校正", 
                       class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 4px 0; margin-bottom: 3px;"),
          uiOutput(ns("mergeRunStatus"))
        )
      ),
      
      # ============================================================
      # 区域二：图片显示（右上）
      # ============================================================
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "merge-plot-card",
          
          h4("图片显示"),
          hr(),
          
          div(
            class = "merge-active-plot-box",
            plotOutput(
              ns("activePlot"),
              height = "285px",
              width = "100%",
              click = ns("activePlot_click")
            )
          ),
          div(
            style = "display: flex; gap: 4px; flex-wrap: wrap; margin-top: 8px;",
            uiOutput(ns("activePlotDownload"))
          )
        )
      )
    ),
    
    # ---- 第二行：区域三 + 区域四合并为结果表 ----
    fluidRow(
      style = "margin: 0;",
      column(
        width = 12,
        style = "padding: 4px;",
        tags$div(
          class = "merge-result-card",
          h4("结果预览"),
          uiOutput(ns("mergeStatus")),
          div(
            class = "merge-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel(
                "结果表",
                div(class = "merge-result-table", uiOutput(ns("resultFileRows")))
              ),
              tabPanel(
                "数据预览",
                DTOutput(ns("mergePreview"))
              ),
              tabPanel(
                "批次信息",
                DTOutput(ns("batchInfoPreview"))
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "merge-qa",
                  tags$dl(
                    tags$dt("Q1：输入文件需要是什么格式？"),
                    tags$dd("每个文件第一列为基因名，后续列为样本表达值。支持 TXT、TSV 和 CSV。"),
                    tags$dt("Q2：批次信息从哪里来？"),
                    tags$dd("模块默认用每个上传文件的文件名作为批次名称，因此建议文件名清晰表示来源或批次。"),
                    tags$dt("Q3：多个数据集怎样合并？"),
                    tags$dd("模块先读取所有表达矩阵，再取共同基因集合，按基因名对齐后横向合并样本。"),
                    tags$dt("Q4：批次校正使用什么方法？"),
                    tags$dd("当前使用 limma::removeBatchEffect 对合并矩阵进行批次效应校正。"),
                    tags$dt("Q5：校正后的矩阵会传给后续模块吗？"),
                    tags$dd("会。运行成功后，校正后的表达矩阵会写入共享状态，后续模块可继续接入。")
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
merge_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储结果 ----
    merge_results <- reactiveVal(NULL)
    merge_status <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    active_plot <- reactiveVal("boxplot")
    download_plot <- reactiveVal("boxplot")

    plot_download_defaults <- function(plot_key) {
      if (identical(plot_key, "combined")) {
        return(list(width = 10, height = 8))
      }
      list(width = 7, height = 4)
    }

    plot_download_filename <- function(plot_key) {
      switch(
        plot_key,
        boxplot = "boxplot_comparison.png",
        pca = "pca_comparison.png",
        combined = "combined_figure.png",
        "batch_effect_figure.png"
      )
    }

    show_plot_download_modal <- function(plot_key) {
      download_plot(plot_key)
      defaults <- plot_download_defaults(plot_key)

      showModal(
        modalDialog(
          title = paste0("下载", merge_plot_label(plot_key)),
          p("设置导出图片尺寸后点击下载。单位为英寸，DPI 用于控制分辨率。", class = "merge-download-modal-note"),
          div(
            class = "merge-download-size-controls",
            numericInput(ns("downloadPlotWidth"), "宽(in)", value = defaults$width, min = 2, max = 20, step = 0.5),
            numericInput(ns("downloadPlotHeight"), "高(in)", value = defaults$height, min = 2, max = 20, step = 0.5),
            numericInput(ns("downloadPlotDpi"), "DPI", value = 300, min = 72, max = 600, step = 50)
          ),
          footer = tagList(
            modalButton("取消"),
            downloadButton(ns("downloadModalPNG"), "下载PNG", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    }

    get_plot_download_size <- function(default_width = 7, default_height = 4) {
      clean_number <- function(value, default, min_value, max_value) {
        value <- suppressWarnings(as.numeric(value))
        if (length(value) != 1 || is.na(value)) {
          value <- default
        }
        max(min_value, min(max_value, value))
      }

      list(
        width = clean_number(input$downloadPlotWidth, default_width, 2, 20),
        height = clean_number(input$downloadPlotHeight, default_height, 2, 20),
        dpi = as.integer(round(clean_number(input$downloadPlotDpi, 300, 72, 600)))
      )
    }

    write_merge_plot_png <- function(file, plot_key, default_width = 7, default_height = 4) {
      size <- get_plot_download_size(default_width, default_height)
      res <- merge_results()

      if (is.null(res)) {
        grDevices::png(
          file,
          width = size$width * size$dpi,
          height = size$height * size$dpi,
          res = size$dpi
        )
        on.exit(grDevices::dev.off(), add = TRUE)
        merge_blank_plot()
        return(invisible(NULL))
      }

      ggplot2::ggsave(
        file,
        merge_figure_by_key(res, plot_key),
        width = size$width,
        height = size$height,
        dpi = size$dpi
      )
    }

    empty_uploads <- function() {
      data.frame(
        upload_id = integer(),
        name = character(),
        size = numeric(),
        type = character(),
        datapath = character(),
        stringsAsFactors = FALSE
      )
    }
    uploaded_files <- reactiveVal(empty_uploads())
    upload_id_counter <- reactiveVal(0L)
    observed_remove_ids <- reactiveVal(integer())

    observe({
      if (isTRUE(is_running())) {
        shinyjs::disable("runMerge")
      } else {
        shinyjs::enable("runMerge")
      }
    })

    output$mergeRunStatus <- renderUI({
      files <- uploaded_files()
      file_count <- if (is.null(files)) 0L else nrow(files)

      if (isTRUE(is_running())) {
        return(div(class = "merge-run-state merge-run-state-active", "正在后台运行，请稍候..."))
      }
      if (file_count < 2) {
        return(div(class = "merge-run-state", sprintf("已选择 %d 个文件，至少需要 2 个表达矩阵文件。", file_count)))
      }
      div(class = "merge-run-state merge-run-state-ready", sprintf("已选择 %d 个文件，可以开始运行。", file_count))
    })

    observeEvent(input$mergeFiles, {
      new_files <- input$mergeFiles
      if (is.null(new_files) || !nrow(new_files)) {
        return()
      }

      current_id <- upload_id_counter()
      stored_files <- lapply(seq_len(nrow(new_files)), function(i) {
        upload_id <- current_id + i
        ext <- tools::file_ext(new_files$name[[i]])
        stored_path <- tempfile(
          pattern = paste0("merge_upload_", upload_id, "_"),
          fileext = if (nzchar(ext)) paste0(".", ext) else ""
        )
        ok <- file.copy(new_files$datapath[[i]], stored_path, overwrite = TRUE)
        if (!isTRUE(ok)) {
          stop(sprintf("上传文件缓存失败：%s", new_files$name[[i]]), call. = FALSE)
        }

        data.frame(
          upload_id = upload_id,
          name = new_files$name[[i]],
          size = new_files$size[[i]],
          type = new_files$type[[i]] %||% "",
          datapath = stored_path,
          stringsAsFactors = FALSE
        )
      })

      upload_id_counter(current_id + nrow(new_files))
      uploaded_files(rbind(uploaded_files(), do.call(rbind, stored_files)))
    }, ignoreInit = TRUE)

    observe({
      files <- uploaded_files()
      if (!nrow(files)) {
        return()
      }

      current_ids <- files$upload_id
      new_ids <- setdiff(current_ids, observed_remove_ids())
      if (!length(new_ids)) {
        return()
      }

      for (id in new_ids) {
        local({
          remove_id <- id
          observeEvent(input[[paste0("removeMergeFile_", remove_id)]], {
            current <- uploaded_files()
            row_index <- which(current$upload_id == remove_id)
            if (!length(row_index)) {
              return()
            }

            unlink(current$datapath[row_index])
            uploaded_files(current[-row_index, , drop = FALSE])
          }, ignoreInit = TRUE)
        })
      }

      observed_remove_ids(unique(c(observed_remove_ids(), new_ids)))
    })

    output$mergeUploadedFileNames <- renderUI({
      files <- uploaded_files()
      if (is.null(files) || !nrow(files)) {
        return(div(class = "merge-file-name-list", div(class = "merge-file-empty", "尚未选择文件。")))
      }

      div(
        class = "merge-file-name-list",
        lapply(seq_len(nrow(files)), function(i) {
          div(
            class = "merge-file-name-row",
            span(sprintf("%02d", i), class = "merge-file-index"),
            span(files$name[[i]], class = "merge-file-name", title = files$name[[i]]),
            span(merge_format_file_size(files$size[[i]]), class = "merge-file-size"),
            actionButton(
              ns(paste0("removeMergeFile_", files$upload_id[[i]])),
              "删除",
              class = "merge-remove-file-btn",
              title = "删除这个文件"
            )
          )
        })
      )
    })
    
    # ============================================================
    # 使用说明弹窗
    # ============================================================
    observeEvent(input$helpBtn, {
      showModal(
        modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span("数据集合并与批次校正 - 使用说明", style = "font-size: 18px; font-weight: bold;"),
            tags$span("v1.0", style = "font-size: 12px; color: #999;")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "max-height: 70vh; overflow-y: auto; padding-right: 10px; font-size: 14px; line-height: 1.8;",
            tags$hr(style = "margin: 8px 0;"),
            
            tags$h5("1. 功能目的", style = "color: #3498db;"),
            tags$p("当您的数据来自多个批次（如不同时间测序、不同平台、不同实验室）时，",
                   "直接合并会引入", tags$strong("批次效应"), "，影响后续差异分析和机器学习的结果准确性。",
                   "本模块通过 ", tags$strong("limma::removeBatchEffect"), " 方法去除批次效应，",
                   "使不同批次的数据可以合并使用。"),
            
            tags$h5("2. 输入要求", style = "color: #2ecc71;"),
            tags$ul(
              tags$li(tags$strong("文件格式"), "：TXT 或 CSV 格式"),
              tags$li(tags$strong("第一列"), "：基因名"),
              tags$li(tags$strong("其他列"), "：样本表达值"),
              tags$li(tags$strong("文件命名"), "：文件名将作为批次标识"),
              tags$li(tags$strong("支持多个文件"), "：同时选择所有批次文件")
            ),
            
            tags$h5("3. 注意事项", style = "color: #e74c3c;"),
            tags$ul(
              tags$li("所有文件必须包含 ", tags$strong("相同的基因名列表")),
              tags$li("基因名不能有重复值"),
              tags$li("表达值应为 ", tags$strong("数值型")),
              tags$li("建议样本总数不少于 6 个")
            ),
            
            tags$h5("4. 输出文件", style = "color: #9b59b6;"),
            tags$ul(
              tags$li(tags$strong("geneMatrix.txt"), "：批次校正后的表达矩阵"),
              tags$li(tags$strong("sample_batch_info.txt"), "：样本与批次的对应关系表"),
              tags$li(tags$strong("箱线图"), "：校正前后表达值分布对比"),
              tags$li(tags$strong("PCA图"), "：校正前后样本聚类对比"),
              tags$li(tags$strong("组合图"), "：四图合一")
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
    
    # ---- 核心分析 ----
    observeEvent(input$runMerge, {
      if (isTRUE(is_running())) {
        showNotification("数据集合并正在运行，请等待当前任务完成。", type = "warning")
        return()
      }

      files <- uploaded_files()
      
      if (is.null(files) || nrow(files) < 2) {
        showNotification("请上传至少两个表达矩阵文件！", type = "error")
        return()
      }

      file_paths <- files$datapath
      file_names <- files$name
      is_running(TRUE)
      merge_results(NULL)
      merge_status(NULL)
      task_note <- app_start_task_notification("数据集合并正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          merge_process_files(file_paths, file_names)
        },
        on_success = function(processed) {
          app_clear_task_notification(task_note)
          shared_data <- session$userData$shared_data
          if (!is.null(shared_data)) {
            update_shared_matrix_state(
              shared_data,
              processed$corrected_expr,
              source = "merge",
              validation = processed$validation
            )
          }

          merge_results(processed)
          active_plot("boxplot")
          merge_status(list(
            genes = processed$n_genes,
            samples = processed$n_samples,
            batches = processed$n_batches,
            batch_names = paste(processed$batch_names, collapse = ", ")
          ))

          showNotification(
            paste0("合并完成：", processed$n_genes, " 个基因，",
                   processed$n_samples, " 个样本，",
                   processed$n_batches, " 个批次"),
            type = "message",
            duration = 5
          )
        },
        on_error = function(error) {
          app_clear_task_notification(task_note)
          showNotification(paste0("错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        on_finally = function() {
          app_clear_task_notification(task_note)
          is_running(FALSE)
        }
      )
    })

    result_files_data <- reactive({
      merge_result_files_table(!is.null(merge_results()))
    })

    output$resultFileRows <- renderUI({
      if (is.null(merge_results())) {
        return(
          div(
            class = "merge-result-file-list",
            div(class = "merge-file-empty", "暂无结果文件。请先运行数据集合并。")
          )
        )
      }

      table_data <- result_files_data()
      text_download_ids <- c(
        "downloadMerged",
        "downloadMergedCSV",
        "downloadBatchInfo"
      )

      div(
        class = "merge-result-file-list",
        lapply(seq_len(nrow(table_data)), function(i) {
          file_name <- table_data[["文件名"]][[i]]
          file_type <- table_data[["类型"]][[i]]
          file_desc <- table_data[["说明"]][[i]]
          plot_key <- table_data[["plot_key"]][[i]]

          file_cell <- if (nzchar(plot_key)) {
            actionButton(
              ns(paste0("showResultFile_", plot_key)),
              file_name,
              class = "merge-result-file-action",
              title = "点击后在上方图片区预览"
            )
          } else {
            span(file_name, class = "merge-result-file-name", title = file_name)
          }

          download_control <- if (nzchar(plot_key)) {
            actionButton(
              ns(paste0("openResultDownload_", plot_key)),
              "下载",
              class = "btn btn-default btn-xs",
              style = "font-size: 10px; padding: 1px 8px;"
            )
          } else {
            downloadButton(
              ns(text_download_ids[[i]]),
              "下载",
              style = "font-size: 10px; padding: 1px 8px;"
            )
          }

          div(
            class = "merge-result-file-row",
            span(sprintf("%02d", i), class = "merge-file-index"),
            file_cell,
            span(file_type, class = "merge-result-file-type"),
            span(file_desc, class = "merge-result-file-desc", title = file_desc),
            span(
              class = "merge-result-file-download",
              download_control
            )
          )
        })
      )
    })

    observeEvent(input$showResultFile_boxplot, {
      if (is.null(merge_results())) {
        return()
      }
      active_plot("boxplot")
    }, ignoreInit = TRUE)

    observeEvent(input$showResultFile_pca, {
      if (is.null(merge_results())) {
        return()
      }
      active_plot("pca")
    }, ignoreInit = TRUE)

    observeEvent(input$showResultFile_combined, {
      if (is.null(merge_results())) {
        return()
      }
      active_plot("combined")
    }, ignoreInit = TRUE)

    observeEvent(input$openResultDownload_boxplot, {
      if (is.null(merge_results())) {
        return()
      }
      show_plot_download_modal("boxplot")
    }, ignoreInit = TRUE)

    observeEvent(input$openResultDownload_pca, {
      if (is.null(merge_results())) {
        return()
      }
      show_plot_download_modal("pca")
    }, ignoreInit = TRUE)

    observeEvent(input$openResultDownload_combined, {
      if (is.null(merge_results())) {
        return()
      }
      show_plot_download_modal("combined")
    }, ignoreInit = TRUE)

    output$activePlot <- renderPlot({
      res <- merge_results()
      if (is.null(res)) {
        merge_blank_plot()
        return()
      }

      print(merge_figure_by_key(res, active_plot()))
    })

    output$activePlotLarge <- renderPlot({
      res <- merge_results()
      if (is.null(res)) {
        merge_blank_plot()
        return()
      }

      print(merge_figure_by_key(res, active_plot()))
    })

    output$activePlotDownload <- renderUI({
      if (is.null(merge_results())) {
        return(NULL)
      }

      actionButton(
        ns("openPlotDownload"),
        "下载当前PNG",
        class = "btn btn-default btn-xs",
        style = "font-size: 10px; padding: 1px 8px;"
      )
    })

    observeEvent(input$openPlotDownload, {
      if (is.null(merge_results())) {
        return()
      }
      show_plot_download_modal(active_plot())
    }, ignoreInit = TRUE)

    output$downloadModalPNG <- downloadHandler(
      filename = function() {
        plot_download_filename(download_plot())
      },
      content = function(file) {
        plot_key <- download_plot()
        defaults <- plot_download_defaults(plot_key)
        write_merge_plot_png(file, plot_key, defaults$width, defaults$height)
      }
    )

    observeEvent(input$activePlot_click, {
      if (is.null(merge_results())) {
        return()
      }

      showModal(
        modalDialog(
          title = paste0(merge_plot_label(active_plot()), " - 放大预览"),
          plotOutput(ns("activePlotLarge"), height = "70vh", width = "100%"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭")
        )
      )
    })
    
    # ---- 数据状态 ----
    output$mergeStatus <- renderUI({
      status <- merge_status()
      if (is.null(status)) {
        return(p("暂无数据", style = "color: #999; font-size: 12px; padding: 4px 0; margin: 0 0 6px 0;"))
      }
      
      div(
        class = "merge-summary-inline",
        div(class = "status-item",
            span("基因数", class = "label"),
            span(status$genes)
        ),
        div(class = "status-item",
            span("样本数", class = "label"),
            span(status$samples)
        ),
        div(class = "status-item",
            span("批次数量", class = "label"),
            span(status$batches)
        ),
        div(class = "status-item",
            span("批次名称", class = "label"),
            span(status$batch_names, style = "font-size: 11px; color: #666;")
        )
      )
    })
    
    # ---- 结果文件列表 ----
    output$mergeFileList <- renderUI({
      res <- merge_results()
      if (is.null(res)) {
        return(p("暂无结果文件", style = "color: #999; font-size: 13px; text-align: center; padding: 20px 0;"))
      }
      
      tagList(
        div(class = "file-item",
            span("geneMatrix.txt"),
            tags$span("校正后数据", style = "font-size: 10px; color: #888;")
        ),
        div(class = "file-item",
            span("sample_batch_info.txt"),
            tags$span("批次信息", style = "font-size: 10px; color: #888;")
        ),
        div(class = "file-item",
            span("boxplot_comparison.png"),
            tags$span("箱线图对比", style = "font-size: 10px; color: #888;")
        ),
        div(class = "file-item",
            span("pca_comparison.png"),
            tags$span("PCA对比", style = "font-size: 10px; color: #888;")
        ),
        div(class = "file-item",
            span("combined_figure.png"),
            tags$span("组合图", style = "font-size: 10px; color: #888;")
        )
      )
    })

    output$mergePreview <- DT::renderDT({
      res <- merge_results()
      preview_data <- if (is.null(res)) {
        merge_empty_table("请先运行数据集合并。")
      } else {
        utils::head(res$corrected_data, 20)
      }

      app_preview_datatable(
        preview_data,
        rownames = FALSE,
        options = list(dom = "tip")
      )
    })

    output$batchInfoPreview <- DT::renderDT({
      res <- merge_results()
      preview_data <- if (is.null(res)) {
        merge_empty_table("请先运行数据集合并。")
      } else {
        res$batch_table
      }

      app_preview_datatable(
        preview_data,
        rownames = FALSE,
        options = list(dom = "tip")
      )
    })
    
    # ============================================================
    # 下载功能
    # ============================================================
    
    output$downloadMerged <- downloadHandler(
      filename = "geneMatrix.txt",
      content = function(file) {
        res <- merge_results()
        if (is.null(res)) {
          writeLines("请先运行数据集合并", file)
          return()
        }
        write.table(res$corrected_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
    
    output$downloadMergedCSV <- downloadHandler(
      filename = "geneMatrix.csv",
      content = function(file) {
        res <- merge_results()
        if (is.null(res)) {
          writeLines("请先运行数据集合并", file)
          return()
        }
        write.csv(res$corrected_data, file, row.names = FALSE, quote = FALSE)
      }
    )
    
    output$downloadBatchInfo <- downloadHandler(
      filename = "sample_batch_info.txt",
      content = function(file) {
        res <- merge_results()
        if (is.null(res)) {
          writeLines("请先运行数据集合并", file)
          return()
        }
        sample_batch_df <- data.frame(
          Sample = colnames(res$expr_matrix),
          Batch = res$batch_info,
          stringsAsFactors = FALSE
        )
        write.table(sample_batch_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
  })
}
