# ============================================================
# 免疫浸润模块 (modules/immune_module.R) - 修复版本
# ============================================================

# ============================================================
# 1. 内置LM22基因特征矩阵
# ============================================================

#' 获取LM22细胞类型名称
get_lm22_celltypes <- function() {
  c("B cells naive", "B cells memory", "Plasma cells",
    "T cells CD8", "T cells CD4 naive", "T cells CD4 memory resting",
    "T cells CD4 memory activated", "T cells follicular helper",
    "T cells regulatory (Tregs)", "T cells gamma delta",
    "NK cells resting", "NK cells activated",
    "Monocytes", "Macrophages M0", "Macrophages M1", "Macrophages M2",
    "Dendritic cells resting", "Dendritic cells activated",
    "Mast cells resting", "Mast cells activated",
    "Eosinophils", "Neutrophils")
}

#' 获取内置LM22基因特征矩阵
get_lm22_signature <- function() {
  lm22_file <- "data/refer.txt"
  
  if (file.exists(lm22_file)) {
    lm22 <- read.table(lm22_file, header = TRUE, sep = "\t", 
                       check.names = FALSE, row.names = 1)
    return(as.matrix(lm22))
  }
  
  message("LM22文件不存在，使用模拟数据...")
  return(generate_mock_lm22())
}

#' 生成模拟LM22矩阵
generate_mock_lm22 <- function() {
  set.seed(123)
  n_genes <- 500
  n_cells <- 22
  
  mat <- matrix(rnorm(n_genes * n_cells, mean = 10, sd = 3), 
                nrow = n_genes, ncol = n_cells)
  
  for (i in 1:n_cells) {
    idx <- ((i-1)*20 + 1):min(i*20, n_genes)
    mat[idx, i] <- mat[idx, i] + 5
  }
  
  mat[mat < 0] <- 0.1
  rownames(mat) <- paste0("Gene", 1:n_genes)
  colnames(mat) <- get_lm22_celltypes()
  
  return(mat)
}

# ============================================================
# 2. CIBERSORT核心算法
# ============================================================

core_algorithm <- function(X, y, nu_values = c(0.25, 0.5, 0.75), fast = TRUE) {
  
  X <- as.matrix(X)
  y <- as.numeric(y)
  
  if (any(is.na(y))) {
    y[is.na(y)] <- mean(y, na.rm = TRUE)
  }

  if (isTRUE(fast)) {
    fit <- tryCatch(stats::lm.fit(x = X, y = y), error = function(e) NULL)
    if (!is.null(fit)) {
      result <- as.numeric(fit$coefficients)
      result[is.na(result)] <- 0
      result[result < 0] <- 0
      if (sum(result) > 0) {
        result <- result / sum(result)
      }
      if (length(result) < ncol(X)) {
        result <- c(result, rep(0, ncol(X) - length(result)))
      }
      return(result[1:ncol(X)])
    }
  }
  
  best_rmse <- Inf
  best_result <- NULL
  
  for (nu in nu_values) {
    tryCatch({
      model <- e1071::svm(x = X, y = y,
                          type = "eps-regression",
                          kernel = "linear",
                          nu = nu,
                          scale = FALSE)
      
      pred <- predict(model, X)
      rmse <- sqrt(mean((pred - y)^2, na.rm = TRUE))
      
      coefs <- t(model$coefs) %*% model$SV
      intercept <- model$rho
      result <- c(coefs, intercept)
      
      result[result < 0] <- 0
      if (sum(result) > 0) {
        result <- result / sum(result)
      }
      
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_result <- result
      }
    }, error = function(e) { NULL })
  }
  
  if (is.null(best_result)) {
    lm_model <- lm(y ~ X - 1)
    best_result <- coef(lm_model)
    best_result[best_result < 0] <- 0
    if (sum(best_result) > 0) {
      best_result <- best_result / sum(best_result)
    }
  }
  
  if (length(best_result) < ncol(X)) {
    best_result <- c(best_result, rep(0, ncol(X) - length(best_result)))
  }
  
  return(best_result[1:ncol(X)])
}

# ============================================================
# 3. CIBERSORT主函数
# ============================================================

run_cibersort <- function(expr_matrix, lm22_matrix = NULL, 
                          quantile_normalize = TRUE,
                          perm_num = 100,
                          fast = TRUE) {
  
  message(">>> 运行 CIBERSORT ...")
  
  if (is.null(lm22_matrix)) {
    lm22_matrix <- get_lm22_signature()
  }
  
  if (is.null(lm22_matrix)) {
    stop("LM22矩阵未找到")
  }
  
  common_genes <- intersect(rownames(expr_matrix), rownames(lm22_matrix))
  if (length(common_genes) < 10) {
    common_genes <- rownames(expr_matrix)[1:min(nrow(expr_matrix), nrow(lm22_matrix))]
    warning("共同基因太少，使用前", length(common_genes), "个基因")
  }
  
  message(paste("共同基因:", length(common_genes), "个"))
  
  expr_common <- expr_matrix[common_genes, , drop = FALSE]
  sig_common <- lm22_matrix[common_genes, , drop = FALSE]
  
  if (quantile_normalize && requireNamespace("limma", quietly = TRUE)) {
    expr_common <- limma::normalizeQuantiles(expr_common)
  }
  
  n_samples <- ncol(expr_common)
  n_cell_types <- ncol(sig_common)
  cell_names <- colnames(sig_common)
  sample_names <- colnames(expr_common)
  
  fractions <- matrix(0, nrow = n_samples, ncol = n_cell_types)
  colnames(fractions) <- cell_names
  rownames(fractions) <- sample_names
  
  pb <- txtProgressBar(min = 0, max = n_samples, style = 3)
  
  for (i in 1:n_samples) {
    sample_expr <- expr_common[, i]
    fractions[i, ] <- core_algorithm(sig_common, sample_expr, fast = fast)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  message("CIBERSORT 完成")
  
  return(list(
    fractions = fractions,
    cell_types = cell_names,
    samples = sample_names
  ))
}

# ============================================================
# 4. ESTIMATE评分
# ============================================================

get_estimate_genes <- function() {
  list(
    immune = c("CD2", "CD3D", "CD3E", "CD3G", "CD4", "CD8A", "CD8B",
               "CD28", "CTLA4", "ICOS", "PDCD1", "LAG3", "TIGIT",
               "CD19", "MS4A1", "CD79A", "CD79B", "PAX5", "BLK",
               "KLRD1", "KLRK1", "NKG7", "FCGR3A", "NCR1",
               "CD68", "CD163", "MSR1", "MARCO", "ITGAM", "CSF1R",
               "TNF", "IFNG", "IL2", "IL4", "IL6", "IL10", "IL12A",
               "TGFB1", "CCL2", "CCL3", "CCL4", "CCL5", "CXCL8", "CXCL9", "CXCL10"),
    stromal = c("ACTA2", "COL1A1", "COL1A2", "COL3A1", "COL4A1", "COL5A1",
                "FAP", "PDGFRA", "PDGFRB", "TGFBR1", "TGFBR2",
                "CD31", "CD34", "CDH5", "ENG", "KDR", "TEK", "TIE1",
                "VEGFA", "VEGFB", "VEGFC", "VEGFR1", "VEGFR2",
                "MMP1", "MMP2", "MMP3", "MMP9", "TIMP1", "TIMP2",
                "FN1", "LAMA1", "LAMA2", "LAMA3", "LAMB1", "LAMC1")
  )
}

run_estimate <- function(expr_matrix) {
  
  message(">>> 运行 ESTIMATE ...")
  
  gene_lists <- get_estimate_genes()
  
  immune_available <- gene_lists$immune[gene_lists$immune %in% rownames(expr_matrix)]
  stromal_available <- gene_lists$stromal[gene_lists$stromal %in% rownames(expr_matrix)]
  
  message(paste("可用免疫基因:", length(immune_available)))
  message(paste("可用基质基因:", length(stromal_available)))
  
  n_samples <- ncol(expr_matrix)
  sample_names <- colnames(expr_matrix)
  
  immune_scores <- numeric(n_samples)
  stromal_scores <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    sample_expr <- expr_matrix[, i]
    if (length(immune_available) > 0) {
      immune_scores[i] <- mean(sample_expr[immune_available], na.rm = TRUE)
    }
    if (length(stromal_available) > 0) {
      stromal_scores[i] <- mean(sample_expr[stromal_available], na.rm = TRUE)
    }
  }
  
  estimate_scores <- immune_scores + stromal_scores
  tumor_purity <- cos(0.6049872018 + 0.0001467804 * estimate_scores)
  tumor_purity <- pmax(0, pmin(1, tumor_purity))
  
  results <- data.frame(
    Sample = sample_names,
    Immune_Score = immune_scores,
    Stromal_Score = stromal_scores,
    ESTIMATE_Score = estimate_scores,
    Tumor_Purity = tumor_purity
  )
  
  message("ESTIMATE 完成")
  return(results)
}

# ============================================================
# 5. 可视化函数 (修复ggplot2警告)
# ============================================================

# 检测ggplot2版本
ggplot2_version <- as.numeric_version(packageVersion("ggplot2"))

plot_immune_heatmap <- function(fractions, group_info = NULL) {
  
  cell_means <- colMeans(fractions, na.rm = TRUE)
  top_cells <- names(sort(cell_means, decreasing = TRUE))[1:min(15, ncol(fractions))]
  fractions_sub <- fractions[, top_cells, drop = FALSE]
  
  annotation_col <- NULL
  ann_colors <- NULL
  
  if (!is.null(group_info)) {
    annotation_col <- data.frame(Group = group_info)
    rownames(annotation_col) <- rownames(fractions)
    ann_colors <- list(Group = c("Control" = "#4CAF50", "Treat" = "#E74C3C"))
  }
  
  pheatmap::pheatmap(
    t(fractions_sub),
    scale = "column",
    main = "免疫细胞浸润热图",
    fontsize = 10,
    fontsize_row = 9,
    fontsize_col = 7,
    annotation_col = annotation_col,
    annotation_colors = ann_colors,
    color = colorRampPalette(c("#313695", "#FFFFBF", "#D73027"))(100),
    border_color = NA,
    cluster_rows = TRUE,
    cluster_cols = TRUE
  )
}

plot_immune_boxplot <- function(fractions, group_info) {
  
  data_long <- reshape2::melt(fractions,
                              varnames = c("Sample", "CellType"),
                              value.name = "Fraction")
  data_long$Group <- group_info[data_long$Sample]
  
  cell_means <- colMeans(fractions, na.rm = TRUE)
  top_cells <- names(sort(cell_means, decreasing = TRUE))[1:min(10, ncol(fractions))]
  data_long <- data_long[data_long$CellType %in% top_cells, ]
  
  # 修复size/linewidth警告 - 根据ggplot2版本选择参数
  p <- ggpubr::ggboxplot(
    data_long,
    x = "CellType",
    y = "Fraction",
    fill = "Group",
    palette = c("Control" = "#4CAF50", "Treat" = "#E74C3C"),
    xlab = "",
    ylab = "免疫细胞丰度",
    legend.title = "分组",
    width = 0.7,
    outlier.shape = NA
  ) +
    ggpubr::stat_compare_means(
      aes(group = Group),
      label = "p.signif",
      method = "wilcox.test",
      symnum.args = list(
        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
        symbols = c("***", "**", "*", "ns")
      ),
      size = 4
    ) +
    ggplot2::geom_jitter(
      aes(color = Group),
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
      size = 1.5,
      alpha = 0.6
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      # 修复element_line的size警告
      axis.line = element_line(linewidth = 0.5),
      axis.ticks = element_line(linewidth = 0.5)
    ) +
    ggplot2::labs(title = "免疫细胞组间比较")
  
  return(p)
}

plot_immune_correlation <- function(fractions) {
  cor_matrix <- cor(fractions, method = "spearman", use = "pairwise.complete.obs")
  corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex = 0.4,
    diag = FALSE,
    title = "免疫细胞相关性 (Spearman)",
    mar = c(0, 0, 2, 0)
  )
}

plot_immune_lollipop <- function(corr_results, gene_name) {
  
  plot_data <- corr_results[corr_results$gene == gene_name, ]
  
  if (nrow(plot_data) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, paste("基因", gene_name, "未找到"), cex = 1.2)
    return(invisible())
  }
  
  plot_data <- plot_data[order(plot_data$r), ]
  plot_data$cell_type <- factor(plot_data$cell_type, levels = plot_data$cell_type)
  
  plot_data$color <- ifelse(plot_data$p < 0.001, "#E74C3C",
                            ifelse(plot_data$p < 0.01, "#F39C12",
                                   ifelse(plot_data$p < 0.05, "#2ECC71", "#95A5A6")))
  
  plot_data$size <- ifelse(abs(plot_data$r) >= 0.5, 6,
                           ifelse(abs(plot_data$r) >= 0.3, 5, 4))
  
  plot_data$signif <- ifelse(plot_data$p < 0.001, "***",
                             ifelse(plot_data$p < 0.01, "**",
                                    ifelse(plot_data$p < 0.05, "*", "")))
  
  x_limit <- max(abs(plot_data$r)) * 1.2
  
  p <- ggplot2::ggplot(plot_data, aes(x = r, y = cell_type)) +
    # 修复geom_segment的size警告
    geom_segment(aes(x = 0, xend = r, y = cell_type, yend = cell_type),
                 color = "#BDC3C7", linewidth = 1) +
    geom_point(aes(color = color, size = size), alpha = 0.85) +
    geom_text(aes(label = signif, x = r + sign(r) * 0.02 * x_limit),
              size = 5, color = "black", hjust = 0.5) +
    scale_color_identity() +
    scale_size_identity() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#95A5A6") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.3)
    ) +
    labs(
      title = paste(gene_name, "与免疫细胞相关性"),
      x = "Spearman相关系数 (r)",
      y = ""
    )
  
  return(p)
}

# ============================================================
# 6. 主分析函数
# ============================================================

run_immune_analysis <- function(expr_matrix, group_info, 
                                target_genes = NULL,
                                methods = "all",
                                lm22_matrix = NULL,
                                perm_num = 100,
                                fast_cibersort = TRUE) {
  
  message("\n========================================")
  message("  免疫浸润分析")
  message("========================================\n")
  
  results <- list()
  
  if (methods %in% c("cibersort", "all")) {
    if (is.null(lm22_matrix)) {
      lm22_matrix <- get_lm22_signature()
    }
    if (!is.null(lm22_matrix)) {
      results$cibersort <- run_cibersort(expr_matrix, lm22_matrix, perm_num = perm_num, fast = fast_cibersort)
    }
  }
  
  if (methods %in% c("estimate", "all")) {
    results$estimate <- run_estimate(expr_matrix)
  }
  
  if (!is.null(target_genes) && !is.null(results$cibersort)) {
    fractions <- results$cibersort$fractions
    corr_results <- data.frame()
    
    for (gene in target_genes) {
      if (gene %in% rownames(expr_matrix)) {
        gene_expr <- as.numeric(expr_matrix[gene, ])
        for (ct in colnames(fractions)) {
          test <- cor.test(gene_expr, fractions[, ct], method = "spearman")
          corr_results <- rbind(corr_results, data.frame(
            gene = gene,
            cell_type = ct,
            r = as.numeric(test$estimate),
            p = as.numeric(test$p.value)
          ))
        }
      }
    }
    results$correlations <- corr_results
  }
  
  return(results)
}

# ============================================================
# 7. UI 和 Server (修复showNotification警告)
# ============================================================

immune_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("🛡️ 免疫浸润分析", style = "margin-bottom: 20px; font-weight: 600;"),
    hr(),
    
    fluidRow(
      column(
        width = 12,
        div(
          class = "panel panel-primary",
          div(class = "panel-heading", "⚙️ 分析参数设置"),
          div(
            class = "panel-body",
            fluidRow(
              column(3,
                     selectInput(ns("method"), "分析方法",
                                 choices = c("全部" = "all",
                                             "CIBERSORT" = "cibersort",
                                             "ESTIMATE" = "estimate"),
                                 selected = "all")
              ),
              column(3,
                     textInput(ns("target_genes"), "目标基因 (逗号分隔)",
                               placeholder = "如: TP53, BRCA1, EGFR",
                               value = "TP53, BRCA1")
              ),
              column(3,
                     numericInput(ns("perm_num"), "置换次数",
                                  value = 100, min = 10, max = 1000, step = 10)
              ),
              column(3,
                     checkboxGroupInput(ns("outputs"), "输出图表",
                                        choices = c("热图" = "heatmap",
                                                    "箱线图" = "boxplot",
                                                    "相关性图" = "correlation",
                                                    "棒棒糖图" = "lollipop"),
                                        selected = c("heatmap", "boxplot", 
                                                     "correlation", "lollipop"),
                                        inline = FALSE)
              )
            ),
            hr(),
            fluidRow(
              column(12,
                     actionButton(ns("run"), "🚀 运行分析", 
                                  class = "btn-success", style = "margin-right: 10px;")
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
             div(class = "well", style = "padding: 10px;",
                 textOutput(ns("status"))
             )
      )
    ),
    
    fluidRow(
      column(6,
             div(class = "panel panel-info",
                 div(class = "panel-heading", "🛡️ 免疫细胞浸润热图"),
                 div(class = "panel-body", style = "min-height: 420px;",
                     plotOutput(ns("heatmap"), height = "400px")
                 )
             )
      ),
      column(6,
             div(class = "panel panel-success",
                 div(class = "panel-heading", "📊 组间免疫细胞比较"),
                 div(class = "panel-body", style = "min-height: 420px;",
                     plotOutput(ns("boxplot"), height = "400px")
                 )
             )
      )
    ),
    
    fluidRow(
      column(6,
             div(class = "panel panel-warning",
                 div(class = "panel-heading", "🔗 免疫细胞相关性"),
                 div(class = "panel-body", style = "min-height: 420px;",
                     plotOutput(ns("correlation"), height = "400px")
                 )
             )
      ),
      column(6,
             div(class = "panel panel-danger",
                 div(class = "panel-heading", "🍭 基因-免疫细胞相关性"),
                 div(class = "panel-body", style = "min-height: 420px;",
                     plotOutput(ns("lollipop"), height = "400px")
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading", "📋 免疫浸润结果汇总"),
                 div(class = "panel-body", style = "overflow-x: auto;",
                     DTOutput(ns("results_table"))
                 )
             )
      )
    )
  )
}

# ============================================================
# 8. 机器学习风格紧凑布局覆盖版
# ============================================================
immune_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .immune-card,
      .immune-plot-card,
      .immune-result-card {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        padding: 12px 16px;
        background-color: #ffffff;
      }
      .immune-card,
      .immune-plot-card {
        height: 370px;
        overflow-y: auto;
      }
      .immune-result-card {
        height: 300px;
        overflow: hidden;
      }
      .immune-card h4,
      .immune-plot-card h4,
      .immune-result-card h4 {
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 14px;
        font-weight: 700;
      }
      .immune-card hr,
      .immune-plot-card hr {
        margin: 4px 0 8px 0;
      }
      .immune-card .form-control {
        font-size: 11px;
        padding: 2px 4px;
        height: 26px;
      }
      .immune-card .shiny-input-container {
        margin-bottom: 6px;
      }
      .immune-card label {
        font-size: 11px;
        color: #263238;
        margin-bottom: 3px;
      }
      .immune-compact-section {
        border: 1px solid #d7dee2;
        background: #ffffff;
        padding: 6px 8px;
        margin-bottom: 7px;
      }
      .immune-compact-title {
        display: block;
        color: #263238;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 5px;
      }
      .immune-param-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 6px 8px;
      }
      .immune-param-grid .shiny-input-container {
        width: 100%;
        margin-bottom: 0;
      }
      .immune-output-grid .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 3px 10px;
        margin-top: 2px;
      }
      .immune-output-grid .checkbox {
        margin: 0;
        font-size: 11px;
      }
      .immune-output-grid label {
        font-size: 11px;
        white-space: nowrap;
      }
      .immune-run-btn {
        width: 100%;
        font-size: 12px;
        font-weight: 700;
        padding: 4px 0;
        margin-top: 4px;
      }
      .immune-hint {
        font-size: 11px;
        color: #78909c;
        line-height: 1.5;
        margin: 0;
      }
      .immune-active-plot-box {
        border: none;
        background: transparent;
        min-height: 285px;
        cursor: zoom-in;
      }
      .immune-result-panel {
        max-width: 100%;
        height: 240px;
        overflow-y: auto;
        overflow-x: hidden;
      }
      .immune-result-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .immune-result-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 18px;
        padding: 8px 2px 9px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .immune-result-panel .nav-tabs > li.active > a,
      .immune-result-panel .nav-tabs > li.active > a:hover,
      .immune-result-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .immune-result-slot {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        min-height: 120px;
        max-height: 190px;
        overflow-y: auto;
      }
      .immune-result-file-list {
        border: 1px solid #d7dee2;
        background: #ffffff;
        max-height: 190px;
        overflow-y: auto;
      }
      .immune-result-file-row {
        display: grid;
        grid-template-columns: 34px minmax(160px, 1fr) 58px minmax(220px, 2fr) 80px;
        gap: 8px;
        align-items: center;
        padding: 7px 8px;
        border-bottom: 1px solid #eef2f4;
        font-size: 11px;
      }
      .immune-result-file-row:last-child {
        border-bottom: none;
      }
      .immune-file-index {
        color: #90a4ae;
        font-weight: 700;
      }
      .immune-result-file-action {
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
      .immune-result-file-name {
        color: #263238;
        font-weight: 700;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .immune-result-file-type {
        color: #607d8b;
        font-weight: 700;
      }
      .immune-result-file-desc {
        color: #607d8b;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .immune-result-file-download .btn {
        font-size: 10px;
        padding: 1px 8px;
      }
      .immune-status-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 8px;
        margin-bottom: 8px;
      }
      .immune-status-item {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        font-size: 12px;
      }
      .immune-status-item b {
        display: block;
        color: #263238;
        font-size: 12px;
      }
      .immune-qa {
        font-size: 12px;
        line-height: 1.7;
        color: #455a64;
        max-height: 190px;
        overflow-y: auto;
      }
      .immune-qa dl { margin: 0; }
      .immune-qa dt {
        margin-top: 8px;
        color: #263238;
      }
      .immune-qa dt:first-child { margin-top: 0; }
      .immune-qa dd {
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
          class = "immune-card",
          h4("参数设置"),
          div(
            class = "immune-compact-section",
            span("分析参数", class = "immune-compact-title"),
            div(
              class = "immune-param-grid",
              selectInput(
                ns("method"),
                "分析方法",
                choices = c("全部" = "all", "CIBERSORT" = "cibersort", "ESTIMATE" = "estimate"),
                selected = "all"
              ),
              numericInput(ns("perm_num"), "置换次数", value = 100, min = 10, max = 1000, step = 10)
            ),
            textInput(
              ns("target_genes"),
              "目标基因（逗号分隔）",
              placeholder = "如 TP53, BRCA1, EGFR",
              value = "TP53, BRCA1"
            )
          ),
          div(
            class = "immune-compact-section",
            span("输出图表", class = "immune-compact-title"),
            div(
              class = "immune-output-grid",
              checkboxGroupInput(
                ns("outputs"),
                NULL,
                choices = c("热图" = "heatmap", "箱线图" = "boxplot", "相关性图" = "correlation", "棒棒糖图" = "lollipop"),
                selected = c("heatmap", "boxplot", "correlation", "lollipop"),
                inline = FALSE
              )
            )
          ),
          div(
            class = "immune-compact-section",
            span("说明", class = "immune-compact-title"),
            p("当前模块使用内置示例数据运行免疫浸润演示分析；运行后点击结果文件名在右侧预览图片。", class = "immune-hint")
          ),
          actionButton(ns("run"), "运行免疫浸润分析", class = "btn-success btn-sm immune-run-btn")
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        div(
          class = "immune-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "immune-active-plot-box",
            plotOutput(
              ns("activeImmunePlot"),
              height = "285px",
              width = "100%",
              click = ns("activeImmunePlot_click")
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
          class = "immune-result-card",
          h4("结果预览"),
          div(
            class = "immune-result-panel",
            tabsetPanel(
              id = ns("immuneResultTabs"),
              type = "tabs",
              tabPanel(
                "结果表",
                div(class = "immune-result-slot", uiOutput(ns("immuneResultFileList")))
              ),
              tabPanel(
                "数据预览",
                div(
                  class = "immune-result-slot",
                  uiOutput(ns("immuneStatusPanel")),
                  DTOutput(ns("results_table"))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "immune-qa",
                  tags$dl(
                    tags$dt("Q1：免疫浸润模块输出什么？"),
                    tags$dd("输出免疫细胞比例热图、组间比较箱线图、免疫细胞相关性图、目标基因相关性棒棒糖图和结果汇总表。"),
                    tags$dt("Q2：图片如何查看？"),
                    tags$dd("运行完成后，在结果表中点击 PNG 文件名，图片会显示在右侧图片区；点击右侧图片可放大查看。"),
                    tags$dt("Q3：目标基因有什么作用？"),
                    tags$dd("目标基因用于生成基因与免疫细胞比例的相关性结果；多个基因用英文逗号分隔。"),
                    tags$dt("Q4：结果表如何理解？"),
                    tags$dd("结果表汇总每种免疫细胞在全部样本、对照组和实验组中的平均比例，并给出模拟检验的 P 值和 FDR 校正值。")
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

immune_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    immune_data <- reactiveVal(NULL)
    running <- reactiveVal(FALSE)
    active_immune_plot <- reactiveVal("heatmap")
    
    output$status <- renderText({
      if (running()) return("⏳ 分析运行中，请稍候...")
      if (!is.null(immune_data())) return("✅ 分析完成！")
      return("💡 请设置参数后点击「运行分析」")
    })
    
    generate_mock_data <- function() {
      set.seed(123)
      n_samples <- 30
      n_cells <- 22
      cell_types <- get_lm22_celltypes()
      
      fractions <- matrix(runif(n_samples * n_cells, 0.001, 0.2), 
                          nrow = n_samples, ncol = n_cells)
      fractions <- fractions / rowSums(fractions)
      colnames(fractions) <- cell_types
      rownames(fractions) <- paste0("Sample", 1:n_samples)
      
      group_info <- c(rep("Control", 15), rep("Treat", 15))
      names(group_info) <- rownames(fractions)
      
      fractions[16:30, 1:5] <- fractions[16:30, 1:5] * 1.8
      fractions[16:30, 6:10] <- fractions[16:30, 6:10] * 0.6
      fractions <- fractions / rowSums(fractions)
      
      return(list(fractions = fractions, group_info = group_info))
    }
    
    observeEvent(input$run, {
      running(TRUE)
      output$status <- renderText("⏳ 分析运行中，请稍候...")
      
      # 修复 showNotification 警告：使用 type 参数
      showNotification("正在运行免疫浸润分析...", type = "message", duration = 3)
      
      Sys.sleep(0.5)
      mock <- generate_mock_data()
      
      target_genes <- NULL
      if (input$target_genes != "") {
        target_genes <- trimws(unlist(strsplit(input$target_genes, ",")))
        target_genes <- target_genes[target_genes != ""]
      }
      
      corr_results <- data.frame()
      if (!is.null(target_genes)) {
        for (gene in target_genes[1:min(3, length(target_genes))]) {
          gene_expr <- rnorm(nrow(mock$fractions), mean = 10, sd = 2)
          for (ct in colnames(mock$fractions)) {
            test <- cor.test(gene_expr, mock$fractions[, ct], method = "spearman")
            corr_results <- rbind(corr_results, data.frame(
              gene = gene,
              cell_type = ct,
              r = as.numeric(test$estimate),
              p = as.numeric(test$p.value)
            ))
          }
        }
      }
      
      immune_data(list(
        fractions = mock$fractions,
        group_info = mock$group_info,
        corr_results = corr_results,
        target_genes = target_genes,
        method = input$method,
        perm_num = input$perm_num,
        outputs = if (is.null(input$outputs) || length(input$outputs) == 0) {
          c("heatmap", "boxplot", "correlation", "lollipop")
        } else {
          input$outputs
        }
      ))

      selected_outputs <- immune_data()$outputs
      active_immune_plot(selected_outputs[1])
      
      running(FALSE)
      output$status <- renderText("✅ 分析完成！")
      showNotification("免疫浸润分析完成！", type = "message", duration = 3)
    })
    
    plot_empty_immune <- function(message = "请先运行免疫浸润分析") {
      plot.new()
      invisible(NULL)
    }

    immune_plot_label <- function(plot_key) {
      switch(
        plot_key,
        heatmap = "免疫细胞浸润热图",
        boxplot = "组间免疫细胞比较",
        correlation = "免疫细胞相关性图",
        lollipop = "基因-免疫细胞相关性",
        "免疫浸润图片"
      )
    }

    draw_immune_plot <- function(plot_key = active_immune_plot(), large = FALSE) {
      data <- immune_data()
      if (is.null(data)) {
        plot_empty_immune()
        return(invisible(NULL))
      }

      if (is.null(plot_key) || !nzchar(plot_key)) {
        plot_key <- "heatmap"
      }

      if (identical(plot_key, "heatmap")) {
        plot_immune_heatmap(data$fractions, data$group_info)
        return(invisible(NULL))
      }

      if (identical(plot_key, "boxplot")) {
        print(plot_immune_boxplot(data$fractions, data$group_info))
        return(invisible(NULL))
      }

      if (identical(plot_key, "correlation")) {
        plot_immune_correlation(data$fractions)
        return(invisible(NULL))
      }

      if (identical(plot_key, "lollipop")) {
        if (!is.null(data$corr_results) && nrow(data$corr_results) > 0 && length(data$target_genes) > 0) {
          print(plot_immune_lollipop(data$corr_results, data$target_genes[1]))
        } else {
          plot_empty_immune("请输入目标基因后重新运行")
        }
        return(invisible(NULL))
      }

      plot_empty_immune()
    }

    build_immune_summary <- function(data) {
      if (is.null(data)) {
        return(NULL)
      }

      fractions <- data$fractions
      group_info <- data$group_info
      control_idx <- names(group_info[group_info == "Control"])
      treat_idx <- names(group_info[group_info == "Treat"])

      p_values <- vapply(seq_len(ncol(fractions)), function(i) {
        tryCatch(
          stats::wilcox.test(fractions[control_idx, i], fractions[treat_idx, i])$p.value,
          error = function(e) NA_real_
        )
      }, numeric(1))

      df <- data.frame(
        CellType = colnames(fractions),
        Mean_All = round(colMeans(fractions), 4),
        Mean_Control = round(colMeans(fractions[control_idx, , drop = FALSE]), 4),
        Mean_Treat = round(colMeans(fractions[treat_idx, , drop = FALSE]), 4),
        P_value = round(p_values, 4),
        stringsAsFactors = FALSE
      )
      df$P_adjust <- round(stats::p.adjust(df$P_value, method = "fdr"), 4)
      df
    }

    output$activeImmunePlot <- renderPlot({
      draw_immune_plot(active_immune_plot())
    })

    output$activeImmunePlotLarge <- renderPlot({
      draw_immune_plot(active_immune_plot(), large = TRUE)
    })

    observeEvent(input$activeImmunePlot_click, {
      if (is.null(immune_data())) {
        return()
      }

      showModal(
        modalDialog(
          title = paste0(immune_plot_label(active_immune_plot()), " - 放大预览"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          plotOutput(ns("activeImmunePlotLarge"), height = "70vh", width = "100%")
        )
      )
    })

    make_immune_file_row <- function(index, name, type, desc, download_id, plot_key = NULL) {
      div(
        class = "immune-result-file-row",
        span(sprintf("%02d", index), class = "immune-file-index"),
        if (!is.null(plot_key)) {
          actionButton(
            ns(paste0("showImmunePlot_", plot_key)),
            name,
            class = "immune-result-file-action",
            title = "点击预览图片"
          )
        } else {
          span(name, class = "immune-result-file-name", title = name)
        },
        span(type, class = "immune-result-file-type"),
        span(desc, class = "immune-result-file-desc", title = desc),
        span(
          class = "immune-result-file-download",
          downloadButton(ns(download_id), "下载", class = "btn-xs")
        )
      )
    }

    output$immuneResultFileList <- renderUI({
      data <- immune_data()
      if (is.null(data)) {
        return(
          div(
            class = "immune-result-file-list",
            div(
              class = "immune-result-file-row",
              span("运行完成后，这里会显示免疫浸润结果文件。", style = "grid-column: 1 / -1;")
            )
          )
        )
      }

      outputs <- data$outputs
      file_rows <- list()
      idx <- 1

      if ("heatmap" %in% outputs) {
        file_rows[[length(file_rows) + 1]] <- make_immune_file_row(idx, "immune_heatmap.png", "PNG", "免疫细胞浸润热图，点击文件名可在上方预览", "downloadImmuneHeatmapPNG", "heatmap")
        idx <- idx + 1
      }
      if ("boxplot" %in% outputs) {
        file_rows[[length(file_rows) + 1]] <- make_immune_file_row(idx, "immune_boxplot.png", "PNG", "组间免疫细胞比例箱线图，点击文件名可在上方预览", "downloadImmuneBoxplotPNG", "boxplot")
        idx <- idx + 1
      }
      if ("correlation" %in% outputs) {
        file_rows[[length(file_rows) + 1]] <- make_immune_file_row(idx, "immune_cell_correlation.png", "PNG", "免疫细胞相关性图，点击文件名可在上方预览", "downloadImmuneCorrelationPNG", "correlation")
        idx <- idx + 1
      }
      if ("lollipop" %in% outputs) {
        file_rows[[length(file_rows) + 1]] <- make_immune_file_row(idx, "gene_immune_lollipop.png", "PNG", "目标基因与免疫细胞相关性图，点击文件名可在上方预览", "downloadImmuneLollipopPNG", "lollipop")
        idx <- idx + 1
      }

      file_rows[[length(file_rows) + 1]] <- make_immune_file_row(idx, "immune_infiltration_summary.csv", "CSV", "免疫细胞比例与组间比较汇总表", "downloadImmuneResults")
      do.call(div, c(list(class = "immune-result-file-list"), file_rows))
    })

    output$immuneStatusPanel <- renderUI({
      data <- immune_data()
      if (is.null(data)) {
        return(NULL)
      }

      target_text <- if (length(data$target_genes) > 0) paste(data$target_genes, collapse = ", ") else "未设置"
      div(
        class = "immune-status-grid",
        div(class = "immune-status-item", tags$b("样本数"), span(nrow(data$fractions))),
        div(class = "immune-status-item", tags$b("免疫细胞"), span(ncol(data$fractions))),
        div(class = "immune-status-item", tags$b("分析方法"), span(data$method)),
        div(class = "immune-status-item", tags$b("目标基因"), span(target_text))
      )
    })

    observeEvent(input$showImmunePlot_heatmap, {
      active_immune_plot("heatmap")
    })

    observeEvent(input$showImmunePlot_boxplot, {
      active_immune_plot("boxplot")
    })

    observeEvent(input$showImmunePlot_correlation, {
      active_immune_plot("correlation")
    })

    observeEvent(input$showImmunePlot_lollipop, {
      active_immune_plot("lollipop")
    })

    output$downloadImmuneHeatmapPNG <- downloadHandler(
      filename = "immune_heatmap.png",
      content = function(file) {
        grDevices::png(file, width = 3200, height = 2400, res = 300)
        draw_immune_plot("heatmap")
        grDevices::dev.off()
      }
    )

    output$downloadImmuneBoxplotPNG <- downloadHandler(
      filename = "immune_boxplot.png",
      content = function(file) {
        grDevices::png(file, width = 3200, height = 2400, res = 300)
        draw_immune_plot("boxplot")
        grDevices::dev.off()
      }
    )

    output$downloadImmuneCorrelationPNG <- downloadHandler(
      filename = "immune_cell_correlation.png",
      content = function(file) {
        grDevices::png(file, width = 3200, height = 2800, res = 300)
        draw_immune_plot("correlation")
        grDevices::dev.off()
      }
    )

    output$downloadImmuneLollipopPNG <- downloadHandler(
      filename = "gene_immune_lollipop.png",
      content = function(file) {
        grDevices::png(file, width = 3200, height = 2400, res = 300)
        draw_immune_plot("lollipop")
        grDevices::dev.off()
      }
    )

    output$downloadImmuneResults <- downloadHandler(
      filename = "immune_infiltration_summary.csv",
      content = function(file) {
        utils::write.csv(build_immune_summary(immune_data()), file, row.names = FALSE)
      }
    )

    output$heatmap <- renderPlot({
      req(immune_data())
      data <- immune_data()
      plot_immune_heatmap(data$fractions, data$group_info)
    })
    
    output$boxplot <- renderPlot({
      req(immune_data())
      data <- immune_data()
      plot_immune_boxplot(data$fractions, data$group_info)
    })
    
    output$correlation <- renderPlot({
      req(immune_data())
      data <- immune_data()
      plot_immune_correlation(data$fractions)
    })
    
    output$lollipop <- renderPlot({
      req(immune_data())
      data <- immune_data()
      if (!is.null(data$corr_results) && nrow(data$corr_results) > 0) {
        print(plot_immune_lollipop(data$corr_results, data$target_genes[1]))
      } else {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "请输入目标基因", cex = 1.2, col = "gray50")
      }
    })
    
    output$results_table <- renderDT({
      df <- build_immune_summary(immune_data())
      if (is.null(df)) {
        return(NULL)
      }
      if (!"CellType" %in% colnames(df)) {
        return(datatable(df, options = list(dom = "t", pageLength = 5), rownames = FALSE))
      }

      datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        formatRound(columns = c("Mean_All", "Mean_Control", "Mean_Treat", "P_value", "P_adjust"), digits = 4)
    })
  })
}

# ============================================================
# 模块结束
# ============================================================

# ============================================================
# 9. 五流程拆分版：免疫浸润 / 相关性 / 关键基因森林图 / 可视化 / 棒棒糖图
# ============================================================

immune_ui <- function(id) {
  ns <- NS(id)

  immune_upload_box <- function(input_id, status_id, title, accept = c(".csv", ".txt", ".tsv")) {
    tags$div(
      class = "immune-upload-row",
      tags$div(
        id = ns(paste0(input_id, "Box")),
        class = "immune-upload-box",
        tags$div(
          class = "immune-upload-placeholder",
          span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
          tags$span(title, class = "immune-upload-title"),
          tags$span(id = ns(status_id), "Drop file here or click to upload", class = "immune-upload-status")
        ),
        fileInput(
          ns(input_id),
          NULL,
          accept = accept,
          buttonLabel = "浏览",
          placeholder = paste0("选择", title)
        )
      )
    )
  }

  tagList(
    tags$style(HTML("
      .immune-card,
      .immune-plot-card,
      .immune-result-card {
        border: 1px solid #b0bec5;
        border-radius: 4px;
        padding: 12px 16px;
        background-color: #ffffff;
      }
      .immune-card,
      .immune-plot-card {
        height: 370px;
        overflow-y: auto;
      }
      .immune-result-card {
        height: 300px;
        overflow: hidden;
      }
      .immune-card h4,
      .immune-plot-card h4,
      .immune-result-card h4 {
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 14px;
        font-weight: 700;
      }
      .immune-card hr,
      .immune-plot-card hr {
        margin: 4px 0 8px 0;
      }
      .immune-card .nav-tabs,
      .immune-result-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .immune-card .nav-tabs > li > a,
      .immune-result-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 14px;
        padding: 7px 2px 8px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .immune-card .nav-tabs > li.active > a,
      .immune-card .nav-tabs > li.active > a:hover,
      .immune-card .nav-tabs > li.active > a:focus,
      .immune-result-panel .nav-tabs > li.active > a,
      .immune-result-panel .nav-tabs > li.active > a:hover,
      .immune-result-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .immune-upload-row {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        align-items: center;
        margin-bottom: 5px;
      }
      .immune-upload-box {
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
      .immune-upload-box:hover {
        background-color: #f7fafc;
      }
      .immune-upload-box .shiny-input-container {
        position: absolute;
        inset: 0;
        width: 100% !important;
        height: 100%;
        margin: 0;
        opacity: 0;
        z-index: 2;
        cursor: pointer;
      }
      .immune-upload-box .input-group,
      .immune-upload-box .input-group-btn,
      .immune-upload-box .btn-file,
      .immune-upload-box input[type='file'] {
        width: 100%;
        height: 100%;
        cursor: pointer;
      }
      .immune-upload-placeholder {
        text-align: center;
        pointer-events: none;
        display: grid;
        gap: 2px;
        justify-items: center;
      }
      .immune-upload-title {
        font-weight: 700;
        font-size: 11px;
        color: #263238;
      }
      .immune-upload-status {
        color: #1e88e5;
        font-size: 11px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 210px;
      }
      .immune-compact-section {
        border: 1px solid #d7dee2;
        background: #ffffff;
        padding: 6px 8px;
        margin-bottom: 7px;
      }
      .immune-compact-title {
        display: block;
        color: #263238;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 5px;
      }
      .immune-param-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 6px 8px;
      }
      .immune-card .form-control {
        font-size: 11px;
        padding: 2px 4px;
        height: 26px;
      }
      .immune-card .shiny-input-container {
        margin-bottom: 6px;
      }
      .immune-card label {
        font-size: 11px;
        color: #263238;
        margin-bottom: 3px;
      }
      .immune-hint {
        font-size: 11px;
        color: #78909c;
        line-height: 1.5;
        margin: 0;
      }
      .immune-run-btn {
        width: 100%;
        font-size: 12px;
        font-weight: 700;
        padding: 4px 0;
        margin-top: 4px;
      }
      .immune-active-plot-box {
        border: none;
        background: transparent;
        min-height: 285px;
        cursor: zoom-in;
      }
      .immune-result-panel {
        max-width: 100%;
        height: 240px;
        overflow-y: auto;
        overflow-x: hidden;
      }
      .immune-result-slot {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        min-height: 120px;
        max-height: 190px;
        overflow-y: auto;
      }
      .immune-result-file-list {
        border: 1px solid #d7dee2;
        background: #ffffff;
        max-height: 190px;
        overflow-y: auto;
      }
      .immune-result-file-row {
        display: grid;
        grid-template-columns: 34px minmax(150px, 1fr) 58px minmax(210px, 2fr) 80px;
        gap: 8px;
        align-items: center;
        padding: 7px 8px;
        border-bottom: 1px solid #eef2f4;
        font-size: 11px;
      }
      .immune-result-file-row:last-child {
        border-bottom: none;
      }
      .immune-file-index {
        color: #1e88e5;
        font-weight: 700;
      }
      .immune-result-file-action {
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
      .immune-result-file-name {
        color: #263238;
        font-weight: 700;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .immune-result-file-type {
        color: #455a64;
        font-weight: 700;
      }
      .immune-result-file-desc {
        color: #607d8b;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .immune-result-file-download .btn {
        font-size: 10px;
        padding: 1px 8px;
      }
      .immune-status-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 8px;
        margin-bottom: 8px;
      }
      .immune-status-item {
        border: 1px solid #d7dee2;
        padding: 8px;
        background: #ffffff;
        font-size: 12px;
      }
      .immune-status-item b {
        display: block;
        color: #263238;
        font-size: 12px;
      }
      .immune-qa {
        font-size: 12px;
        line-height: 1.7;
        color: #455a64;
        max-height: 190px;
        overflow-y: auto;
      }
      .immune-qa dl { margin: 0; }
      .immune-qa dt {
        margin-top: 8px;
        color: #263238;
      }
      .immune-qa dt:first-child { margin-top: 0; }
      .immune-qa dd {
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
          class = "immune-card",
          h4("参数设置"),
          tabsetPanel(
            id = ns("immuneTabset"),
            type = "tabs",
            tabPanel(
              "免疫浸润分析",
              value = "infiltration",
              immune_upload_box("infExprFile", "infExprFileStatus", "表达矩阵/Sample Type Matrix.csv", c(".csv", ".txt", ".tsv")),
              div(
                class = "immune-compact-section",
                span("分析参数", class = "immune-compact-title"),
                div(
                  class = "immune-param-grid",
                  selectInput(ns("infMethod"), "分析方法", choices = c("CIBERSORT" = "cibersort"), selected = "cibersort"),
                  checkboxInput(ns("infFastMode"), "快速计算", value = TRUE)
                )
              ),
              div(class = "immune-compact-section", p("只运行 CIBERSORT。若样本名含 _con/_tre 后缀，会自动识别分组并生成组间比较图。", class = "immune-hint")),
              actionButton(ns("runInfiltration"), "运行免疫浸润分析", class = "btn-success btn-sm immune-run-btn")
            ),
            tabPanel(
              "基因与免疫细胞相关性",
              value = "gene_corr",
              immune_upload_box("corrExprFile", "corrExprFileStatus", "表达矩阵/Sample Type Matrix.csv", c(".csv", ".txt", ".tsv")),
              immune_upload_box("corrGeneFile", "corrGeneFileStatus", "目标基因列表", c(".txt", ".csv", ".tsv")),
              immune_upload_box("corrImmuneFile", "corrImmuneFileStatus", "CIBERSORT结果（可选）", c(".csv", ".txt", ".tsv")),
              div(class = "immune-compact-section", p("未上传 CIBERSORT 结果时，会优先使用上一页免疫浸润分析的结果。", class = "immune-hint")),
              actionButton(ns("runGeneCorr"), "运行相关性分析", class = "btn-success btn-sm immune-run-btn")
            ),
            tabPanel(
              "关键基因HR森林图",
              value = "forest",
              immune_upload_box("forestExprFile", "forestExprFileStatus", "表达矩阵 / geneexp.csv", c(".csv", ".txt", ".tsv")),
              immune_upload_box("forestGeneFile", "forestGeneFileStatus", "关键基因列表", c(".txt", ".csv", ".tsv")),
              div(class = "immune-compact-section", p("根据关键基因表达计算组间差异、AUC 和 95% CI，并绘制森林图；样本名需包含 _con/_tre 后缀以识别分组。", class = "immune-hint")),
              actionButton(ns("runForest"), "生成关键基因森林图", class = "btn-success btn-sm immune-run-btn")
            ),
            tabPanel(
              "免疫基因可视化",
              value = "visual",
              immune_upload_box("vizImmuneFile", "vizImmuneFileStatus", "CIBERSORT结果", c(".csv", ".txt", ".tsv")),
              div(class = "immune-compact-section", p("未上传 CIBERSORT 结果时，会使用免疫浸润分析结果；样本名需包含 _con/_tre 后缀以识别分组。", class = "immune-hint")),
              actionButton(ns("runVisual"), "生成免疫可视化", class = "btn-success btn-sm immune-run-btn")
            ),
            tabPanel(
              "棒棒糖图",
              value = "lollipop",
              immune_upload_box("lolliCorrFile", "lolliCorrFileStatus", "相关性结果（可选）", c(".csv", ".txt", ".tsv")),
              div(
                class = "immune-compact-section",
                span("绘图参数", class = "immune-compact-title"),
                textInput(ns("lolliGene"), "目标基因", value = "", placeholder = "留空则使用相关性结果中的第一个基因")
              ),
              div(class = "immune-compact-section", p("未上传相关性结果时，会使用“基因与免疫细胞相关性”步骤的结果。", class = "immune-hint")),
              actionButton(ns("runLollipop"), "生成棒棒糖图", class = "btn-success btn-sm immune-run-btn")
            )
          )
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        div(
          class = "immune-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "immune-active-plot-box",
            plotOutput(ns("activeImmunePlot"), height = "285px", width = "100%", click = ns("activeImmunePlot_click"))
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
          class = "immune-result-card",
          h4("结果预览"),
          div(
            class = "immune-result-panel",
            tabsetPanel(
              id = ns("immuneResultTabs"),
              type = "tabs",
              tabPanel("结果表", div(class = "immune-result-slot", uiOutput(ns("immuneResultFileList")))),
              tabPanel("数据预览", div(class = "immune-result-slot", uiOutput(ns("immuneStatusPanel")), DTOutput(ns("results_table")))),
              tabPanel(
                "Q&A",
                div(
                  class = "immune-qa",
                  tags$dl(
                    tags$dt("Q1：这 5 个子功能的推荐顺序是什么？"),
                    tags$dd("先运行免疫浸润分析得到 CIBERSORT 结果，再做基因与免疫细胞相关性、关键基因森林图、免疫可视化和棒棒糖图。"),
                    tags$dt("Q2：免疫浸润需要哪些输入？"),
                    tags$dd("免疫浸润分析只需要表达矩阵，第一列为基因名，其余列为样本表达值。需要组间比较、森林图或免疫可视化时，样本名请使用 _con 和 _tre 后缀自动标记对照组与实验组。"),
                    tags$dt("Q3：CIBERSORT 结果文件是什么格式？"),
                    tags$dd("第一列为样本名，其余列为免疫细胞比例。可直接使用免疫浸润分析导出的 CIBERSORT-Results.csv。"),
                    tags$dt("Q4：关键基因HR森林图如何理解？"),
                    tags$dd("这里按原脚本逻辑展示关键基因的 AUC 及 95% CI，可用于比较关键基因区分两组样本的能力。"),
                    tags$dt("Q5：棒棒糖图需要什么输入？"),
                    tags$dd("棒棒糖图使用基因与免疫细胞相关性结果，显示一个基因与各免疫细胞的 Spearman 相关系数和显著性。")
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

immune_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    immune_results <- reactiveVal(list())
    active_plot <- reactiveVal(NULL)
    active_step <- reactiveVal("infiltration")

    `%||%` <- function(x, y) if (is.null(x)) y else x

    immune_set_upload_status <- function(status_id, label) {
      label <- gsub("\\\\", "\\\\\\\\", label)
      label <- gsub('"', '\\"', label, fixed = TRUE)
      shinyjs::runjs(sprintf('$("#%s").text("%s")', ns(status_id), label))
    }

    upload_pairs <- c(
      infExprFile = "infExprFileStatus",
      corrExprFile = "corrExprFileStatus",
      corrGeneFile = "corrGeneFileStatus",
      corrImmuneFile = "corrImmuneFileStatus",
      forestExprFile = "forestExprFileStatus",
      forestGeneFile = "forestGeneFileStatus",
      vizImmuneFile = "vizImmuneFileStatus",
      lolliCorrFile = "lolliCorrFileStatus"
    )
    lapply(names(upload_pairs), function(input_id) {
      local({
        id_local <- input_id
        status_local <- upload_pairs[[input_id]]
        observeEvent(input[[id_local]], {
          req(input[[id_local]]$name)
          immune_set_upload_status(status_local, input[[id_local]]$name)
        }, ignoreInit = TRUE)
      })
    })

    observeEvent(input$immuneTabset, {
      active_step(input$immuneTabset)
    }, ignoreInit = FALSE)

    read_first_column <- function(file, empty_ok = FALSE) {
      if (is.null(file) || is.null(file$datapath)) {
        if (empty_ok) return(character())
        stop("请上传所需列表文件。", call. = FALSE)
      }
      path <- file$datapath[[1]]
      ext <- tolower(tools::file_ext(file$name[[1]] %||% path))
      sep <- if (ext == "csv") "," else "\t"
      tab <- tryCatch(
        utils::read.table(path, header = FALSE, sep = sep, stringsAsFactors = FALSE, check.names = FALSE, quote = "", comment.char = ""),
        error = function(e) utils::read.table(path, header = FALSE, sep = "", stringsAsFactors = FALSE, check.names = FALSE, quote = "", comment.char = "")
      )
      vals <- trimws(as.character(tab[[1]]))
      vals[nzchar(vals) & !is.na(vals)]
    }

    read_expression_matrix <- function(file) {
      if (is.null(file) || is.null(file$datapath)) {
        stop("请上传表达矩阵。", call. = FALSE)
      }
      path <- file$datapath[[1]]
      ext <- tolower(tools::file_ext(file$name[[1]] %||% path))
      sep <- if (ext == "csv") "," else "\t"
      mat <- tryCatch(
        utils::read.table(path, header = TRUE, sep = sep, row.names = 1, check.names = FALSE, quote = "", comment.char = ""),
        error = function(e) utils::read.table(path, header = TRUE, sep = "", row.names = 1, check.names = FALSE, quote = "", comment.char = "")
      )
      mat <- as.matrix(mat)
      suppressWarnings(storage.mode(mat) <- "numeric")
      mat <- mat[rowSums(is.na(mat)) < ncol(mat), , drop = FALSE]
      mat[is.na(mat)] <- 0
      if (!nrow(mat) || ncol(mat) < 2) {
        stop("表达矩阵为空或样本列不足。", call. = FALSE)
      }
      mat
    }

    read_immune_matrix <- function(file = NULL) {
      if (!is.null(file) && !is.null(file$datapath)) {
        path <- file$datapath[[1]]
        ext <- tolower(tools::file_ext(file$name[[1]] %||% path))
        sep <- if (ext == "csv") "," else "\t"
        tab <- tryCatch(
          utils::read.table(path, header = TRUE, sep = sep, row.names = 1, check.names = FALSE, quote = "", comment.char = ""),
          error = function(e) utils::read.table(path, header = TRUE, sep = "", row.names = 1, check.names = FALSE, quote = "", comment.char = "")
        )
        mat <- as.matrix(tab)
        suppressWarnings(storage.mode(mat) <- "numeric")
        mat <- mat[, colSums(is.na(mat)) < nrow(mat), drop = FALSE]
        mat[is.na(mat)] <- 0
        return(mat)
      }

      res <- immune_results()
      if (!is.null(res$infiltration$fractions)) {
        return(res$infiltration$fractions)
      }
      stop("请上传 CIBERSORT 结果，或先运行免疫浸润分析。", call. = FALSE)
    }

    make_group_info <- function(sample_names, control_file = NULL, treat_file = NULL) {
      if (!is.null(control_file) && !is.null(control_file$datapath) &&
          !is.null(treat_file) && !is.null(treat_file$datapath)) {
        control <- read_first_column(control_file)
        treat <- read_first_column(treat_file)
        group <- rep(NA_character_, length(sample_names))
        names(group) <- sample_names
        group[sample_names %in% control] <- "Control"
        group[sample_names %in% treat] <- "Treat"
      } else {
        group <- ifelse(grepl("_con$", sample_names, ignore.case = TRUE), "Control",
                        ifelse(grepl("_tre$", sample_names, ignore.case = TRUE), "Treat", NA))
        names(group) <- sample_names
      }

      keep <- !is.na(group)
      if (sum(group == "Control", na.rm = TRUE) < 1 || sum(group == "Treat", na.rm = TRUE) < 1) {
        stop("无法识别对照组和实验组样本，请上传与表达矩阵列名一致的分组列表。", call. = FALSE)
      }
      group[keep]
    }

    infer_group_info <- function(sample_names) {
      group <- ifelse(grepl("_con$", sample_names, ignore.case = TRUE), "Control",
                      ifelse(grepl("_tre$", sample_names, ignore.case = TRUE), "Treat", NA))
      names(group) <- sample_names
      keep <- !is.na(group)
      if (sum(group == "Control", na.rm = TRUE) < 1 || sum(group == "Treat", na.rm = TRUE) < 1) {
        return(NULL)
      }
      group[keep]
    }

    align_expr_by_group <- function(expr, group) {
      common <- intersect(colnames(expr), names(group))
      if (length(common) < 2) {
        stop("表达矩阵和分组列表没有足够共同样本。", call. = FALSE)
      }
      expr <- expr[, common, drop = FALSE]
      group <- group[common]
      ord <- order(factor(group, levels = c("Control", "Treat")))
      list(expr = expr[, ord, drop = FALSE], group = group[ord])
    }

    build_summary <- function(fractions, group_info = NULL) {
      if (is.null(group_info)) {
        return(data.frame(
          CellType = colnames(fractions),
          Mean_All = round(colMeans(fractions), 4),
          Mean_Control = NA_real_,
          Mean_Treat = NA_real_,
          P_value = NA_real_,
          P_adjust = NA_real_,
          stringsAsFactors = FALSE
        ))
      }
      common <- intersect(rownames(fractions), names(group_info))
      fractions <- fractions[common, , drop = FALSE]
      group_info <- group_info[common]
      control_idx <- names(group_info[group_info == "Control"])
      treat_idx <- names(group_info[group_info == "Treat"])
      if (!length(control_idx) || !length(treat_idx)) {
        return(data.frame(
          CellType = colnames(fractions),
          Mean_All = round(colMeans(fractions), 4),
          Mean_Control = NA_real_,
          Mean_Treat = NA_real_,
          P_value = NA_real_,
          P_adjust = NA_real_,
          stringsAsFactors = FALSE
        ))
      }
      p_values <- vapply(seq_len(ncol(fractions)), function(i) {
        tryCatch(stats::wilcox.test(fractions[control_idx, i], fractions[treat_idx, i])$p.value, error = function(e) NA_real_)
      }, numeric(1))
      df <- data.frame(
        CellType = colnames(fractions),
        Mean_All = round(colMeans(fractions), 4),
        Mean_Control = round(colMeans(fractions[control_idx, , drop = FALSE]), 4),
        Mean_Treat = round(colMeans(fractions[treat_idx, , drop = FALSE]), 4),
        P_value = round(p_values, 4),
        stringsAsFactors = FALSE
      )
      df$P_adjust <- round(stats::p.adjust(df$P_value, method = "fdr"), 4)
      df
    }

    compute_gene_correlations <- function(expr, immune_mat, genes) {
      common <- intersect(colnames(expr), rownames(immune_mat))
      if (length(common) < 3) {
        stop("表达矩阵和免疫浸润结果共同样本不足。", call. = FALSE)
      }
      expr <- expr[, common, drop = FALSE]
      immune_mat <- immune_mat[common, , drop = FALSE]
      genes <- intersect(genes, rownames(expr))
      if (!length(genes)) {
        stop("目标基因未在表达矩阵中找到。", call. = FALSE)
      }

      rows <- list()
      for (gene in genes) {
        gene_expr <- as.numeric(expr[gene, ])
        for (cell_type in colnames(immune_mat)) {
          cell_vec <- as.numeric(immune_mat[, cell_type])
          if (stats::sd(cell_vec, na.rm = TRUE) == 0 || stats::sd(gene_expr, na.rm = TRUE) == 0) next
          test <- suppressWarnings(stats::cor.test(gene_expr, cell_vec, method = "spearman"))
          rows[[length(rows) + 1]] <- data.frame(
            gene = gene,
            cell_type = cell_type,
            r = as.numeric(test$estimate),
            p = as.numeric(test$p.value),
            stringsAsFactors = FALSE
          )
        }
      }
      out <- do.call(rbind, rows)
      out$P_adjust <- stats::p.adjust(out$p, method = "fdr")
      out
    }

    compute_forest <- function(expr, genes, group_info) {
      genes <- intersect(genes, rownames(expr))
      if (!length(genes)) {
        stop("关键基因未在表达矩阵中找到。", call. = FALSE)
      }
      common <- intersect(colnames(expr), names(group_info))
      expr <- expr[, common, drop = FALSE]
      group_info <- group_info[common]
      y <- ifelse(group_info == "Control", 0, 1)
      rows <- list()
      for (gene in genes) {
        values <- as.numeric(expr[gene, ])
        p_value <- tryCatch(stats::t.test(values ~ group_info)$p.value, error = function(e) NA_real_)
        se_value <- tryCatch(stats::t.test(values ~ group_info)$stderr, error = function(e) NA_real_)
        roc_obj <- tryCatch(pROC::roc(y, values, quiet = TRUE), error = function(e) NULL)
        ci_vec <- if (!is.null(roc_obj)) as.numeric(pROC::ci.auc(roc_obj, method = "bootstrap", boot.n = 200)) else c(NA_real_, NA_real_, NA_real_)
        rows[[length(rows) + 1]] <- data.frame(
          Gene = gene,
          P_Value = p_value,
          AUC = ci_vec[2],
          AUC_Lower_CI = ci_vec[1],
          AUC_Upper_CI = ci_vec[3],
          SE = se_value,
          Mean_Control = mean(values[group_info == "Control"], na.rm = TRUE),
          Mean_Treat = mean(values[group_info == "Treat"], na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }
      do.call(rbind, rows)
    }

    build_visual_data <- function(fractions, group_info) {
      common <- intersect(rownames(fractions), names(group_info))
      fractions <- fractions[common, , drop = FALSE]
      group_info <- group_info[common]
      data_long <- reshape2::melt(
        cbind(Sample = rownames(fractions), Group = unname(group_info), as.data.frame(fractions, check.names = FALSE)),
        id.vars = c("Sample", "Group"),
        variable.name = "Immune",
        value.name = "Fraction"
      )
      summary_table <- aggregate(Fraction ~ Immune + Group, data_long, function(x) c(Mean = mean(x), Median = median(x), SD = stats::sd(x), Count = length(x)))
      summary_table <- do.call(data.frame, summary_table)
      names(summary_table) <- c("Immune", "Group", "MeanFraction", "MedianFraction", "SD", "Count")
      p_values <- do.call(rbind, lapply(split(data_long, data_long$Immune), function(df) {
        data.frame(Immune = unique(df$Immune), P_value = tryCatch(stats::wilcox.test(Fraction ~ Group, data = df)$p.value, error = function(e) NA_real_))
      }))
      list(fractions = fractions, group_info = group_info, data_long = data_long, summary = summary_table, p_values = p_values)
    }

    plot_empty <- function(message = "请先运行当前步骤") {
      plot.new()
      invisible(NULL)
    }

    plot_forest <- function(df) {
      if (is.null(df) || !nrow(df)) return(plot_empty())
      df <- df[order(df$AUC), , drop = FALSE]
      df$Gene <- factor(df$Gene, levels = df$Gene)
      ggplot2::ggplot(df, ggplot2::aes(x = AUC, y = Gene)) +
        ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "#90a4ae") +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = AUC_Lower_CI, xmax = AUC_Upper_CI), height = 0.18, color = "#455a64") +
        ggplot2::geom_point(size = 3, color = "#1e88e5") +
        ggplot2::coord_cartesian(xlim = c(0, 1)) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::labs(title = "关键基因 AUC 森林图", x = "AUC (95% CI)", y = NULL)
    }

    plot_gene_scatter_pair <- function(row, expr, immune_mat) {
      common <- intersect(colnames(expr), rownames(immune_mat))
      df <- data.frame(
        GeneExpr = as.numeric(expr[row$gene[[1]], common]),
        ImmuneFraction = as.numeric(immune_mat[common, row$cell_type[[1]]])
      )
      ggplot2::ggplot(df, ggplot2::aes(GeneExpr, ImmuneFraction)) +
        ggplot2::geom_point(size = 2.5, color = "#1e88e5", alpha = 0.8) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#e53935", linewidth = 0.8) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::labs(
          title = paste(row$gene[[1]], "vs", row$cell_type[[1]]),
          subtitle = sprintf("Spearman r = %.3f, p = %.3g", row$r[[1]], row$p[[1]]),
          x = paste(row$gene[[1]], "表达量"),
          y = paste(row$cell_type[[1]], "比例")
        )
    }

    plot_gene_scatter <- function(corr_df, expr, immune_mat) {
      if (is.null(corr_df) || !nrow(corr_df)) return(plot_empty("暂无相关性结果"))
      row <- corr_df[order(corr_df$p, -abs(corr_df$r)), , drop = FALSE][1, ]
      plot_gene_scatter_pair(row, expr, immune_mat)
    }

    plot_visual <- function(viz, type = "box") {
      if (is.null(viz)) return(plot_empty())
      if (identical(type, "corr")) {
        return(plot_immune_correlation(viz$fractions))
      }
      if (identical(type, "ridge") && requireNamespace("ggridges", quietly = TRUE)) {
        return(
          ggplot2::ggplot(viz$data_long, ggplot2::aes(x = Fraction, y = Immune, fill = Group, color = Group)) +
            ggridges::geom_density_ridges(alpha = 0.45, position = "identity", scale = 0.8, linewidth = 0.5) +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::labs(title = "免疫细胞比例山脊图", x = "Fraction", y = NULL)
        )
      }
      ggpubr::ggboxplot(
        viz$data_long,
        x = "Immune",
        y = "Fraction",
        fill = "Group",
        palette = c("Control" = "#4CAF50", "Treat" = "#E74C3C"),
        xlab = "",
        ylab = "Fraction",
        legend.title = "分组",
        width = 0.7,
        outlier.shape = NA
      ) +
        ggpubr::stat_compare_means(ggplot2::aes(group = Group), label = "p.signif", method = "wilcox.test", size = 3) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
        ggplot2::labs(title = "免疫细胞组间比较")
    }

    draw_active_plot <- function() {
      res <- immune_results()
      key <- active_plot()
      if (is.null(key)) return(plot_empty())
      if (identical(key, "inf_heatmap")) return(plot_immune_heatmap(res$infiltration$fractions, res$infiltration$group_info))
      if (identical(key, "inf_boxplot")) {
        if (is.null(res$infiltration$group_info)) return(plot_empty("未识别到 _con/_tre 分组，无法生成组间箱线图"))
        return(print(plot_immune_boxplot(res$infiltration$fractions, res$infiltration$group_info)))
      }
      if (identical(key, "inf_corrplot")) return(plot_immune_correlation(res$infiltration$fractions))
      if (identical(key, "gene_scatter")) return(print(plot_gene_scatter(res$gene_corr$corr, res$gene_corr$expr, res$gene_corr$immune)))
      if (identical(key, "forest")) return(print(plot_forest(res$forest$table)))
      if (identical(key, "viz_box")) return(print(plot_visual(res$visual, "box")))
      if (identical(key, "viz_corr")) return(plot_visual(res$visual, "corr"))
      if (identical(key, "viz_ridge")) return(print(plot_visual(res$visual, "ridge")))
      if (identical(key, "lollipop")) return(print(plot_immune_lollipop(res$lollipop$corr, res$lollipop$gene)))
      plot_empty()
    }

    output$activeImmunePlot <- renderPlot({
      draw_active_plot()
    })

    output$activeImmunePlotLarge <- renderPlot({
      draw_active_plot()
    })

    observeEvent(input$activeImmunePlot_click, {
      req(!is.null(active_plot()))
      showModal(modalDialog(
        title = "图片放大预览",
        plotOutput(ns("activeImmunePlotLarge"), height = "70vh", width = "100%"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("关闭")
      ))
    }, ignoreInit = TRUE)

    set_results <- function(step_name, value, plot_key = NULL) {
      res <- immune_results()
      res[[step_name]] <- value
      immune_results(res)
      active_step(step_name)
      if (!is.null(plot_key)) active_plot(plot_key)
    }

    observeEvent(input$runInfiltration, {
      showNotification("正在运行免疫浸润分析...", type = "message", duration = 3)
      tryCatch({
        expr <- read_expression_matrix(input$infExprFile)
        group <- infer_group_info(colnames(expr))
        aligned <- if (is.null(group)) list(expr = expr, group = NULL) else align_expr_by_group(expr, group)
        analysis <- run_immune_analysis(
          aligned$expr,
          aligned$group,
          methods = input$infMethod,
          perm_num = 0,
          fast_cibersort = isTRUE(input$infFastMode)
        )
        fractions <- analysis$cibersort$fractions %||% NULL
        if (is.null(fractions)) {
          stop("当前选择未生成 CIBERSORT 免疫细胞比例，请选择 CIBERSORT 或全部。", call. = FALSE)
        }
        summary <- build_summary(fractions, aligned$group)
        set_results("infiltration", list(expr = aligned$expr, group_info = aligned$group, fractions = fractions, estimate = analysis$estimate, summary = summary), "inf_heatmap")
        showNotification("免疫浸润分析完成。", type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste0("错误: ", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    observeEvent(input$runGeneCorr, {
      showNotification("正在计算基因与免疫细胞相关性...", type = "message", duration = 3)
      tryCatch({
        expr <- read_expression_matrix(input$corrExprFile)
        genes <- read_first_column(input$corrGeneFile)
        immune_mat <- read_immune_matrix(input$corrImmuneFile)
        corr <- compute_gene_correlations(expr, immune_mat, genes)
        set_results("gene_corr", list(expr = expr, immune = immune_mat, genes = genes, corr = corr), "gene_scatter")
        showNotification("相关性分析完成。", type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste0("错误: ", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    observeEvent(input$runForest, {
      showNotification("正在生成关键基因森林图...", type = "message", duration = 3)
      tryCatch({
        expr <- read_expression_matrix(input$forestExprFile)
        genes <- read_first_column(input$forestGeneFile)
        group <- infer_group_info(colnames(expr))
        if (is.null(group)) {
          stop("无法从样本名识别对照组和实验组，请使用 _con/_tre 后缀命名样本。", call. = FALSE)
        }
        aligned <- align_expr_by_group(expr, group)
        table <- compute_forest(aligned$expr, genes, aligned$group)
        set_results("forest", list(expr = aligned$expr, group_info = aligned$group, genes = genes, table = table), "forest")
        showNotification("关键基因森林图完成。", type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste0("错误: ", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    observeEvent(input$runVisual, {
      showNotification("正在生成免疫可视化...", type = "message", duration = 3)
      tryCatch({
        immune_mat <- read_immune_matrix(input$vizImmuneFile)
        group <- infer_group_info(rownames(immune_mat))
        if (is.null(group)) {
          stop("无法从样本名识别对照组和实验组，请使用 _con/_tre 后缀命名样本。", call. = FALSE)
        }
        viz <- build_visual_data(immune_mat, group)
        set_results("visual", viz, "viz_box")
        showNotification("免疫可视化完成。", type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste0("错误: ", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    observeEvent(input$runLollipop, {
      showNotification("正在生成棒棒糖图...", type = "message", duration = 3)
      tryCatch({
        corr <- NULL
        if (!is.null(input$lolliCorrFile) && !is.null(input$lolliCorrFile$datapath)) {
          corr <- utils::read.csv(input$lolliCorrFile$datapath, header = TRUE, check.names = FALSE)
          names(corr) <- sub("^spec$", "gene", names(corr))
          names(corr) <- sub("^env$", "cell_type", names(corr))
        } else {
          corr <- immune_results()$gene_corr$corr
        }
        if (is.null(corr) || !nrow(corr)) {
          stop("请先运行相关性分析或上传相关性结果。", call. = FALSE)
        }
        gene <- trimws(input$lolliGene %||% "")
        if (!nzchar(gene)) gene <- corr$gene[[1]]
        if (!gene %in% corr$gene) {
          stop("指定基因不在相关性结果中。", call. = FALSE)
        }
        set_results("lollipop", list(corr = corr, gene = gene), "lollipop")
        showNotification("棒棒糖图完成。", type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste0("错误: ", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    make_file_row <- function(index, file, type, desc, download_id, plot_key = NULL) {
      div(
        class = "immune-result-file-row",
        span(sprintf("%02d", index), class = "immune-file-index"),
        if (!is.null(plot_key)) {
          actionButton(ns(paste0("showImmunePlot_", plot_key)), file, class = "immune-result-file-action", title = "点击后在上方图片区预览")
        } else {
          span(file, class = "immune-result-file-name", title = file)
        },
        span(type, class = "immune-result-file-type"),
        span(desc, class = "immune-result-file-desc", title = desc),
        span(class = "immune-result-file-download", downloadButton(ns(download_id), "下载", class = "btn-xs"))
      )
    }

    rows_for_step <- function(step) {
      res <- immune_results()
      rows <- list()
      i <- 1
      add <- function(file, type, desc, download_id, plot_key = NULL) {
        rows[[length(rows) + 1]] <<- make_file_row(i, file, type, desc, download_id, plot_key)
        i <<- i + 1
      }
      if (identical(step, "infiltration") && !is.null(res$infiltration)) {
        add("CIBERSORT-Results.csv", "CSV", "免疫细胞比例矩阵", "downloadInfFractions")
        add("immune_infiltration_summary.csv", "CSV", "免疫细胞组间比较汇总表", "downloadInfSummary")
        add("immune_heatmap.png", "PNG", "免疫细胞浸润热图", "downloadInfHeatmap", "inf_heatmap")
        if (!is.null(res$infiltration$group_info)) {
          add("immune_boxplot.png", "PNG", "免疫细胞组间箱线图", "downloadInfBoxplot", "inf_boxplot")
        }
        add("immune_cell_correlation.png", "PNG", "免疫细胞相关性图", "downloadInfCorrplot", "inf_corrplot")
      }
      if (identical(step, "gene_corr") && !is.null(res$gene_corr)) {
        add("gene_immune_correlation.csv", "CSV", "基因与免疫细胞 Spearman 相关性", "downloadGeneCorrTable")
        add("gene_immune_scatter_preview.png", "PNG", "最显著相关性散点图预览", "downloadGeneScatter", "gene_scatter")
        add("gene_immune_scatter_by_gene.zip", "ZIP", "按基因分文件夹保存全部散点图", "downloadGeneScatterByGene")
      }
      if (identical(step, "forest") && !is.null(res$forest)) {
        add("key_gene_auc_results.csv", "CSV", "关键基因差异/AUC统计表", "downloadForestTable")
        add("key_gene_forest.png", "PNG", "关键基因 AUC 森林图", "downloadForestPlot", "forest")
      }
      if (identical(step, "visual") && !is.null(res$visual)) {
        add("barplot_data_long.csv", "CSV", "免疫细胞长格式数据", "downloadVizLong")
        add("boxplot_summary_table.csv", "CSV", "免疫细胞箱线图统计汇总", "downloadVizSummary")
        add("immune_pvalues.csv", "CSV", "免疫细胞组间P值", "downloadVizPvalues")
        add("immune_diff_boxplot.png", "PNG", "组合箱线图", "downloadVizBox", "viz_box")
        add("immune_corrplot.png", "PNG", "免疫细胞相关热图", "downloadVizCorr", "viz_corr")
        add("immune_ridges_overlay.png", "PNG", "两组叠加山脊图", "downloadVizRidge", "viz_ridge")
      }
      if (identical(step, "lollipop") && !is.null(res$lollipop)) {
        add("gene_immune_lollipop.png", "PNG", "基因与免疫细胞相关性棒棒糖图", "downloadLollipopPlot", "lollipop")
        add("lollipop_correlation.csv", "CSV", "当前基因相关性数据", "downloadLollipopTable")
      }
      rows
    }

    output$immuneResultFileList <- renderUI({
      step <- active_step()
      rows <- rows_for_step(step)
      if (!length(rows)) {
        return(NULL)
      }
      do.call(div, c(list(class = "immune-result-file-list"), rows))
    })

    observeEvent(input$showImmunePlot_inf_heatmap, active_plot("inf_heatmap"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_inf_boxplot, active_plot("inf_boxplot"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_inf_corrplot, active_plot("inf_corrplot"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_gene_scatter, active_plot("gene_scatter"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_forest, active_plot("forest"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_viz_box, active_plot("viz_box"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_viz_corr, active_plot("viz_corr"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_viz_ridge, active_plot("viz_ridge"), ignoreInit = TRUE)
    observeEvent(input$showImmunePlot_lollipop, active_plot("lollipop"), ignoreInit = TRUE)

    preview_table <- reactive({
      res <- immune_results()
      step <- active_step()
      if (identical(step, "infiltration") && !is.null(res$infiltration)) return(res$infiltration$summary)
      if (identical(step, "gene_corr") && !is.null(res$gene_corr)) return(res$gene_corr$corr)
      if (identical(step, "forest") && !is.null(res$forest)) return(res$forest$table)
      if (identical(step, "visual") && !is.null(res$visual)) return(res$visual$summary)
      if (identical(step, "lollipop") && !is.null(res$lollipop)) return(res$lollipop$corr[res$lollipop$corr$gene == res$lollipop$gene, , drop = FALSE])
      NULL
    })

    output$immuneStatusPanel <- renderUI({
      res <- immune_results()
      step <- active_step()
      preview <- preview_table()
      if (is.null(preview)) {
        return(NULL)
      }
      label <- switch(step, infiltration = "免疫浸润分析", gene_corr = "基因与免疫细胞相关性", forest = "关键基因HR森林图", visual = "免疫基因可视化", lollipop = "棒棒糖图", "免疫浸润")
      count <- nrow(preview)
      div(
        class = "immune-status-grid",
        div(class = "immune-status-item", tags$b("当前功能"), span(label)),
        div(class = "immune-status-item", tags$b("结果行数"), span(count)),
        div(class = "immune-status-item", tags$b("当前图片"), span(active_plot() %||% "未选择")),
        div(class = "immune-status-item", tags$b("状态"), span(if (length(res)) "已有结果" else "待运行"))
      )
    })

    output$results_table <- renderDT({
      preview <- preview_table()
      if (is.null(preview)) {
        return(NULL)
      }
      DT::datatable(preview, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })

    write_png <- function(file, plot_key, width = 3200, height = 2400) {
      old_key <- active_plot()
      active_plot(plot_key)
      grDevices::png(file, width = width, height = height, res = 300)
      on.exit({
        grDevices::dev.off()
        active_plot(old_key)
      }, add = TRUE)
      draw_active_plot()
    }

    safe_file_name <- function(x) {
      x <- gsub("[\\\\/:*?\"<>|]", "_", x)
      x <- gsub("\\s+", "_", trimws(x))
      ifelse(nzchar(x), x, "unnamed")
    }

    write_gene_scatter_zip <- function(file) {
      res <- immune_results()$gene_corr
      req(!is.null(res), !is.null(res$corr), nrow(res$corr) > 0)
      corr <- res$corr[order(res$corr$gene, res$corr$cell_type), , drop = FALSE]
      out_dir <- tempfile("gene_immune_scatter_")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

      for (i in seq_len(nrow(corr))) {
        row <- corr[i, , drop = FALSE]
        gene_dir <- file.path(out_dir, safe_file_name(row$gene[[1]]))
        dir.create(gene_dir, recursive = TRUE, showWarnings = FALSE)
        png_file <- file.path(gene_dir, paste0(safe_file_name(row$cell_type[[1]]), ".png"))
        grDevices::png(png_file, width = 2100, height = 1500, res = 300)
        print(plot_gene_scatter_pair(row, res$expr, res$immune))
        grDevices::dev.off()
      }

      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(out_dir)
      files <- list.files(".", recursive = TRUE, full.names = FALSE)
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = file, files = files)
      } else {
        utils::zip(zipfile = file, files = files)
      }
    }

    output$downloadInfFractions <- downloadHandler("CIBERSORT-Results.csv", function(file) utils::write.csv(immune_results()$infiltration$fractions, file))
    output$downloadInfSummary <- downloadHandler("immune_infiltration_summary.csv", function(file) utils::write.csv(immune_results()$infiltration$summary, file, row.names = FALSE))
    output$downloadGeneCorrTable <- downloadHandler("gene_immune_correlation.csv", function(file) utils::write.csv(immune_results()$gene_corr$corr, file, row.names = FALSE))
    output$downloadForestTable <- downloadHandler("key_gene_auc_results.csv", function(file) utils::write.csv(immune_results()$forest$table, file, row.names = FALSE))
    output$downloadVizLong <- downloadHandler("barplot_data_long.csv", function(file) utils::write.csv(immune_results()$visual$data_long, file, row.names = FALSE))
    output$downloadVizSummary <- downloadHandler("boxplot_summary_table.csv", function(file) utils::write.csv(immune_results()$visual$summary, file, row.names = FALSE))
    output$downloadVizPvalues <- downloadHandler("immune_pvalues.csv", function(file) utils::write.csv(immune_results()$visual$p_values, file, row.names = FALSE))
    output$downloadLollipopTable <- downloadHandler("lollipop_correlation.csv", function(file) {
      res <- immune_results()$lollipop
      utils::write.csv(res$corr[res$corr$gene == res$gene, , drop = FALSE], file, row.names = FALSE)
    })

    output$downloadInfHeatmap <- downloadHandler("immune_heatmap.png", function(file) write_png(file, "inf_heatmap"))
    output$downloadInfBoxplot <- downloadHandler("immune_boxplot.png", function(file) write_png(file, "inf_boxplot"))
    output$downloadInfCorrplot <- downloadHandler("immune_cell_correlation.png", function(file) write_png(file, "inf_corrplot", 3200, 2800))
    output$downloadGeneScatter <- downloadHandler("gene_immune_scatter_preview.png", function(file) write_png(file, "gene_scatter"))
    output$downloadGeneScatterByGene <- downloadHandler("gene_immune_scatter_by_gene.zip", function(file) write_gene_scatter_zip(file))
    output$downloadForestPlot <- downloadHandler("key_gene_forest.png", function(file) write_png(file, "forest"))
    output$downloadVizBox <- downloadHandler("immune_diff_boxplot.png", function(file) write_png(file, "viz_box", 3600, 2600))
    output$downloadVizCorr <- downloadHandler("immune_corrplot.png", function(file) write_png(file, "viz_corr", 3200, 3200))
    output$downloadVizRidge <- downloadHandler("immune_ridges_overlay.png", function(file) write_png(file, "viz_ridge", 3600, 2600))
    output$downloadLollipopPlot <- downloadHandler("gene_immune_lollipop.png", function(file) write_png(file, "lollipop"))
  })
}
