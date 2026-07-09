# wgcna_module.R - WGCNA 共表达网络分析模块（完整版）
# 功能：构建共表达网络，检测模块，分析模块与性状关系
# 参考差异分析模块的布局和风格

# WGCNA can use multiple CPU cores for correlation/TOM-heavy steps.
.wgcna_thread_count <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
options(WGCNA.threads = .wgcna_thread_count)
try(WGCNA::enableWGCNAThreads(nThreads = .wgcna_thread_count), silent = TRUE)
rm(.wgcna_thread_count)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

wgcna_require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("缺少 R 包：", pkg, "。请先安装后再运行 WGCNA 模块。"), call. = FALSE)
  }
}

# ============================================================
# UI
# ============================================================
wgcna_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .wgcna-card,
        .wgcna-plot-card,
        .wgcna-result-card {
            border: 1px solid #b0bec5;
            border-radius: 4px;
            padding: 12px 16px;
            background-color: #ffffff;
        }
        .wgcna-card,
        .wgcna-plot-card {
            height: 370px;
            overflow-y: auto;
        }
        .wgcna-result-card {
            height: 330px;
            overflow: hidden;
        }
        .wgcna-card h4,
        .wgcna-plot-card h4,
        .wgcna-result-card h4 {
            color: #2c3e50;
            margin-top: 0;
            margin-bottom: 10px;
            font-size: 14px;
            font-weight: 700;
        }
        .wgcna-card hr,
        .wgcna-plot-card hr {
            margin: 4px 0 8px 0;
        }
        .wgcna-upload-row {
            display: grid;
            grid-template-columns: 1fr;
            gap: 6px;
            align-items: center;
            margin-bottom: 5px;
        }
        .wgcna-upload-box {
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
        .wgcna-upload-box:hover {
            background-color: #f7fafc;
        }
        .wgcna-upload-box .shiny-input-container {
            position: absolute;
            inset: 0;
            width: 100% !important;
            height: 100%;
            margin: 0;
            opacity: 0;
            z-index: 2;
            cursor: pointer;
        }
        .wgcna-upload-box .input-group,
        .wgcna-upload-box .input-group-btn,
        .wgcna-upload-box .btn-file,
        .wgcna-upload-box input[type='file'] {
            width: 100%;
            height: 100%;
            cursor: pointer;
        }
        .wgcna-upload-placeholder {
            text-align: center;
            pointer-events: none;
            display: grid;
            gap: 2px;
            justify-items: center;
        }
        .wgcna-upload-title {
            font-weight: 700;
            font-size: 11px;
            color: #263238;
        }
        .wgcna-upload-status {
            color: #1e88e5;
            font-size: 11px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
            max-width: 170px;
        }
        .wgcna-compact-section {
            border: 1px solid #d7dee2;
            background: #ffffff;
            padding: 6px 8px;
            margin-bottom: 8px;
        }
        .wgcna-compact-title {
            display: block;
            color: #263238;
            font-size: 11px;
            font-weight: 700;
            margin-bottom: 5px;
        }
        .wgcna-param-grid {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px 8px;
            align-items: center;
        }
        .wgcna-param-grid-2 {
            grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .wgcna-mini-control {
            display: grid;
            grid-template-columns: auto minmax(0, 1fr);
            gap: 5px;
            align-items: center;
            font-size: 10px;
            color: #263238;
        }
        .wgcna-mini-control .shiny-input-container {
            margin-bottom: 0;
        }
        .wgcna-mini-control .form-control {
            height: 24px;
            padding: 2px 4px;
            font-size: 11px;
            border-radius: 0;
        }
        .wgcna-plot-box {
            min-height: 285px;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .wgcna-result-panel {
            max-width: 100%;
            overflow-x: hidden;
        }
        .wgcna-result-panel .nav-tabs {
            border-bottom: 1px solid #d7dee2;
            margin-bottom: 8px;
        }
        .wgcna-result-panel .nav-tabs > li > a {
            border-radius: 0;
            border: none;
            margin-right: 26px;
            padding: 8px 2px 9px 2px;
            color: #37474f;
            background: transparent;
            font-size: 12px;
        }
        .wgcna-result-panel .nav-tabs > li.active > a,
        .wgcna-result-panel .nav-tabs > li.active > a:hover,
        .wgcna-result-panel .nav-tabs > li.active > a:focus {
            border: none;
            border-bottom: 2px solid #1e88e5;
            color: #1e88e5;
            background: transparent;
            font-weight: 700;
        }
        .wgcna-result-file-list {
            border: 1px solid #d7dee2;
            background: #ffffff;
            max-height: 220px;
            overflow-y: auto;
        }
        .wgcna-result-file-row {
            display: grid;
            grid-template-columns: 28px minmax(150px, 1fr) 54px minmax(150px, 1.4fr) 70px;
            gap: 8px;
            align-items: center;
            padding: 6px 8px;
            border-bottom: 1px solid #eef2f4;
            font-size: 11px;
        }
        .wgcna-result-file-row:last-child {
            border-bottom: none;
        }
        .wgcna-file-index {
            color: #1e88e5;
            font-weight: 700;
        }
        .wgcna-result-file-action {
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
        .wgcna-result-file-name,
        .wgcna-result-file-desc {
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .wgcna-result-file-name {
            font-weight: 700;
            color: #263238;
        }
        .wgcna-result-file-type {
            color: #455a64;
            font-weight: 700;
        }
        .wgcna-result-file-desc {
            color: #607d8b;
        }
        .wgcna-result-file-download .btn {
            font-size: 10px;
            padding: 1px 8px;
            line-height: 1.4;
        }
        .wgcna-result-data-preview {
            max-height: 220px;
            overflow-y: auto;
        }
        .wgcna-result-data-preview h5 {
            margin: 4px 0 6px 0;
            font-size: 12px;
            font-weight: 700;
            color: #263238;
        }
        .wgcna-qa {
            font-size: 12px;
            line-height: 1.7;
            color: #455a64;
            max-height: 220px;
            overflow-y: auto;
        }
        .wgcna-qa dl {
            margin: 0;
        }
        .wgcna-qa dt {
            margin-top: 8px;
            color: #263238;
        }
        .wgcna-qa dt:first-child {
            margin-top: 0;
        }
        .wgcna-qa dd {
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
          class = "wgcna-card",
          h4("文件上传"),
          div(
            class = "wgcna-upload-row",
            div(
              class = "wgcna-upload-box",
              id = ns("countFileBox"),
              tags$div(
                class = "wgcna-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span("表达矩阵 / Sample Type Matrix", class = "wgcna-upload-title"),
                tags$span(id = ns("countFileStatus"), "Drop file here or click to upload", class = "wgcna-upload-status")
              ),
              fileInput(ns("countFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择表达矩阵文件")
            )
          ),
          div(
            class = "wgcna-compact-section",
            span("模块检测参数", class = "wgcna-compact-title"),
            div(
              class = "wgcna-param-grid wgcna-param-grid-3",
              div(class = "wgcna-mini-control", span("minModuleSize"), numericInput(ns("minModuleSize"), NULL, value = 50, min = 2, max = 500, step = 1)),
              div(class = "wgcna-mini-control", span("deepSplit"), numericInput(ns("deepSplit"), NULL, value = 2, min = 0, max = 4, step = 1)),
              div(class = "wgcna-mini-control", span("mergeThreshold"), numericInput(ns("mergeThreshold"), NULL, value = 0.25, min = 0.01, max = 1, step = 0.01))
            )
          ),
          actionButton(ns("runWgcna"), "运行 WGCNA 分析", 
                       class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 5px 0; border-radius: 0;")
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "wgcna-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "wgcna-plot-box",
            plotOutput(
              ns("activeWgcnaPlot"),
              height = "285px",
              width = "100%",
              click = ns("activeWgcnaPlot_click")
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
          class = "wgcna-result-card",
          h4("结果预览"),
          div(
            class = "wgcna-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel("结果表", uiOutput(ns("resultFileList"))),
              tabPanel(
                "数据预览",
                div(
                  class = "wgcna-result-data-preview",
                  h5("模块信息"),
                  DTOutput(ns("moduleTable")),
                  h5("基因信息"),
                  DTOutput(ns("geneInfoTable"))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "wgcna-qa",
                  tags$dl(
                    tags$dt("Q1：WGCNA 用于什么？"),
                    tags$dd("WGCNA 用于构建基因共表达网络，识别表达模式相似的基因模块，并分析模块与表型性状之间的相关性。"),
                    tags$dt("Q2：需要上传哪些文件？"),
                    tags$dd("只需要上传 1 个表达矩阵文件。第一列为基因名，后续列为样本表达值；对照组样本列名以 _con 结尾，实验组样本列名以 _tre 结尾。"),
                    tags$dt("Q3：软阈值 power 如何理解？"),
                    tags$dd("软阈值用于把相关性转为网络连接强度，目标是让网络近似无标度。通常参考 Scale Free Topology Model Fit，R² 达到阈值后选择较小 power。"),
                    tags$dt("Q4：minModuleSize、deepSplit、mergeThreshold 怎么调？"),
                    tags$dd("minModuleSize 控制最小模块基因数；deepSplit 越高模块切分越细；mergeThreshold 控制相似模块合并阈值，越高越容易合并。"),
                    tags$dt("Q5：模块-性状热图怎么看？"),
                    tags$dd("热图展示模块特征基因与 Normal/Disease 性状的相关性。相关系数绝对值越大且 P 值越小，说明该模块越可能与表型相关。"),
                    tags$dt("Q6：MM vs GS 图有什么作用？"),
                    tags$dd("MM 表示基因与模块特征基因的相关性，GS 表示基因与性状的相关性。两者同时较高的基因常作为模块内候选 hub gene。")
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
wgcna_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储结果 ----
    wgcna_results <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    active_wgcna_plot <- reactiveVal("power")

    wgcna_set_upload_status <- function(status_id, label) {
      label <- gsub("\\\\", "\\\\\\\\", label)
      label <- gsub('"', '\\"', label, fixed = TRUE)
      shinyjs::runjs(sprintf('$("#%s").text("%s")', ns(status_id), label))
    }
    
    wgcna_clean_number <- function(value, default, min_value, max_value) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) != 1 || is.na(value)) {
        value <- default
      }
      max(min_value, min(max_value, value))
    }
    
    wgcna_rsquared_cut <- function(res = NULL) {
      if (!is.null(res) && !is.null(res$rsquared_cut)) {
        return(res$rsquared_cut)
      }
      0.85
    }
    
    wgcna_plot_label <- function(plot_key) {
      labels <- c(
        sample_cluster = "样本聚类图",
        sample_trait = "样本性状热图",
        power = "软阈值图",
        gene_cluster = "基因聚类图",
        dendro = "动态模块树状图",
        module_cluster = "模块特征基因聚类图",
        merged = "模块合并前后对比图",
        trait = "模块-性状热图",
        module_count = "模块基因数量图",
        mmgs = "MM vs GS",
        boxplot_mm = "MM箱线图",
        violin_gs = "GS小提琴图",
        me_corr = "模块相关性热图"
      )
      labels[[plot_key]] %||% "WGCNA图片"
    }

    wgcna_prepare_expression_matrix <- function(count_file, sd_cutoff) {
      expr_matrix <- read_expression_matrix(count_file)

      if (any(is.na(expr_matrix))) {
        stop("表达矩阵中含有无法识别的数值", call. = FALSE)
      }

      if (requireNamespace("limma", quietly = TRUE)) {
        expr_matrix <- limma::avereps(expr_matrix)
      }

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

      data_ctrl <- expr_matrix[, ctrl_samples, drop = FALSE]
      data_treat <- expr_matrix[, treat_samples, drop = FALSE]
      combined_expr <- cbind(data_ctrl, data_treat)

      need_log <- max(combined_expr, na.rm = TRUE) > 1000
      if (isTRUE(need_log)) {
        combined_expr[combined_expr < 0] <- 0
        combined_expr <- log2(combined_expr + 1)
      }
      if (requireNamespace("limma", quietly = TRUE)) {
        combined_expr <- suppressWarnings(limma::normalizeBetweenArrays(combined_expr, method = "quantile"))
      }
      combined_expr[is.na(combined_expr)] <- 0

      gene_sd <- apply(combined_expr, 1, stats::sd, na.rm = TRUE)
      combined_expr <- combined_expr[gene_sd > sd_cutoff, , drop = FALSE]
      if (nrow(combined_expr) < 2) {
        stop("SD过滤后剩余基因少于 2 个，请降低 SD阈值。", call. = FALSE)
      }

      list(
        combined_expr = combined_expr,
        ctrl_samples = ctrl_samples,
        treat_samples = treat_samples,
        num_ctrl = length(ctrl_samples),
        num_treat = length(treat_samples),
        log_transformed = need_log
      )
    }
    
    wgcna_plot_filename <- function(plot_key) {
      switch(
        plot_key,
        sample_cluster = "Sample_Clustering.png",
        sample_trait = "Sample_Heatmap.png",
        power = "Scale_Independence_and_Mean_Connectivity.png",
        gene_cluster = "Gene_Clustering.png",
        dendro = "Dynamic_Tree_Modules.png",
        module_cluster = "Module_Clustering.png",
        merged = "Merged_Modules_Comparison.png",
        trait = "Module_Trait_Heatmap.png",
        module_count = "Module_Gene_Counts.png",
        mmgs = "WGCNA_MM_vs_GS.png",
        boxplot_mm = "Boxplot_MM.png",
        violin_gs = "ViolinPlot_GS.png",
        me_corr = "Module_Module_Correlation.png",
        "WGCNA_plot.png"
      )
    }

    wgcna_safe_filename <- function(value) {
      value <- as.character(value)
      value <- gsub("[^A-Za-z0-9._-]+", "_", value)
      value <- gsub("^_+|_+$", "", value)
      if (!nzchar(value)) "module" else value
    }

    wgcna_module_gene_lists <- function(res) {
      if (is.null(res) || is.null(res$datExpr) || is.null(res$moduleColors)) {
        return(list())
      }
      gene_names <- colnames(res$datExpr)
      module_colors <- as.character(res$moduleColors)
      if (length(gene_names) != length(module_colors)) {
        stop("基因数量与模块标签数量不一致，无法生成模块基因 TXT 文件。", call. = FALSE)
      }
      split(gene_names, factor(module_colors, levels = unique(module_colors)))
    }

    wgcna_module_gene_file_info <- function(res) {
      module_gene_lists <- wgcna_module_gene_lists(res)
      if (!length(module_gene_lists)) {
        return(data.frame(
          Module = character(0),
          GeneCount = integer(0),
          FileName = character(0),
          DownloadId = character(0),
          Description = character(0),
          stringsAsFactors = FALSE
        ))
      }

      module_names <- names(module_gene_lists)
      txt_names <- paste0("module_", vapply(module_names, wgcna_safe_filename, character(1)), "_genes.txt")
      txt_names <- make.unique(txt_names, sep = "_")

      data.frame(
        Module = module_names,
        GeneCount = lengths(module_gene_lists),
        FileName = txt_names,
        DownloadId = paste0("downloadModuleGenesTxt_", seq_along(module_gene_lists)),
        Description = paste0("模块 ", module_names, " 基因列表，共 ", lengths(module_gene_lists), " 个基因"),
        stringsAsFactors = FALSE
      )
    }

    wgcna_write_module_gene_txt <- function(file, res, module_name) {
      module_gene_lists <- wgcna_module_gene_lists(res)
      genes <- module_gene_lists[[module_name]]
      if (is.null(genes)) {
        stop(paste0("未找到模块：", module_name), call. = FALSE)
      }

      con <- file(file, open = "w", encoding = "UTF-8")
      tryCatch(
        writeLines(genes, con = con),
        finally = close(con)
      )
      invisible(file)
    }
    
    wgcna_blank_plot <- function(message = "") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    }
    
    draw_sample_cluster_plot <- function(res, large = FALSE) {
      if (is.null(res) || is.null(res$sampleDendro)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(cex = if (large) 0.9 else 0.75, mar = c(0, 4, 3, 0))
      plot(res$sampleDendro,
           main = "Sample Clustering for Outlier Detection",
           sub = "", xlab = "", cex.lab = 1.2, cex.axis = 1, cex.main = 1.3)
      if (!is.null(res$sample_cut_height) && is.finite(res$sample_cut_height)) {
        abline(h = res$sample_cut_height, col = "red", lwd = 2)
      }
    }
    
    draw_sample_trait_plot <- function(res, large = FALSE) {
      if (is.null(res) || is.null(res$sampleDendro2) || is.null(res$traitColors)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      WGCNA::plotDendroAndColors(
        res$sampleDendro2,
        res$traitColors,
        groupLabels = colnames(res$traitData),
        main = "Sample Dendrogram and Trait Heatmap",
        dendroLabels = FALSE
      )
    }
    
    draw_power_plot <- function(res, large = FALSE) {
      if (is.null(res)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      sft <- res$sft
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mfrow = c(1, 2), mar = if (large) c(5, 5, 4, 2) else c(4, 4, 3, 2))
      cex1 <- if (large) 1 else 0.9
      plot(sft$fitIndices[, 1],
           -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
           xlab = "Soft Threshold (power)",
           ylab = "Scale Free Topology Model Fit (signed R²)",
           type = "n",
           main = "Scale Independence Analysis",
           cex.lab = 1, cex.main = 1.1)
      text(sft$fitIndices[, 1],
           -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
           labels = sft$fitIndices[, 1], cex = cex1, col = "blue")
      abline(h = wgcna_rsquared_cut(res), col = "red", lwd = 2)
      
      plot(sft$fitIndices[, 1], sft$fitIndices[, 5],
           xlab = "Soft Threshold (power)",
           ylab = "Mean Connectivity",
           type = "n",
           main = "Mean Connectivity",
           cex.lab = 1, cex.main = 1.1)
      text(sft$fitIndices[, 1], sft$fitIndices[, 5],
           labels = sft$fitIndices[, 1], cex = cex1, col = "blue")
    }
    
    draw_dendro_plot <- function(res) {
      if (is.null(res)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      plotDendroAndColors(
        res$geneTree,
        res$dendroColors %||% res$moduleColors,
        "Dynamic Tree Cut Modules",
        dendroLabels = FALSE,
        hang = 0.03,
        addGuide = TRUE,
        guideHang = 0.05,
        main = "Gene Dendrogram and Module Colors",
        cex.main = 1.1
      )
    }
    
    draw_gene_cluster_plot <- function(res, large = FALSE) {
      if (is.null(res) || is.null(res$geneTree)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      plot(res$geneTree, xlab = "", sub = "",
           main = "Gene Clustering Based on TOM",
           labels = FALSE, hang = 0.04,
           cex.main = if (large) 1.3 else 1.1)
    }
    
    draw_module_cluster_plot <- function(res, large = FALSE) {
      if (is.null(res) || is.null(res$moduleEigDendro)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      plot(res$moduleEigDendro,
           main = "Clustering of Module Eigengenes",
           xlab = "", sub = "",
           cex.main = if (large) 1.3 else 1.1)
      abline(h = res$merge_cut_height %||% 0.25, col = "red", lwd = 2)
    }
    
    draw_merged_plot <- function(res, large = FALSE) {
      if (is.null(res) || is.null(res$geneTree)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      original_colors <- res$originalModuleColors %||% res$moduleColors
      merged_colors <- res$dendroColors %||% res$moduleColors
      if (length(original_colors) != length(merged_colors)) {
        original_colors <- merged_colors
      }
      WGCNA::plotDendroAndColors(
        res$geneTree,
        cbind(original_colors, merged_colors),
        c("Original Modules", "Merged Modules"),
        dendroLabels = FALSE,
        hang = 0.03,
        addGuide = TRUE,
        guideHang = 0.05,
        main = "Module Comparison Pre- and Post-Merging"
      )
    }
    
    make_module_count_plot <- function(res, base_size = NULL) {
      if (is.null(res)) return(NULL)
      base_size <- wgcna_clean_number(base_size %||% 13, 13, 6, 24)
      module_sizes <- table(res$moduleColors)
      module_df <- data.frame(Module = names(module_sizes), GeneCount = as.numeric(module_sizes))
      ggplot(module_df, aes(x = Module, y = GeneCount, fill = Module)) +
        geom_bar(stat = "identity") +
        theme_minimal(base_size = base_size) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
        labs(title = "Gene Counts per Module", x = "Module", y = "Gene Count")
    }
    
    make_trait_plot <- function(res, base_size = NULL) {
      if (is.null(res)) {
        return(NULL)
      }
      base_size <- wgcna_clean_number(base_size %||% 13, 13, 6, 24)
      cor_df <- reshape2::melt(res$moduleTraitCor)
      pval_df <- reshape2::melt(res$moduleTraitPvalue)
      heatmap_df <- merge(cor_df, pval_df, by = c("Var1", "Var2"))
      colnames(heatmap_df) <- c("Module", "Trait", "Correlation", "Pvalue")
      heatmap_df$Label <- sprintf("%.2f\n(%s)", heatmap_df$Correlation, formatC(heatmap_df$Pvalue, format = "e", digits = 1))
      
      ggplot(heatmap_df, aes(x = Trait, y = Module, fill = Correlation)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = 0, limit = c(-1, 1),
          name = "Correlation"
        ) +
        geom_text(aes(label = Label),
                  size = max(2.5, base_size / 3.2)) +
        theme_minimal(base_size = base_size) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        labs(title = "Module-Trait Correlation", x = "", y = "")
    }
    
    make_boxplot_mm <- function(res, base_size = NULL) {
      if (is.null(res) || is.null(res$module_summary_df)) return(NULL)
      base_size <- wgcna_clean_number(base_size %||% 13, 13, 6, 24)
      ggplot(res$module_summary_df, aes(x = Module, y = MM, fill = Module)) +
        geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
        theme_minimal(base_size = base_size) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold")) +
        labs(title = "Boxplot of Module Membership across Modules", x = "Module", y = "Module Membership")
    }
    
    make_violin_gs <- function(res, base_size = NULL) {
      if (is.null(res) || is.null(res$module_summary_df)) return(NULL)
      base_size <- wgcna_clean_number(base_size %||% 13, 13, 6, 24)
      ggplot(res$module_summary_df, aes(x = Module, y = GS, fill = Module)) +
        geom_violin(alpha = 0.7, trim = FALSE) +
        geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.6) +
        theme_minimal(base_size = base_size) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold")) +
        labs(title = "Violin Plot of Gene Significance across Modules", x = "Module", y = "Gene Significance for Disease")
    }
    
    make_me_correlation_plot <- function(res, base_size = NULL) {
      if (is.null(res) || is.null(res$moduleEigengeneCor)) return(NULL)
      base_size <- wgcna_clean_number(base_size %||% 13, 13, 6, 24)
      cor_df <- reshape2::melt(res$moduleEigengeneCor)
      colnames(cor_df) <- c("Module1", "Module2", "Correlation")
      ggplot(cor_df, aes(x = Module1, y = Module2, fill = Correlation)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
        geom_text(aes(label = sprintf("%.2f", Correlation)), size = max(2.2, base_size / 4)) +
        theme_minimal(base_size = base_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold")) +
        labs(title = "Module-Module Correlation", x = "", y = "")
    }
    
    draw_mmgs_plot <- function(res, large = FALSE) {
      if (is.null(res)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      targetTrait <- "Disease"
      allModules <- unique(res$moduleColors)
      allModules <- allModules[paste0("ME", allModules) %in% colnames(res$MM)]
      if (!length(allModules)) {
        wgcna_blank_plot("没有可绘制的模块")
        return(invisible(NULL))
      }
      displayModules <- allModules[seq_len(min(6, length(allModules)))]
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mfrow = c(2, 3), mar = if (large) c(4, 4, 3, 2) else c(3.5, 3.5, 2.5, 1.5))
      
      for (mod in displayModules) {
        moduleGenes <- (res$moduleColors == mod)
        mmColName <- paste0("ME", mod)
        gsColName <- targetTrait
        MM <- as.numeric(res$MM[moduleGenes, mmColName])
        GS <- as.numeric(res$GS[moduleGenes, gsColName])
        corTest <- cor.test(MM, GS)
        corVal <- corTest$estimate
        pVal <- corTest$p.value
        plot(MM, GS,
             xlab = paste("MM in", mod),
             ylab = paste("GS for", targetTrait),
             main = paste0("Module: ", mod, "\ncor = ", signif(corVal, 3),
                           ", p = ", format(pVal, scientific = TRUE, digits = 2)),
             pch = 19, col = adjustcolor(mod, alpha.f = 0.6),
             cex = if (large) 0.8 else 0.7,
             cex.main = if (large) 0.9 else 0.8,
             cex.lab = 0.8)
        abline(lm(GS ~ MM), col = "blue", lwd = if (large) 2 else 1.5, lty = 2)
      }
    }
    
    draw_wgcna_plot <- function(plot_key, large = FALSE) {
      res <- wgcna_results()
      if (identical(plot_key, "sample_cluster")) {
        draw_sample_cluster_plot(res, large)
      } else if (identical(plot_key, "sample_trait")) {
        draw_sample_trait_plot(res, large)
      } else if (identical(plot_key, "power")) {
        draw_power_plot(res, large)
      } else if (identical(plot_key, "gene_cluster")) {
        draw_gene_cluster_plot(res, large)
      } else if (identical(plot_key, "dendro")) {
        draw_dendro_plot(res)
      } else if (identical(plot_key, "module_cluster")) {
        draw_module_cluster_plot(res, large)
      } else if (identical(plot_key, "merged")) {
        draw_merged_plot(res, large)
      } else if (identical(plot_key, "trait")) {
        plot_obj <- make_trait_plot(res, if (large) 16 else NULL)
        if (is.null(plot_obj)) wgcna_blank_plot("请先运行 WGCNA 分析") else print(plot_obj)
      } else if (identical(plot_key, "module_count")) {
        plot_obj <- make_module_count_plot(res, if (large) 16 else NULL)
        if (is.null(plot_obj)) wgcna_blank_plot("请先运行 WGCNA 分析") else print(plot_obj)
      } else if (identical(plot_key, "mmgs")) {
        draw_mmgs_plot(res, large)
      } else if (identical(plot_key, "boxplot_mm")) {
        plot_obj <- make_boxplot_mm(res, if (large) 16 else NULL)
        if (is.null(plot_obj)) wgcna_blank_plot("请先运行 WGCNA 分析") else print(plot_obj)
      } else if (identical(plot_key, "violin_gs")) {
        plot_obj <- make_violin_gs(res, if (large) 16 else NULL)
        if (is.null(plot_obj)) wgcna_blank_plot("请先运行 WGCNA 分析") else print(plot_obj)
      } else if (identical(plot_key, "me_corr")) {
        plot_obj <- make_me_correlation_plot(res, if (large) 16 else NULL)
        if (is.null(plot_obj)) wgcna_blank_plot("请先运行 WGCNA 分析") else print(plot_obj)
      } else {
        wgcna_blank_plot("")
      }
    }
    
    get_wgcna_download_size <- function() {
      list(
        width = 10,
        height = 8,
        dpi = 300L
      )
    }
    
    write_wgcna_png <- function(file, plot_key) {
      size <- get_wgcna_download_size()
      png(file, width = size$width * size$dpi, height = size$height * size$dpi, res = size$dpi)
      on.exit(dev.off(), add = TRUE)
      draw_wgcna_plot(plot_key, large = TRUE)
    }
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$countFile, {
      if (!is.null(input$countFile)) {
        wgcna_set_upload_status("countFileStatus", input$countFile$name)
      }
    })
    
    # ---- 清除文件 ----
    observeEvent(input$clearCountFile, {
      shinyjs::reset("countFile")
      shinyjs::runjs(paste0('$("#', ns("countFileStatus"), '").text("Drop file here or click to upload")'))
      wgcna_results(NULL)
    })
    
    # ---- 核心分析 ----
    observeEvent(input$runWgcna, {
      
      if (is.null(input$countFile)) {
        showNotification("请上传带 _con/_tre 后缀的表达矩阵文件！", type = "error")
        return()
      }
      
      count_file <- input$countFile
      power_start <- 1L
      rsquared_cut <- 0.90
      sd_cutoff <- 0.5
      min_module_size <- as.integer(round(wgcna_clean_number(input$minModuleSize, 50, 2, 500)))
      deep_split <- as.integer(round(wgcna_clean_number(input$deepSplit, 2, 0, 4)))
      merge_cut_height <- wgcna_clean_number(input$mergeThreshold, 0.25, 0.01, 1)
      
      is_running(TRUE)
      wgcna_results(NULL)
      task_note <- app_start_task_notification("WGCNA 分析正在后台运行，可以切换到其它模块继续操作。")
      
      run_async_task(
        task = function() {
          wgcna_require_pkg("WGCNA")
          wgcna_require_pkg("dynamicTreeCut")
          wgcna_require_pkg("reshape2")
          if (exists("app_cleanup_leaked_resources", mode = "function")) {
            app_cleanup_leaked_resources(force = TRUE)
          } else {
            try(closeAllConnections(), silent = TRUE)
          }
          task_started_at <- Sys.time()
          step_timings <- list()
          time_wgcna_step <- function(label, expr) {
            step_started_at <- Sys.time()
            value <- force(expr)
            step_timings[[label]] <<- as.numeric(difftime(Sys.time(), step_started_at, units = "secs"))
            value
          }
          
          prep <- time_wgcna_step("预处理", {
            wgcna_prepare_expression_matrix(count_file, sd_cutoff)
          })
          combined_expr <- prep$combined_expr
          ctrl_samples <- prep$ctrl_samples
          treat_samples <- prep$treat_samples
          num_ctrl <- prep$num_ctrl
          num_treat <- prep$num_treat
          data_ctrl <- combined_expr[, ctrl_samples, drop = FALSE]
          data_treat <- combined_expr[, treat_samples, drop = FALSE]
          datExpr <- t(combined_expr)
          
          gsg <- WGCNA::goodSamplesGenes(datExpr, verbose = 3)
          if (!gsg$allOK) {
            datExpr <- datExpr[gsg$goodSamples, gsg$goodGenes]
          }
          
          sampleDendro <- stats::hclust(stats::dist(datExpr), method = "average")
          sample_cut_height <- 20000
          sample_min_size <- min(10L, nrow(datExpr))
          if (nrow(datExpr) >= sample_min_size && sample_min_size >= 2L) {
            clusterCut <- WGCNA::cutreeStatic(sampleDendro, cutHeight = sample_cut_height, minSize = sample_min_size)
            if (length(unique(clusterCut)) > 1) {
              keepSamples <- clusterCut == 1
              datExpr <- datExpr[keepSamples, , drop = FALSE]
            }
          }
          
          sample_group <- c(stats::setNames(rep("Normal", num_ctrl), colnames(data_ctrl)),
                            stats::setNames(rep("Disease", num_treat), colnames(data_treat)))
          traitData <- data.frame(
            Normal = as.integer(sample_group[rownames(datExpr)] == "Normal"),
            Disease = as.integer(sample_group[rownames(datExpr)] == "Disease"),
            row.names = rownames(datExpr)
          )
          sampleDendro2 <- stats::hclust(stats::dist(datExpr), method = "average")
          traitColors <- WGCNA::numbers2colors(traitData, signed = FALSE)
          
          powers <- seq(power_start, 20L, by = 1L)
          sft <- time_wgcna_step("软阈值选择", {
            WGCNA::pickSoftThreshold(
              datExpr,
              powerVector = powers,
              verbose = 5
            )
          })
          
          if (is.null(sft$powerEstimate) || is.na(sft$powerEstimate)) {
            stop("未能确定最佳软阈值！请检查数据。", call. = FALSE)
          } else {
            optimalPower <- sft$powerEstimate
          }
          
          network <- time_wgcna_step("模块构建", {
            adjacencyMatrix <- WGCNA::adjacency(datExpr, power = optimalPower)
            TOMMatrix <- WGCNA::TOMsimilarity(adjacencyMatrix)
            dissTOM <- 1 - TOMMatrix
            geneTree <- stats::hclust(stats::as.dist(dissTOM), method = "average")
            effective_min_module_size <- min(min_module_size, max(2L, floor(ncol(datExpr) / 3)))
            dynamicModuleLabels <- dynamicTreeCut::cutreeDynamic(
              dendro = geneTree,
              distM = dissTOM,
              deepSplit = deep_split,
              pamRespectsDendro = FALSE,
              minClusterSize = effective_min_module_size
            )
            originalColors <- WGCNA::labels2colors(dynamicModuleLabels)
            moduleEigengenesList <- WGCNA::moduleEigengenes(datExpr, colors = originalColors, excludeGrey = FALSE)
            MEs <- moduleEigengenesList$eigengenes
            moduleDiss <- 1 - stats::cor(MEs)
            moduleEigDendro <- stats::hclust(stats::as.dist(moduleDiss), method = "average")
            mergeResult <- WGCNA::mergeCloseModules(
              datExpr,
              originalColors,
              cutHeight = merge_cut_height,
              verbose = 3
            )
            list(
              geneTree = geneTree,
              originalColors = originalColors,
              mergedColors = mergeResult$colors,
              mergedMEs = mergeResult$newMEs,
              moduleEigDendro = moduleEigDendro,
              effective_min_module_size = effective_min_module_size
            )
          })
          mergedColors <- network$mergedColors
          if (is.null(mergedColors) || !length(mergedColors)) {
            stop(
              "没有检测到 WGCNA 模块颜色，请降低 MAD阈值、降低最小模块或提高最大基因数后重试。",
              call. = FALSE
            )
          }
          mergedMEs <- network$mergedMEs
          if (is.null(mergedMEs) || !ncol(mergedMEs)) {
            mergedMEs <- WGCNA::moduleEigengenes(datExpr, colors = mergedColors, excludeGrey = FALSE)$eigengenes
          }
          mergedMEs <- WGCNA::orderMEs(mergedMEs)
          if (!ncol(mergedMEs)) {
            stop("未能计算模块特征基因，请降低最小模块或提高最大基因数后重试。", call. = FALSE)
          }
          geneTree <- network$geneTree
          dendroColors <- mergedColors
          originalModuleColors <- network$originalColors %||% mergedColors
          dendroOriginalColors <- originalModuleColors
          moduleEigengeneCor <- stats::cor(mergedMEs, use = "p")
          moduleEigDendro <- network$moduleEigDendro
          
          moduleTraitCor <- stats::cor(mergedMEs, traitData, use = "p")
          moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCor, nrow(datExpr))
          
          GS <- as.data.frame(stats::cor(datExpr, traitData, use = "p"))
          MM <- as.data.frame(stats::cor(datExpr, mergedMEs, use = "p"))
          GSPvalue <- as.data.frame(WGCNA::corPvalueStudent(as.matrix(GS), nrow(datExpr)))
          MMPvalue <- as.data.frame(WGCNA::corPvalueStudent(as.matrix(MM), nrow(datExpr)))
          module_summary_df <- data.frame(
            Module = mergedColors,
            MM = vapply(seq_along(mergedColors), function(i) {
              col <- paste0("ME", mergedColors[i])
              if (col %in% colnames(MM)) as.numeric(MM[i, col]) else NA_real_
            }, numeric(1)),
            GS = as.numeric(GS[, "Disease"]),
            stringsAsFactors = FALSE
          )
          
          list(
            datExpr = datExpr,
            traitData = traitData,
            sampleDendro = sampleDendro,
            sampleDendro2 = sampleDendro2,
            sample_cut_height = sample_cut_height,
            traitColors = traitColors,
            sft = sft,
            optimalPower = optimalPower,
            geneTree = geneTree,
            dendroColors = dendroColors,
            originalModuleColors = dendroOriginalColors,
            moduleColors = mergedColors,
            moduleEigDendro = moduleEigDendro,
            moduleEigengeneCor = moduleEigengeneCor,
            mergedMEs = mergedMEs,
            moduleTraitCor = moduleTraitCor,
            moduleTraitPvalue = moduleTraitPvalue,
            GS = GS,
            MM = MM,
            GSPvalue = GSPvalue,
            MMPvalue = MMPvalue,
            module_summary_df = module_summary_df,
            num_ctrl = num_ctrl,
            num_treat = num_treat,
            rsquared_cut = rsquared_cut,
            sd_cutoff = sd_cutoff,
            min_module_size = network$effective_min_module_size,
            merge_cut_height = merge_cut_height,
            deep_split = deep_split,
            elapsed_seconds = as.numeric(difftime(Sys.time(), task_started_at, units = "secs")),
            step_timings = unlist(step_timings, use.names = TRUE)
          )
        },
        on_success = function(result) {
          app_clear_task_notification(task_note)
          wgcna_results(result)
          active_wgcna_plot("power")
          timing_text <- ""
          if (!is.null(result$elapsed_seconds)) {
            timing_text <- paste0("，耗时 ", round(result$elapsed_seconds, 1), " 秒")
            if (!is.null(result$step_timings) && length(result$step_timings)) {
              timing_text <- paste0(
                timing_text,
                "（",
                paste(paste0(names(result$step_timings), " ", round(result$step_timings, 1), "秒"), collapse = "；"),
                "）"
              )
            }
          }
          showNotification(
            paste0("WGCNA 分析完成！共检测到 ",
                   length(unique(result$moduleColors)), " 个模块", timing_text),
            type = "message",
            duration = 20
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
    
    output$activeWgcnaPlot <- renderPlot({
      draw_wgcna_plot(active_wgcna_plot())
    })
    
    observeEvent(input$activeWgcnaPlot_click, {
      if (is.null(wgcna_results())) {
        return()
      }
      showModal(
        modalDialog(
          title = wgcna_plot_label(active_wgcna_plot()),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("activeWgcnaPlotLarge"), height = "70vh", width = "100%")
          )
        )
      )
    })
    
    output$activeWgcnaPlotLarge <- renderPlot({
      draw_wgcna_plot(active_wgcna_plot(), large = TRUE)
    })
    
    output$resultFileList <- renderUI({
      has_results <- !is.null(wgcna_results())
      files <- data.frame(
        文件名 = c(
          "Sample_Clustering.png",
          "Sample_Heatmap.png",
          "Scale_Independence_and_Mean_Connectivity.png",
          "Gene_Clustering.png",
          "Dynamic_Tree_Modules.png",
          "Module_Clustering.png",
          "Merged_Modules_Comparison.png",
          "Module_Trait_Heatmap.png",
          "Module_Gene_Counts.png",
          "WGCNA_MM_vs_GS.png",
          "Boxplot_MM.png",
          "ViolinPlot_GS.png",
          "Module_Module_Correlation.png",
          "WGCNA_module_info.csv",
          "WGCNA_gene_info.csv"
        ),
        类型 = c(rep("PNG", 13), "CSV", "CSV"),
        说明 = c(
          "样本聚类与异常值检查图，点击文件名可在上方显示",
          "样本聚类树与分组性状热图，点击文件名可在上方显示",
          "软阈值选择图，点击文件名可在上方显示",
          "基于网络拓扑的基因聚类图，点击文件名可在上方显示",
          "动态树切割模块识别图，点击文件名可在上方显示",
          "模块特征基因聚类及合并阈值图，点击文件名可在上方显示",
          "模块合并前后对比图，点击文件名可在上方显示",
          "模块与性状相关性热图，点击文件名可在上方显示",
          "每个模块包含基因数量图，点击文件名可在上方显示",
          "模块成员度与基因显著性散点图，点击文件名可在上方显示",
          "不同模块 MM 分布箱线图，点击文件名可在上方显示",
          "不同模块 GS 分布小提琴图，点击文件名可在上方显示",
          "模块特征基因相关性热图，点击文件名可在上方显示",
          "各模块基因数及与性状相关性",
          "每个基因的模块归属、GS、MM 及对应 P 值"
        ),
        plot_key = c("sample_cluster", "sample_trait", "power", "gene_cluster", "dendro", "module_cluster", "merged", "trait", "module_count", "mmgs", "boxplot_mm", "violin_gs", "me_corr", "", ""),
        download_id = c(rep("", 13), "downloadModuleTable", "downloadGeneInfo"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      if (!has_results) {
        return(NULL)
      }

      module_file_info <- wgcna_module_gene_file_info(wgcna_results())
      if (nrow(module_file_info)) {
        files <- rbind(
          files,
          data.frame(
            文件名 = module_file_info$FileName,
            类型 = "TXT",
            说明 = module_file_info$Description,
            plot_key = "",
            download_id = module_file_info$DownloadId,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        )
      }
      
      tags$div(
        class = "wgcna-result-file-list",
        lapply(seq_len(nrow(files)), function(i) {
          plot_key <- files$plot_key[i]
          is_png <- identical(files$类型[i], "PNG")
          name_control <- if (is_png) {
            actionButton(
              ns(paste0("showWgcnaPlot_", plot_key)),
              files$文件名[i],
              class = "wgcna-result-file-action",
              title = "点击后在上方图片区显示"
            )
          } else {
            span(files$文件名[i], class = "wgcna-result-file-name", title = files$文件名[i])
          }
          
          download_control <- if (is_png) {
            downloadButton(ns(paste0("downloadWgcnaPlotFile_", plot_key)), "下载", class = "btn-primary btn-xs")
          } else {
            downloadButton(ns(files$download_id[i]), "下载", class = "btn-primary btn-xs")
          }
          
          tags$div(
            class = "wgcna-result-file-row",
            span(sprintf("%02d", i), class = "wgcna-file-index"),
            name_control,
            span(files$类型[i], class = "wgcna-result-file-type"),
            span(files$说明[i], class = "wgcna-result-file-desc", title = files$说明[i]),
            span(class = "wgcna-result-file-download", download_control)
          )
        })
      )
    })

    observe({
      module_file_info <- wgcna_module_gene_file_info(wgcna_results())
      if (!nrow(module_file_info)) {
        return()
      }

      for (i in seq_len(nrow(module_file_info))) {
        local({
          module_name <- module_file_info$Module[i]
          download_id <- module_file_info$DownloadId[i]
          txt_filename <- module_file_info$FileName[i]

          output[[download_id]] <- downloadHandler(
            filename = function() txt_filename,
            content = function(file) {
              wgcna_write_module_gene_txt(file, wgcna_results(), module_name)
            },
            contentType = "text/plain"
          )
        })
      }
    })
    
    lapply(c("sample_cluster", "sample_trait", "power", "gene_cluster", "dendro", "module_cluster", "merged", "trait", "module_count", "mmgs", "boxplot_mm", "violin_gs", "me_corr"), function(plot_key) {
      local({
        key <- plot_key
        observeEvent(input[[paste0("showWgcnaPlot_", key)]], {
          active_wgcna_plot(key)
        })
        output[[paste0("downloadWgcnaPlotFile_", key)]] <- downloadHandler(
          filename = function() wgcna_plot_filename(key),
          content = function(file) write_wgcna_png(file, key)
        )
      })
    })
    
    # ---- 软阈值图 ----
    output$powerPlot <- renderPlot({
      res <- wgcna_results()
      if (is.null(res)) {
        wgcna_blank_plot()
        return()
      }
      
      sft <- res$sft
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      cex1 <- 0.9
      
      plot(sft$fitIndices[, 1], 
           -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
           xlab = "Soft Threshold (power)", 
           ylab = "Scale Free Topology Model Fit (signed R²)",
           type = "n", 
           main = "Scale Independence Analysis",
           cex.lab = 1, cex.main = 1.1)
      text(sft$fitIndices[, 1], 
           -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
           labels = sft$fitIndices[, 1], cex = cex1, col = "blue")
      abline(h = wgcna_rsquared_cut(res), col = "red", lwd = 2)
      
      plot(sft$fitIndices[, 1], sft$fitIndices[, 5],
           xlab = "Soft Threshold (power)", 
           ylab = "Mean Connectivity",
           type = "n", 
           main = "Mean Connectivity",
           cex.lab = 1, cex.main = 1.1)
      text(sft$fitIndices[, 1], sft$fitIndices[, 5],
           labels = sft$fitIndices[, 1], cex = cex1, col = "blue")
    })
    
    # ---- 模块树状图 ----
    output$dendroPlot <- renderPlot({
      res <- wgcna_results()
      if (is.null(res)) {
        wgcna_blank_plot()
        return()
      }
      
      plotDendroAndColors(
        res$geneTree, 
        res$dendroColors %||% res$moduleColors,
        "Dynamic Tree Cut Modules",
        dendroLabels = FALSE, 
        hang = 0.03,
        addGuide = TRUE, 
        guideHang = 0.05,
        main = "Gene Dendrogram and Module Colors",
        cex.main = 1.1
      )
    })
    
    # ---- 模块-性状热图 ----
    output$traitPlot <- renderPlot({
      res <- wgcna_results()
      if (is.null(res)) {
        wgcna_blank_plot()
        return()
      }
      
      cor_df <- reshape2::melt(res$moduleTraitCor)
      pval_df <- reshape2::melt(res$moduleTraitPvalue)
      heatmap_df <- merge(cor_df, pval_df, by = c("Var1", "Var2"))
      colnames(heatmap_df) <- c("Module", "Trait", "Correlation", "Pvalue")
      
      ggplot(heatmap_df, aes(x = Trait, y = Module, fill = Correlation)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = 0, limit = c(-1, 1),
          name = "Correlation"
        ) +
        geom_text(aes(label = sprintf("%.2f\n(%s)", Correlation, formatC(Pvalue, format = "e", digits = 1))), 
                  size = 3.5) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
        ) +
        labs(title = "Module-Trait Correlation", x = "", y = "")
    })
    
    # ---- MM vs GS 散点图 ----
    output$mmgsPlot <- renderPlot({
      res <- wgcna_results()
      if (is.null(res)) {
        wgcna_blank_plot()
        return()
      }
      
      targetTrait <- "Disease"
      allModules <- unique(res$moduleColors)
      displayModules <- allModules[1:min(6, length(allModules))]
      
      par(mfrow = c(2, 3), mar = c(3.5, 3.5, 2.5, 1.5))
      
      for (mod in displayModules) {
        moduleGenes <- (res$moduleColors == mod)
        mmColName <- paste0("ME", mod)
        gsColName <- targetTrait
        
        MM <- as.numeric(res$MM[moduleGenes, mmColName])
        GS <- as.numeric(res$GS[moduleGenes, gsColName])
        
        corTest <- cor.test(MM, GS)
        corVal <- corTest$estimate
        pVal <- corTest$p.value
        
        plot(MM, GS,
             xlab = paste("MM in", mod),
             ylab = paste("GS for", targetTrait),
             main = paste0("Module: ", mod, "\ncor = ", signif(corVal, 3),
                           ", p = ", format(pVal, scientific = TRUE, digits = 2)),
             pch = 19, col = adjustcolor(mod, alpha.f = 0.6),
             cex = 0.7, cex.main = 0.8, cex.lab = 0.8)
        abline(lm(GS ~ MM), col = "blue", lwd = 1.5, lty = 2)
      }
    })
    
    # ---- 模块信息表 ----
    output$moduleTable <- renderDT({
      res <- wgcna_results()
      if (is.null(res)) {
        return(NULL)
      }
      
      module_sizes <- table(res$moduleColors)
      module_df <- data.frame(
        模块 = names(module_sizes),
        基因数 = as.numeric(module_sizes)
      )
      
      disease_cor <- res$moduleTraitCor[, "Disease"]
      disease_pval <- res$moduleTraitPvalue[, "Disease"]
      module_df$与Disease相关性 <- disease_cor[module_df$模块]
      module_df$Disease_P值 <- disease_pval[module_df$模块]
      
      normal_cor <- res$moduleTraitCor[, "Normal"]
      normal_pval <- res$moduleTraitPvalue[, "Normal"]
      module_df$与Normal相关性 <- normal_cor[module_df$模块]
      module_df$Normal_P值 <- normal_pval[module_df$模块]
      
      datatable(module_df, options = list(pageLength = 8, scrollX = TRUE, dom = 'ftp'),
                rownames = FALSE)
    })
    
    # ---- 更新模块计数 ----
    output$moduleCount <- renderUI({
      res <- wgcna_results()
      if (is.null(res)) {
        tags$span("共 0 个模块", style = "font-size: 11px; color: #888;")
      } else {
        tags$span(paste0("共 ", length(unique(res$moduleColors)), " 个模块"), 
                  style = "font-size: 11px; color: #888;")
      }
    })
    
    # ---- 基因信息表 ----
    output$geneInfoTable <- renderDT({
      res <- wgcna_results()
      if (is.null(res)) {
        return(NULL)
      }
      
      gene_names <- colnames(res$datExpr)
      gene_df <- data.frame(
        基因 = gene_names,
        模块 = res$moduleColors,
        GS_Disease = res$GS[, "Disease"],
        p_GS_Disease = res$GSPvalue[, "Disease"],
        GS_Normal = res$GS[, "Normal"],
        p_GS_Normal = res$GSPvalue[, "Normal"]
      )
      
      for (mod in colnames(res$MM)) {
        gene_df[[paste0("MM_", mod)]] <- res$MM[, mod]
        gene_df[[paste0("p.MM_", mod)]] <- res$MMPvalue[, mod]
      }
      
      datatable(gene_df, options = list(pageLength = 8, scrollX = TRUE, dom = 'ftp'),
                rownames = FALSE)
    })
    
    # ---- 更新基因计数 ----
    output$geneCount <- renderUI({
      res <- wgcna_results()
      if (is.null(res)) {
        tags$span("共 0 个基因", style = "font-size: 11px; color: #888;")
      } else {
        tags$span(paste0("共 ", ncol(res$datExpr), " 个基因"), 
                  style = "font-size: 11px; color: #888;")
      }
    })
    
    # ============================================================
    # 下载功能
    # ============================================================
    
    output$downloadPowerPlot <- downloadHandler(
      filename = "WGCNA_power_plot.png",
      content = function(file) {
        res <- wgcna_results()
        if (is.null(res)) {
          png(file, width = 2100, height = 1800, res = 300)
          plot(1, type = "n", main = "请先运行 WGCNA 分析")
          dev.off()
          return()
        }
        
        png(file, width = 2100, height = 1800, res = 300)
        sft <- res$sft
        par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
        
        plot(sft$fitIndices[, 1], 
             -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
             xlab = "Soft Threshold (power)", 
             ylab = "Scale Free Topology Model Fit (signed R²)",
             type = "n", 
             main = "Scale Independence Analysis")
        text(sft$fitIndices[, 1], 
             -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
             labels = sft$fitIndices[, 1], col = "blue")
        abline(h = wgcna_rsquared_cut(res), col = "red", lwd = 2)
        
        plot(sft$fitIndices[, 1], sft$fitIndices[, 5],
             xlab = "Soft Threshold (power)", 
             ylab = "Mean Connectivity",
             type = "n", 
             main = "Mean Connectivity")
        text(sft$fitIndices[, 1], sft$fitIndices[, 5],
             labels = sft$fitIndices[, 1], col = "blue")
        dev.off()
      }
    )
    
    output$downloadDendroPlot <- downloadHandler(
      filename = "WGCNA_dendro.png",
      content = function(file) {
        res <- wgcna_results()
        if (is.null(res)) {
          png(file, width = 2100, height = 1800, res = 300)
          plot(1, type = "n", main = "请先运行 WGCNA 分析")
          dev.off()
          return()
        }
        
        png(file, width = 2100, height = 1800, res = 300)
        plotDendroAndColors(
          res$geneTree, 
          res$dendroColors %||% res$moduleColors,
          "Dynamic Tree Cut Modules",
          dendroLabels = FALSE, 
          hang = 0.03,
          addGuide = TRUE, 
          guideHang = 0.05,
          main = "Gene Dendrogram and Module Colors"
        )
        dev.off()
      }
    )
    
    output$downloadTraitPlot <- downloadHandler(
      filename = "WGCNA_trait_heatmap.png",
      content = function(file) {
        res <- wgcna_results()
        if (is.null(res)) {
          png(file, width = 2100, height = 1800, res = 300)
          plot(1, type = "n", main = "请先运行 WGCNA 分析")
          dev.off()
          return()
        }
        
        cor_df <- reshape2::melt(res$moduleTraitCor)
        pval_df <- reshape2::melt(res$moduleTraitPvalue)
        heatmap_df <- merge(cor_df, pval_df, by = c("Var1", "Var2"))
        colnames(heatmap_df) <- c("Module", "Trait", "Correlation", "Pvalue")
        
        p <- ggplot(heatmap_df, aes(x = Trait, y = Module, fill = Correlation)) +
          geom_tile(color = "white") +
          scale_fill_gradient2(
            low = "blue", high = "red", mid = "white",
            midpoint = 0, limit = c(-1, 1),
            name = "Correlation"
          ) +
          geom_text(aes(label = sprintf("%.2f\n(%s)", Correlation, formatC(Pvalue, format = "e", digits = 1))), 
                    size = 4) +
          theme_minimal(base_size = 14) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
          ) +
          labs(title = "Module-Trait Correlation", x = "", y = "")
        
        ggsave(file, p, width = 7, height = 6, dpi = 300)
      }
    )
    
    output$downloadMmgsPlot <- downloadHandler(
      filename = "WGCNA_MM_vs_GS.png",
      content = function(file) {
        res <- wgcna_results()
        if (is.null(res)) {
          png(file, width = 2100, height = 1800, res = 300)
          plot(1, type = "n", main = "请先运行 WGCNA 分析")
          dev.off()
          return()
        }
        
        targetTrait <- "Disease"
        allModules <- unique(res$moduleColors)
        displayModules <- allModules[1:min(6, length(allModules))]
        
        png(file, width = 2100, height = 1800, res = 300)
        par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
        
        for (mod in displayModules) {
          moduleGenes <- (res$moduleColors == mod)
          mmColName <- paste0("ME", mod)
          gsColName <- targetTrait
          
          MM <- as.numeric(res$MM[moduleGenes, mmColName])
          GS <- as.numeric(res$GS[moduleGenes, gsColName])
          
          corTest <- cor.test(MM, GS)
          corVal <- corTest$estimate
          pVal <- corTest$p.value
          
          plot(MM, GS,
               xlab = paste("MM in", mod),
               ylab = paste("GS for", targetTrait),
               main = paste0("Module: ", mod, "\ncor = ", signif(corVal, 3),
                             ", p = ", format(pVal, scientific = TRUE, digits = 2)),
               pch = 19, col = adjustcolor(mod, alpha.f = 0.6),
               cex = 0.8, cex.main = 0.9)
          abline(lm(GS ~ MM), col = "blue", lwd = 2, lty = 2)
        }
        dev.off()
      }
    )
    
    output$downloadModuleTable <- downloadHandler(
      filename = "WGCNA_module_info.csv",
      content = function(file) {
        res <- wgcna_results()
        if (is.null(res)) {
          write.csv(data.frame(信息 = "请先运行 WGCNA 分析"), file, row.names = FALSE)
          return()
        }
        
        module_sizes <- table(res$moduleColors)
        module_df <- data.frame(
          Module = names(module_sizes),
          GeneCount = as.numeric(module_sizes)
        )
        disease_cor <- res$moduleTraitCor[, "Disease"]
        disease_pval <- res$moduleTraitPvalue[, "Disease"]
        module_df$Correlation_Disease <- disease_cor[module_df$Module]
        module_df$Pvalue_Disease <- disease_pval[module_df$Module]
        normal_cor <- res$moduleTraitCor[, "Normal"]
        normal_pval <- res$moduleTraitPvalue[, "Normal"]
        module_df$Correlation_Normal <- normal_cor[module_df$Module]
        module_df$Pvalue_Normal <- normal_pval[module_df$Module]
        write.csv(module_df, file, row.names = FALSE)
      }
    )
    
    output$downloadGeneInfo <- downloadHandler(
      filename = "WGCNA_gene_info.csv",
      content = function(file) {
        res <- wgcna_results()
        if (is.null(res)) {
          write.csv(data.frame(信息 = "请先运行 WGCNA 分析"), file, row.names = FALSE)
          return()
        }
        
        gene_names <- colnames(res$datExpr)
        gene_df <- data.frame(
          Gene = gene_names,
          Module = res$moduleColors,
          GS_Disease = res$GS[, "Disease"],
          p_GS_Disease = res$GSPvalue[, "Disease"],
          GS_Normal = res$GS[, "Normal"],
          p_GS_Normal = res$GSPvalue[, "Normal"]
        )
        for (mod in colnames(res$MM)) {
          gene_df[[paste0("MM_", mod)]] <- res$MM[, mod]
          gene_df[[paste0("p.MM_", mod)]] <- res$MMPvalue[, mod]
        }
        write.csv(gene_df, file, row.names = FALSE)
      }
    )

  })
}
