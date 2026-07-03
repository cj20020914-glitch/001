# wgcna_module.R - WGCNA 共表达网络分析模块（完整版）
# 功能：构建共表达网络，检测模块，分析模块与性状关系
# 参考差异分析模块的布局和风格

# 关闭多线程警告
options(WGCNA.threads = 0)

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
            max-height: 190px;
            overflow-y: auto;
        }
        .wgcna-result-data-preview h5 {
            margin: 4px 0 6px 0;
            font-size: 12px;
            font-weight: 700;
            color: #263238;
        }
        .wgcna-download-size-controls {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px;
            margin-top: 8px;
        }
        .wgcna-download-size-controls .shiny-input-container {
            width: 100%;
            margin-bottom: 0;
        }
        .wgcna-qa {
            font-size: 12px;
            line-height: 1.7;
            color: #455a64;
            max-height: 190px;
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
          h4("参数设置"),
          div(
            class = "wgcna-upload-row",
            div(
              class = "wgcna-upload-box",
              id = ns("countFileBox"),
              tags$div(
                class = "wgcna-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(
                  tags$span("表达矩阵", class = "wgcna-upload-title"),
                  tags$span(id = ns("countFileStatus"), "Drop file here or click to upload", class = "wgcna-upload-status")
                )
              ),
              fileInput(ns("countFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择表达矩阵文件")
            )
          ),
          div(
            class = "wgcna-upload-row",
            div(
              class = "wgcna-upload-box",
              id = ns("ctrlFileBox"),
              tags$div(
                class = "wgcna-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(
                  tags$span("对照组列表", class = "wgcna-upload-title"),
                  tags$span(id = ns("ctrlFileStatus"), "Drop file here or click to upload", class = "wgcna-upload-status")
                )
              ),
              fileInput(ns("ctrlFile"), NULL,
                        accept = c(".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择对照组列表")
            )
          ),
          div(
            class = "wgcna-upload-row",
            div(
              class = "wgcna-upload-box",
              id = ns("treatFileBox"),
              tags$div(
                class = "wgcna-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(
                  tags$span("实验组列表", class = "wgcna-upload-title"),
                  tags$span(id = ns("treatFileStatus"), "Drop file here or click to upload", class = "wgcna-upload-status")
                )
              ),
              fileInput(ns("treatFile"), NULL,
                        accept = c(".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择实验组列表")
            )
          ),
          div(
            class = "wgcna-compact-section",
            span("模块检测参数", class = "wgcna-compact-title"),
            div(
              class = "wgcna-param-grid wgcna-param-grid-2",
              div(class = "wgcna-mini-control", span("MAD阈值"), numericInput(ns("madCutoff"), NULL, value = 0.5, min = 0, max = 10, step = 0.1)),
              div(class = "wgcna-mini-control", span("最小模块"), numericInput(ns("minModuleSize"), NULL, value = 50, min = 20, max = 200)),
              div(class = "wgcna-mini-control", span("合并阈值"), numericInput(ns("mergeCutHeight"), NULL, value = 0.25, min = 0.05, max = 0.5, step = 0.05)),
              div(class = "wgcna-mini-control", span("deepSplit"), numericInput(ns("deepSplit"), NULL, value = 2, min = 0, max = 4, step = 1))
            )
          ),
          div(
            class = "wgcna-compact-section",
            span("图片参数", class = "wgcna-compact-title"),
            div(
              class = "wgcna-param-grid",
              div(class = "wgcna-mini-control", span("宽度"), numericInput(ns("wgcnaWidth"), NULL, value = 7, min = 3, max = 20, step = 0.5)),
              div(class = "wgcna-mini-control", span("高度"), numericInput(ns("wgcnaHeight"), NULL, value = 6, min = 3, max = 20, step = 0.5)),
              div(class = "wgcna-mini-control", span("字号"), numericInput(ns("wgcnaBaseSize"), NULL, value = 11, min = 6, max = 24, step = 1))
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
                    tags$dd("需要表达矩阵、对照组样本列表和实验组样本列表。表达矩阵第一列为基因名，后续列为样本表达值；分组列表每行一个样本名。"),
                    tags$dt("Q3：软阈值 power 如何理解？"),
                    tags$dd("软阈值用于把相关性转为网络连接强度，目标是让网络近似无标度。通常参考 Scale Free Topology Model Fit，R² 达到阈值后选择较小 power。"),
                    tags$dt("Q4：最小模块和合并阈值怎么调？"),
                    tags$dd("最小模块越大，模块数量通常越少且更稳定；合并阈值越大，相似模块越容易合并。默认值适合先跑通流程，再根据模块数量调整。"),
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
    download_wgcna_plot <- reactiveVal("power")

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
        power = "软阈值图",
        dendro = "模块树状图",
        trait = "模块-性状热图",
        mmgs = "MM vs GS"
      )
      labels[[plot_key]] %||% "WGCNA图片"
    }

    wgcna_plot_filename <- function(plot_key) {
      switch(
        plot_key,
        power = "WGCNA_power_plot.png",
        dendro = "WGCNA_dendro.png",
        trait = "WGCNA_trait_heatmap.png",
        mmgs = "WGCNA_MM_vs_GS.png",
        "WGCNA_plot.png"
      )
    }

    wgcna_blank_plot <- function(message = "") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
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
        res$moduleColors,
        "Dynamic Tree Cut Modules",
        dendroLabels = FALSE,
        hang = 0.03,
        addGuide = TRUE,
        guideHang = 0.05,
        main = "Gene Dendrogram and Module Colors",
        cex.main = 1.1
      )
    }

    make_trait_plot <- function(res, base_size = NULL) {
      if (is.null(res)) {
        return(NULL)
      }
      base_size <- wgcna_clean_number(base_size %||% input$wgcnaBaseSize, 11, 6, 24)
      cor_df <- melt(res$moduleTraitCor)
      pval_df <- melt(res$moduleTraitPvalue)
      heatmap_df <- merge(cor_df, pval_df, by = c("Var1", "Var2"))
      colnames(heatmap_df) <- c("Module", "Trait", "Correlation", "Pvalue")
      heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.05, "*", "")
      heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.01, "**", heatmap_df$Signif)
      heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.001, "***", heatmap_df$Signif)

      ggplot(heatmap_df, aes(x = Trait, y = Module, fill = Correlation)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = 0, limit = c(-1, 1),
          name = "Correlation"
        ) +
        geom_text(aes(label = paste0(sprintf("%.2f", Correlation), Signif)),
                  size = max(2.5, base_size / 3.2)) +
        theme_minimal(base_size = base_size) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        labs(title = "Module-Trait Correlation", x = "", y = "")
    }

    draw_mmgs_plot <- function(res, large = FALSE) {
      if (is.null(res)) {
        wgcna_blank_plot("请先运行 WGCNA 分析")
        return(invisible(NULL))
      }
      targetTrait <- "Disease"
      allModules <- unique(res$moduleColors)
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
      if (identical(plot_key, "power")) {
        draw_power_plot(res, large)
      } else if (identical(plot_key, "dendro")) {
        draw_dendro_plot(res)
      } else if (identical(plot_key, "trait")) {
        plot_obj <- make_trait_plot(res, if (large) wgcna_clean_number(input$wgcnaBaseSize, 11, 6, 24) + 3 else NULL)
        if (is.null(plot_obj)) wgcna_blank_plot("请先运行 WGCNA 分析") else print(plot_obj)
      } else if (identical(plot_key, "mmgs")) {
        draw_mmgs_plot(res, large)
      } else {
        wgcna_blank_plot("")
      }
    }

    get_wgcna_download_size <- function() {
      list(
        width = wgcna_clean_number(input$downloadWgcnaWidth, wgcna_clean_number(input$wgcnaWidth, 7, 3, 20), 3, 20),
        height = wgcna_clean_number(input$downloadWgcnaHeight, wgcna_clean_number(input$wgcnaHeight, 6, 3, 20), 3, 20),
        dpi = as.integer(round(wgcna_clean_number(input$downloadWgcnaDpi, 300, 72, 600)))
      )
    }

    write_wgcna_png <- function(file, plot_key) {
      size <- get_wgcna_download_size()
      png(file, width = size$width * size$dpi, height = size$height * size$dpi, res = size$dpi)
      on.exit(dev.off(), add = TRUE)
      draw_wgcna_plot(plot_key, large = TRUE)
    }

    show_wgcna_download_modal <- function(plot_key) {
      download_wgcna_plot(plot_key)
      showModal(
        modalDialog(
          title = paste0("下载", wgcna_plot_label(plot_key)),
          p("设置导出图片尺寸后点击下载。单位为英寸，DPI 用于控制分辨率。",
            style = "color: #607d8b; font-size: 12px; margin: 0 0 8px 0;"),
          div(
            class = "wgcna-download-size-controls",
            numericInput(ns("downloadWgcnaWidth"), "宽(in)", value = wgcna_clean_number(input$wgcnaWidth, 7, 3, 20), min = 3, max = 20, step = 0.5),
            numericInput(ns("downloadWgcnaHeight"), "高(in)", value = wgcna_clean_number(input$wgcnaHeight, 6, 3, 20), min = 3, max = 20, step = 0.5),
            numericInput(ns("downloadWgcnaDpi"), "DPI", value = 300, min = 72, max = 600, step = 50)
          ),
          footer = tagList(
            modalButton("取消"),
            downloadButton(ns("downloadWgcnaModalPNG"), "下载PNG", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    }
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$countFile, {
      if (!is.null(input$countFile)) {
        shinyjs::runjs(paste0('$("#', ns("countFileStatus"), '").text("', input$countFile$name, '")'))
      }
    })
    
    observeEvent(input$ctrlFile, {
      if (!is.null(input$ctrlFile)) {
        shinyjs::runjs(paste0('$("#', ns("ctrlFileStatus"), '").text("', input$ctrlFile$name, '")'))
      }
    })
    
    observeEvent(input$treatFile, {
      if (!is.null(input$treatFile)) {
        shinyjs::runjs(paste0('$("#', ns("treatFileStatus"), '").text("', input$treatFile$name, '")'))
      }
    })
    
    # ---- 清除文件 ----
    observeEvent(input$clearCountFile, {
      shinyjs::reset("countFile")
      shinyjs::runjs(paste0('$("#', ns("countFileStatus"), '").text("Drop file here or click to upload")'))
      wgcna_results(NULL)
    })
    
    observeEvent(input$clearCtrlFile, {
      shinyjs::reset("ctrlFile")
      shinyjs::runjs(paste0('$("#', ns("ctrlFileStatus"), '").text("Drop file here or click to upload")'))
      wgcna_results(NULL)
    })
    
    observeEvent(input$clearTreatFile, {
      shinyjs::reset("treatFile")
      shinyjs::runjs(paste0('$("#', ns("treatFileStatus"), '").text("Drop file here or click to upload")'))
      wgcna_results(NULL)
    })

    # ---- 核心分析 ----
    observeEvent(input$runWgcna, {
      
      if (is.null(input$countFile) || is.null(input$ctrlFile) || is.null(input$treatFile)) {
        showNotification("请上传所有必需的文件！", type = "error")
        return()
      }

      count_file <- input$countFile
      ctrl_file <- input$ctrlFile
      treat_file <- input$treatFile
      power_start <- 1L
      power_end <- 20L
      rsquared_cut <- 0.85
      mad_cutoff <- wgcna_clean_number(input$madCutoff, 0.5, 0, 10)
      min_module_size <- as.integer(round(wgcna_clean_number(input$minModuleSize, 50, 20, 200)))
      merge_cut_height <- wgcna_clean_number(input$mergeCutHeight, 0.25, 0.05, 0.5)
      deep_split <- as.integer(round(wgcna_clean_number(input$deepSplit, 2, 0, 4)))

      is_running(TRUE)
      wgcna_results(NULL)
      task_note <- app_start_task_notification("WGCNA 分析正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          expr_matrix <- read_expression_matrix(count_file)

          if (any(is.na(expr_matrix))) {
            stop("表达矩阵中含有无法识别的数值", call. = FALSE)
          }

          gene_mad <- apply(expr_matrix, 1, stats::mad, na.rm = TRUE)
          expr_matrix <- expr_matrix[gene_mad > mad_cutoff, , drop = FALSE]
          if (nrow(expr_matrix) < 2) {
            stop("MAD过滤后剩余基因少于 2 个，请降低 MAD阈值。", call. = FALSE)
          }

          ctrl_samples <- read_sample_list(ctrl_file)
          treat_samples <- read_sample_list(treat_file)
          validate_expression_inputs(expr_matrix, ctrl_samples, treat_samples)

          all_samples <- colnames(expr_matrix)
          missing_ctrl <- ctrl_samples[!ctrl_samples %in% all_samples]
          missing_treat <- treat_samples[!treat_samples %in% all_samples]

          if (length(missing_ctrl) > 0 || length(missing_treat) > 0) {
            msg <- ""
            if (length(missing_ctrl) > 0) {
              msg <- paste0("对照组缺失: ", paste(missing_ctrl, collapse = ", "))
            }
            if (length(missing_treat) > 0) {
              msg <- paste0(msg, "实验组缺失: ", paste(missing_treat, collapse = ", "))
            }
            stop(paste0("样本名不匹配: ", msg), call. = FALSE)
          }

          data_ctrl <- expr_matrix[, ctrl_samples, drop = FALSE]
          data_treat <- expr_matrix[, treat_samples, drop = FALSE]
          combined_expr <- cbind(data_ctrl, data_treat)

          num_ctrl <- ncol(data_ctrl)
          num_treat <- ncol(data_treat)
          datExpr <- t(combined_expr)

          gsg <- WGCNA::goodSamplesGenes(datExpr, verbose = 3)
          if (!gsg$allOK) {
            datExpr <- datExpr[gsg$goodSamples, gsg$goodGenes]
          }

          traitData <- data.frame(
            Normal = c(rep(1, num_ctrl), rep(0, num_treat)),
            Disease = c(rep(0, num_ctrl), rep(1, num_treat))
          )
          rownames(traitData) <- rownames(datExpr)

          powers <- seq(power_start, power_end, by = 1)
          sft <- WGCNA::pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)

          if (is.null(sft$powerEstimate)) {
            best_idx <- which.max(-sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2])
            optimalPower <- sft$fitIndices[best_idx, 1]
          } else {
            optimalPower <- sft$powerEstimate
          }

          adjacency <- WGCNA::adjacency(datExpr, power = optimalPower)
          TOM <- WGCNA::TOMsimilarity(adjacency)
          dissTOM <- 1 - TOM

          geneTree <- stats::hclust(stats::as.dist(dissTOM), method = "average")
          dynamicMods <- dynamicTreeCut::cutreeDynamic(
            dendro = geneTree,
            distM = dissTOM,
            deepSplit = deep_split,
            pamRespectsDendro = FALSE,
            minClusterSize = min_module_size
          )
          moduleColors <- WGCNA::labels2colors(dynamicMods)

          MEList <- WGCNA::moduleEigengenes(datExpr, colors = moduleColors)
          MEs <- MEList$eigengenes
          merge <- WGCNA::mergeCloseModules(datExpr, moduleColors, cutHeight = merge_cut_height, verbose = 3)
          mergedColors <- merge$colors
          mergedMEs <- merge$newMEs

          moduleTraitCor <- stats::cor(mergedMEs, traitData, use = "p")
          moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCor, nrow(datExpr))

          GS <- as.data.frame(stats::cor(datExpr, traitData, use = "p"))
          MM <- as.data.frame(stats::cor(datExpr, mergedMEs, use = "p"))

          list(
            datExpr = datExpr,
            traitData = traitData,
            sft = sft,
            optimalPower = optimalPower,
            geneTree = geneTree,
            dissTOM = dissTOM,
            moduleColors = mergedColors,
            mergedMEs = mergedMEs,
            moduleTraitCor = moduleTraitCor,
            moduleTraitPvalue = moduleTraitPvalue,
            GS = GS,
            MM = MM,
            num_ctrl = num_ctrl,
            num_treat = num_treat,
            rsquared_cut = rsquared_cut,
            mad_cutoff = mad_cutoff,
            deep_split = deep_split
          )
        },
        on_success = function(result) {
          app_clear_task_notification(task_note)
          wgcna_results(result)
          active_wgcna_plot("power")
          showNotification(
            paste0("WGCNA 分析完成！共检测到 ",
                   length(unique(result$moduleColors)), " 个模块"),
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
          "WGCNA_power_plot.png",
          "WGCNA_dendro.png",
          "WGCNA_trait_heatmap.png",
          "WGCNA_MM_vs_GS.png",
          "WGCNA_module_info.csv",
          "WGCNA_gene_info.csv"
        ),
        类型 = c("PNG", "PNG", "PNG", "PNG", "CSV", "CSV"),
        说明 = c(
          "软阈值选择图，点击文件名可在上方显示",
          "基因聚类树和模块颜色图，点击文件名可在上方显示",
          "模块与性状相关性热图，点击文件名可在上方显示",
          "模块成员度与基因显著性散点图，点击文件名可在上方显示",
          "各模块基因数及与性状相关性",
          "每个基因的模块归属、GS 和 MM 指标"
        ),
        plot_key = c("power", "dendro", "trait", "mmgs", "", ""),
        download_id = c("", "", "", "", "downloadModuleTable", "downloadGeneInfo"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (!has_results) {
        return(NULL)
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
            actionButton(ns(paste0("downloadWgcnaPlot_", plot_key)), "下载", class = "btn-primary btn-xs")
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

    lapply(c("power", "dendro", "trait", "mmgs"), function(plot_key) {
      local({
        key <- plot_key
        observeEvent(input[[paste0("showWgcnaPlot_", key)]], {
          active_wgcna_plot(key)
        })
        observeEvent(input[[paste0("downloadWgcnaPlot_", key)]], {
          show_wgcna_download_modal(key)
        })
      })
    })

    output$downloadWgcnaModalPNG <- downloadHandler(
      filename = function() wgcna_plot_filename(download_wgcna_plot()),
      content = function(file) {
        write_wgcna_png(file, download_wgcna_plot())
      }
    )
    
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
        res$moduleColors,
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
      
      cor_df <- melt(res$moduleTraitCor)
      pval_df <- melt(res$moduleTraitPvalue)
      heatmap_df <- merge(cor_df, pval_df, by = c("Var1", "Var2"))
      colnames(heatmap_df) <- c("Module", "Trait", "Correlation", "Pvalue")
      
      heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.05, "*", "")
      heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.01, "**", heatmap_df$Signif)
      heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.001, "***", heatmap_df$Signif)
      
      ggplot(heatmap_df, aes(x = Trait, y = Module, fill = Correlation)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = 0, limit = c(-1, 1),
          name = "Correlation"
        ) +
        geom_text(aes(label = paste0(sprintf("%.2f", Correlation), Signif)), 
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
        GS_Normal = res$GS[, "Normal"]
      )
      
      for (mod in colnames(res$MM)) {
        gene_df[[paste0("MM_", mod)]] <- res$MM[, mod]
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
          res$moduleColors,
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
        
        cor_df <- melt(res$moduleTraitCor)
        pval_df <- melt(res$moduleTraitPvalue)
        heatmap_df <- merge(cor_df, pval_df, by = c("Var1", "Var2"))
        colnames(heatmap_df) <- c("Module", "Trait", "Correlation", "Pvalue")
        
        heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.05, "*", "")
        heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.01, "**", heatmap_df$Signif)
        heatmap_df$Signif <- ifelse(heatmap_df$Pvalue < 0.001, "***", heatmap_df$Signif)
        
        p <- ggplot(heatmap_df, aes(x = Trait, y = Module, fill = Correlation)) +
          geom_tile(color = "white") +
          scale_fill_gradient2(
            low = "blue", high = "red", mid = "white",
            midpoint = 0, limit = c(-1, 1),
            name = "Correlation"
          ) +
          geom_text(aes(label = paste0(sprintf("%.2f", Correlation), Signif)), 
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
          GS_Normal = res$GS[, "Normal"]
        )
        for (mod in colnames(res$MM)) {
          gene_df[[paste0("MM_", mod)]] <- res$MM[, mod]
        }
        write.csv(gene_df, file, row.names = FALSE)
      }
    )
    
  })
}
