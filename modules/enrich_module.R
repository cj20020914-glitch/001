# enrich_module.R - GO/KEGG 富集分析模块（完整版）
# 功能：GO/KEGG富集分析，支持条形图、气泡图、圈图
# 参考差异分析模块的布局和风格

# ============================================================
# UI
# ============================================================
enrich_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
        .enrich-card,
        .enrich-plot-card,
        .enrich-result-card {
            border: 1px solid #b0bec5;
            border-radius: 4px;
            padding: 12px 16px;
            background-color: #ffffff;
        }
        .enrich-card,
        .enrich-plot-card {
            height: 370px;
            overflow-y: auto;
        }
        .enrich-card h4,
        .enrich-plot-card h4,
        .enrich-result-card h4 {
            margin-top: 0;
            margin-bottom: 8px;
            font-size: 14px;
            font-weight: 700;
            color: #263238;
        }
        .enrich-card hr,
        .enrich-plot-card hr {
            margin: 4px 0 8px 0;
        }
        .enrich-upload-row {
            display: grid;
            grid-template-columns: 1fr;
            gap: 6px;
            align-items: center;
            margin-bottom: 6px;
        }
        .enrich-upload-box {
            position: relative;
            border: 1px dashed #b0bec5;
            min-height: 58px;
            padding: 6px 10px;
            background: #ffffff;
            cursor: pointer;
            overflow: hidden;
        }
        .enrich-upload-box:hover {
            background: #f7fafc;
        }
        .enrich-upload-box .shiny-input-container {
            position: absolute;
            inset: 0;
            width: 100% !important;
            height: 100%;
            margin: 0;
            opacity: 0;
            z-index: 2;
            cursor: pointer;
        }
        .enrich-upload-placeholder {
            pointer-events: none;
            display: grid;
            grid-template-columns: 26px minmax(0, 1fr);
            gap: 8px;
            align-items: center;
            min-height: 42px;
        }
        .enrich-upload-title {
            display: block;
            color: #263238;
            font-size: 12px;
            font-weight: 700;
            line-height: 1.2;
        }
        .enrich-upload-status {
            display: block;
            color: #1e88e5;
            font-size: 11px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .enrich-upload-row .btn-xs {
            width: 28px;
            height: 28px;
            padding: 0;
            border-radius: 0;
            font-weight: 700;
        }
        .enrich-compact-section {
            border: 1px solid #d7dee2;
            background: #ffffff;
            padding: 6px 8px;
            margin-bottom: 8px;
        }
        .enrich-compact-title {
            display: block;
            color: #263238;
            font-size: 11px;
            font-weight: 700;
            margin-bottom: 5px;
        }
        .enrich-param-grid {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px 8px;
            align-items: center;
        }
        .enrich-mini-control {
            display: grid;
            grid-template-columns: auto minmax(0, 1fr);
            gap: 5px;
            align-items: center;
            font-size: 10px;
            color: #263238;
        }
        .enrich-mini-control .shiny-input-container {
            margin-bottom: 0;
        }
        .enrich-mini-control .form-control {
            height: 24px;
            padding: 2px 4px;
            font-size: 11px;
            border-radius: 0;
        }
        .enrich-check-row .shiny-options-group {
            display: flex;
            gap: 10px;
            align-items: center;
            flex-wrap: wrap;
        }
        .enrich-check-row label,
        .enrich-check-row .checkbox {
            margin: 0;
            font-size: 11px;
            font-weight: normal;
        }
        .enrich-plot-box {
            min-height: 285px;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .enrich-result-panel {
            max-width: 100%;
            overflow-x: hidden;
        }
        .enrich-result-panel .nav-tabs {
            border-bottom: 1px solid #d7dee2;
            margin-bottom: 8px;
        }
        .enrich-result-panel .nav-tabs > li > a {
            border-radius: 0;
            border: none;
            margin-right: 26px;
            padding: 8px 2px 9px 2px;
            color: #37474f;
            background: transparent;
            font-size: 12px;
        }
        .enrich-result-panel .nav-tabs > li.active > a,
        .enrich-result-panel .nav-tabs > li.active > a:hover,
        .enrich-result-panel .nav-tabs > li.active > a:focus {
            border: none;
            border-bottom: 2px solid #1e88e5;
            color: #1e88e5;
            background: transparent;
            font-weight: 700;
        }
        .enrich-result-file-list {
            border: 1px solid #d7dee2;
            background: #ffffff;
        }
        .enrich-result-file-row {
            display: grid;
            grid-template-columns: 28px minmax(150px, 1fr) 54px minmax(150px, 1.4fr) 70px;
            gap: 8px;
            align-items: center;
            padding: 6px 8px;
            border-bottom: 1px solid #eef2f4;
            font-size: 11px;
        }
        .enrich-result-file-row:last-child {
            border-bottom: none;
        }
        .enrich-file-index {
            color: #1e88e5;
            font-weight: 700;
        }
        .enrich-result-file-action {
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
        .enrich-result-file-name,
        .enrich-result-file-desc {
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .enrich-result-file-name {
            font-weight: 700;
            color: #263238;
        }
        .enrich-result-file-type {
            color: #455a64;
            font-weight: 700;
        }
        .enrich-result-file-desc {
            color: #607d8b;
        }
        .enrich-result-file-download .btn {
            font-size: 10px;
            padding: 1px 8px;
            line-height: 1.4;
        }
        .enrich-result-data-preview {
            max-height: 190px;
            overflow-y: auto;
        }
        .enrich-result-data-preview h5 {
            margin: 4px 0 6px 0;
            font-size: 12px;
            font-weight: 700;
            color: #263238;
        }
        .enrich-download-size-controls {
            display: grid;
            grid-template-columns: repeat(3, minmax(0, 1fr));
            gap: 6px;
            margin-top: 8px;
        }
        .enrich-download-size-controls .shiny-input-container {
            width: 100%;
            margin-bottom: 0;
        }
        .enrich-qa {
            font-size: 12px;
            line-height: 1.7;
            color: #455a64;
            max-height: 190px;
            overflow-y: auto;
        }
        .enrich-qa dl {
            margin: 0;
        }
        .enrich-qa dt {
            margin-top: 8px;
            color: #263238;
        }
        .enrich-qa dt:first-child {
            margin-top: 0;
        }
        .enrich-qa dd {
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
          class = "enrich-card",
          h4("参数设置"),
          div(
            class = "enrich-upload-row",
            div(
              class = "enrich-upload-box",
              id = ns("geneFileBox"),
              tags$div(
                class = "enrich-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(
                  tags$span("基因列表", class = "enrich-upload-title"),
                  tags$span(id = ns("geneFileStatus"), "Drop file here or click to upload", class = "enrich-upload-status")
                )
              ),
              fileInput(ns("geneFile"), NULL,
                        accept = c(".txt", ".csv", ".tsv"),
                        buttonLabel = "浏览",
                        placeholder = "选择基因列表文件")
            )
          ),
          div(
            class = "enrich-upload-row",
            div(
              class = "enrich-upload-box",
              id = ns("exprFileBox"),
              tags$div(
                class = "enrich-upload-placeholder",
                span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 22px; line-height: 1;"),
                tags$span(
                  tags$span("表达矩阵（可选）", class = "enrich-upload-title"),
                  tags$span(id = ns("exprFileStatus"), "Drop file here or click to upload", class = "enrich-upload-status")
                )
              ),
              fileInput(ns("exprFile"), NULL,
                        accept = c(".csv", ".tsv", ".txt"),
                        buttonLabel = "浏览",
                        placeholder = "选择表达矩阵文件")
            )
          ),
          div(
            class = "enrich-compact-section",
            span("富集类型", class = "enrich-compact-title"),
            div(
              class = "enrich-check-row",
              checkboxGroupInput(ns("goOntologies"), NULL,
                                 choices = c("BP" = "BP", "CC" = "CC", "MF" = "MF"),
                                 selected = c("BP", "CC", "MF"),
                                 inline = TRUE),
              checkboxInput(ns("doKEGG"), "KEGG", value = TRUE)
            )
          ),
          div(
            class = "enrich-compact-section",
            span("分析参数", class = "enrich-compact-title"),
            div(
              class = "enrich-param-grid",
              div(class = "enrich-mini-control", span("P值"), numericInput(ns("pCut"), NULL, value = 0.05, min = 0.001, max = 0.2, step = 0.01)),
              div(class = "enrich-mini-control", span("Q值"), numericInput(ns("qCut"), NULL, value = 0.05, min = 0.001, max = 0.2, step = 0.01)),
              div(class = "enrich-mini-control", span("Top N"), numericInput(ns("showNum"), NULL, value = 10, min = 5, max = 50, step = 5))
            )
          ),
          div(
            class = "enrich-compact-section",
            span("图片参数", class = "enrich-compact-title"),
            div(
              class = "enrich-param-grid",
              div(class = "enrich-mini-control", span("宽度"), numericInput(ns("enrichWidth"), NULL, value = 8, min = 3, max = 20, step = 0.5)),
              div(class = "enrich-mini-control", span("高度"), numericInput(ns("enrichHeight"), NULL, value = 6, min = 3, max = 20, step = 0.5)),
              div(class = "enrich-mini-control", span("字号"), numericInput(ns("enrichBaseSize"), NULL, value = 11, min = 6, max = 24, step = 1))
            )
          ),
          actionButton(ns("runEnrich"), "运行富集分析", 
                        class = "btn-success btn-sm",
                       style = "width: 100%; font-size: 12px; font-weight: bold; padding: 5px 0; border-radius: 0;")
        )
      ),
      column(
        width = 6,
        style = "padding: 4px;",
        tags$div(
          class = "enrich-plot-card",
          h4("图片显示"),
          hr(),
          div(
            class = "enrich-plot-box",
            plotOutput(
              ns("activeEnrichPlot"),
              height = "285px",
              width = "100%",
              click = ns("activeEnrichPlot_click")
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
          class = "enrich-result-card",
          h4("结果预览"),
          div(
            class = "enrich-result-panel",
            tabsetPanel(
              id = ns("resultTabs"),
              type = "tabs",
              tabPanel("结果表", uiOutput(ns("resultFileList"))),
              tabPanel(
                "数据预览",
                div(
                  class = "enrich-result-data-preview",
                  h5("GO结果"),
                  DTOutput(ns("goTable")),
                  h5("KEGG结果"),
                  DTOutput(ns("keggTable"))
                )
              ),
              tabPanel(
                "Q&A",
                div(
                  class = "enrich-qa",
                  tags$dl(
                    tags$dt("Q1：富集分析用于什么？"),
                    tags$dd("富集分析用于把基因列表解释为生物学过程、细胞组分、分子功能或 KEGG 通路，常接在差异分析、Venn 交集、WGCNA 或机器学习筛选之后。"),
                    tags$dt("Q2：基因列表格式有什么要求？"),
                    tags$dd("支持 TXT、CSV、TSV，默认读取第一列基因名。建议使用人类 Gene Symbol，例如 TP53、EGFR、MYC。空行和重复基因会自动过滤。"),
                    tags$dt("Q3：表达矩阵为什么是可选？"),
                    tags$dd("表达矩阵只用于导出输入基因对应的表达数据，不影响 GO/KEGG 富集计算。没有表达矩阵时仍然可以完成富集分析。"),
                    tags$dt("Q4：BP、CC、MF 分别是什么？"),
                    tags$dd("BP 是 Biological Process，表示生物学过程；CC 是 Cellular Component，表示细胞组分；MF 是 Molecular Function，表示分子功能。"),
                    tags$dt("Q5：P值、Q值和 Top N 如何设置？"),
                    tags$dd("P值和Q值越小筛选越严格。默认 0.05 适合常规分析；Top N 控制每类图中展示的条目数量，结果很多时可适当增大。"),
                    tags$dt("Q6：结果文件如何使用？"),
                    tags$dd("GO/KEGG CSV 可用于论文表格和后续筛选；PNG 图用于汇报展示；基因表达 CSV 可进入热图、机器学习或其它下游分析。")
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
enrich_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- 存储结果 ----
    enrich_results <- reactiveVal(NULL)
    go_results_data <- reactiveVal(NULL)
    kegg_results_data <- reactiveVal(NULL)
    expr_data_results <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    active_enrich_plot <- reactiveVal("go_bar")
    download_enrich_plot <- reactiveVal("go_bar")

    enrich_clean_number <- function(value, default, min_value, max_value) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) != 1 || is.na(value)) {
        value <- default
      }
      max(min_value, min(max_value, value))
    }

    enrich_plot_label <- function(plot_key) {
      labels <- c(
        go_bar = "GO条形图",
        go_bubble = "GO气泡图",
        go_chord = "GO圈图",
        kegg_bar = "KEGG条形图",
        kegg_bubble = "KEGG气泡图",
        kegg_chord = "KEGG圈图"
      )
      labels[[plot_key]] %||% "富集图"
    }

    enrich_plot_filename <- function(plot_key) {
      switch(
        plot_key,
        go_bar = "GO_barplot.png",
        go_bubble = "GO_bubble.png",
        go_chord = "GO_chord.png",
        kegg_bar = "KEGG_barplot.png",
        kegg_bubble = "KEGG_bubble.png",
        kegg_chord = "KEGG_chord.png",
        "enrichment_plot.png"
      )
    }

    enrich_blank_plot <- function(message = "") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    }

    enrich_go_top <- function(res) {
      if (is.null(res) || nrow(res) == 0) {
        return(NULL)
      }
      res %>%
        group_by(ONTOLOGY) %>%
        slice_head(n = input$showNum) %>%
        ungroup()
    }

    enrich_kegg_top <- function(kegg_df) {
      if (is.null(kegg_df) || nrow(kegg_df) == 0) {
        return(NULL)
      }
      top_n <- min(input$showNum, nrow(kegg_df))
      kegg_df[order(kegg_df$p.adjust), ][seq_len(top_n), , drop = FALSE]
    }

    make_go_barplot <- function(res, base_size = NULL) {
      top_go <- enrich_go_top(res)
      if (is.null(top_go) || nrow(top_go) == 0) {
        return(NULL)
      }
      base_size <- enrich_clean_number(base_size %||% input$enrichBaseSize, 11, 6, 24)
      color_param <- if (input$qCut > 0.05) "pvalue" else "p.adjust"

      ggplot(top_go, aes(x = reorder(Description, -log10(p.adjust)),
                         y = -log10(p.adjust),
                         fill = !!sym(color_param))) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
        scale_fill_gradientn(colors = c("#FF6666", "#FFB266", "#FFFF99",
                                        "#99FF99", "#6666FF", "#B266FF")) +
        labs(title = "GO Enrichment", x = "", y = "-log10(Adjusted P)") +
        theme_minimal(base_size = base_size) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              strip.text = element_text(face = "bold"),
              axis.text.y = element_text(size = max(6, base_size - 3)))
    }

    make_go_bubble <- function(res, base_size = NULL) {
      top_go <- enrich_go_top(res)
      if (is.null(top_go) || nrow(top_go) == 0) {
        return(NULL)
      }
      base_size <- enrich_clean_number(base_size %||% input$enrichBaseSize, 11, 6, 24)
      color_param <- if (input$qCut > 0.05) "pvalue" else "p.adjust"
      top_go$GeneRatio_num <- sapply(top_go$GeneRatio, function(x) {
        parts <- strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      })

      ggplot(top_go, aes(x = GeneRatio_num,
                         y = reorder(Description, GeneRatio_num),
                         size = Count,
                         color = !!sym(color_param))) +
        geom_point(alpha = 0.8) +
        facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
        scale_color_gradientn(colors = c("#FFB266", "#FFFF99", "#99FF99",
                                         "#6666FF", "#B266FF"),
                              name = "Adjusted P") +
        scale_size_continuous(range = c(3, 8), name = "Count") +
        labs(title = "GO Enrichment Dotplot", x = "Gene Ratio", y = "") +
        theme_minimal(base_size = base_size) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              strip.text = element_text(face = "bold"),
              axis.text.y = element_text(size = max(6, base_size - 3)))
    }

    draw_go_chord <- function(res) {
      if (is.null(res) || nrow(res) == 0) {
        enrich_blank_plot("请先运行富集分析")
        return(invisible(NULL))
      }

      top_go <- res %>%
        group_by(ONTOLOGY) %>%
        slice_head(n = 10) %>%
        ungroup()

      top_go$Description_wrapped <- sapply(top_go$Description, function(x) {
        if (nchar(x) > 30) paste(strwrap(x, width = 30), collapse = "\n") else x
      })

      ontologies <- c("BP", "CC", "MF")
      mat <- matrix(0, nrow = length(ontologies), ncol = nrow(top_go))
      rownames(mat) <- ontologies
      colnames(mat) <- top_go$Description_wrapped

      for (i in seq_len(nrow(top_go))) {
        ont <- top_go$ONTOLOGY[i]
        mat[ont, i] <- -log10(top_go$p.adjust[i])
      }

      grid_col <- c(BP = "#E69F00", CC = "#56B4E9", MF = "#009E73")
      term_colors <- rep("#BBBBBB", ncol(mat))
      names(term_colors) <- colnames(mat)
      grid_col <- c(grid_col, term_colors)

      circos.clear()
      on.exit(circos.clear(), add = TRUE)
      circos.par(start.degree = 90, gap.degree = c(rep(2, 2), 10, rep(1, ncol(mat) - 1), 10))
      chordDiagram(mat, grid.col = grid_col, transparency = 0.4,
                   annotationTrack = "grid",
                   preAllocateTracks = list(track.height = 0.15))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        sector.name <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(mean(xlim), ylim[1] + 0.1, sector.name,
                    facing = "clockwise", niceFacing = TRUE,
                    adj = c(0, 0.5), cex = 0.5)
      }, bg.border = NA)
      title("GO Enrichment Chord")
    }

    make_kegg_barplot <- function(kegg_df, base_size = NULL) {
      top_kegg <- enrich_kegg_top(kegg_df)
      if (is.null(top_kegg) || nrow(top_kegg) == 0) {
        return(NULL)
      }
      base_size <- enrich_clean_number(base_size %||% input$enrichBaseSize, 11, 6, 24)
      top_kegg <- top_kegg[order(top_kegg$Count), , drop = FALSE]

      ggplot(top_kegg, aes(x = reorder(Description, Count),
                           y = Count,
                           fill = p.adjust)) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        geom_text(aes(label = Count), hjust = -0.2, size = max(2.5, base_size / 3)) +
        scale_fill_gradient(low = "#2c7bb6", high = "#d7191c",
                            name = "Adjusted P") +
        labs(title = "KEGG Enrichment", x = "", y = "Gene Count") +
        theme_minimal(base_size = base_size) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              axis.text.y = element_text(size = max(6, base_size - 3)))
    }

    make_kegg_bubble <- function(kegg_df, base_size = NULL) {
      top_kegg <- enrich_kegg_top(kegg_df)
      if (is.null(top_kegg) || nrow(top_kegg) == 0) {
        return(NULL)
      }
      base_size <- enrich_clean_number(base_size %||% input$enrichBaseSize, 11, 6, 24)
      top_kegg$GeneRatio_num <- sapply(top_kegg$GeneRatio, function(x) {
        parts <- strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      })

      ggplot(top_kegg, aes(x = GeneRatio_num,
                           y = reorder(Description, GeneRatio_num),
                           size = Count,
                           color = p.adjust)) +
        geom_point(alpha = 0.8) +
        scale_color_gradient(low = "#2c7bb6", high = "#d7191c",
                             name = "Adjusted P") +
        scale_size_continuous(range = c(3, 8), name = "Count") +
        labs(title = "KEGG Dotplot", x = "Gene Ratio", y = "") +
        theme_minimal(base_size = base_size) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              axis.text.y = element_text(size = max(6, base_size - 3)))
    }

    draw_kegg_chord <- function(kegg_df) {
      if (is.null(kegg_df) || nrow(kegg_df) == 0) {
        enrich_blank_plot("请先运行富集分析（或未启用 KEGG）")
        return(invisible(NULL))
      }

      top_n <- min(20, nrow(kegg_df))
      top_kegg <- kegg_df[order(kegg_df$p.adjust), ][seq_len(top_n), , drop = FALSE]
      all_genes <- unlist(strsplit(top_kegg$geneID, "/"))
      gene_freq <- sort(table(all_genes), decreasing = TRUE)
      top_genes <- names(head(gene_freq, n = min(30, length(gene_freq))))

      top_kegg$Description_wrapped <- sapply(top_kegg$Description, function(x) {
        if (nchar(x) > 25) paste(strwrap(x, width = 25), collapse = "\n") else x
      })

      edge_list <- data.frame()
      for (i in seq_len(nrow(top_kegg))) {
        genes <- strsplit(top_kegg$geneID[i], "/")[[1]]
        genes <- intersect(genes, top_genes)
        if (length(genes) > 0) {
          edge_list <- rbind(edge_list, data.frame(
            pathway = rep(top_kegg$Description_wrapped[i], length(genes)),
            gene = genes,
            stringsAsFactors = FALSE
          ))
        }
      }

      if (nrow(edge_list) == 0) {
        enrich_blank_plot("无足够数据绘制圈图")
        return(invisible(NULL))
      }

      circos.clear()
      on.exit(circos.clear(), add = TRUE)
      circos.par(start.degree = 90)
      chordDiagram(edge_list[, c("pathway", "gene")],
                   annotationTrack = "grid",
                   annotationTrackHeight = c(0.05),
                   preAllocateTracks = list(track.height = 0.1))
      circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
        sector.name <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(mean(xlim), ylim[1] + mm_y(2), sector.name,
                    facing = "clockwise", niceFacing = TRUE,
                    adj = c(0, 0.5), cex = 0.4)
      }, bg.border = NA)
      title("KEGG Pathway-Gene Association")
    }

    draw_enrich_plot <- function(plot_key, large = FALSE) {
      base_size <- enrich_clean_number(input$enrichBaseSize, 11, 6, 24)
      if (large) {
        base_size <- base_size + 3
      }

      if (identical(plot_key, "go_bar")) {
        plot_obj <- make_go_barplot(go_results_data(), base_size)
        if (is.null(plot_obj)) enrich_blank_plot("请先运行富集分析") else print(plot_obj)
        return(invisible(NULL))
      }
      if (identical(plot_key, "go_bubble")) {
        plot_obj <- make_go_bubble(go_results_data(), base_size)
        if (is.null(plot_obj)) enrich_blank_plot("请先运行富集分析") else print(plot_obj)
        return(invisible(NULL))
      }
      if (identical(plot_key, "go_chord")) {
        draw_go_chord(go_results_data())
        return(invisible(NULL))
      }
      if (identical(plot_key, "kegg_bar")) {
        plot_obj <- make_kegg_barplot(kegg_results_data(), base_size)
        if (is.null(plot_obj)) enrich_blank_plot("请先运行富集分析（或未启用 KEGG）") else print(plot_obj)
        return(invisible(NULL))
      }
      if (identical(plot_key, "kegg_bubble")) {
        plot_obj <- make_kegg_bubble(kegg_results_data(), base_size)
        if (is.null(plot_obj)) enrich_blank_plot("请先运行富集分析（或未启用 KEGG）") else print(plot_obj)
        return(invisible(NULL))
      }
      if (identical(plot_key, "kegg_chord")) {
        draw_kegg_chord(kegg_results_data())
        return(invisible(NULL))
      }

      enrich_blank_plot("")
    }

    get_enrich_download_size <- function() {
      list(
        width = enrich_clean_number(input$downloadEnrichWidth, enrich_clean_number(input$enrichWidth, 8, 3, 20), 3, 20),
        height = enrich_clean_number(input$downloadEnrichHeight, enrich_clean_number(input$enrichHeight, 6, 3, 20), 3, 20),
        dpi = as.integer(round(enrich_clean_number(input$downloadEnrichDpi, 300, 72, 600)))
      )
    }

    write_enrich_png <- function(file, plot_key) {
      size <- get_enrich_download_size()
      png(file, width = size$width * size$dpi, height = size$height * size$dpi, res = size$dpi)
      on.exit(dev.off(), add = TRUE)
      draw_enrich_plot(plot_key, large = TRUE)
    }

    show_enrich_download_modal <- function(plot_key) {
      download_enrich_plot(plot_key)
      showModal(
        modalDialog(
          title = paste0("下载", enrich_plot_label(plot_key)),
          p("设置导出图片尺寸后点击下载。单位为英寸，DPI 用于控制分辨率。",
            style = "color: #607d8b; font-size: 12px; margin: 0 0 8px 0;"),
          div(
            class = "enrich-download-size-controls",
            numericInput(ns("downloadEnrichWidth"), "宽(in)", value = enrich_clean_number(input$enrichWidth, 8, 3, 20), min = 3, max = 20, step = 0.5),
            numericInput(ns("downloadEnrichHeight"), "高(in)", value = enrich_clean_number(input$enrichHeight, 6, 3, 20), min = 3, max = 20, step = 0.5),
            numericInput(ns("downloadEnrichDpi"), "DPI", value = 300, min = 72, max = 600, step = 50)
          ),
          footer = tagList(
            modalButton("取消"),
            downloadButton(ns("downloadEnrichModalPNG"), "下载PNG", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    }
    
    # ---- 文件选择状态更新 ----
    observeEvent(input$geneFile, {
      if (!is.null(input$geneFile)) {
        shinyjs::runjs(paste0('$("#', ns("geneFileStatus"), '").text("', input$geneFile$name, '")'))
      }
    })
    
    observeEvent(input$exprFile, {
      if (!is.null(input$exprFile)) {
        shinyjs::runjs(paste0('$("#', ns("exprFileStatus"), '").text("', input$exprFile$name, '")'))
      }
    })
    
    # ---- 清除文件 ----
    observeEvent(input$clearGeneFile, {
      shinyjs::reset("geneFile")
      shinyjs::runjs(paste0('$("#', ns("geneFileStatus"), '").text("Drop file here or click to upload")'))
      enrich_results(NULL)
      go_results_data(NULL)
      kegg_results_data(NULL)
      expr_data_results(NULL)
    })
    
    observeEvent(input$clearExprFile, {
      shinyjs::reset("exprFile")
      shinyjs::runjs(paste0('$("#', ns("exprFileStatus"), '").text("Drop file here or click to upload")'))
    })
    
    # ---- 核心分析 ----
    observeEvent(input$runEnrich, {
      
      if (is.null(input$geneFile)) {
        showNotification("请上传基因列表文件！", type = "error")
        return()
      }

      gene_file <- input$geneFile
      expr_file <- input$exprFile
      go_ontologies <- input$goOntologies
      do_kegg <- isTRUE(input$doKEGG)
      p_cut <- input$pCut
      q_cut <- input$qCut

      is_running(TRUE)
      enrich_results(NULL)
      go_results_data(NULL)
      kegg_results_data(NULL)
      expr_data_results(NULL)
      task_note <- app_start_task_notification("富集分析正在后台运行，可以切换到其它模块继续操作。")

      run_async_task(
        task = function() {
          gene_symbols <- read_gene_list_file(gene_file)

          if (length(gene_symbols) == 0) {
            stop("基因列表为空！", call. = FALSE)
          }

          expr_df <- NULL
          if (!is.null(expr_file)) {
            expr_df <- tryCatch({
              expr_mat_num <- read_expression_matrix(expr_file)
              matched_genes <- intersect(rownames(expr_mat_num), gene_symbols)
              if (length(matched_genes) > 0) {
                expr_subset <- expr_mat_num[matched_genes, , drop = FALSE]
                data.frame(
                  Gene = rownames(expr_subset),
                  expr_subset,
                  stringsAsFactors = FALSE,
                  check.names = FALSE
                )
              } else {
                NULL
              }
            }, error = function(error) {
              NULL
            })
          }

          entrez_ids <- suppressWarnings(
            clusterProfiler::bitr(
              gene_symbols,
              fromType = "SYMBOL",
              toType = "ENTREZID",
              OrgDb = org.Hs.eg.db::org.Hs.eg.db
            )
          )

          if (nrow(entrez_ids) == 0) {
            stop("无法转换基因名为 Entrez ID", call. = FALSE)
          }

          gene_ids <- entrez_ids$ENTREZID
          gene_mapping <- data.frame(
            GeneSymbol = entrez_ids$SYMBOL,
            EntrezID = entrez_ids$ENTREZID,
            stringsAsFactors = FALSE
          )

          results <- list()
          results$gene_count <- length(gene_ids)
          results$gene_mapping <- gene_mapping

          go_results <- list()
          go_data <- NULL
          if (length(go_ontologies) > 0) {
            go_res <- clusterProfiler::enrichGO(
              gene = gene_ids,
              OrgDb = org.Hs.eg.db::org.Hs.eg.db,
              pvalueCutoff = 1,
              qvalueCutoff = 1,
              ont = "all",
              readable = TRUE
            )

            go_df <- as.data.frame(go_res)
            go_df <- go_df[go_df$pvalue < p_cut & go_df$p.adjust < q_cut, ]

            for (ont in go_ontologies) {
              ont_df <- go_df[go_df$ONTOLOGY == ont, ]
              if (nrow(ont_df) > 0) {
                go_results[[ont]] <- ont_df
              }
            }

            if (length(go_results) > 0) {
              go_data <- data.frame()
              for (ont in names(go_results)) {
                df <- go_results[[ont]]
                df$ONTOLOGY <- ont
                go_data <- rbind(go_data, df)
              }
            }
          }
          results$go <- go_results

          kegg_data <- NULL
          if (isTRUE(do_kegg)) {
            kegg_res <- clusterProfiler::enrichKEGG(
              gene = gene_ids,
              organism = "hsa",
              pvalueCutoff = 1,
              qvalueCutoff = 1
            )

            kegg_df <- as.data.frame(kegg_res)
            kegg_df <- kegg_df[kegg_df$pvalue < p_cut & kegg_df$p.adjust < q_cut, ]

            if (nrow(kegg_df) > 0) {
              kegg_df$geneID <- vapply(kegg_df$geneID, function(x) {
                ids <- strsplit(x, "/")[[1]]
                paste(gene_mapping$GeneSymbol[match(ids, gene_mapping$EntrezID)],
                      collapse = "/")
              }, character(1))
              kegg_data <- kegg_df
            }
          }

          list(
            results = results,
            go_data = go_data,
            kegg_data = kegg_data,
            expr_data = expr_df,
            gene_count = length(gene_symbols),
            mapped_count = nrow(entrez_ids)
          )
        },
        on_success = function(result) {
          app_clear_task_notification(task_note)
          enrich_results(result$results)
          go_results_data(result$go_data)
          kegg_results_data(result$kegg_data)
          expr_data_results(result$expr_data)

          if (!is.null(result$go_data) && nrow(result$go_data) > 0) {
            active_enrich_plot("go_bar")
          } else if (!is.null(result$kegg_data) && nrow(result$kegg_data) > 0) {
            active_enrich_plot("kegg_bar")
          }

          showNotification(
            paste0("富集分析完成！基因映射率 ",
                   round(result$mapped_count / result$gene_count * 100, 1), "%"),
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
      return()
      
      is_running(TRUE)
      enrich_results(NULL)
      go_results_data(NULL)
      kegg_results_data(NULL)
      expr_data_results(NULL)
      
      withProgress(message = "富集分析运行中...", value = 0, {
        
        incProgress(0.1, detail = "读取基因列表...")
        
        tryCatch({
          # ---- 1. 读取基因列表 ----
          gene_symbols <- read_gene_list_file(input$geneFile)
          
          if (length(gene_symbols) == 0) {
            showNotification("基因列表为空！", type = "error")
            is_running(FALSE)
            return()
          }
          
          showNotification(paste0("检测到 ", length(gene_symbols), " 个基因"), 
                           type = "message", duration = 3)
          
          # ---- 2. 提取表达数据 ----
          if (!is.null(input$exprFile)) {
            incProgress(0.2, detail = "提取基因表达数据...")
            
            tryCatch({
              expr_mat_num <- read_expression_matrix(input$exprFile)
              expr_genes <- rownames(expr_mat_num)
              
              matched_genes <- intersect(expr_genes, gene_symbols)
              if (length(matched_genes) > 0) {
                expr_subset <- expr_mat_num[matched_genes, , drop = FALSE]
                expr_df <- data.frame(
                  Gene = rownames(expr_subset),
                  expr_subset,
                  stringsAsFactors = FALSE,
                  check.names = FALSE
                )
                expr_data_results(expr_df)
              }
            }, error = function(e) {
              showNotification(paste0("提取表达数据失败: ", e$message), type = "warning")
            })
          }
          
          incProgress(0.3, detail = "转换 Entrez ID...")
          
          # ---- 3. 转换 Entrez ID ----
          entrez_ids <- suppressWarnings(
            bitr(gene_symbols, fromType = "SYMBOL", 
                 toType = "ENTREZID", OrgDb = org.Hs.eg.db)
          )
          
          if (nrow(entrez_ids) == 0) {
            showNotification("无法转换基因名为 Entrez ID", type = "error")
            is_running(FALSE)
            return()
          }
          
          map_rate <- round(nrow(entrez_ids) / length(gene_symbols) * 100, 1)
          showNotification(paste0("基因映射率: ", map_rate, "% (", 
                                  nrow(entrez_ids), "/", length(gene_symbols), ")"), 
                           type = "message", duration = 3)
          
          gene_ids <- entrez_ids$ENTREZID
          gene_mapping <- data.frame(
            GeneSymbol = entrez_ids$SYMBOL,
            EntrezID = entrez_ids$ENTREZID,
            stringsAsFactors = FALSE
          )
          
          results <- list()
          results$gene_count <- length(gene_ids)
          results$gene_mapping <- gene_mapping
          
          # ---- 4. GO 富集分析 ----
          go_ontologies <- input$goOntologies
          go_results <- list()
          
          if (length(go_ontologies) > 0) {
            incProgress(0.5, detail = "运行 GO 富集分析...")
            
            go_res <- enrichGO(gene = gene_ids,
                               OrgDb = org.Hs.eg.db,
                               pvalueCutoff = 1,
                               qvalueCutoff = 1,
                               ont = "all",
                               readable = TRUE)
            
            go_df <- as.data.frame(go_res)
            go_df <- go_df[go_df$pvalue < input$pCut & 
                             go_df$p.adjust < input$qCut, ]
            
            for (ont in go_ontologies) {
              ont_df <- go_df[go_df$ONTOLOGY == ont, ]
              if (nrow(ont_df) > 0) {
                go_results[[ont]] <- ont_df
              }
            }
            
            if (length(go_results) > 0) {
              all_go <- data.frame()
              for (ont in names(go_results)) {
                df <- go_results[[ont]]
                df$ONTOLOGY <- ont
                all_go <- rbind(all_go, df)
              }
              go_results_data(all_go)
            }
          }
          results$go <- go_results
          
          # ---- 5. KEGG 富集分析 ----
          if (input$doKEGG) {
            incProgress(0.7, detail = "运行 KEGG 富集分析...")
            
            kegg_res <- enrichKEGG(gene = gene_ids,
                                   organism = "hsa",
                                   pvalueCutoff = 1,
                                   qvalueCutoff = 1)
            
            kegg_df <- as.data.frame(kegg_res)
            kegg_df <- kegg_df[kegg_df$pvalue < input$pCut & 
                                 kegg_df$p.adjust < input$qCut, ]
            
            if (nrow(kegg_df) > 0) {
              kegg_df$geneID <- sapply(kegg_df$geneID, function(x) {
                ids <- strsplit(x, "/")[[1]]
                paste(gene_mapping$GeneSymbol[match(ids, gene_mapping$EntrezID)], 
                      collapse = "/")
              })
              kegg_results_data(kegg_df)
            }
          }
          
          enrich_results(results)
          if (!is.null(go_results_data()) && nrow(go_results_data()) > 0) {
            active_enrich_plot("go_bar")
          } else if (!is.null(kegg_results_data()) && nrow(kegg_results_data()) > 0) {
            active_enrich_plot("kegg_bar")
          }
          
          is_running(FALSE)
          incProgress(1.0, detail = "完成！")
          
          showNotification("富集分析完成！", type = "message", duration = 5)
          
        }, error = function(e) {
          showNotification(paste0("错误: ", e$message), type = "error", duration = 10)
          is_running(FALSE)
        })
      })
    })

    output$activeEnrichPlot <- renderPlot({
      draw_enrich_plot(active_enrich_plot())
    })

    observeEvent(input$activeEnrichPlot_click, {
      if (is.null(enrich_results())) {
        return()
      }
      showModal(
        modalDialog(
          title = enrich_plot_label(active_enrich_plot()),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(
            style = "text-align: center;",
            plotOutput(ns("activeEnrichPlotLarge"), height = "620px")
          )
        )
      )
    })

    output$activeEnrichPlotLarge <- renderPlot({
      draw_enrich_plot(active_enrich_plot(), large = TRUE)
    })

    output$resultFileList <- renderUI({
      has_results <- !is.null(enrich_results())
      files <- data.frame(
        文件名 = c(
          "GO_barplot.png",
          "GO_bubble.png",
          "GO_chord.png",
          "KEGG_barplot.png",
          "KEGG_bubble.png",
          "KEGG_chord.png",
          "GO_results.csv",
          "KEGG_results.csv",
          "gene_expression.csv"
        ),
        类型 = c("PNG", "PNG", "PNG", "PNG", "PNG", "PNG", "CSV", "CSV", "CSV"),
        说明 = c(
          "GO 富集条形图，点击文件名可在上方显示",
          "GO 富集气泡图，点击文件名可在上方显示",
          "GO 富集圈图，点击文件名可在上方显示",
          "KEGG 富集条形图，点击文件名可在上方显示",
          "KEGG 富集气泡图，点击文件名可在上方显示",
          "KEGG 通路-基因圈图，点击文件名可在上方显示",
          "GO 富集结果表",
          "KEGG 富集结果表",
          "输入基因对应表达矩阵子集"
        ),
        plot_key = c("go_bar", "go_bubble", "go_chord", "kegg_bar", "kegg_bubble", "kegg_chord", "", "", ""),
        download_id = c("", "", "", "", "", "", "downloadGoTable", "downloadKeggTable", "downloadExprData"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (!has_results) {
        return(NULL)
      }

      tags$div(
        class = "enrich-result-file-list",
        lapply(seq_len(nrow(files)), function(i) {
          plot_key <- files$plot_key[i]
          is_png <- identical(files$类型[i], "PNG")
          name_control <- if (is_png) {
            actionButton(
              ns(paste0("showEnrichPlot_", plot_key)),
              files$文件名[i],
              class = "enrich-result-file-action",
              title = "点击后在上方图片区显示"
            )
          } else {
            span(files$文件名[i], class = "enrich-result-file-name", title = files$文件名[i])
          }

          download_control <- if (is_png) {
            actionButton(ns(paste0("downloadEnrichPlot_", plot_key)), "下载", class = "btn-primary btn-xs")
          } else {
            downloadButton(ns(files$download_id[i]), "下载", class = "btn-primary btn-xs")
          }

          tags$div(
            class = "enrich-result-file-row",
            span(sprintf("%02d", i), class = "enrich-file-index"),
            name_control,
            span(files$类型[i], class = "enrich-result-file-type"),
            span(files$说明[i], class = "enrich-result-file-desc", title = files$说明[i]),
            span(class = "enrich-result-file-download", download_control)
          )
        })
      )
    })

    lapply(c("go_bar", "go_bubble", "go_chord", "kegg_bar", "kegg_bubble", "kegg_chord"), function(plot_key) {
      local({
        key <- plot_key
        observeEvent(input[[paste0("showEnrichPlot_", key)]], {
          active_enrich_plot(key)
        })
        observeEvent(input[[paste0("downloadEnrichPlot_", key)]], {
          show_enrich_download_modal(key)
        })
      })
    })

    output$downloadEnrichModalPNG <- downloadHandler(
      filename = function() enrich_plot_filename(download_enrich_plot()),
      content = function(file) {
        write_enrich_png(file, download_enrich_plot())
      }
    )
    
    # ============================================================
    # GO 可视化
    # ============================================================
    
    output$goBarplot <- renderPlot({
      res <- go_results_data()
      if (is.null(res) || nrow(res) == 0) {
        enrich_blank_plot()
        return()
      }
      
      top_go <- res %>%
        group_by(ONTOLOGY) %>%
        slice_head(n = input$showNum) %>%
        ungroup()
      
      color_param <- if (input$qCut > 0.05) "pvalue" else "p.adjust"
      
      p <- ggplot(top_go, aes(x = reorder(Description, -log10(p.adjust)), 
                              y = -log10(p.adjust), 
                              fill = !!sym(color_param))) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
        scale_fill_gradientn(colors = c("#FF6666", "#FFB266", "#FFFF99", 
                                        "#99FF99", "#6666FF", "#B266FF")) +
        labs(title = "GO Enrichment", x = "", y = "-log10(Adjusted P)") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
              strip.text = element_text(face = "bold", size = 10),
              axis.text.y = element_text(size = 8))
      print(p)
    })
    
    output$goBubble <- renderPlot({
      res <- go_results_data()
      if (is.null(res) || nrow(res) == 0) {
        enrich_blank_plot()
        return()
      }
      
      top_go <- res %>%
        group_by(ONTOLOGY) %>%
        slice_head(n = input$showNum) %>%
        ungroup()
      
      color_param <- if (input$qCut > 0.05) "pvalue" else "p.adjust"
      
      top_go$GeneRatio_num <- sapply(top_go$GeneRatio, function(x) {
        parts <- strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      })
      
      p <- ggplot(top_go, aes(x = GeneRatio_num, 
                              y = reorder(Description, GeneRatio_num),
                              size = Count, 
                              color = !!sym(color_param))) +
        geom_point(alpha = 0.8) +
        facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
        scale_color_gradientn(colors = c("#FFB266", "#FFFF99", "#99FF99", 
                                         "#6666FF", "#B266FF"),
                              name = "Adjusted P") +
        scale_size_continuous(range = c(3, 8), name = "Count") +
        labs(title = "GO Enrichment Dotplot", x = "Gene Ratio", y = "") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
              strip.text = element_text(face = "bold", size = 10),
              axis.text.y = element_text(size = 8))
      print(p)
    })
    
    output$goChord <- renderPlot({
      res <- go_results_data()
      if (is.null(res) || nrow(res) == 0) {
        enrich_blank_plot()
        return()
      }
      
      top_go <- res %>%
        group_by(ONTOLOGY) %>%
        slice_head(n = 10) %>%
        ungroup()
      
      top_go$Description_wrapped <- sapply(top_go$Description, function(x) {
        if (nchar(x) > 30) paste(strwrap(x, width = 30), collapse = "\n") else x
      })
      
      ontologies <- c("BP", "CC", "MF")
      mat <- matrix(0, nrow = length(ontologies), ncol = nrow(top_go))
      rownames(mat) <- ontologies
      colnames(mat) <- top_go$Description_wrapped
      
      for (i in 1:nrow(top_go)) {
        ont <- top_go$ONTOLOGY[i]
        mat[ont, i] <- -log10(top_go$p.adjust[i])
      }
      
      grid_col <- c(BP = "#E69F00", CC = "#56B4E9", MF = "#009E73")
      term_colors <- rep("#BBBBBB", ncol(mat))
      names(term_colors) <- colnames(mat)
      grid_col <- c(grid_col, term_colors)
      
      circos.clear()
      circos.par(start.degree = 90, gap.degree = c(rep(2, 2), 10, rep(1, ncol(mat)-1), 10))
      
      chordDiagram(mat, grid.col = grid_col, transparency = 0.4,
                   annotationTrack = "grid",
                   preAllocateTracks = list(track.height = 0.15))
      
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        sector.name <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(mean(xlim), ylim[1] + 0.1, sector.name,
                    facing = "clockwise", niceFacing = TRUE,
                    adj = c(0, 0.5), cex = 0.5)
      }, bg.border = NA)
      
      title("GO Enrichment Chord")
      circos.clear()
    })
    
    # ============================================================
    # KEGG 可视化
    # ============================================================
    
    output$keggBarplot <- renderPlot({
      kegg_df <- kegg_results_data()
      if (is.null(kegg_df) || nrow(kegg_df) == 0) {
        enrich_blank_plot()
        return()
      }
      
      top_n <- min(input$showNum, nrow(kegg_df))
      top_kegg <- kegg_df[order(kegg_df$p.adjust), ][1:top_n, ]
      top_kegg <- top_kegg[order(top_kegg$Count), ]
      
      p <- ggplot(top_kegg, aes(x = reorder(Description, Count), 
                                y = Count, 
                                fill = p.adjust)) +
        geom_bar(stat = "identity", width = 0.7) +
        coord_flip() +
        geom_text(aes(label = Count), hjust = -0.2, size = 3.5) +
        scale_fill_gradient(low = "#2c7bb6", high = "#d7191c",
                            name = "Adjusted P") +
        labs(title = "KEGG Enrichment", x = "", y = "Gene Count") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
              axis.text.y = element_text(size = 8))
      print(p)
    })
    
    output$keggBubble <- renderPlot({
      kegg_df <- kegg_results_data()
      if (is.null(kegg_df) || nrow(kegg_df) == 0) {
        enrich_blank_plot()
        return()
      }
      
      top_n <- min(input$showNum, nrow(kegg_df))
      top_kegg <- kegg_df[order(kegg_df$p.adjust), ][1:top_n, ]
      
      top_kegg$GeneRatio_num <- sapply(top_kegg$GeneRatio, function(x) {
        parts <- strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      })
      
      p <- ggplot(top_kegg, aes(x = GeneRatio_num, 
                                y = reorder(Description, GeneRatio_num),
                                size = Count,
                                color = p.adjust)) +
        geom_point(alpha = 0.8) +
        scale_color_gradient(low = "#2c7bb6", high = "#d7191c",
                             name = "Adjusted P") +
        scale_size_continuous(range = c(3, 8), name = "Count") +
        labs(title = "KEGG Dotplot", x = "Gene Ratio", y = "") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
              axis.text.y = element_text(size = 8))
      print(p)
    })
    
    output$keggChord <- renderPlot({
      kegg_df <- kegg_results_data()
      if (is.null(kegg_df) || nrow(kegg_df) == 0) {
        enrich_blank_plot()
        return()
      }
      
      top_n <- min(20, nrow(kegg_df))
      top_kegg <- kegg_df[order(kegg_df$p.adjust), ][1:top_n, ]
      
      all_genes <- unlist(strsplit(top_kegg$geneID, "/"))
      gene_freq <- sort(table(all_genes), decreasing = TRUE)
      top_genes <- names(head(gene_freq, n = min(30, length(gene_freq))))
      
      top_kegg$Description_wrapped <- sapply(top_kegg$Description, function(x) {
        if (nchar(x) > 25) paste(strwrap(x, width = 25), collapse = "\n") else x
      })
      
      edge_list <- data.frame()
      for (i in 1:nrow(top_kegg)) {
        genes <- strsplit(top_kegg$geneID[i], "/")[[1]]
        genes <- intersect(genes, top_genes)
        if (length(genes) > 0) {
          temp <- data.frame(
            pathway = rep(top_kegg$Description_wrapped[i], length(genes)),
            gene = genes,
            stringsAsFactors = FALSE
          )
          edge_list <- rbind(edge_list, temp)
        }
      }
      
      if (nrow(edge_list) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "无足够数据绘制圈图")
        return()
      }
      
      circos.clear()
      circos.par(start.degree = 90)
      
      chordDiagram(edge_list[, c("pathway", "gene")],
                   annotationTrack = "grid",
                   annotationTrackHeight = c(0.05),
                   preAllocateTracks = list(track.height = 0.1))
      
      circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
        sector.name <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(mean(xlim), ylim[1] + mm_y(2), sector.name,
                    facing = "clockwise", niceFacing = TRUE,
                    adj = c(0, 0.5), cex = 0.4)
      }, bg.border = NA)
      
      title("KEGG Pathway-Gene Association")
      circos.clear()
    })
    
    # ============================================================
    # 结果表格
    # ============================================================
    
    output$goTable <- renderDT({
      res <- go_results_data()
      if (is.null(res) || nrow(res) == 0) {
        return(NULL)
      }
      
      datatable(res[, c("ONTOLOGY", "Description", "GeneRatio", 
                        "pvalue", "p.adjust", "Count")],
                options = list(pageLength = 8, scrollX = TRUE, dom = 'ftp'),
                rownames = FALSE)
    })
    
    # ---- 更新GO计数 ----
    output$goCount <- renderUI({
      res <- go_results_data()
      if (is.null(res)) {
        tags$span("共 0 条", style = "font-size: 11px; color: #888;")
      } else {
        tags$span(paste0("共 ", nrow(res), " 条"), style = "font-size: 11px; color: #888;")
      }
    })
    
    output$keggTable <- renderDT({
      kegg_df <- kegg_results_data()
      if (is.null(kegg_df) || nrow(kegg_df) == 0) {
        return(NULL)
      }
      
      datatable(kegg_df[, c("ID", "Description", "GeneRatio", 
                            "pvalue", "p.adjust", "Count")],
                options = list(pageLength = 8, scrollX = TRUE, dom = 'ftp'),
                rownames = FALSE)
    })
    
    # ---- 更新KEGG计数 ----
    output$keggCount <- renderUI({
      res <- kegg_results_data()
      if (is.null(res)) {
        tags$span("共 0 条", style = "font-size: 11px; color: #888;")
      } else {
        tags$span(paste0("共 ", nrow(res), " 条"), style = "font-size: 11px; color: #888;")
      }
    })
    
    # ============================================================
    # 下载功能
    # ============================================================
    
    output$downloadGoBarplot <- downloadHandler(
      filename = "GO_barplot.png",
      content = function(file) {
        res <- go_results_data()
        if (is.null(res) || nrow(res) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行富集分析")
          dev.off()
          return()
        }
        
        top_go <- res %>%
          group_by(ONTOLOGY) %>%
          slice_head(n = input$showNum) %>%
          ungroup()
        
        color_param <- if (input$qCut > 0.05) "pvalue" else "p.adjust"
        
        p <- ggplot(top_go, aes(x = reorder(Description, -log10(p.adjust)), 
                                y = -log10(p.adjust), 
                                fill = !!sym(color_param))) +
          geom_bar(stat = "identity", width = 0.7) +
          coord_flip() +
          facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
          scale_fill_gradientn(colors = c("#FF6666", "#FFB266", "#FFFF99", 
                                          "#99FF99", "#6666FF", "#B266FF")) +
          labs(title = "GO Enrichment", x = "", y = "-log10(Adjusted P)") +
          theme_minimal(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                strip.text = element_text(face = "bold", size = 16),
                axis.text.y = element_text(size = 12))
        
        ggsave(file, p, width = 14, height = 12, dpi = 300)
      }
    )
    
    output$downloadGoBubble <- downloadHandler(
      filename = "GO_bubble.png",
      content = function(file) {
        res <- go_results_data()
        if (is.null(res) || nrow(res) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行富集分析")
          dev.off()
          return()
        }
        
        top_go <- res %>%
          group_by(ONTOLOGY) %>%
          slice_head(n = input$showNum) %>%
          ungroup()
        
        color_param <- if (input$qCut > 0.05) "pvalue" else "p.adjust"
        
        top_go$GeneRatio_num <- sapply(top_go$GeneRatio, function(x) {
          parts <- strsplit(x, "/")[[1]]
          as.numeric(parts[1]) / as.numeric(parts[2])
        })
        
        p <- ggplot(top_go, aes(x = GeneRatio_num, 
                                y = reorder(Description, GeneRatio_num),
                                size = Count, 
                                color = !!sym(color_param))) +
          geom_point(alpha = 0.8) +
          facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
          scale_color_gradientn(colors = c("#FFB266", "#FFFF99", "#99FF99", 
                                           "#6666FF", "#B266FF"),
                                name = "Adjusted P") +
          scale_size_continuous(range = c(3, 10), name = "Count") +
          labs(title = "GO Enrichment Dotplot", x = "Gene Ratio", y = "") +
          theme_minimal(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                strip.text = element_text(face = "bold", size = 16),
                axis.text.y = element_text(size = 12))
        
        ggsave(file, p, width = 14, height = 12, dpi = 300)
      }
    )
    
    output$downloadGoChord <- downloadHandler(
      filename = "GO_chord.png",
      content = function(file) {
        res <- go_results_data()
        if (is.null(res) || nrow(res) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行富集分析")
          dev.off()
          return()
        }
        
        top_go <- res %>%
          group_by(ONTOLOGY) %>%
          slice_head(n = 10) %>%
          ungroup()
        
        top_go$Description_wrapped <- sapply(top_go$Description, function(x) {
          if (nchar(x) > 30) paste(strwrap(x, width = 30), collapse = "\n") else x
        })
        
        ontologies <- c("BP", "CC", "MF")
        mat <- matrix(0, nrow = length(ontologies), ncol = nrow(top_go))
        rownames(mat) <- ontologies
        colnames(mat) <- top_go$Description_wrapped
        
        for (i in 1:nrow(top_go)) {
          ont <- top_go$ONTOLOGY[i]
          mat[ont, i] <- -log10(top_go$p.adjust[i])
        }
        
        grid_col <- c(BP = "#E69F00", CC = "#56B4E9", MF = "#009E73")
        term_colors <- rep("#BBBBBB", ncol(mat))
        names(term_colors) <- colnames(mat)
        grid_col <- c(grid_col, term_colors)
        
        png(file, width = 4000, height = 4000, res = 300)
        circos.clear()
        circos.par(start.degree = 90, gap.degree = c(rep(2, 2), 10, rep(1, ncol(mat)-1), 10))
        chordDiagram(mat, grid.col = grid_col, transparency = 0.4,
                     annotationTrack = "grid",
                     preAllocateTracks = list(track.height = 0.2))
        circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
          sector.name <- get.cell.meta.data("sector.index")
          xlim <- get.cell.meta.data("xlim")
          ylim <- get.cell.meta.data("ylim")
          circos.text(mean(xlim), ylim[1] + 0.1, sector.name,
                      facing = "clockwise", niceFacing = TRUE,
                      adj = c(0, 0.5), cex = 0.6)
        }, bg.border = NA)
        title("GO Enrichment Chord")
        circos.clear()
        dev.off()
      }
    )
    
    output$downloadGoTable <- downloadHandler(
      filename = "GO_results.csv",
      content = function(file) {
        res <- go_results_data()
        if (is.null(res) || nrow(res) == 0) {
          write.csv(data.frame(信息 = "无 GO 结果"), file, row.names = FALSE)
          return()
        }
        write.csv(res, file, row.names = FALSE)
      }
    )
    
    output$downloadKeggBarplot <- downloadHandler(
      filename = "KEGG_barplot.png",
      content = function(file) {
        kegg_df <- kegg_results_data()
        if (is.null(kegg_df) || nrow(kegg_df) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行富集分析")
          dev.off()
          return()
        }
        
        top_n <- min(input$showNum, nrow(kegg_df))
        top_kegg <- kegg_df[order(kegg_df$p.adjust), ][1:top_n, ]
        top_kegg <- top_kegg[order(top_kegg$Count), ]
        
        p <- ggplot(top_kegg, aes(x = reorder(Description, Count), 
                                  y = Count, 
                                  fill = p.adjust)) +
          geom_bar(stat = "identity", width = 0.7) +
          coord_flip() +
          geom_text(aes(label = Count), hjust = -0.2, size = 5) +
          scale_fill_gradient(low = "#2c7bb6", high = "#d7191c",
                              name = "Adjusted P") +
          labs(title = "KEGG Enrichment", x = "", y = "Gene Count") +
          theme_minimal(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                axis.text.y = element_text(size = 12))
        
        ggsave(file, p, width = 14, height = 12, dpi = 300)
      }
    )
    
    output$downloadKeggBubble <- downloadHandler(
      filename = "KEGG_bubble.png",
      content = function(file) {
        kegg_df <- kegg_results_data()
        if (is.null(kegg_df) || nrow(kegg_df) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行富集分析")
          dev.off()
          return()
        }
        
        top_n <- min(input$showNum, nrow(kegg_df))
        top_kegg <- kegg_df[order(kegg_df$p.adjust), ][1:top_n, ]
        
        top_kegg$GeneRatio_num <- sapply(top_kegg$GeneRatio, function(x) {
          parts <- strsplit(x, "/")[[1]]
          as.numeric(parts[1]) / as.numeric(parts[2])
        })
        
        p <- ggplot(top_kegg, aes(x = GeneRatio_num, 
                                  y = reorder(Description, GeneRatio_num),
                                  size = Count,
                                  color = p.adjust)) +
          geom_point(alpha = 0.8) +
          scale_color_gradient(low = "#2c7bb6", high = "#d7191c",
                               name = "Adjusted P") +
          scale_size_continuous(range = c(3, 10), name = "Count") +
          labs(title = "KEGG Dotplot", x = "Gene Ratio", y = "") +
          theme_minimal(base_size = 18) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
                axis.text.y = element_text(size = 12))
        
        ggsave(file, p, width = 14, height = 12, dpi = 300)
      }
    )
    
    output$downloadKeggChord <- downloadHandler(
      filename = "KEGG_chord.png",
      content = function(file) {
        kegg_df <- kegg_results_data()
        if (is.null(kegg_df) || nrow(kegg_df) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "请先运行富集分析")
          dev.off()
          return()
        }
        
        top_n <- min(20, nrow(kegg_df))
        top_kegg <- kegg_df[order(kegg_df$p.adjust), ][1:top_n, ]
        
        all_genes <- unlist(strsplit(top_kegg$geneID, "/"))
        gene_freq <- sort(table(all_genes), decreasing = TRUE)
        top_genes <- names(head(gene_freq, n = min(30, length(gene_freq))))
        
        top_kegg$Description_wrapped <- sapply(top_kegg$Description, function(x) {
          if (nchar(x) > 25) paste(strwrap(x, width = 25), collapse = "\n") else x
        })
        
        edge_list <- data.frame()
        for (i in 1:nrow(top_kegg)) {
          genes <- strsplit(top_kegg$geneID[i], "/")[[1]]
          genes <- intersect(genes, top_genes)
          if (length(genes) > 0) {
            temp <- data.frame(
              pathway = rep(top_kegg$Description_wrapped[i], length(genes)),
              gene = genes,
              stringsAsFactors = FALSE
            )
            edge_list <- rbind(edge_list, temp)
          }
        }
        
        if (nrow(edge_list) == 0) {
          png(file, width = 4000, height = 4000, res = 300)
          plot(1, type = "n", main = "无足够数据")
          dev.off()
          return()
        }
        
        png(file, width = 4000, height = 4000, res = 300)
        circos.clear()
        circos.par(start.degree = 90)
        chordDiagram(edge_list[, c("pathway", "gene")],
                     annotationTrack = "grid",
                     annotationTrackHeight = c(0.05),
                     preAllocateTracks = list(track.height = 0.1))
        circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
          sector.name <- get.cell.meta.data("sector.index")
          xlim <- get.cell.meta.data("xlim")
          ylim <- get.cell.meta.data("ylim")
          circos.text(mean(xlim), ylim[1] + mm_y(2), sector.name,
                      facing = "clockwise", niceFacing = TRUE,
                      adj = c(0, 0.5), cex = 0.5)
        }, bg.border = NA)
        title("KEGG Pathway-Gene Association")
        circos.clear()
        dev.off()
      }
    )
    
    output$downloadKeggTable <- downloadHandler(
      filename = "KEGG_results.csv",
      content = function(file) {
        kegg_df <- kegg_results_data()
        if (is.null(kegg_df) || nrow(kegg_df) == 0) {
          write.csv(data.frame(信息 = "无 KEGG 结果"), file, row.names = FALSE)
          return()
        }
        write.csv(kegg_df, file, row.names = FALSE)
      }
    )
    
    output$downloadExprData <- downloadHandler(
      filename = function() paste0("gene_expression_", Sys.Date(), ".csv"),
      content = function(file) {
        expr_df <- expr_data_results()
        if (is.null(expr_df) || nrow(expr_df) == 0) {
          write.csv(data.frame(信息 = "无基因表达数据"), file, row.names = FALSE)
          return()
        }
        write.csv(expr_df, file, row.names = FALSE)
      }
    )
    
  })
}
