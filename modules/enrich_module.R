# enrich_module.R - GO/KEGG 富集分析模块
# 按用户提供的 GO 与 KEGG 原始脚本重新生成。

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
      .enrich-result-card {
        min-height: 350px;
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
      .enrich-card .nav-tabs,
      .enrich-result-panel .nav-tabs {
        border-bottom: 1px solid #d7dee2;
        margin-bottom: 8px;
      }
      .enrich-card .nav-tabs > li > a,
      .enrich-result-panel .nav-tabs > li > a {
        border: none;
        border-radius: 0;
        margin-right: 26px;
        padding: 8px 2px 9px 2px;
        color: #37474f;
        background: transparent;
        font-size: 12px;
      }
      .enrich-card .nav-tabs > li.active > a,
      .enrich-card .nav-tabs > li.active > a:hover,
      .enrich-card .nav-tabs > li.active > a:focus,
      .enrich-result-panel .nav-tabs > li.active > a,
      .enrich-result-panel .nav-tabs > li.active > a:hover,
      .enrich-result-panel .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 2px solid #1e88e5;
        color: #1e88e5;
        background: transparent;
        font-weight: 700;
      }
      .enrich-upload-row {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        align-items: center;
        margin-bottom: 8px;
      }
      .enrich-upload-box {
        position: relative;
        border: 1px dashed #b0bec5;
        min-height: 88px;
        padding: 8px 10px;
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
      .enrich-upload-box .input-group,
      .enrich-upload-box .input-group-btn,
      .enrich-upload-box .btn-file,
      .enrich-upload-box input[type='file'] {
        width: 100%;
        height: 100%;
        cursor: pointer;
      }
      .enrich-upload-placeholder {
        pointer-events: none;
        display: grid;
        grid-template-columns: 1fr;
        gap: 4px;
        align-items: center;
        justify-items: center;
        min-height: 68px;
        text-align: center;
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
        max-width: 100%;
        color: #1e88e5;
        font-size: 11px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .enrich-run-btn {
        width: 100%;
        font-size: 12px;
        font-weight: 700;
        padding: 5px 0;
        border-radius: 0;
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
      .enrich-result-file-list {
        border: 1px solid #d7dee2;
        background: #ffffff;
      }
      .enrich-result-file-row {
        display: grid;
        grid-template-columns: 28px minmax(160px, 1fr) 54px minmax(180px, 1.4fr) 70px;
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
        max-height: 220px;
        overflow-y: auto;
      }
      .enrich-qa {
        font-size: 12px;
        line-height: 1.7;
        color: #455a64;
        max-height: 220px;
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
          tabsetPanel(
            id = ns("enrichTabset"),
            type = "tabs",
            tabPanel(
              "GO富集分析",
              div(
                class = "enrich-upload-row",
                div(
                  class = "enrich-upload-box",
                  tags$div(
                    class = "enrich-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 26px; line-height: 1;"),
                    tags$span("GO基因列表", class = "enrich-upload-title"),
                    tags$span(id = ns("goGeneFileStatus"), "Drop file here or click to upload", class = "enrich-upload-status")
                  ),
                  fileInput(ns("goGeneFile"), NULL,
                            accept = c(".txt", ".csv", ".tsv"),
                            buttonLabel = "浏览",
                            placeholder = "选择GO基因列表文件")
                )
              ),
              actionButton(ns("runGO"), "运行GO富集分析", class = "btn-success btn-sm enrich-run-btn")
            ),
            tabPanel(
              "KEGG富集分析",
              div(
                class = "enrich-upload-row",
                div(
                  class = "enrich-upload-box",
                  tags$div(
                    class = "enrich-upload-placeholder",
                    span(class = "glyphicon glyphicon-cloud-upload", style = "color: #a7adb7; font-size: 26px; line-height: 1;"),
                    tags$span("KEGG基因列表", class = "enrich-upload-title"),
                    tags$span(id = ns("keggGeneFileStatus"), "Drop file here or click to upload", class = "enrich-upload-status")
                  ),
                  fileInput(ns("keggGeneFile"), NULL,
                            accept = c(".txt", ".csv", ".tsv"),
                            buttonLabel = "浏览",
                            placeholder = "选择KEGG基因列表文件")
                )
              ),
              actionButton(ns("runKEGG"), "运行KEGG富集分析", class = "btn-success btn-sm enrich-run-btn")
            )
          )
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
            plotOutput(ns("activeEnrichPlot"), height = "285px", width = "100%", click = ns("activeEnrichPlot_click"))
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
              tabPanel("数据预览", uiOutput(ns("dataPreview"))),
              tabPanel(
                "Q&A",
                div(
                  class = "enrich-qa",
                  tags$dl(
                    tags$dt("Q1：GO 和 KEGG 是否独立运行？"),
                    tags$dd("是。左侧切到 GO 或 KEGG 后分别上传基因文件并运行，结果和图片分别保存。"),
                    tags$dt("Q2：输入文件格式是什么？"),
                    tags$dd("支持 TXT、CSV、TSV，默认读取第一列 Gene Symbol。CSV 可以有表头，常见 Gene/genes/symbol 表头会自动去掉。"),
                    tags$dt("Q3：分析参数如何设置？"),
                    tags$dd("参数按你提供的原始代码固定：GO 使用 pvalue < 0.05 且 qvalue < 0.05；KEGG 使用 pvalue < 0.05 且 p.adjust < 1。"),
                    tags$dt("Q4：图片有哪些？"),
                    tags$dd("GO 按 BP、CC、MF 分别输出 txt、barplot、bubble、circos，并额外输出 BP/CC/MF 组合气泡图；KEGG 输出 KEGG.txt、custom barplot、custom dotplot、cnetplot、chord diagram。图片在无显著结果时会使用未过滤结果的 Top 项绘制。")
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

enrich_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    go_state <- reactiveVal(NULL)
    kegg_state <- reactiveVal(NULL)
    active_go_plot <- reactiveVal("go_BP_barplot")
    active_kegg_plot <- reactiveVal("kegg_bar_custom")
    is_running <- reactiveVal(FALSE)

    go_categories <- c("BP", "CC", "MF")
    go_plot_defs <- do.call(rbind, lapply(go_categories, function(ont) {
      data.frame(
        key = paste0("go_", ont, c("_barplot", "_bubble", "_circos")),
        file = paste0(ont, c(".barplot.png", ".bubble.png", ".circos.png")),
        desc = paste0("GO ", ont, c(" barplot图", " bubble图", " circos图")),
        ontology = ont,
        plot_type = c("barplot", "bubble", "circos"),
        stringsAsFactors = FALSE
      )
    }))
    go_plot_defs <- rbind(
      go_plot_defs,
      data.frame(
        key = "go_combined_dotplot",
        file = "GO_combined_dotplot.png",
        desc = "GO BP/CC/MF组合气泡图",
        ontology = "ALL",
        plot_type = "combined_dotplot",
        stringsAsFactors = FALSE
      )
    )

    kegg_plot_defs <- data.frame(
      key = c("kegg_bar_custom", "kegg_dot_custom", "kegg_cnet", "kegg_chord"),
      file = c("barplot_custom.png", "dotplot_custom.png", "cnetplot_custom.png", "chordDiagram_custom.png"),
      desc = c("KEGG自定义条形图", "KEGG自定义气泡图", "KEGG基因-通路网络图", "KEGG chord弦图"),
      stringsAsFactors = FALSE
    )

    current_mode <- reactive({
      if (identical(input$enrichTabset, "KEGG富集分析")) "kegg" else "go"
    })

    current_plot_key <- reactive({
      if (identical(current_mode(), "kegg")) active_kegg_plot() else active_go_plot()
    })

    read_enrich_genes <- function(file) {
      genes <- read_gene_list_file(file)
      genes <- genes[!tolower(genes) %in% c("gene", "genes", "symbol", "gene_symbol", "genesymbol")]
      unique(genes[nzchar(genes)])
    }

    map_symbols_to_entrez <- function(gene_symbols, keep_all = FALSE) {
      entrez_mapping <- mget(gene_symbols, org.Hs.eg.db::org.Hs.egSYMBOL2EG, ifnotfound = NA)
      entrez_ids <- vapply(entrez_mapping, function(x) {
        if (length(x) == 0 || all(is.na(x))) return(NA_character_)
        as.character(x[[1]])
      }, character(1))
      valid <- !is.na(entrez_ids) & entrez_ids != "NA"
      mapping <- data.frame(
        genes = gene_symbols[valid],
        entrezIDs = entrez_ids[valid],
        stringsAsFactors = FALSE
      )
      if (isTRUE(keep_all)) {
        mapping <- data.frame(
          genes = gene_symbols,
          entrezID = entrez_ids,
          entrezIDs = entrez_ids,
          stringsAsFactors = FALSE
        )
      }
      mapping
    }

    gene_ratio_num <- function(x) {
      vapply(strsplit(as.character(x), "/"), function(parts) {
        if (length(parts) != 2) return(NA_real_)
        numerator <- suppressWarnings(as.numeric(parts[1]))
        denominator <- suppressWarnings(as.numeric(parts[2]))
        if (is.na(numerator) || is.na(denominator) || denominator == 0) return(NA_real_)
        numerator / denominator
      }, numeric(1))
    }

    blank_plot <- function(message = "请先运行分析") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
      if (nzchar(message)) text(1, 1, message, col = "#607d8b")
    }

    enrich_cnetplot <- function(x, show_category = 5) {
      cnet_args <- names(formals(enrichplot::cnetplot))
      if ("colorEdge" %in% cnet_args || "circular" %in% cnet_args) {
        return(enrichplot::cnetplot(
          x,
          circular = TRUE,
          showCategory = show_category,
          colorEdge = TRUE
        ))
      }

      if ("color_edge" %in% cnet_args) {
        return(enrichplot::cnetplot(
          x,
          showCategory = show_category,
          color_edge = "grey50"
        ))
      }

      enrichplot::cnetplot(x, showCategory = show_category)
    }

    go_top_terms <- function(filtered_go) {
      if (is.null(filtered_go) || nrow(filtered_go) == 0) return(NULL)
      top_go <- filtered_go %>%
        dplyr::filter(ONTOLOGY %in% c("BP", "CC", "MF")) %>%
        dplyr::group_by(ONTOLOGY) %>%
        dplyr::slice_min(order_by = p.adjust, n = 10, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(ONTOLOGY, p.adjust) %>%
        dplyr::group_by(ONTOLOGY) %>%
        dplyr::mutate(Description = factor(Description, levels = rev(unique(Description)))) %>%
        dplyr::ungroup()
      top_go$GeneRatio_num <- gene_ratio_num(top_go$GeneRatio)
      top_go
    }

    kegg_top_terms <- function(kegg_df) {
      if (is.null(kegg_df) || nrow(kegg_df) == 0) return(NULL)
      show_num <- min(30, nrow(kegg_df))
      top_kegg <- kegg_df[order(kegg_df$p.adjust), , drop = FALSE][seq_len(show_num), , drop = FALSE]
      top_kegg$Description <- factor(top_kegg$Description, levels = rev(top_kegg$Description))
      top_kegg$GeneRatio_num <- gene_ratio_num(top_kegg$GeneRatio)
      top_kegg
    }

    run_go_analysis <- function(file) {
      gene_symbols <- read_enrich_genes(file)
      if (!length(gene_symbols)) stop("未找到有效的基因符号", call. = FALSE)

      mapping_all <- map_symbols_to_entrez(gene_symbols, keep_all = TRUE)
      mapping <- mapping_all[!is.na(mapping_all$entrezIDs) & mapping_all$entrezIDs != "NA", , drop = FALSE]
      if (!nrow(mapping)) stop("无有效的 Entrez 基因 ID", call. = FALSE)

      go_analyses <- list()
      go_results <- list()
      all_go_results <- list()

      for (ont in go_categories) {
        go_analysis <- clusterProfiler::enrichGO(
          gene = mapping$entrezIDs,
          OrgDb = org.Hs.eg.db::org.Hs.eg.db,
          pvalueCutoff = 1,
          qvalueCutoff = 1,
          ont = ont,
          readable = TRUE
        )

        go_result <- as.data.frame(go_analysis)
        if (nrow(go_result) && !"ONTOLOGY" %in% colnames(go_result)) {
          go_result$ONTOLOGY <- ont
        }
        filtered_go <- go_result[go_result$pvalue < 0.05 & go_result$qvalue < 0.05, , drop = FALSE]
        if (nrow(filtered_go) && !"ONTOLOGY" %in% colnames(filtered_go)) {
          filtered_go$ONTOLOGY <- ont
        }

        go_analyses[[ont]] <- go_analysis
        go_results[[ont]] <- filtered_go
        all_go_results[[ont]] <- go_result
      }

      combined_result <- do.call(rbind, go_results)
      if (is.null(combined_result)) {
        combined_result <- data.frame()
      }
      combined_all_result <- do.call(rbind, all_go_results)
      if (is.null(combined_all_result)) {
        combined_all_result <- data.frame()
      }

      list(
        analyses = go_analyses,
        results_by_ontology = go_results,
        all_results_by_ontology = all_go_results,
        result = combined_result,
        all_result = combined_all_result,
        id_table = mapping_all[, c("genes", "entrezID"), drop = FALSE],
        gene_count = length(gene_symbols),
        mapped_count = nrow(mapping)
      )
    }

    run_kegg_analysis <- function(file) {
      genes <- read_enrich_genes(file)
      if (!length(genes)) stop("未找到有效的基因符号", call. = FALSE)

      mapping <- map_symbols_to_entrez(genes)
      if (!nrow(mapping)) stop("无有效的 Entrez 基因 ID", call. = FALSE)

      kk <- clusterProfiler::enrichKEGG(
        gene = mapping$entrezIDs,
        organism = "hsa",
        pvalueCutoff = 1,
        qvalueCutoff = 1
      )

      kkx <- clusterProfiler::setReadable(kk, OrgDb = org.Hs.eg.db::org.Hs.eg.db, keyType = "ENTREZID")
      all_kegg <- as.data.frame(kkx)
      kegg <- all_kegg[all_kegg$pvalue < 0.05 & all_kegg$p.adjust < 1, , drop = FALSE]
      list(
        analysis = kkx,
        result = kegg,
        all_result = all_kegg,
        gene_count = length(genes),
        mapped_count = nrow(mapping)
      )
    }

    go_plot_source <- function(state, ont) {
      filtered_go <- state$results_by_ontology[[ont]]
      if (!is.null(filtered_go) && nrow(filtered_go)) {
        return(filtered_go)
      }
      all_go <- state$all_results_by_ontology[[ont]]
      if (!is.null(all_go) && nrow(all_go)) {
        return(all_go)
      }
      data.frame()
    }

    go_display_num <- function(go_df) {
      if (is.null(go_df) || !nrow(go_df)) return(0L)
      min(12L, nrow(go_df))
    }

    wrap_term_label <- function(text, width = 58) {
      vapply(as.character(text), function(value) {
        paste(strwrap(value, width = width), collapse = "\n")
      }, character(1))
    }

    parse_go_plot_key <- function(key) {
      matched <- go_plot_defs[match(key, go_plot_defs$key), , drop = FALSE]
      if (!nrow(matched)) return(NULL)
      list(ontology = matched$ontology[1], plot_type = matched$plot_type[1])
    }

    go_available_ontologies <- function(state) {
      if (is.null(state) || is.null(state$results_by_ontology)) return(character(0))
      go_categories[vapply(go_categories, function(ont) {
        result <- state$results_by_ontology[[ont]]
        !is.null(result) && nrow(result) > 0
      }, logical(1))]
    }

    draw_go_plot <- function(key, large = FALSE) {
      state <- go_state()
      if (is.null(state)) {
        blank_plot("请先运行GO富集分析")
        return(invisible(NULL))
      }

      parsed <- parse_go_plot_key(key)
      if (is.null(parsed)) {
        blank_plot("未知GO图片")
        return(invisible(NULL))
      }

      ont <- parsed$ontology
      plot_type <- parsed$plot_type
      if (identical(plot_type, "combined_dotplot")) {
        combined_go <- do.call(rbind, lapply(go_categories, function(category) {
          data <- go_plot_source(state, category)
          if (!nrow(data)) return(NULL)
          data$ONTOLOGY <- category
          data[order(data$pvalue), , drop = FALSE][seq_len(min(10L, nrow(data))), , drop = FALSE]
        }))
        if (is.null(combined_go) || !nrow(combined_go)) {
          blank_plot("GO无可绘制结果")
          return(invisible(NULL))
        }
        combined_go$GeneRatio_num <- gene_ratio_num(combined_go$GeneRatio)
        combined_go$DescriptionLabel <- wrap_term_label(combined_go$Description, width = 54)
        combined_go$DescriptionLabel <- factor(combined_go$DescriptionLabel, levels = rev(unique(combined_go$DescriptionLabel)))
        p <- ggplot2::ggplot(combined_go, ggplot2::aes(x = GeneRatio_num, y = DescriptionLabel, size = Count, color = pvalue)) +
          ggplot2::geom_point(alpha = 0.85) +
          ggplot2::facet_wrap(~ONTOLOGY, scales = "free_y", ncol = 1) +
          ggplot2::scale_color_gradientn(colors = RColorBrewer::brewer.pal(7, "Spectral"), name = "pvalue") +
          ggplot2::theme_classic(base_size = if (large) 15 else 12) +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = if (large) 10 else 8),
            strip.text = ggplot2::element_text(face = "bold"),
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
          ) +
          ggplot2::labs(x = "Gene Ratio", y = "GO Term", title = "GO BP/CC/MF Combined Dotplot")
        print(p)
        return(invisible(NULL))
      }

      go_analysis <- state$analyses[[ont]]
      plot_go <- go_plot_source(state, ont)
      display_num <- go_display_num(plot_go)
      if (is.null(go_analysis) || display_num == 0) {
        blank_plot(paste0("GO ", ont, " 无可绘制结果"))
        return(invisible(NULL))
      }

      if (identical(plot_type, "barplot")) {
        p <- enrichplot:::barplot.enrichResult(
          go_analysis,
          drop = TRUE,
          showCategory = display_num,
          label_format = 60,
          color = "pvalue"
        ) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = if (large) 10 else 8))
        print(p)
        return(invisible(NULL))
      }

      if (identical(plot_type, "bubble")) {
        p <- enrichplot::dotplot(
          go_analysis,
          showCategory = display_num,
          orderBy = "GeneRatio",
          label_format = 60,
          color = "pvalue"
        ) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = if (large) 10 else 8))
        print(p)
        return(invisible(NULL))
      }

      if (identical(plot_type, "circos")) {
        p <- enrich_cnetplot(go_analysis, show_category = min(5L, display_num))
        print(p)
        return(invisible(NULL))
      }

      blank_plot("未知GO图片")
    }

    draw_go_chord <- function(top_go) {
      insert_linebreak <- function(text, line_length = 35) {
        if (nchar(text) <= line_length) return(text)
        paste(strwrap(text, width = line_length), collapse = "\n")
      }
      top_go$Description_new <- vapply(as.character(top_go$Description), insert_linebreak, character(1))
      mat <- table(
        factor(top_go$ONTOLOGY, levels = c("BP", "CC", "MF")),
        factor(top_go$Description_new, levels = unique(top_go$Description_new))
      )
      n_ont <- 3
      n_term <- ncol(mat)
      if (n_term == 0) {
        blank_plot("GO无显著结果")
        return(invisible(NULL))
      }
      gap_degree <- c(rep(1, n_ont - 1), 10, rep(1, n_term - 1), 10)
      grid_col <- c(BP = "#E69F00", CC = "#56B4E9", MF = "#009E73", stats::setNames(rep("#BBBBBB", n_term), colnames(mat)))
      circlize::circos.clear()
      on.exit(circlize::circos.clear(), add = TRUE)
      circlize::circos.par(gap.degree = gap_degree, start.degree = 90)
      circlize::chordDiagram(
        mat,
        grid.col = grid_col,
        transparency = 0.4,
        annotationTrack = c("", "grid"),
        preAllocateTracks = list(track.height = 0.2)
      )
      circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        sector.name <- circlize::get.cell.meta.data("sector.index")
        xlim <- circlize::get.cell.meta.data("xlim")
        ylim <- circlize::get.cell.meta.data("ylim")
        circlize::circos.text(mean(xlim), ylim[1] + 0.1, sector.name,
                              facing = "clockwise", niceFacing = TRUE,
                              adj = c(0, 0.5), cex = 0.60)
      }, bg.border = NA)
      title("GO Ontology")
    }

    draw_kegg_plot <- function(key, large = FALSE) {
      state <- kegg_state()
      if (is.null(state)) {
        blank_plot("请先运行KEGG富集分析")
        return(invisible(NULL))
      }
      plot_kegg <- state$result
      if (is.null(plot_kegg) || !nrow(plot_kegg)) {
        plot_kegg <- state$all_result
      }
      if (is.null(plot_kegg) || nrow(plot_kegg) == 0) {
        blank_plot("KEGG无可绘制结果")
        return(invisible(NULL))
      }

      base_size <- if (large) 17 else 14
      top_kegg <- kegg_top_terms(plot_kegg)

      if (identical(key, "kegg_bar_custom")) {
        p <- ggplot2::ggplot(top_kegg, ggplot2::aes(x = Description, y = -log10(p.adjust), fill = -log10(p.adjust))) +
          ggplot2::geom_bar(stat = "identity", width = 0.8) +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, "YlOrRd")) +
          ggplot2::theme_minimal(base_size = base_size) +
          ggplot2::labs(x = "KEGG Pathway", y = "-log10(Adjusted p-value)", title = "KEGG Pathway Enrichment Analysis") +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "darkblue"))
        print(p)
        return(invisible(NULL))
      }

      if (identical(key, "kegg_dot_custom")) {
        p <- ggplot2::ggplot(top_kegg, ggplot2::aes(x = GeneRatio_num, y = Description, size = Count, color = p.adjust)) +
          ggplot2::geom_point(alpha = 0.8) +
          ggplot2::scale_color_gradientn(colors = RColorBrewer::brewer.pal(7, "Spectral")) +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::labs(x = "Gene Ratio", y = "KEGG Pathway", title = "KEGG Dotplot") +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "darkred"))
        print(p)
        return(invisible(NULL))
      }

      if (identical(key, "kegg_cnet")) {
        p <- enrich_cnetplot(state$analysis, show_category = 5)
        print(p + ggplot2::ggtitle("Gene-Pathway Network") +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "purple")))
        return(invisible(NULL))
      }

      if (identical(key, "kegg_chord")) {
        draw_kegg_chord(plot_kegg)
      }
    }

    draw_kegg_chord <- function(kegg_df) {
      top_kegg_chord <- kegg_df[seq_len(min(10, nrow(kegg_df))), , drop = FALSE]
      gene_path_df <- data.frame(Pathway = character(), Gene = character(), stringsAsFactors = FALSE)
      for (i in seq_len(nrow(top_kegg_chord))) {
        pathway <- top_kegg_chord$Description[i]
        genes <- unlist(strsplit(top_kegg_chord$geneID[i], "/"))
        gene_path_df <- rbind(gene_path_df, data.frame(Pathway = rep(pathway, length(genes)), Gene = genes, stringsAsFactors = FALSE))
      }
      if (!nrow(gene_path_df)) {
        blank_plot("无足够数据绘制弦图")
        return(invisible(NULL))
      }

      chord_matrix <- as.matrix(table(gene_path_df$Pathway, gene_path_df$Gene))
      all_sectors <- union(rownames(chord_matrix), colnames(chord_matrix))
      n_pathways <- length(rownames(chord_matrix))
      n_genes <- length(colnames(chord_matrix))
      pathway_colors <- RColorBrewer::brewer.pal(n = max(3, min(n_pathways, 8)), name = "Set2")
      if (n_pathways > length(pathway_colors)) pathway_colors <- rep(pathway_colors, length.out = n_pathways)
      gene_colors <- RColorBrewer::brewer.pal(n = max(3, min(n_genes, 8)), name = "Pastel1")
      if (n_genes > length(gene_colors)) gene_colors <- rep(gene_colors, length.out = n_genes)
      grid_col <- c(stats::setNames(pathway_colors, rownames(chord_matrix)), stats::setNames(gene_colors, colnames(chord_matrix)))
      grid_col <- grid_col[all_sectors]

      circlize::circos.clear()
      on.exit(circlize::circos.clear(), add = TRUE)
      circlize::chordDiagram(chord_matrix, grid.col = grid_col, transparency = 0.25,
                             annotationTrack = "grid", preAllocateTracks = list(track.height = 0.05))
      title("Gene-Pathway Chord Diagram")
      circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        sector.name <- circlize::get.cell.meta.data("sector.index")
        xcenter <- circlize::get.cell.meta.data("xcenter")
        ylim <- circlize::get.cell.meta.data("ylim")
        circlize::circos.text(xcenter, ylim[1] - circlize::mm_y(5),
                              sector.name, facing = "clockwise", niceFacing = TRUE,
                              adj = c(0, 0.5), cex = 0.8)
      }, bg.border = NA)
    }

    draw_active_plot <- function(large = FALSE) {
      if (identical(current_mode(), "kegg")) {
        draw_kegg_plot(active_kegg_plot(), large = large)
      } else {
        draw_go_plot(active_go_plot(), large = large)
      }
    }

    write_plot_png <- function(file, key) {
      width <- if (identical(key, "go_combined_dotplot")) 10 else if (grepl("^go_.*_circos$", key)) 12 else if (grepl("^go_", key)) 6 else if (identical(key, "kegg_cnet")) 9 else 8
      height <- if (identical(key, "go_combined_dotplot")) 12 else if (grepl("^go_.*_circos$", key)) 8 else if (grepl("^go_", key)) 7 else if (identical(key, "kegg_cnet")) 5.25 else 8
      if (identical(key, "kegg_bar_custom") || identical(key, "kegg_dot_custom")) height <- 7
      png(file, width = width * 300, height = height * 300, res = 300)
      on.exit(dev.off(), add = TRUE)
      if (grepl("^go_", key)) draw_go_plot(key, large = TRUE) else draw_kegg_plot(key, large = TRUE)
    }

    observeEvent(input$goGeneFile, {
      if (!is.null(input$goGeneFile)) {
        label <- gsub("\\\\", "\\\\\\\\", input$goGeneFile$name)
        label <- gsub('"', '\\"', label, fixed = TRUE)
        shinyjs::runjs(sprintf('$("#%s").text("%s")', ns("goGeneFileStatus"), label))
      }
    })

    observeEvent(input$keggGeneFile, {
      if (!is.null(input$keggGeneFile)) {
        label <- gsub("\\\\", "\\\\\\\\", input$keggGeneFile$name)
        label <- gsub('"', '\\"', label, fixed = TRUE)
        shinyjs::runjs(sprintf('$("#%s").text("%s")', ns("keggGeneFileStatus"), label))
      }
    })

    observeEvent(input$runGO, {
      if (is.null(input$goGeneFile)) {
        showNotification("请先上传 GO 基因列表文件！", type = "error")
        return()
      }
      gene_file <- input$goGeneFile
      is_running(TRUE)
      go_state(NULL)
      task_note <- app_start_task_notification("GO富集分析正在运行，请稍候。")

      tryCatch(
        {
          result <- run_go_analysis(gene_file)
          app_clear_task_notification(task_note)
          go_state(result)
          available_ont <- go_available_ontologies(result)
          active_go_plot(if (length(available_ont)) paste0("go_", available_ont[1], "_barplot") else "go_BP_barplot")
          note <- if (is.null(result$result) || nrow(result$result) == 0) "；GO无显著结果" else ""
          showNotification(
            paste0("GO富集分析完成！基因映射率 ",
                   round(result$mapped_count / result$gene_count * 100, 1), "%", note),
            type = "message",
            duration = 15
          )
        },
        error = function(error) {
          app_clear_task_notification(task_note)
          showNotification(paste0("GO错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        finally = {
          app_clear_task_notification(task_note)
          is_running(FALSE)
        }
      )
    })

    observeEvent(input$runKEGG, {
      if (is.null(input$keggGeneFile)) {
        showNotification("请先上传 KEGG 基因列表文件！", type = "error")
        return()
      }
      gene_file <- input$keggGeneFile
      is_running(TRUE)
      kegg_state(NULL)
      task_note <- app_start_task_notification("KEGG富集分析正在运行，请稍候。")

      tryCatch(
        {
          result <- run_kegg_analysis(gene_file)
          app_clear_task_notification(task_note)
          kegg_state(result)
          active_kegg_plot("kegg_bar_custom")
          note <- if (is.null(result$result) || nrow(result$result) == 0) "；KEGG无显著结果" else ""
          showNotification(
            paste0("KEGG富集分析完成！基因映射率 ",
                   round(result$mapped_count / result$gene_count * 100, 1), "%", note),
            type = "message",
            duration = 15
          )
        },
        error = function(error) {
          app_clear_task_notification(task_note)
          showNotification(paste0("KEGG错误: ", conditionMessage(error)), type = "error", duration = 10)
        },
        finally = {
          app_clear_task_notification(task_note)
          is_running(FALSE)
        }
      )
    })

    output$activeEnrichPlot <- renderPlot({
      draw_active_plot()
    })

    observeEvent(input$activeEnrichPlot_click, {
      state <- if (identical(current_mode(), "kegg")) kegg_state() else go_state()
      if (is.null(state)) return()
      showModal(
        modalDialog(
          title = if (identical(current_mode(), "kegg")) "KEGG图片预览" else "GO图片预览",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭"),
          div(style = "text-align: center;", plotOutput(ns("activeEnrichPlotLarge"), height = "620px"))
        )
      )
    })

    output$activeEnrichPlotLarge <- renderPlot({
      draw_active_plot(large = TRUE)
    })

    render_file_list <- function(files) {
      tags$div(
        class = "enrich-result-file-list",
        lapply(seq_len(nrow(files)), function(i) {
          key <- files$key[i]
          is_png <- identical(files$type[i], "PNG")
          name_control <- if (is_png) {
            actionButton(ns(paste0("show_", key)), files$file[i], class = "enrich-result-file-action")
          } else {
            span(files$file[i], class = "enrich-result-file-name", title = files$file[i])
          }
          download_control <- if (is_png) {
            downloadButton(ns(paste0("download_", key)), "下载", class = "btn-primary btn-xs")
          } else {
            downloadButton(ns(paste0("download_", key)), "下载", class = "btn-primary btn-xs")
          }
          tags$div(
            class = "enrich-result-file-row",
            span(sprintf("%02d", i), class = "enrich-file-index"),
            name_control,
            span(files$type[i], class = "enrich-result-file-type"),
            span(files$desc[i], class = "enrich-result-file-desc", title = files$desc[i]),
            span(class = "enrich-result-file-download", download_control)
          )
        })
      )
    }

    output$resultFileList <- renderUI({
      if (identical(current_mode(), "kegg")) {
        if (is.null(kegg_state())) return(NULL)
        files <- rbind(
          data.frame(key = kegg_plot_defs$key, file = kegg_plot_defs$file, type = "PNG", desc = kegg_plot_defs$desc, stringsAsFactors = FALSE),
          data.frame(key = "kegg_table", file = "KEGG.txt", type = "TXT", desc = "KEGG富集结果表", stringsAsFactors = FALSE)
        )
        render_file_list(files)
      } else {
        state <- go_state()
        if (is.null(state)) return(NULL)
        go_plot_files <- go_plot_defs[, c("key", "file", "desc"), drop = FALSE]
        go_plot_files$type <- "PNG"
        go_plot_files <- go_plot_files[, c("key", "file", "type", "desc"), drop = FALSE]
        files <- rbind(
          go_plot_files,
          data.frame(key = "go_id_table", file = "id.txt", type = "TXT", desc = "基因Symbol与Entrez ID转换表", stringsAsFactors = FALSE),
          data.frame(key = paste0("go_", go_categories, "_table"), file = paste0(go_categories, ".txt"), type = "TXT", desc = paste0("GO ", go_categories, "富集结果表"), stringsAsFactors = FALSE)
        )
        render_file_list(files)
      }
    })

    output$dataPreview <- renderUI({
      if (identical(current_mode(), "kegg")) {
        div(class = "enrich-result-data-preview", DT::DTOutput(ns("keggTable")))
      } else {
        div(class = "enrich-result-data-preview", DT::DTOutput(ns("goTable")))
      }
    })

    output$goTable <- DT::renderDT({
      state <- go_state()
      data <- if (is.null(state)) data.frame(信息 = "请先运行GO富集分析") else state$result
      if (is.null(data) || nrow(data) == 0) data <- data.frame(信息 = "无GO显著结果")
      DT::datatable(data, options = list(pageLength = 8, scrollX = TRUE, dom = "ftp"), rownames = FALSE)
    })

    output$keggTable <- DT::renderDT({
      state <- kegg_state()
      data <- if (is.null(state)) data.frame(信息 = "请先运行KEGG富集分析") else state$result
      if (is.null(data) || nrow(data) == 0) data <- data.frame(信息 = "无KEGG显著结果")
      DT::datatable(data, options = list(pageLength = 8, scrollX = TRUE, dom = "ftp"), rownames = FALSE)
    })

    lapply(go_plot_defs$key, function(key) {
      local({
        plot_key <- key
        observeEvent(input[[paste0("show_", plot_key)]], {
          active_go_plot(plot_key)
        })
        output[[paste0("download_", plot_key)]] <- downloadHandler(
          filename = function() go_plot_defs$file[match(plot_key, go_plot_defs$key)],
          content = function(file) write_plot_png(file, plot_key)
        )
      })
    })

    lapply(kegg_plot_defs$key, function(key) {
      local({
        plot_key <- key
        observeEvent(input[[paste0("show_", plot_key)]], {
          active_kegg_plot(plot_key)
        })
        output[[paste0("download_", plot_key)]] <- downloadHandler(
          filename = function() kegg_plot_defs$file[match(plot_key, kegg_plot_defs$key)],
          content = function(file) write_plot_png(file, plot_key)
        )
      })
    })

    output$download_go_id_table <- downloadHandler(
      filename = "id.txt",
      content = function(file) {
        state <- go_state()
        data <- if (is.null(state) || is.null(state$id_table)) data.frame(信息 = "无ID转换结果") else state$id_table
        utils::write.table(data, file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    )

    lapply(go_categories, function(ont) {
      local({
        current_ont <- ont
        output[[paste0("download_go_", current_ont, "_table")]] <- downloadHandler(
          filename = paste0(current_ont, ".txt"),
          content = function(file) {
            state <- go_state()
            data <- if (is.null(state) || is.null(state$results_by_ontology[[current_ont]])) {
              data.frame(信息 = paste0("无GO ", current_ont, "结果"))
            } else {
              state$results_by_ontology[[current_ont]]
            }
            utils::write.table(data, file = file, sep = "\t", quote = FALSE, row.names = FALSE)
          }
        )
      })
    })

    output$download_kegg_table <- downloadHandler(
      filename = "KEGG.txt",
      content = function(file) {
        state <- kegg_state()
        data <- if (is.null(state) || is.null(state$result)) data.frame(信息 = "无KEGG结果") else state$result
        utils::write.table(data, file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    )
  })
}
