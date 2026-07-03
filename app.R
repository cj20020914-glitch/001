# app.R - 课题组生信分析平台

# ============================================================
# 1. 全局配置
# ============================================================
source("global.R", local = TRUE, encoding = "UTF-8")

# ============================================================
# 2. 模块注册表与安全加载
# ============================================================
register_module <- function(id, category, menu_name, file, ui_fun, server_fun, deps = character()) {
  list(
    id = id,
    category = category,
    menu_name = menu_name,
    menu_id = paste0("menu_", id),
    file = file,
    ui_fun = ui_fun,
    server_fun = server_fun,
    deps = deps,
    ui = NULL,
    server = NULL,
    env = NULL,
    available = FALSE,
    error = NULL
  )
}

module_registry <- list(
  register_module(
    id = "geo_symbol",
    category = "数据预处理",
    menu_name = "GEO数据整理",
    file = file.path("modules", "geo_symbol_module.R"),
    ui_fun = "geo_symbol_ui",
    server_fun = "geo_symbol_server",
    deps = c("DT", "data.table")
  ),
  register_module(
    id = "merge",
    category = "数据预处理",
    menu_name = "数据集合并",
    file = file.path("modules", "merge_module.R"),
    ui_fun = "merge_ui",
    server_fun = "merge_server",
    deps = c("DT", "limma", "ggplot2", "reshape2", "tools", "ggpubr", "RColorBrewer", "patchwork")
  ),
  register_module(
    id = "deg",
    category = "核心分析",
    menu_name = "差异分析",
    file = file.path("modules", "deg_module.R"),
    ui_fun = "deg_ui",
    server_fun = "deg_server",
    deps = c("pheatmap", "ggplot2", "RColorBrewer", "ggrepel", "DT", "limma", "colourpicker")
  ),
  register_module(
    id = "wgcna",
    category = "核心分析",
    menu_name = "WGCNA分析",
    file = file.path("modules", "wgcna_module.R"),
    ui_fun = "wgcna_ui",
    server_fun = "wgcna_server",
    deps = c("WGCNA", "ggplot2", "RColorBrewer", "dplyr", "tidyr", "reshape2", "DT")
  ),
  register_module(
    id = "venn",
    category = "核心分析",
    menu_name = "Venn图",
    file = file.path("modules", "venn_module.R"),
    ui_fun = "venn_ui",
    server_fun = "venn_server",
    deps = c("ggvenn", "ggplot2", "DT", "colourpicker")
  ),
  register_module(
    id = "enrich",
    category = "核心分析",
    menu_name = "富集分析",
    file = file.path("modules", "enrich_module.R"),
    ui_fun = "enrich_ui",
    server_fun = "enrich_server",
    deps = c("clusterProfiler", "org.Hs.eg.db", "enrichplot", "ggplot2", "RColorBrewer", "circlize", "stringr", "dplyr", "ggpubr", "ComplexHeatmap", "DT")
  ),
  register_module(
    id = "ml",
    category = "预测建模",
    menu_name = "机器学习",
    file = file.path("modules", "ml_module.R"),
    ui_fun = "ml_ui",
    server_fun = "ml_server",
    deps = c("limma", "glmnet", "randomForest", "e1071", "caret", "pROC", "ggplot2", "dplyr")
  ),
  register_module(
    id = "nomogram",
    category = "预测建模",
    menu_name = "列线图",
    file = file.path("modules", "nomogram_module.R"),
    ui_fun = "nomogram_ui",
    server_fun = "nomogram_server",
    deps = c("rms", "regplot", "rmda", "pROC", "ggplot2", "DT")
  ),
  register_module(
    id = "roc",
    category = "预测建模",
    menu_name = "ROC曲线",
    file = file.path("modules", "roc_module.R"),
    ui_fun = "roc_ui",
    server_fun = "roc_server",
    deps = c("limma", "pROC", "ggplot2", "reshape2", "DT", "patchwork")
  ),
  register_module(
    id = "immune",
    category = "高级分析",
    menu_name = "免疫浸润",
    file = file.path("modules", "immune_module.R"),
    ui_fun = "immune_ui",
    server_fun = "immune_server",
    deps = c("e1071", "limma", "pheatmap", "reshape2", "ggpubr", "ggplot2", "corrplot", "pROC")
  )
)

load_module <- function(module) {
  module_env <- new.env(parent = environment(load_module))
  module$env <- module_env

  if (!file.exists(module$file)) {
    module$error <- paste0("模块文件不存在：", module$file)
    return(module)
  }

  tryCatch(
    {
      invisible(vapply(module$deps, safe_library, logical(1), required = TRUE))

      suppressPackageStartupMessages(
        source(module$file, local = module_env)
      )

      module$ui <- get(module$ui_fun, envir = module_env, inherits = FALSE)
      module$server <- get(module$server_fun, envir = module_env, inherits = FALSE)

      if (!is.function(module$ui)) {
        stop("UI 函数不是可调用函数：", module$ui_fun, call. = FALSE)
      }
      if (!is.function(module$server)) {
        stop("Server 函数不是可调用函数：", module$server_fun, call. = FALSE)
      }

      module$available <- TRUE
    },
    error = function(e) {
      module$error <- conditionMessage(e)
      module$available <- FALSE
    }
  )

  module
}

module_registry <- lapply(module_registry, load_module)

get_module <- function(module_id, registry = module_registry) {
  module_index <- which(vapply(registry, function(module) identical(module$id, module_id), logical(1)))
  if (!length(module_index)) {
    return(NULL)
  }
  registry[[module_index[1]]]
}

module_unavailable_ui <- function(module, reason = NULL, title = "模块不可用") {
  tagList(
    div(
      class = "module-unavailable-panel",
      h3(title),
      p(sprintf("“%s”当前不可用，其他模块仍可继续使用。", module$menu_name)),
      if (!is.null(reason) && nzchar(reason)) {
        tags$details(
          tags$summary("查看原因"),
          tags$pre(reason)
        )
      }
    )
  )
}

render_module_menu <- function(registry) {
  categories <- unique(vapply(registry, `[[`, character(1), "category"))
  tagList(lapply(categories, function(category) {
    category_modules <- registry[
      vapply(registry, function(module) identical(module$category, category), logical(1))
    ]

    tagList(
      div(class = "menu-label", category),
      lapply(category_modules, function(module) {
        actionButton(
          inputId = module$menu_id,
          label = module$menu_name,
          width = "100%",
          class = if (isTRUE(module$available)) "module-menu-btn" else "module-menu-btn module-menu-unavailable",
          title = if (isTRUE(module$available)) module$menu_name else paste0(module$menu_name, "：模块不可用"),
          onclick = sprintf("window.switchModulePage && window.switchModulePage('%s', '%s');", module$id, module$menu_id)
        )
      })
    )
  }))
}

# ============================================================
# 3. UI 界面
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),

  tags$head(
    tags$meta(charset = "utf-8"),
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
      }
      .app-header {
        background-color: #2c3e50;
        color: white;
        padding: 15px 20px;
        border-radius: 8px 8px 0 0;
      }
      .app-title {
        font-weight: bold;
        font-size: 24px;
      }
      .app-version {
        font-size: 14px;
        color: #d5dde5;
        margin-left: 10px;
      }
      .sidebar-menu {
        background-color: #2c3e50;
        min-height: 100vh;
        padding: 10px 0;
        border-radius: 0;
      }
      .sidebar-menu .btn {
        width: 100%;
        text-align: left;
        padding: 12px 20px;
        margin-bottom: 2px;
        border-radius: 0;
        border: none;
        background-color: transparent;
        color: #aab8c5;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.2s;
      }
      .sidebar-menu .btn:hover {
        background-color: #34495e;
        color: white;
      }
      .sidebar-menu .btn-active {
        background-color: #3498db;
        color: white;
      }
      .sidebar-menu .btn-active:hover {
        background-color: #2980b9;
        color: white;
      }
      .btn-success,
      .btn-success:focus {
        background-color: #111111;
        border-color: #111111;
        color: #ffffff;
      }
      .btn-success:hover,
      .btn-success:active,
      .btn-success.active,
      .open > .dropdown-toggle.btn-success {
        background-color: #000000;
        border-color: #000000;
        color: #ffffff;
      }
      .sidebar-menu .module-menu-unavailable {
        color: #7f8c9d;
        font-style: italic;
      }
      .sidebar-menu .menu-label {
        color: #7f8c9d;
        padding: 15px 20px 8px 20px;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 600;
      }
      .main-content {
        padding: 20px 25px;
        background-color: #f8f9fa;
        min-height: 100vh;
      }
      .content-panel {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
      }
      .module-unavailable-panel {
        border: 1px solid #f0c36d;
        background-color: #fff8e5;
        border-radius: 8px;
        padding: 20px;
      }
      .module-unavailable-panel h3 {
        margin-top: 0;
        color: #8a5a00;
      }
      .module-unavailable-panel pre {
        margin-top: 10px;
        white-space: pre-wrap;
        word-break: break-word;
        background: #fffdf5;
        border-color: #f0dca6;
      }
      .module-page {
        display: none;
      }
    "))
  ),

  tags$script(HTML("
    window.switchModulePage = function(moduleId, menuId) {
      var pageId = 'module_page_' + moduleId;
      $('.module-page').hide();
      $('#' + pageId).show();
      $('.module-menu-btn').removeClass('btn-active');
      $('#' + menuId).addClass('btn-active');

      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('clientCurrentPage', moduleId, {priority: 'event'});
      }
    };

    $(document).on('shiny:connected', function() {
      var firstPage = $('.module-page').first();
      var firstMenu = $('.module-menu-btn').first();
      if (firstPage.length && firstMenu.length && !$('.module-page:visible').length) {
        window.switchModulePage(firstPage.data('module-id'), firstMenu.attr('id'));
      }
    });
  ")),

  div(
    class = "app-header",
    span(APP_NAME, class = "app-title"),
    span(APP_VERSION, class = "app-version")
  ),

  fluidRow(
    column(
      width = 2,
      div(
        class = "sidebar-menu",
        render_module_menu(module_registry)
      )
    ),
    column(
      width = 10,
      div(
        class = "main-content",
        div(
          class = "content-panel",
          uiOutput("mainContent")
        )
      )
    )
  )
)

# ============================================================
# 4. Server 逻辑
# ============================================================
server <- function(input, output, session) {
  currentPage <- reactiveVal(module_registry[[1]]$id)
  module_server_errors <- reactiveVal(list())
  shared_data <- create_shared_data_state()
  session$userData$shared_data <- shared_data

  set_module_server_error <- function(module_id, error_message) {
    errors <- module_server_errors()
    errors[[module_id]] <- error_message
    module_server_errors(errors)
  }

  invisible(lapply(module_registry, function(module) {
    force(module)
    observeEvent(input[[module$menu_id]], {
      currentPage(module$id)
    }, ignoreInit = TRUE)
  }))

  observeEvent(input$clientCurrentPage, {
    if (!is.null(get_module(input$clientCurrentPage))) {
      currentPage(input$clientCurrentPage)
    }
  }, ignoreInit = TRUE)

  observe({
    current_module <- currentPage()
    invisible(lapply(module_registry, function(module) {
      active_value <- if (identical(module$id, current_module)) "true" else "false"
      runjs(sprintf(
        '$("#%s").toggleClass("btn-active", %s);',
        module$menu_id,
        active_value
      ))
    }))
  })

  # 兼容旧入口中的共享示例数据下载与预览逻辑。
  dataLoaded <- reactiveVal(FALSE)

  output$downloadExampleCounts <- downloadHandler(
    filename = "geneMatrix.txt",
    content = function(file) {
      if (file.exists(EXAMPLE_DATA$counts)) {
        file.copy(EXAMPLE_DATA$counts, file)
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
      if (file.exists(EXAMPLE_DATA$control)) {
        file.copy(EXAMPLE_DATA$control, file)
      } else {
        writeLines(paste0("Sample", 1:6), file)
      }
    }
  )

  output$downloadExampleTreat <- downloadHandler(
    filename = "treat.txt",
    content = function(file) {
      if (file.exists(EXAMPLE_DATA$treat)) {
        file.copy(EXAMPLE_DATA$treat, file)
      } else {
        writeLines(paste0("Sample", 7:12), file)
      }
    }
  )

  observeEvent(input$loadData, {
    req(input$countFile, input$ctrlFile, input$treatFile)

    tryCatch(
      {
        expr_matrix <- read_expression_matrix(input$countFile)
        ctrl <- read_sample_list(input$ctrlFile)
        treat <- read_sample_list(input$treatFile)
        validation <- validate_expression_inputs(expr_matrix, ctrl, treat)
        update_shared_expression_state(
          shared_data,
          expr_matrix,
          ctrl,
          treat,
          source = "app_upload",
          validation = validation
        )

        dataLoaded(TRUE)
        showNotification("数据加载成功！", type = "message")
      },
      error = function(e) {
        showNotification(paste0("加载失败：", conditionMessage(e)), type = "error")
      }
    )
  })

  output$dataStatus <- renderText({
    if (isTRUE(shared_data$loaded)) {
      "数据已加载"
    } else {
      "请上传数据并点击“加载数据”"
    }
  })

  output$dataPreview <- renderDT({
    req(input$countFile)
    expr_matrix <- read_expression_matrix(input$countFile)
    preview_rows <- seq_len(min(10, nrow(expr_matrix)))
    preview_cols <- seq_len(min(6, ncol(expr_matrix)))
    preview <- as.data.frame(expr_matrix[preview_rows, preview_cols, drop = FALSE])
    preview <- cbind(Gene = rownames(preview), preview)

    datatable(preview, options = list(dom = "t", scrollX = TRUE))
  })

  output$mainContent <- renderUI({
    server_errors <- module_server_errors()

    tagList(lapply(seq_along(module_registry), function(index) {
      module <- module_registry[[index]]
      server_error <- server_errors[[module$id]]
      page_style <- if (index == 1) "display: block;" else "display: none;"

      page_content <- if (!is.null(server_error)) {
        module_unavailable_ui(module, server_error, "模块服务挂载失败")
      } else if (!isTRUE(module$available)) {
        module_unavailable_ui(module, module$error)
      } else {
        tryCatch(
          module$ui(module$id),
          error = function(e) {
            module_unavailable_ui(module, conditionMessage(e), "模块界面渲染失败")
          }
        )
      }

      div(
        id = paste0("module_page_", module$id),
        class = "module-page",
        `data-module-id` = module$id,
        style = page_style,
        page_content
      )
    }))
  })

  invisible(lapply(module_registry, function(module) {
    force(module)
    if (!isTRUE(module$available)) {
      return(NULL)
    }

    tryCatch(
      module$server(module$id),
      error = function(e) {
        set_module_server_error(module$id, conditionMessage(e))
      }
    )
  }))
}

shinyApp(ui, server)
