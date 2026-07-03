# global.R - 全局配置文件

# ============================================================
# 1. 全局选项
# ============================================================
options(stringsAsFactors = FALSE)
options(ggrepel.max.overlaps = Inf)
options(shiny.maxRequestSize = 120 * 1024^2) # 120MB 上传限制

# ============================================================
# 2. 应用配置
# ============================================================
APP_NAME <- "研析通生物信息学综合分析软件"
APP_VERSION <- "v1.0"

EXAMPLE_DATA <- list(
  counts = file.path("data", "geneMatrix.txt"),
  control = file.path("data", "control.txt"),
  treat = file.path("data", "treat.txt")
)

APP_LOADED_PACKAGES <- character()
APP_MISSING_PACKAGES <- character()

# ============================================================
# 3. 安全依赖加载
# ============================================================
safe_library <- function(package, required = FALSE, quietly = TRUE) {
  package <- as.character(package)[1]

  if (!requireNamespace(package, quietly = TRUE)) {
    APP_MISSING_PACKAGES <<- unique(c(APP_MISSING_PACKAGES, package))
    message <- sprintf("缺少 R 包：%s", package)
    if (isTRUE(required)) {
      stop(message, call. = FALSE)
    }
    if (!isTRUE(quietly)) {
      warning(message, call. = FALSE, immediate. = TRUE)
    }
    return(FALSE)
  }

  suppressPackageStartupMessages(
    library(package, character.only = TRUE)
  )
  APP_LOADED_PACKAGES <<- unique(c(APP_LOADED_PACKAGES, package))
  TRUE
}

load_package_group <- function(packages, required = FALSE) {
  stats::setNames(
    vapply(packages, safe_library, logical(1), required = required),
    packages
  )
}

APP_CORE_PACKAGES <- c(
  "shiny", "shinyjs", "shinythemes", "DT"
)

APP_COMMON_PACKAGES <- c(
  "ggplot2", "plotly", "ggrepel", "ggpubr", "RColorBrewer",
  "pheatmap", "limma", "dplyr", "tidyr", "tibble", "stringr",
  "ComplexHeatmap", "circlize", "rlang"
)

load_package_group(APP_CORE_PACKAGES, required = FALSE)
load_package_group(APP_COMMON_PACKAGES, required = FALSE)

app_preview_datatable <- function(data, page_length = 20, options = list(), ...) {
  dt_options <- utils::modifyList(
    list(pageLength = page_length, scrollX = TRUE),
    options
  )
  DT::datatable(data, options = dt_options, ...)
}

app_start_task_notification <- function(message) {
  shiny::showNotification(
    message,
    type = "message",
    duration = NULL,
    closeButton = FALSE
  )
}

app_clear_task_notification <- function(notification_id) {
  if (!is.null(notification_id)) {
    shiny::removeNotification(notification_id)
  }
  invisible(NULL)
}

# ============================================================
# 3.1 异步任务支持
# ============================================================
APP_ASYNC_PACKAGES <- c("future", "promises", "later")
APP_ASYNC_AVAILABLE <- all(vapply(APP_ASYNC_PACKAGES, requireNamespace, logical(1), quietly = TRUE))

if (isTRUE(APP_ASYNC_AVAILABLE)) {
  invisible(load_package_group(APP_ASYNC_PACKAGES, required = FALSE))
  detected_cores <- parallel::detectCores(logical = TRUE)
  if (is.na(detected_cores) || detected_cores < 2) {
    detected_cores <- 2
  }
  async_workers <- max(2, min(4, detected_cores - 1))
  future::plan(future::multisession, workers = async_workers)
}

run_async_task <- function(task,
                           on_success,
                           on_error = NULL,
                           on_finally = NULL,
                           fallback_sync = TRUE) {
  stopifnot(is.function(task), is.function(on_success))

  task_domain <- if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::getDefaultReactiveDomain()
  } else {
    NULL
  }

  domain_alive <- function() {
    if (is.null(task_domain)) {
      return(TRUE)
    }
    closed <- tryCatch(
      {
        if (is.function(task_domain$isClosed)) {
          task_domain$isClosed()
        } else {
          FALSE
        }
      },
      error = function(error) TRUE
    )
    !isTRUE(closed)
  }

  is_destroyed_session_error <- function(error) {
    grepl(
      "session has been destroyed|Can't access reactive|session is closed",
      conditionMessage(error)
    )
  }

  safe_callback <- function(callback, ...) {
    if (!is.function(callback) || !domain_alive()) {
      return(invisible(NULL))
    }
    tryCatch(
      callback(...),
      error = function(error) {
        if (is_destroyed_session_error(error)) {
          return(invisible(NULL))
        }
        stop(error)
      }
    )
    invisible(NULL)
  }

  finish <- function() {
    safe_callback(on_finally)
    invisible(NULL)
  }

  fail <- function(error) {
    if (is.function(on_error)) {
      safe_callback(on_error, error)
    } else if (domain_alive()) {
      warning(conditionMessage(error), call. = FALSE)
    }
    finish()
  }

  if (!isTRUE(APP_ASYNC_AVAILABLE)) {
    if (!isTRUE(fallback_sync)) {
      fail(simpleError("缺少 future/promises/later，无法启动后台任务。"))
      return(invisible(NULL))
    }
    tryCatch(
      {
        value <- task()
        safe_callback(on_success, value)
        finish()
      },
      error = fail
    )
    return(invisible(NULL))
  }

  promise <- promises::future_promise({
    task()
  })

  promise <- promises::then(
    promise,
    onFulfilled = function(value) {
      safe_callback(on_success, value)
      finish()
      invisible(NULL)
    },
    onRejected = function(error) {
      fail(error)
      invisible(NULL)
    }
  )

  invisible(promise)
}

# ============================================================
# 4. 通用数据读取与校验
# ============================================================
get_upload_path <- function(file) {
  if (is.null(file)) {
    stop("未提供文件。", call. = FALSE)
  }
  if (is.list(file) && !is.null(file$datapath)) {
    return(file$datapath)
  }
  as.character(file)[1]
}

get_upload_name <- function(file) {
  if (is.list(file) && !is.null(file$name)) {
    return(file$name)
  }
  basename(get_upload_path(file))
}

detect_table_separator <- function(file) {
  ext <- tolower(tools::file_ext(get_upload_name(file)))
  if (identical(ext, "csv")) {
    return(",")
  }
  "\t"
}

read_expression_matrix <- function(file,
                                   sep = NULL,
                                   header = TRUE,
                                   row_names = 1,
                                   check_names = FALSE) {
  path <- get_upload_path(file)
  if (!file.exists(path)) {
    stop("表达矩阵文件不存在。", call. = FALSE)
  }

  sep <- sep %||% detect_table_separator(file)
  expr_data <- utils::read.table(
    path,
    sep = sep,
    header = header,
    check.names = check_names,
    stringsAsFactors = FALSE,
    comment.char = "",
    quote = "\""
  )

  if (!nrow(expr_data) || !ncol(expr_data)) {
    stop("表达矩阵为空。", call. = FALSE)
  }
  if (is.numeric(row_names) && ncol(expr_data) < row_names) {
    stop("表达矩阵缺少基因名列。", call. = FALSE)
  }

  gene_ids <- expr_data[[row_names]]
  gene_ids <- make.unique(trimws(as.character(gene_ids)))
  expr_data <- expr_data[, -row_names, drop = FALSE]

  expr_matrix <- as.matrix(expr_data)
  suppressWarnings(storage.mode(expr_matrix) <- "numeric")
  rownames(expr_matrix) <- gene_ids

  if (is.null(colnames(expr_matrix)) || any(colnames(expr_matrix) == "")) {
    stop("表达矩阵缺少样本名。", call. = FALSE)
  }

  expr_matrix
}

read_sample_list <- function(file, sep = NULL, column = 1, header = FALSE) {
  path <- get_upload_path(file)
  if (!file.exists(path)) {
    stop("样本分组文件不存在。", call. = FALSE)
  }

  sep <- sep %||% detect_table_separator(file)
  sample_data <- utils::read.table(
    path,
    sep = sep,
    header = header,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = "",
    quote = "\""
  )

  if (!nrow(sample_data) || ncol(sample_data) < column) {
    stop("样本分组文件为空或列数不足。", call. = FALSE)
  }

  samples <- trimws(as.character(sample_data[[column]]))
  unique(samples[nzchar(samples)])
}

validate_expression_inputs <- function(expr_matrix,
                                       control_samples = NULL,
                                       treat_samples = NULL,
                                       min_genes = 1,
                                       min_samples = 2,
                                       allow_na = FALSE,
                                       stop_on_error = TRUE) {
  errors <- character()
  warnings <- character()

  if (is.data.frame(expr_matrix)) {
    expr_matrix <- as.matrix(expr_matrix)
  }

  if (!is.matrix(expr_matrix)) {
    errors <- c(errors, "表达数据必须是矩阵或数据框。")
  } else {
    if (nrow(expr_matrix) < min_genes) {
      errors <- c(errors, sprintf("表达矩阵至少需要 %s 个基因。", min_genes))
    }
    if (ncol(expr_matrix) < min_samples) {
      errors <- c(errors, sprintf("表达矩阵至少需要 %s 个样本。", min_samples))
    }
    if (is.null(rownames(expr_matrix)) || any(!nzchar(rownames(expr_matrix)))) {
      errors <- c(errors, "表达矩阵缺少有效基因名。")
    }
    if (is.null(colnames(expr_matrix)) || any(!nzchar(colnames(expr_matrix)))) {
      errors <- c(errors, "表达矩阵缺少有效样本名。")
    }
    if (!is.numeric(expr_matrix)) {
      errors <- c(errors, "表达矩阵必须全部为数值。")
    }
    if (anyNA(expr_matrix) && isTRUE(allow_na)) {
      warnings <- c(warnings, "表达矩阵中存在 NA，后续分析可能需要过滤或填补。")
    } else if (anyNA(expr_matrix)) {
      errors <- c(errors, "表达矩阵中存在 NA，请检查是否包含非数值内容或缺失值。")
    }
  }

  if (!is.null(control_samples) && !is.null(treat_samples) && is.matrix(expr_matrix)) {
    missing_control <- setdiff(control_samples, colnames(expr_matrix))
    missing_treat <- setdiff(treat_samples, colnames(expr_matrix))
    overlap_samples <- intersect(control_samples, treat_samples)

    if (length(missing_control)) {
      errors <- c(errors, paste0("对照组样本不在表达矩阵中：", paste(missing_control, collapse = ", ")))
    }
    if (length(missing_treat)) {
      errors <- c(errors, paste0("实验组样本不在表达矩阵中：", paste(missing_treat, collapse = ", ")))
    }
    if (length(overlap_samples)) {
      errors <- c(errors, paste0("对照组和实验组样本重复：", paste(overlap_samples, collapse = ", ")))
    }
  }

  result <- list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    expr_matrix = expr_matrix
  )

  if (isTRUE(stop_on_error) && length(errors)) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  result
}

read_gene_list_file <- function(file, sep = NULL, column = 1, header = FALSE) {
  path <- get_upload_path(file)
  if (!file.exists(path)) {
    stop("基因列表文件不存在。", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(get_upload_name(file)))
  if (is.null(sep)) {
    sep <- if (identical(ext, "csv")) "," else "\t"
  }

  gene_data <- utils::read.table(
    path,
    sep = sep,
    header = header,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = "",
    quote = "\""
  )

  if (!nrow(gene_data) || ncol(gene_data) < column) {
    stop("基因列表文件为空或列数不足。", call. = FALSE)
  }

  genes <- trimws(as.character(gene_data[[column]]))
  unique(genes[nzchar(genes)])
}

create_shared_data_state <- function() {
  shiny::reactiveValues(
    loaded = FALSE,
    expression_matrix = NULL,
    control_samples = NULL,
    treat_samples = NULL,
    sample_groups = NULL,
    source = NULL,
    updated_at = NULL,
    validation = NULL
  )
}

update_shared_expression_state <- function(state,
                                           expr_matrix,
                                           control_samples,
                                           treat_samples,
                                           source = "upload",
                                           validation = NULL) {
  stopifnot(!is.null(state))

  state$expression_matrix <- expr_matrix
  state$control_samples <- control_samples
  state$treat_samples <- treat_samples
  state$sample_groups <- data.frame(
    sample = c(control_samples, treat_samples),
    group = c(rep("Control", length(control_samples)), rep("Treatment", length(treat_samples))),
    stringsAsFactors = FALSE
  )
  state$source <- source
  state$updated_at <- Sys.time()
  state$validation <- validation
  state$loaded <- TRUE

  invisible(state)
}

update_shared_matrix_state <- function(state,
                                       expr_matrix,
                                       source = "matrix",
                                       validation = NULL) {
  stopifnot(!is.null(state))

  state$expression_matrix <- expr_matrix
  state$control_samples <- NULL
  state$treat_samples <- NULL
  state$sample_groups <- NULL
  state$source <- source
  state$updated_at <- Sys.time()
  state$validation <- validation
  state$loaded <- TRUE

  invisible(state)
}

clear_shared_expression_state <- function(state) {
  stopifnot(!is.null(state))

  state$loaded <- FALSE
  state$expression_matrix <- NULL
  state$control_samples <- NULL
  state$treat_samples <- NULL
  state$sample_groups <- NULL
  state$source <- NULL
  state$updated_at <- NULL
  state$validation <- NULL

  invisible(state)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ============================================================
# 5. 全局工具函数
# ============================================================
filter_deg <- function(results_long, test_sel, p_sel, direction,
                       pval_cut, fc_cut, label_col) {
  results_filtered <- results_long %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::filter(if (!is.na(test_sel)) test == test_sel else TRUE) %>%
    dplyr::filter(if (p_sel == "Padj") Adj.P.Value < pval_cut else P.Value < pval_cut) %>%
    dplyr::filter(
      if (direction == "Up") logFC >= fc_cut
      else if (direction == "Down") logFC <= -fc_cut
      else abs(logFC) >= fc_cut
    ) %>%
    dplyr::arrange(dplyr::desc(abs(logFC))) %>%
    dplyr::mutate(label_id = !!rlang::sym(label_col)) %>%
    dplyr::filter(!is.na(label_id) & label_id != "") %>%
    as.data.frame()

  results_filtered
}

process_gene_list <- function(gene_list_text) {
  if (grepl("\n", gene_list_text)) {
    genes <- stringr::str_split(gene_list_text, "\n")[[1]]
  } else if (grepl(",", gene_list_text)) {
    genes <- stringr::str_split(gene_list_text, ",")[[1]]
  } else {
    genes <- gene_list_text
  }
  genes <- gsub(" ", "", genes, fixed = TRUE)
  unique(genes[genes != ""])
}

get_user_color <- function(palette_name, items) {
  n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
  item_count <- length(unique(items))
  if (item_count <= n) {
    colors <- RColorBrewer::brewer.pal(item_count, palette_name)
  } else {
    colors <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(n, palette_name)
    )(item_count)
  }
  colors
}

theme_publication <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = base_size + 4),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 2),
      legend.title = ggplot2::element_text(face = "bold", size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 2),
      panel.grid.major = ggplot2::element_line(color = "#e0e0e0", linetype = "dotted"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "#333333", linewidth = 0.5),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size)
    )
}

footer_text <- sprintf(
  '<hr><div align="center" style="font-size:12px; color:#666666;">%s %s</div>',
  APP_NAME,
  APP_VERSION
)

# ============================================================
# 6. 启动信息
# ============================================================
cat("========================================\n")
cat("  ", APP_NAME, " ", APP_VERSION, "\n", sep = "")
cat("  全局配置加载完成\n")
cat("========================================\n")
cat("已加载包数量:", length(APP_LOADED_PACKAGES), "\n")
if (length(APP_MISSING_PACKAGES)) {
  cat("缺失 R 包:", paste(APP_MISSING_PACKAGES, collapse = ", "), "\n")
}
cat("示例数据路径:\n")
cat("  - 表达矩阵:", EXAMPLE_DATA$counts, "\n")
cat("  - 对照组:", EXAMPLE_DATA$control, "\n")
cat("  - 实验组:", EXAMPLE_DATA$treat, "\n")
cat("========================================\n")
