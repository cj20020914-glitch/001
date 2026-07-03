`%||%` <- function(x, y) if (length(x) && !is.na(x) && nzchar(x)) x else y

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)][1] %||% "scripts/launch_app.R")
script_dir <- normalizePath(dirname(file_arg), winslash = "/", mustWork = TRUE)
root_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
local_lib <- file.path(root_dir, "R", "library")

if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

source(file.path(script_dir, "app_packages.R"), encoding = "UTF-8")

missing <- app_required_packages[
  !vapply(app_required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(
    "缺少运行依赖包：", paste(missing, collapse = ", "),
    "\n请先运行 scripts/prepare_portable_packages.R 或补齐 R/library。",
    call. = FALSE
  )
}

dir.create(file.path(root_dir, "logs"), showWarnings = FALSE, recursive = TRUE)
setwd(root_dir)

port <- suppressWarnings(as.integer(Sys.getenv("APP_PORT", "3838")))
if (is.na(port) || port <= 0) {
  port <- 3838L
}

url <- sprintf("http://127.0.0.1:%d", port)
cat("Starting Shiny app...\n")
cat("Project:", root_dir, "\n")
cat("URL:", url, "\n")
cat("Close this window to stop the app.\n\n")

shiny::runApp(root_dir, host = "127.0.0.1", port = port, launch.browser = TRUE)
