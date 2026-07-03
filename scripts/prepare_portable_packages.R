`%||%` <- function(x, y) if (length(x) && !is.na(x) && nzchar(x)) x else y

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)][1] %||% "scripts/prepare_portable_packages.R")
script_dir <- normalizePath(dirname(file_arg), winslash = "/", mustWork = TRUE)
root_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
local_lib <- file.path(root_dir, "R", "library")

source(file.path(script_dir, "app_packages.R"), encoding = "UTF-8")
dir.create(local_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))

installed_db <- utils::installed.packages()
missing_roots <- setdiff(app_required_packages, rownames(installed_db))
if (length(missing_roots)) {
  stop(
    "当前电脑还没安装这些包，无法复制进本地版 R/library：",
    paste(missing_roots, collapse = ", "),
    "\n请先在当前 R 环境安装这些包，再重新运行本脚本。",
    call. = FALSE
  )
}

deps <- tools::package_dependencies(
  app_required_packages,
  db = installed_db,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)

packages <- unique(c(app_required_packages, unlist(deps, use.names = FALSE)))
packages <- packages[packages %in% rownames(installed_db)]
packages <- packages[is.na(installed_db[packages, "Priority"]) | installed_db[packages, "Priority"] == ""]

cat("Preparing portable R packages...\n")
cat("Target library:", normalizePath(local_lib, winslash = "/", mustWork = FALSE), "\n")
cat("Package count:", length(packages), "\n\n")

for (pkg in packages) {
  src <- tryCatch(find.package(pkg, quiet = TRUE), error = function(e) character())
  if (!length(src) || !dir.exists(src)) {
    stop("找不到包目录：", pkg, call. = FALSE)
  }

  dest <- file.path(local_lib, pkg)
  src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)
  dest_norm <- normalizePath(dest, winslash = "/", mustWork = FALSE)
  if (identical(src_norm, dest_norm)) {
    cat("[skip] ", pkg, " already in local library\n", sep = "")
    next
  }

  if (dir.exists(dest)) {
    unlink(dest, recursive = TRUE, force = TRUE)
  }

  ok <- file.copy(src, dirname(dest), recursive = TRUE, copy.date = TRUE, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("复制包失败：", pkg, "\n源目录：", src, "\n目标目录：", dest, call. = FALSE)
  }
  cat("[copy] ", pkg, "\n", sep = "")
}

cat("\nPORTABLE_PACKAGES_READY\n")
