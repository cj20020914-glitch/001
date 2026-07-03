`%||%` <- function(x, y) if (length(x) && !is.na(x) && nzchar(x)) x else y

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)][1] %||% "scripts/check_dependencies.R")
script_dir <- normalizePath(dirname(file_arg), winslash = "/", mustWork = TRUE)

source(file.path(script_dir, "app_packages.R"), encoding = "UTF-8")

root_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
local_lib <- file.path(root_dir, "R", "library")
if (dir.exists(local_lib)) {
  if (identical(Sys.getenv("APP_STRICT_LOCAL_LIB"), "1")) {
    .libPaths(local_lib)
  } else {
    .libPaths(c(local_lib, .libPaths()))
  }
}

missing <- app_required_packages[
  !vapply(app_required_packages, requireNamespace, logical(1), quietly = TRUE)
]

cat("R executable:", file.path(R.home("bin"), "Rscript.exe"), "\n")
cat("R version:", R.version.string, "\n")
cat("Library paths:\n")
cat(paste0("  - ", normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)), sep = "\n")
cat("\n")

if (length(missing)) {
  cat("MISSING_PACKAGES=", paste(missing, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}

cat("MISSING_PACKAGES=<none>\n")
cat("DEPENDENCY_CHECK_OK\n")
