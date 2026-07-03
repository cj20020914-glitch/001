# Light Shiny smoke tests for the app entrypoint and module registry.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else file.path("tests", "smoke_test.R")
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(project_dir)

assert_true <- function(value, message) {
  if (!isTRUE(value)) {
    stop(message, call. = FALSE)
  }
}

assert_nonempty <- function(value, message) {
  if (is.null(value) || !length(value)) {
    stop(message, call. = FALSE)
  }
}

app_env <- new.env(parent = globalenv())
suppressPackageStartupMessages(
  source("app.R", local = app_env, encoding = "UTF-8")
)

assert_true(exists("ui", envir = app_env), "app.R did not create ui.")
assert_true(exists("server", envir = app_env), "app.R did not create server.")
assert_true(is.function(app_env$server), "server is not a function.")
assert_true(exists("module_registry", envir = app_env), "module_registry is missing.")

registry <- app_env$module_registry
assert_nonempty(registry, "module_registry is empty.")

module_ids <- vapply(registry, `[[`, character(1), "id")
menu_ids <- vapply(registry, `[[`, character(1), "menu_id")
available <- vapply(registry, function(module) isTRUE(module$available), logical(1))

assert_true(!anyDuplicated(module_ids), "module ids must be unique.")
assert_true(!anyDuplicated(menu_ids), "menu ids must be unique.")
assert_true(all(available), paste0("Some modules failed to load: ", paste(module_ids[!available], collapse = ", ")))

shiny::testServer(app_env$server, {
  for (i in seq_along(registry)) {
    module <- registry[[i]]
    input_values <- list(i)
    names(input_values) <- module$menu_id
    do.call(session$setInputs, input_values)

    page_ui <- output$mainContent
    assert_nonempty(page_ui, paste0("mainContent did not render for module: ", module$id))
  }
})

fake_module <- registry[[1]]
fake_module$available <- FALSE
fake_module$error <- "smoke test forced unavailable module"

unavailable_ui <- app_env$module_unavailable_ui(fake_module, fake_module$error)
unavailable_html <- paste(as.character(shiny::tagList(unavailable_ui)), collapse = "\n")

assert_true(grepl("不可用", unavailable_html, fixed = TRUE), "Unavailable-module UI did not show the expected Chinese prompt.")
assert_true(grepl(fake_module$error, unavailable_html, fixed = TRUE), "Unavailable-module UI did not include the failure reason.")

cat("SMOKE_OK\n")
cat("MODULES=", paste(module_ids, collapse = ","), "\n", sep = "")
