# Execute one R Jupyter notebook and write a sidecar of cell outputs.
# Usage: Rscript tools/execute_one_notebook.R path/to/file.ipynb

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript tools/execute_one_notebook.R notebook.ipynb")
path <- args[[1]]

suppressPackageStartupMessages({
  library(jsonlite)
  library(evaluate)
  library(base64enc)
})

encode_plot <- function(recorded, width = 900, height = 500) {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = width, height = height, res = 120)
  on.exit({
    if (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
  }, add = TRUE)
  try(grDevices::replayPlot(recorded), silent = TRUE)
  if (grDevices::dev.cur() > 1) grDevices::dev.off()
  if (!file.exists(tf) || !file.info(tf)$size) return(NULL)
  raw <- readBin(tf, what = "raw", n = file.info(tf)$size)
  unlink(tf)
  base64enc::base64encode(raw)
}

encode_gg <- function(plot_obj, width = 9, height = 5) {
  tf <- tempfile(fileext = ".png")
  tryCatch({
    ggplot2::ggsave(tf, plot = plot_obj, width = width, height = height, dpi = 120)
  }, error = function(e) NULL)
  if (!file.exists(tf) || !file.info(tf)$size) return(NULL)
  raw <- readBin(tf, what = "raw", n = file.info(tf)$size)
  unlink(tf)
  base64enc::base64encode(raw)
}

as_text <- function(x) {
  txt <- paste(utils::capture.output(print(x)), collapse = "\n")
  if (!nzchar(txt)) txt <- paste(as.character(x), collapse = "\n")
  if (!endsWith(txt, "\n")) txt <- paste0(txt, "\n")
  txt
}

png_output <- function(b64) {
  list(
    output_type = "display_data",
    data = list(`image/png` = b64, `text/plain` = "<plot>"),
    metadata = list()
  )
}

stream_output <- function(text, name = "stdout") {
  if (!nzchar(text)) return(NULL)
  if (!endsWith(text, "\n")) text <- paste0(text, "\n")
  list(output_type = "stream", name = name, text = text)
}

run_cell <- function(src, env) {
  outputs <- list()
  recs <- tryCatch(
    evaluate(src, envir = env, stop_on_error = 1, new_device = TRUE),
    error = function(e) e
  )
  if (inherits(recs, "error")) {
    return(list(list(
      output_type = "error",
      ename = class(recs)[[1]],
      evalue = conditionMessage(recs),
      traceback = list(conditionMessage(recs))
    )))
  }
  for (item in recs) {
    if (inherits(item, "source")) next
    if (is.character(item) && !inherits(item, "source")) {
      out <- stream_output(paste(item, collapse = ""))
      if (!is.null(out)) outputs <- c(outputs, list(out))
    } else if (inherits(item, "warning") || inherits(item, "message")) {
      out <- stream_output(conditionMessage(item), name = "stderr")
      if (!is.null(out)) outputs <- c(outputs, list(out))
    } else if (inherits(item, "error")) {
      outputs <- c(outputs, list(list(
        output_type = "error",
        ename = class(item)[[1]],
        evalue = conditionMessage(item),
        traceback = list(conditionMessage(item))
      )))
    } else if (inherits(item, "recordedplot")) {
      b64 <- encode_plot(item)
      if (!is.null(b64)) outputs <- c(outputs, list(png_output(b64)))
    } else if (inherits(item, c("ggplot", "gg"))) {
      b64 <- encode_gg(item)
      if (!is.null(b64)) outputs <- c(outputs, list(png_output(b64)))
    } else {
      txt <- tryCatch(as_text(item), error = function(e) "")
      if (nzchar(txt) && txt != "NULL\n") {
        outputs <- c(outputs, list(list(
          output_type = "execute_result",
          data = list(`text/plain` = txt),
          metadata = list()
        )))
      }
    }
  }
  outputs
}

message("Executing ", path)
nb <- fromJSON(path, simplifyVector = FALSE)
env <- new.env(parent = globalenv())
eval(quote({
  options(warn = 1, knitr.table.format = "pipe")
  if ("package:MASS" %in% search()) {
    detach("package:MASS", unload = FALSE)
  }
}), envir = env)

sidecar <- list()
count <- 0L
for (i in seq_along(nb$cells)) {
  cell <- nb$cells[[i]]
  ctype <- if (is.list(cell$cell_type)) cell$cell_type[[1]] else cell$cell_type
  if (!identical(ctype, "code")) next
  count <- count + 1L
  src <- paste(unlist(cell$source), collapse = "")
  outputs <- run_cell(src, env)
  sidecar[[length(sidecar) + 1]] <- list(
    cell_index = i - 1L,
    execution_count = count,
    outputs = outputs
  )
}

out_path <- paste0(path, ".outputs.json")
write_json(sidecar, out_path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
message("Wrote ", out_path, " (", length(sidecar), " code cells)")
