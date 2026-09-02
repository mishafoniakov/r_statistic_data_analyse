# Execute all R notebooks in separate processes and merge outputs.
# Usage from repo root: Rscript execute_notebooks.R

files <- c(
  "01_data_preparation.ipynb",
  "02_score_statistics.ipynb",
  "03_score_accuracy.ipynb",
  "04_score_regression.ipynb",
  "05_score_reliability.ipynb"
)

for (f in files) {
  message("==== ", f)
  status <- system2("Rscript", c("tools/execute_one_notebook.R", f))
  if (!identical(status, 0L)) stop("R execution failed: ", f, call. = FALSE)
  py <- system2(
    "python3",
    c("tools/merge_notebook_outputs.py", f, paste0(f, ".outputs.json"))
  )
  if (!identical(py, 0L)) stop("Merge failed: ", f, call. = FALSE)
  unlink(paste0(f, ".outputs.json"))
}

message("All notebooks executed.")
