# Load the package if it is installed, otherwise source the functions from R/
if (requireNamespace("wastech4", quietly = TRUE)) {
  library(wastech4)
  inputs <- load_inputs()
} else {
  for (f in list.files("R", full.names = TRUE)) source(f)
  inputs <- load_inputs("inst/extdata")
}
suppressPackageStartupMessages(library(ggplot2))
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom"))
save_fig <- function(p, name, w = 7, h = 4.2) {
  ggsave(file.path("results/figures", name), p, width = w, height = h, dpi = 110, bg = "white")
}

write_res <- function(d, file, digits = 4) {
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], signif, digits = digits)
  utils::write.csv(d, file, row.names = FALSE)
}
