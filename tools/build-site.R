pkgdown::init_site()
source("tools/coverage.R")
pkgdown::build_site(new_process = FALSE, install = TRUE, preview = FALSE)

# Serve the installed examples unchanged so catalogue checksums remain valid.
dir.create("docs/examples", recursive = TRUE, showWarnings = FALSE)
example_files <- list.files("inst/examples", full.names = TRUE)
if (!all(file.copy(example_files, "docs/examples", recursive = TRUE, overwrite = TRUE))) {
  stop("Could not copy the reference examples into the website.", call. = FALSE)
}
