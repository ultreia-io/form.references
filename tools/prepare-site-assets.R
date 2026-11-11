# Require the package root before preparing generated website assets.
example_catalogue <- "inst/examples/survey/catalogue"
if (!dir.exists(example_catalogue) || !file.exists("LICENSE"))
  stop("Run Rscript tools/prepare-site-assets.R from the package root.", call. = FALSE)

# Replace generated assets so removed example files cannot remain in the site input.
website_assets <- "pkgdown/assets"
unlink(website_assets, recursive = TRUE)
website_examples <- file.path(website_assets, "examples")
dir.create(website_examples, recursive = TRUE, showWarnings = FALSE)

# Copy the example catalogues and tables without changing their contents.
if (!all(file.copy(list.files(example_catalogue, full.names = TRUE), website_examples,
    recursive = TRUE, overwrite = TRUE))) stop("Could not prepare example downloads.")

# Copy the licence linked from the maintainer guide.
if (!file.copy("LICENSE", file.path(website_assets, "LICENSE"), overwrite = TRUE))
  stop("Could not prepare the website licence.")
