# Identify the resource archive with the package version being built.
resources_version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
resources_archive <- paste0("form.references-resources-", resources_version, ".zip")

# Replace generated assets so removed example files cannot remain in the site input.
website_assets <- "pkgdown/assets"
unlink(website_assets, recursive = TRUE)
dir.create(website_assets, recursive = TRUE, showWarnings = FALSE)

# Publish every packaged schema and example with its original directory layout.
if (!all(file.copy(c("inst/schema", "inst/examples"), website_assets,
    recursive = TRUE, overwrite = TRUE))) stop("Could not prepare package resource downloads.")

# Bundle the canonical resource directories before adding compatibility endpoints.
zip::zipr(resources_archive, c("schema", "examples"), root = website_assets)

# Retain the catalogue URLs used by existing website examples and consumers.
example_catalogue <- "inst/examples/survey/catalogue"
website_examples <- file.path(website_assets, "examples")
if (!all(file.copy(list.files(example_catalogue, full.names = TRUE), website_examples,
    recursive = TRUE, overwrite = TRUE))) stop("Could not prepare example downloads.")

# Copy the licence linked from the maintainer guide.
if (!file.copy("LICENSE", file.path(website_assets, "LICENSE"), overwrite = TRUE))
  stop("Could not prepare the website licence.")
