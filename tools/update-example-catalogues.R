# Refresh catalogue checksums after editing the synthetic survey CSVs.
# Versions and source dates remain those maintained in each catalogue CSV.
# Requires digest; this script never downloads data or deploys the website.
directories <- commandArgs(trailingOnly = TRUE)
if (!length(directories)) directories <- c("inst/examples/survey/catalogue/code_lists",
  "inst/examples/survey/catalogue/registries", "inst/examples/survey/catalogue-source/code_lists")
published_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
for (source_directory in directories) {
  catalogue <- file.path(source_directory, "catalogue.csv")
  mapping <- utils::read.csv(catalogue, colClasses = "character", check.names = FALSE,
                             na.strings = NULL, fileEncoding = "UTF-8-BOM")
  mapping <- mapping[order(mapping$domain, mapping$id),
                     c("domain", "id", setdiff(names(mapping), c("domain", "id")))]
  mapping$published_at <- published_at
  mapping$sha256 <- vapply(file.path(source_directory, mapping$csv_path),
                           function(path) digest::digest(file = path, algo = "sha256"), character(1))
  utils::write.table(mapping, catalogue, sep = ",", row.names = FALSE, quote = TRUE,
                     na = "", fileEncoding = "UTF-8")
  writeLines(c(sprintf('published_at: "%s"', published_at),
               sprintf('sha256: "%s"', digest::digest(file = catalogue, algo = "sha256"))),
             file.path(source_directory, "catalogue.yaml"), useBytes = TRUE)
}
