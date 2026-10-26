# Refresh catalogue checksums after editing the synthetic survey CSVs.
# Versions and source dates remain those maintained in each catalogue CSV.
# Requires digest; this script never downloads data or deploys the website.
directory <- "inst/examples"
published_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
for (name in c("codelists", "registries")) {
  catalogue <- file.path(directory, paste0(name, ".csv"))
  mapping <- utils::read.csv(catalogue, colClasses = "character", check.names = FALSE,
                             na.strings = NULL, fileEncoding = "UTF-8-BOM")
  mapping$published_at <- published_at
  mapping$sha256 <- vapply(file.path(directory, mapping$csv_path),
                           function(path) digest::digest(file = path, algo = "sha256"), character(1))
  utils::write.table(mapping, catalogue, sep = ",", row.names = FALSE, quote = TRUE,
                     na = "", fileEncoding = "UTF-8")
  writeLines(c(sprintf('published_at: "%s"', published_at),
               sprintf('sha256: "%s"', digest::digest(file = catalogue, algo = "sha256"))),
             file.path(directory, paste0(name, ".yaml")), useBytes = TRUE)
}
