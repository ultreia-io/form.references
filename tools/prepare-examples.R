# Refresh catalogue checksums after editing the synthetic survey CSVs.
# Versions and source dates remain those maintained in codelists.csv.
# Requires digest; this script never downloads data or deploys the website.
directory <- "inst/examples"
catalogue <- file.path(directory, "codelists.csv")
mapping <- utils::read.csv(catalogue, colClasses = "character", check.names = FALSE,
                           na.strings = NULL, fileEncoding = "UTF-8-BOM")
published_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
mapping$published_at <- published_at
mapping$sha256 <- vapply(file.path(directory, mapping$csv_path),
                         function(path) digest::digest(file = path, algo = "sha256"), character(1))
utils::write.table(mapping, catalogue, sep = ",", row.names = FALSE, quote = TRUE,
                   na = "", fileEncoding = "UTF-8")
writeLines(c(sprintf('published_at: "%s"', published_at),
             sprintf('sha256: "%s"', digest::digest(file = catalogue, algo = "sha256"))),
           file.path(directory, "codelists.yaml"), useBytes = TRUE)
