# Source configuration ------------------------------------------------------

#' Configure a reference source
#'
#' @param url HTTP(S) base URL or local directory containing the selected
#'   catalogue's YAML and CSV files. URLs must not contain credentials, query strings or fragments.
#' @param cache Local cache directory. Different sources receive separate
#'   subdirectories. Use separate caches for separate authenticated identities.
#' @param headers Named HTTP header values, or a function returning them at
#'   request time. NULL sends no authentication headers.
#' @param timeout Positive request timeout in seconds.
#' @param max_connections Maximum simultaneous CSV downloads. The default is four.
#' @param catalogue `"codelists"` or `"registries"`; selects the catalogue filenames.
#' @return A reference_source configuration. No files are downloaded.
#' @details Credentials stay in memory and are never written into the cache.
#'   Authenticated requests do not follow redirects; supply the final base URL.
#' @export
#' @examples
#' source <- reference_source(system.file("examples", package = "form.references"),
#'                            cache = tempfile("references-"))
#' source
reference_source <- function(url, cache, headers = NULL, timeout = 60,
                             max_connections = 4L, catalogue = "codelists") {
  scalar_text(url, "url")
  scalar_text(cache, "cache")
  if (!is.character(catalogue) || length(catalogue) != 1L || is.na(catalogue) ||
      !catalogue %in% c("codelists", "registries"))
    stop("catalogue must be codelists or registries.", call. = FALSE)
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0) stop("timeout must be a positive number.", call. = FALSE)
  if (!is.numeric(max_connections) || length(max_connections) != 1L ||
      is.na(max_connections) || !is.finite(max_connections) ||
      max_connections < 1 || max_connections != as.integer(max_connections))
    stop("max_connections must be a positive whole number.", call. = FALSE)
  remote <- grepl("^https?://", url)
  if (remote) {
    if (grepl("[?#[:cntrl:]]", url) || grepl("^https?://[^/]*@", url))
      stop("Use a base URL without credentials, query strings or fragments.", call. = FALSE)
    url <- paste0(sub("/+$", "", url), "/")
  } else {
    if (!dir.exists(url)) stop("Reference source directory does not exist.", call. = FALSE)
    url <- normalizePath(url, winslash = "/", mustWork = TRUE)
  }
  if (!is.function(headers)) reference_headers(headers)
  structure(list(url = url, cache = path.expand(cache), remote = remote,
                 catalogue = catalogue,
                 headers = headers, timeout = timeout,
                 max_connections = as.integer(max_connections)), class = "reference_source")
}

#' Print a reference source without authentication details
#' @param x A reference_source object.
#' @param ... Unused arguments.
#' @return x, invisibly.
#' @export
print.reference_source <- function(x, ...) {
  cat("Reference source: ", x$url, "\nCache: ", x$cache, "\n", sep = "")
  invisible(x)
}

#' Require one nonempty text value
#' @param value Value to check.
#' @param label Argument name used in errors.
#' @return NULL, invisibly.
#' @keywords internal
scalar_text <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
    stop(label, " must be one nonempty string.", call. = FALSE)
  invisible(NULL)
}

#' Resolve HTTP headers without persisting credentials
#' @param headers Named strings or a function returning them.
#' @return A named character vector, possibly empty.
#' @keywords internal
reference_headers <- function(headers) {
  if (is.function(headers)) headers <- headers()
  if (is.null(headers)) return(character())
  if (!is.character(headers) || anyNA(headers) || is.null(names(headers)) ||
      anyNA(names(headers)) || any(!nzchar(names(headers))) || anyDuplicated(names(headers)) ||
      any(grepl("[\r\n]", c(names(headers), headers))))
    stop("headers must contain named HTTP header strings.", call. = FALSE)
  headers
}

#' Resolve the source-specific cache directory
#' @param source Reference source configuration.
#' @return Source-specific directory path.
#' @keywords internal
reference_cache <- function(source) {
  if (!inherits(source, "reference_source")) stop("Expected a reference_source.", call. = FALSE)
  identity <- if (identical(source$catalogue, "registries"))
    paste0(source$url, "#registries") else source$url
  file.path(source$cache, digest::digest(identity, algo = "sha256", serialize = FALSE))
}

# Catalogue verification ---------------------------------------------------

#' Read the catalogue selected by a source
#' @param source Source configuration.
#' @param offline Use the verified cache only.
#' @return Catalogue rows with publication metadata.
#' @keywords internal
read_catalogue <- function(source, offline = FALSE) {
  if (!is.logical(offline) || length(offline) != 1L || is.na(offline))
    stop("offline must be TRUE or FALSE.", call. = FALSE)
  directory <- reference_cache(source)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  lock <- file.path(directory, ".catalogue-lock")
  if (!dir.create(lock, showWarnings = FALSE))
    stop("Reference cache is busy: ", directory, ".", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  metadata_name <- paste0(source$catalogue, ".yaml")
  csv_name <- paste0(source$catalogue, ".csv")
  pointer <- file.path(directory, metadata_name)
  if (offline) {
    if (!file.exists(pointer)) stop("No cached catalogue is available.", call. = FALSE)
    metadata <- read_catalogue_metadata(pointer)
  } else {
    staged <- tempfile("catalogue-", tmpdir = directory)
    on.exit(unlink(staged), add = TRUE)
    fetch_reference_file(source, metadata_name, staged)
    metadata <- read_catalogue_metadata(staged)
  }
  csv <- cached_reference_file(source, csv_name, metadata$sha256, directory, offline)
  catalogue <- read_reference_csv(csv)
  validate_reference_catalogue(catalogue)
  if (!offline) replace_cache_file(staged, pointer)
  attr(catalogue, "publication") <- metadata
  catalogue
}

#' Read catalogue publication metadata without evaluating YAML expressions
#' @param path Local YAML path.
#' @return A list containing published_at and sha256 strings.
#' @keywords internal
read_catalogue_metadata <- function(path) {
  metadata <- yaml::read_yaml(path, eval.expr = FALSE)
  if (!is.list(metadata) || !all(c("published_at", "sha256") %in% names(metadata)) ||
      anyDuplicated(names(metadata)))
    stop("Catalogue metadata must declare published_at and sha256.", call. = FALSE)
  scalar_text(metadata$published_at, "published_at")
  scalar_text(metadata$sha256, "sha256")
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", metadata$published_at))
    stop("Catalogue published_at must be a quoted UTC timestamp.", call. = FALSE)
  if (!grepl("^[a-f0-9]{64}$", metadata$sha256))
    stop("Catalogue sha256 must be a lowercase SHA-256 checksum.", call. = FALSE)
  metadata[c("published_at", "sha256")]
}

#' Validate the downloadable catalogue contract
#' @param catalogue Parsed CSV table.
#' @return NULL, invisibly; invalid catalogues raise an error.
#' @keywords internal
validate_reference_catalogue <- function(catalogue) {
  required <- c("id", "domain", "csv_path", "version", "updated_at", "published_at", "sha256")
  if (!all(required %in% names(catalogue)))
    stop("Catalogue is missing columns: ", paste(setdiff(required, names(catalogue)), collapse = ", "), call. = FALSE)
  if (anyNA(catalogue[required])) stop("Catalogue fields must be text; use empty strings for missing metadata.", call. = FALSE)
  if (any(!grepl("^[a-z][a-z0-9_.-]*$", catalogue$id)) || anyDuplicated(catalogue$id))
    stop("Catalogue IDs must be unique lowercase reference names.", call. = FALSE)
  if (any(!grepl("^[a-f0-9]{64}$", catalogue$sha256)))
    stop("Catalogue contains an invalid SHA-256 checksum.", call. = FALSE)
  valid_reference_paths(catalogue$csv_path)
  invisible(NULL)
}

#' Require relative resource paths confined to the source
#' @param paths Character vector of slash-separated paths.
#' @return NULL, invisibly.
#' @keywords internal
valid_reference_paths <- function(paths) {
  if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths)) ||
      any(grepl("^/|[\\\\:%?#[:cntrl:]]", paths)) ||
      any(vapply(strsplit(paths, "/", fixed = TRUE),
                 function(parts) any(parts %in% c("", ".", "..")), logical(1))))
    stop("Catalogue paths must be relative paths without traversal or URL parameters.", call. = FALSE)
  invisible(NULL)
}
