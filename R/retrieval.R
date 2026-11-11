# Public operations ---------------------------------------------------------

#' Retrieve selected tables from the configured catalogue
#' @param ids Exact catalogue IDs.
#' @param source Source configuration.
#' @param offline Use verified cache only.
#' @return Verified tables, paths, catalogue rows and publication metadata.
#' @details CSV values remain text, including leading zeros. The source selects
#'   the catalogue; the caller supplies its table IDs. No model interpretation occurs.
#' @examples
#' # Choose a temporary location for this example.
#' reference_cache_directory <- tempfile("references-")
#' # Configure the reference catalogue sources.
#' reference_source_configuration <- reference_source(
#'   system.file("examples", "survey", "catalogue", package = "form.references"),
#'   reference_cache_directory, catalogue = "code_lists")
#' # Retrieve the selected tables and verify their checksums.
#' reference_tables <- get_reference_tables(c("stations", "observers"), reference_source_configuration)
#' # Display the selected reference rows or values.
#' reference_tables$data$stations$code
#' # Remove the temporary files created by this example.
#' unlink(reference_cache_directory, recursive = TRUE)
#' @export
get_reference_tables <- function(ids, source, offline = FALSE) {
  if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids)))
    stop("ids must contain nonempty reference names.", call. = FALSE)
  ids <- unique(ids)
  catalogue <- read_reference_catalogue(source, offline = offline)
  missing <- setdiff(ids, catalogue$id)
  if (length(missing)) stop("References absent from catalogue: ", paste(missing, collapse = ", "), call. = FALSE)
  selected <- catalogue[match(ids, catalogue$id), , drop = FALSE]
  paths <- stats::setNames(character(length(ids)), ids)
  tables <- stats::setNames(vector("list", length(ids)), ids)
  directory <- reference_cache(source)
  paths[] <- cached_reference_files(source, selected, directory, offline)
  for (i in seq_along(ids)) {
    tables[[i]] <- read_reference_csv(paths[i])
  }
  list(data = tables, catalogue = selected, paths = paths,
       publication = attr(catalogue, "publication"))
}

#' Read the catalogue selected by a source
#' @param source Source configuration.
#' @param offline Use the verified cache only.
#' @return Catalogue rows with publication metadata.
#' @details Catalogue and CSV checksums are verified before cached files are replaced.
#' @examples
#' # Choose a temporary location for this example.
#' reference_cache_directory <- tempfile("references-")
#' # Configure the reference catalogue sources.
#' reference_source_configuration <- reference_source(
#'   system.file("examples", "survey", "catalogue", package = "form.references"),
#'   reference_cache_directory, catalogue = "code_lists")
#' # Read the catalogue and display its table IDs and versions.
#' read_reference_catalogue(reference_source_configuration)[, c("id", "version")]
#' # Remove the temporary files created by this example.
#' unlink(reference_cache_directory, recursive = TRUE)
#' @export
read_reference_catalogue <- function(source, offline = FALSE) {
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
  csv <- cached_reference_file(source, csv_name, metadata$sha256, directory, offline, id = source$catalogue)
  catalogue <- read_reference_csv(csv)
  validate_reference_catalogue(catalogue)
  if (!offline) replace_cache_file(staged, pointer)
  attr(catalogue, "publication") <- metadata
  catalogue
}

# Catalogue checks ----------------------------------------------------------

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

# Retrieval -----------------------------------------------------------------

#' Resolve several reference CSVs, downloading missing ones concurrently
#' @param source Reference source configuration.
#' @param selected Catalogue rows in requested order.
#' @param directory Source-specific cache directory.
#' @param offline Whether source requests are forbidden.
#' @return Verified cached CSV paths in the same order as selected.
#' @keywords internal
cached_reference_files <- function(source, selected, directory, offline) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- reference_cache_paths(directory, selected$id, selected$sha256)
  missing <- which(!vapply(seq_along(paths), function(i)
    reference_hash_matches(paths[i], selected$sha256[i]), logical(1)))
  if (!length(missing)) return(paths)
  if (offline)
    stop("Verified cache unavailable for ", selected$csv_path[missing[[1L]]], ".", call. = FALSE)
  if (!source$remote || length(missing) == 1L) {
    for (i in missing)
      cached_reference_file(source, selected$csv_path[i], selected$sha256[i], directory, FALSE, id = selected$id[i])
    return(paths)
  }
  staged_files <- character()
  on.exit(unlink(staged_files), add = TRUE)
  for (start in seq.int(1L, length(missing), by = source$max_connections)) {
    batch <- missing[seq.int(start, min(start + source$max_connections - 1L, length(missing)))]
    staged <- vapply(batch, function(i) tempfile("download-", tmpdir = directory), character(1))
    staged_files <- c(staged_files, staged)
    handles <- lapply(selected$csv_path[batch], function(path) reference_request_handle(source, path))
    result <- tryCatch(curl::multi_download(handles, destfiles = staged, progress = FALSE),
                       error = function(e) NULL)
    if (is.null(result)) stop("Could not download reference resources.", call. = FALSE)
    for (j in seq_along(batch)) {
      path <- selected$csv_path[batch[j]]
      if (!isTRUE(result$success[j]))
        stop("Could not download reference resource: ", path, call. = FALSE)
      if (result$status_code[j] != 200L)
        stop("HTTP ", result$status_code[j], " while downloading ", path, ".", call. = FALSE)
      if (!reference_hash_matches(staged[j], selected$sha256[batch[j]]))
        stop("SHA-256 mismatch for ", path, "; cached files were not replaced.", call. = FALSE)
    }
    for (j in seq_along(batch)) replace_cache_file(staged[j], paths[batch[j]])
  }
  paths
}

#' Resolve a verified cached CSV named by reference ID and checksum
#' @param source Reference source configuration.
#' @param path Relative source path.
#' @param checksum Expected SHA-256.
#' @param directory Source-specific cache directory.
#' @param offline Whether network and source reads are forbidden.
#' @param id Catalogue ID, or the catalogue name for its own CSV.
#' @return Verified local CSV path.
#' @keywords internal
cached_reference_file <- function(source, path, checksum, directory, offline, id) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  target <- reference_cache_paths(directory, id, checksum)
  if (reference_hash_matches(target, checksum)) return(target)
  if (offline) stop("Verified cache unavailable for ", path, ".", call. = FALSE)
  staged <- tempfile("download-", tmpdir = directory)
  on.exit(unlink(staged), add = TRUE)
  fetch_reference_file(source, path, staged)
  if (!reference_hash_matches(staged, checksum))
    stop("SHA-256 mismatch for ", path, "; cached files were not replaced.", call. = FALSE)
  replace_cache_file(staged, target)
  target
}

#' Download a source resource to a temporary destination
#' @param source Reference source configuration.
#' @param path Validated relative resource path.
#' @param destination Local file to write.
#' @return Destination path, invisibly.
#' @keywords internal
fetch_reference_file <- function(source, path, destination) {
  valid_reference_paths(path)
  if (!source$remote) {
    origin <- file.path(source$url, path)
    resolved <- normalizePath(origin, winslash = "/", mustWork = FALSE)
    if (!startsWith(resolved, paste0(sub("/+$", "", source$url), "/")) ||
        !file.exists(origin) || dir.exists(origin) || !file.copy(origin, destination, overwrite = TRUE))
      stop("Cannot read reference resource: ", path, call. = FALSE)
    return(invisible(destination))
  }
  handle <- reference_request_handle(source, path)
  result <- tryCatch(curl::curl_fetch_disk(curl::handle_data(handle)$url, destination, handle),
                     error = function(e) NULL)
  if (is.null(result)) stop("Could not download reference resource: ", path, call. = FALSE)
  if (result$status_code != 200L)
    stop("HTTP ", result$status_code, " while downloading ", path, ".", call. = FALSE)
  invisible(destination)
}

#' Configure one restricted HTTP reference request
#' @param source Reference source configuration.
#' @param path Validated relative resource path.
#' @return A curl handle with the URL and request policy applied.
#' @keywords internal
reference_request_handle <- function(source, path) {
  valid_reference_paths(path)
  headers <- reference_headers(source$headers)
  encoded <- paste(vapply(strsplit(path, "/", fixed = TRUE)[[1]],
                          curl::curl_escape, character(1)), collapse = "/")
  handle <- curl::new_handle(url = paste0(source$url, encoded),
                             timeout = source$timeout, connecttimeout = min(source$timeout, 15),
                             followlocation = !length(headers), maxredirs = 5,
                             protocols = 3L, redir_protocols = 3L)
  if (length(headers)) curl::handle_setheaders(handle, .list = as.list(headers))
  handle
}

# File and cache helpers ----------------------------------------------------

#' Read a reference CSV without value conversion
#' @param path CSV path.
#' @return A data.frame with character columns and original column names.
#' @keywords internal
read_reference_csv <- function(path) {
  result <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                            na.strings = NULL, fileEncoding = "UTF-8-BOM",
                            comment.char = "", strip.white = FALSE)
  if (anyDuplicated(names(result)) || any(!nzchar(names(result))))
    stop("Reference CSV has empty or duplicate column names.", call. = FALSE)
  result
}

#' Resolve the source-specific cache directory
#' @param source Reference source configuration.
#' @return Source-specific directory path.
#' @keywords internal
reference_cache <- function(source) {
  if (!inherits(source, "reference_source")) stop("Expected a reference_source.", call. = FALSE)
  label <- sub("^https?://", "", source$url)
  if (!source$remote) label <- basename(source$url)
  label <- sub("-+$", "", gsub("[^A-Za-z0-9._-]+", "-", label))
  label <- substr(label, 1L, 80L)
  key <- substr(digest::digest(source$url, algo = "sha256", serialize = FALSE), 1L, 12L)
  file.path(source$cache, paste0(label, "--", key), source$catalogue)
}

#' Name cached CSVs by catalogue ID and content revision
#' @param directory Source-specific cache directory.
#' @param ids Validated catalogue IDs or catalogue name.
#' @param checksums Expected SHA-256 checksums.
#' @return Readable cache paths. Full checksums still verify every file.
#' @keywords internal
reference_cache_paths <- function(directory, ids, checksums) {
  file.path(directory, paste0(ids, "--", substr(checksums, 1L, 12L), ".csv"))
}

#' Compare a file's bytes with an expected checksum
#' @param path Local path.
#' @param checksum Expected SHA-256.
#' @return TRUE only for an existing file with the expected checksum.
#' @keywords internal
reference_hash_matches <- function(path, checksum) {
  file.exists(path) && !dir.exists(path) &&
    identical(digest::digest(file = path, algo = "sha256"), checksum)
}

#' Replace a cache file while retaining the previous file on failure
#' @param staged Complete temporary file on the same filesystem.
#' @param target Final path.
#' @return Target path, invisibly.
#' @keywords internal
replace_cache_file <- function(staged, target) {
  # POSIX rename replaces atomically. Windows requires moving the old file first.
  if (suppressWarnings(file.rename(staged, target))) return(invisible(target))
  backup <- tempfile("previous-", tmpdir = dirname(target))
  if (file.exists(target) && !file.rename(target, backup))
    stop("Cannot preserve the previous cache file.", call. = FALSE)
  if (!file.rename(staged, target)) {
    if (file.exists(backup)) file.rename(backup, target)
    stop("Cannot install the downloaded cache file.", call. = FALSE)
  }
  unlink(backup)
  invisible(target)
}
