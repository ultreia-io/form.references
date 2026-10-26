# Verified downloads and cache --------------------------------------------

#' Retrieve selected tables from the configured catalogue
#' @param ids Exact catalogue IDs.
#' @param source Source configuration.
#' @param offline Use verified cache only.
#' @return Verified tables, paths, catalogue rows and publication metadata.
#' @keywords internal
get_catalogue_tables <- function(ids, source, offline = FALSE) {
  if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids)))
    stop("ids must contain nonempty reference names.", call. = FALSE)
  ids <- unique(ids)
  catalogue <- read_catalogue(source, offline = offline)
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

#' Resolve several reference CSVs, downloading missing ones concurrently
#' @param source Reference source configuration.
#' @param selected Catalogue rows in requested order.
#' @param directory Source-specific cache directory.
#' @param offline Whether source requests are forbidden.
#' @return Verified cached CSV paths in the same order as selected.
#' @keywords internal
cached_reference_files <- function(source, selected, directory, offline) {
  objects <- file.path(directory, "objects")
  dir.create(objects, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(objects, paste0(selected$sha256, ".csv"))
  missing <- which(!vapply(seq_along(paths), function(i)
    reference_hash_matches(paths[i], selected$sha256[i]), logical(1)))
  if (!length(missing)) return(paths)
  if (offline)
    stop("Verified cache unavailable for ", selected$csv_path[missing[[1L]]], ".", call. = FALSE)
  if (!source$remote || length(missing) == 1L) {
    for (i in missing)
      cached_reference_file(source, selected$csv_path[i], selected$sha256[i], directory, FALSE)
    return(paths)
  }
  staged_files <- character()
  on.exit(unlink(staged_files), add = TRUE)
  for (start in seq.int(1L, length(missing), by = source$max_connections)) {
    batch <- missing[seq.int(start, min(start + source$max_connections - 1L, length(missing)))]
    staged <- vapply(batch, function(i) tempfile("download-", tmpdir = objects), character(1))
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

#' Resolve a verified checksum-addressed cached CSV
#' @param source Reference source configuration.
#' @param path Relative source path.
#' @param checksum Expected SHA-256.
#' @param directory Source-specific cache directory.
#' @param offline Whether network and source reads are forbidden.
#' @return Verified local CSV path.
#' @keywords internal
cached_reference_file <- function(source, path, checksum, directory, offline) {
  objects <- file.path(directory, "objects")
  dir.create(objects, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(objects, paste0(checksum, ".csv"))
  if (reference_hash_matches(target, checksum)) return(target)
  if (offline) stop("Verified cache unavailable for ", path, ".", call. = FALSE)
  staged <- tempfile("download-", tmpdir = objects)
  on.exit(unlink(staged), add = TRUE)
  fetch_reference_file(source, path, staged)
  if (!reference_hash_matches(staged, checksum))
    stop("SHA-256 mismatch for ", path, "; cached files were not replaced.", call. = FALSE)
  replace_cache_file(staged, target)
  target
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
