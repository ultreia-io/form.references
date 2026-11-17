# Configuration and credentials ----------------------------------------------

#' Internal environment-variable reference objects
#'
#' The internal S3 class `reference_env` marks an environment-variable name parsed from a YAML `!env` tag.
#'
#' @section Construction and structure:
#' The YAML handler inside [read_reference_configuration()] creates this temporary object.
#'
#' It contains the tagged variable name, represented as a character scalar, with class `reference_env`.
#'
#' For example, `password: !env REGISTRY_PASSWORD` marks the name `REGISTRY_PASSWORD`, not its secret value.
#'
#' @section Lifecycle:
#' - Configuration validation checks the variable name and whether `!env` is allowed at that location.
#' - Source configuration resolves the variable before constructing a [reference_source-class] object.
#' - Missing values raise an error, unless the configured password prompt supplies a value.
#' - The marker is consumed during reading and is not retained in the returned reference configuration.
#'
#' This class has no public constructor or class-specific S3 methods.
#'
#' Configure environment references through YAML rather than constructing this internal marker directly.
#'
#' @seealso [read_reference_configuration()], [reference_configuration-class]
#' @name reference_env-class
#' @keywords internal
NULL

#' Check a configuration against its packaged YAML schema
#' @param configuration Parsed YAML configuration.
#' @return NULL, invisibly; invalid configurations raise path-specific errors.
#' @details Uses the package's form.references/schema/1 dialect, not JSON Schema.
#'   Diagnostics never include configuration values or resolved credentials.
#' @keywords internal
check_reference_configuration <- function(configuration) {
  schema <- reference_schema()
  root <- schema$root
  check <- function(value, node, path) {
    fail <- function(message) stop(path, ": ", message, call. = FALSE)
    while (!is.null(node$ref)) node <- schema$definitions[[node$ref]]
    if (inherits(value, "reference_env") && isTRUE(node$environment)) {
      variable <- unclass(value)
      if (!is.character(variable) || length(variable) != 1L || is.na(variable) ||
          !grepl("^[A-Za-z_][A-Za-z0-9_]*$", variable))
        fail("!env must name a valid environment variable")
      return(invisible(NULL))
    }
    plain <- all(names(attributes(value)) %in% "names")
    if (identical(node$type, "mapping")) {
      if (!is.list(value) || !plain || (length(value) && is.null(names(value))) ||
          anyNA(names(value)) || any(!nzchar(names(value))) || anyDuplicated(names(value)))
        fail("expected a mapping with unique nonempty names")
      if (!is.null(node$min_items) && length(value) < node$min_items)
        fail(paste("requires at least", node$min_items, "items"))
      if (!is.null(node$values)) {
        for (name in names(value)) check(value[[name]], node$values, paste0(path, ".", name))
      } else {
        unknown <- setdiff(names(value), names(node$properties))
        if (length(unknown)) fail(paste("unsupported property", unknown[[1L]]))
        missing <- setdiff(unlist(node$required), names(value))
        if (length(missing)) fail(paste("missing required property", missing[[1L]]))
        for (name in names(value)) check(value[[name]], node$properties[[name]], paste0(path, ".", name))
      }
      return(invisible(NULL))
    }
    valid <- is.atomic(value) && plain && length(value) == 1L && !is.na(value)
    valid <- valid && switch(node$type,
      string = is.character(value),
      boolean = is.logical(value),
      number = is.numeric(value) && is.finite(value),
      integer = is.numeric(value) && is.finite(value) && value == floor(value),
      FALSE)
    if (!valid) fail(paste("expected", node$type))
    if (!is.null(node$enum) && !value %in% unlist(node$enum)) fail("unsupported value")
    if (!is.null(node$min_length) && nchar(value) < node$min_length) fail("must not be empty")
    if (!is.null(node$pattern) && !grepl(node$pattern, value)) fail("does not match the required pattern")
    if (!is.null(node$minimum) && value < node$minimum) fail(paste("must be >=", node$minimum))
    if (!is.null(node$exclusive_minimum) && value <= node$exclusive_minimum)
      fail(paste("must be >", node$exclusive_minimum))
    invisible(NULL)
  }
  check(configuration, schema$definitions[[root]], "$")
}

reference_source_configuration <- function(config, prompt, directory) {
  for (name in names(config)) {
    if (inherits(config[[name]], "reference_env"))
      config[[name]] <- reference_environment_value(
        unclass(config[[name]]), prompt = prompt && name == "password")
  }
  if (!is.null(directory)) {
    for (name in intersect(c("url", "cache"), names(config))) {
      scalar_text(config[[name]], name)
      if (name == "url" && grepl("^https?://", config[[name]])) next
      path <- path.expand(config[[name]])
      if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) path <- file.path(directory, path)
      config[[name]] <- path
    }
  }
  if (any(c("user", "password") %in% names(config))) {
    scalar_text(config$user, "user")
    scalar_text(config$password, "password")
    scalar_text(config$url, "url")
    if (!startsWith(config$url, "https://"))
      stop("Basic authentication requires an HTTPS source URL.", call. = FALSE)
    if (grepl(":", config$user, fixed = TRUE))
      stop("Basic authentication user must not contain a colon.", call. = FALSE)
    config$headers <- c(Authorization = paste("Basic",
      base64enc::base64encode(charToRaw(enc2utf8(paste0(config$user, ":", config$password))))))
    config$user <- config$password <- NULL
  }
  do.call(reference_source, config)
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

#' Resolve a YAML environment-variable reference
#' @param variable Environment-variable name supplied through the YAML !env tag.
#' @param prompt Whether to request a missing secret using askpass.
#' @return The nonempty environment-variable value or prompted secret.
#' @keywords internal
reference_environment_value <- function(variable, prompt = FALSE) {
  scalar_text(variable, "!env variable")
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", variable))
    stop("!env must name a valid environment variable.", call. = FALSE)
  value <- Sys.getenv(variable, unset = "")
  if (!nzchar(value) && prompt) {
    if (!requireNamespace("askpass", quietly = TRUE))
      stop("Install askpass to use the secure password prompt.", call. = FALSE)
    value <- askpass::askpass(paste("Password for", variable))
    if (is.null(value)) stop("Password entry cancelled.", call. = FALSE)
    if (length(value) != 1L || is.na(value) || !nzchar(value))
      stop("Password must not be empty.", call. = FALSE)
  }
  if (!nzchar(value))
    stop("Environment variable ", variable, " is missing or empty.", call. = FALSE)
  value
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

# Catalogue validation and retrieval -----------------------------------------

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
  if (any(catalogue$id != "_aliases_" & !grepl("^[a-z][a-z0-9_.-]*$", catalogue$id)) || anyDuplicated(catalogue$id))
    stop("Catalogue IDs must be unique lowercase reference names.", call. = FALSE)
  if (any(!grepl("^[a-f0-9]{64}$", catalogue$sha256)))
    stop("Catalogue contains an invalid SHA-256 checksum.", call. = FALSE)
  if (any(catalogue$id == "_aliases_" & catalogue$domain != "catalogue"))
    stop("The aliases table must use domain catalogue.", call. = FALSE)
  valid_reference_paths(catalogue$csv_path)
  invisible(NULL)
}

#' Check catalogue-owned alias rows
#' @param aliases Downloaded aliases table.
#' @param catalogue Complete catalogue inventory.
#' @param tables Retrieved reference tables.
#' @return Alias mappings grouped by retrieved table and column.
#' @keywords internal
validate_reference_aliases <- function(aliases, catalogue, tables) {
  required <- c("table", "column", "alias", "canonical")
  if (!identical(names(aliases), required) || anyNA(aliases) ||
      any(!nzchar(as.matrix(aliases))) ||
      anyDuplicated(aliases[c("table", "column", "alias")]))
    stop("Aliases require table,column,alias,canonical and unique nonempty mappings.", call. = FALSE)
  if (any(!aliases$table %in% setdiff(catalogue$id, "_aliases_")))
    stop("Aliases refer to a table absent from the catalogue.", call. = FALSE)
  selected <- aliases[aliases$table %in% names(tables), , drop = FALSE]
  grouped <- split(selected, selected$table)
  for (table in names(grouped)) {
    grouped[[table]] <- split(grouped[[table]], grouped[[table]]$column)
    for (column in names(grouped[[table]])) {
      if (!column %in% names(tables[[table]]))
        stop("Alias lookup column is absent from ", table, ".", column, ".", call. = FALSE)
      grouped[[table]][[column]] <- reference_table_aliases(
        grouped[[table]][[column]], table, column, tables[[table]][[column]])
    }
  }
  grouped
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

# Cache management -----------------------------------------------------------

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
  file.path(source$cache, paste0(label, "--", key))
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

# Lookup indexes and values --------------------------------------------------

# Build an index after catalogue aliases have been selected internally.
build_reference_index <- function(data, column = NULL, matching = "exact",
                                  aliases = character(), ignore_case = TRUE) {
  if (!is.logical(ignore_case) || length(ignore_case) != 1L || is.na(ignore_case))
    stop("ignore_case must be TRUE or FALSE", call. = FALSE)
  matching <- match.arg(matching, c("exact", "name"))
  if (is.data.frame(data)) {
    if (!is.character(column) || length(column) != 1L || is.na(column) ||
        sum(names(data) == column) != 1L) stop("Select one existing lookup column", call. = FALSE)
    values <- data[[column]]
  } else {
    if (!is.null(column)) stop("column requires a data frame", call. = FALSE)
    values <- data
  }
  if (!(is.atomic(values) || is.list(values)) || is.data.frame(values) || !is.null(dim(values)))
    stop("Reference values must be a character vector or list of text values", call. = FALSE)
  scalars <- lapply(seq_along(values), function(i) values[[i]])
  if (any(!vapply(scalars, reference_text, logical(1))))
    stop("Malformed reference keys", call. = FALSE)
  exact <- split(seq_along(scalars), vapply(scalars, reference_key, character(1), ignore_case = ignore_case))
  normalized <- if (matching == "name") split(seq_along(scalars),
    normalized_name_key(vapply(scalars, identity, character(1)), ignore_case)) else list()
  alias_rows <- exact[reference_key(unname(aliases), ignore_case)]
  names(alias_rows) <- if (ignore_case) tolower(names(aliases)) else names(aliases)
  structure(list(data = data, column = column, values = values,
    matching = matching, ignore_case = ignore_case, aliases = aliases, exact = exact,
    normalized = normalized, alias_rows = alias_rows), class = "reference_index")
}

#' Select catalogue aliases for one lookup
#' @param aliases Alias rows already selected for one table and column, or NULL.
#' @param table Reference table identifier.
#' @param column Lookup column.
#' @param values Canonical values from the reference table.
#' @param ignore_case Whether comparison ignores case.
#' @return A named character vector mapping aliases to canonical values.
#' @keywords internal
reference_table_aliases <- function(aliases, table, column, values, ignore_case = FALSE) {
  if (is.null(aliases)) return(character())
  selected <- aliases
  if (!nrow(selected)) return(character())
  if (any(!selected$canonical %in% values))
    stop("Alias targets are absent from ", table, ".", column, ".", call. = FALSE)
  alias_keys <- reference_key(selected$alias, ignore_case)
  target_keys <- reference_key(selected$canonical, ignore_case)
  value_keys <- reference_key(values, ignore_case)
  if (any(alias_keys %in% value_keys))
    stop("Aliases conflict with canonical values in ", table, ".", column, ".", call. = FALSE)
  if (any(vapply(split(target_keys, alias_keys),
      function(targets) length(unique(targets)) > 1L, logical(1))))
    stop("Conflicting alias targets in ", table, ".", column, ".", call. = FALSE)
  stats::setNames(selected$canonical, selected$alias)
}

#' Encode a text comparison key
#' @param x One nonmissing text value.
#' @param ignore_case Ignore letter case? Defaults to FALSE.
#' @return A UTF-8 comparison key, prefixed so empty text has a nonempty key.
#' @keywords internal
reference_key <- function(x, ignore_case = FALSE) {
  if (ignore_case) x <- tolower(x)
  paste0(":", enc2utf8(x))
}

#' Check whether a value is plain text
#' @param x One lookup value.
#' @return TRUE for one unclassed, nonmissing character value.
#' @keywords internal
reference_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    all(names(attributes(x)) %in% "names")
}

#' Identify missing lookup values
#' @param x One lookup value.
#' @return TRUE for NULL, NA or NA_character_.
#' @keywords internal
reference_null_value <- function(x) {
  is.null(x) || identical(x, NA) || identical(x, NA_character_)
}
