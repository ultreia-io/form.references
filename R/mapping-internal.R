# Configuration validation ---------------------------------------------------

# Validate this dialect independently of reference-source configuration.
check_mapping_configuration <- function(configuration) {
  schema <- mapping_schema()
  check <- function(value, node, path) {
    fail <- function(message) stop(path, ": ", message, call. = FALSE)
    while (!is.null(node$ref)) node <- schema$definitions[[node$ref]]
    if (!is.null(node$one_of)) {
      outcomes <- lapply(node$one_of, function(alternative) {
        tryCatch({ check(value, alternative, path); NULL }, error = conditionMessage)
      })
      if (sum(vapply(outcomes, is.null, logical(1))) != 1L)
        fail(paste0("expected exactly one schema alternative: ",
          paste(unlist(outcomes), collapse = "; ")))
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
      exclusive <- unlist(node$mutually_exclusive)
      present <- vapply(exclusive, function(key) {
        selected <- value
        for (part in strsplit(key, ".", fixed = TRUE)[[1L]]) {
          if (!is.list(selected) || !part %in% names(selected)) return(FALSE)
          selected <- selected[[part]]
        }
        TRUE
      }, logical(1))
      if (sum(present) > 1L)
        fail(paste("mutually exclusive properties:", paste(exclusive[present], collapse = ", ")))
      return(invisible(NULL))
    }
    if (identical(node$type, "sequence")) {
      if (!is.list(value) || !plain || !is.null(names(value))) fail("expected an unnamed sequence")
      if (!is.null(node$min_items) && length(value) < node$min_items)
        fail(paste("requires at least", node$min_items, "items"))
      for (i in seq_along(value)) check(value[[i]], node$items, paste0(path, "[", i, "]"))
      return(invisible(NULL))
    }
    if (!is.character(value) || !plain || length(value) != 1L || is.na(value))
      fail("expected string")
    if (!is.null(node$min_length) && nchar(value) < node$min_length) fail("must not be empty")
    if (!is.null(node$enum) && !value %in% unlist(node$enum)) fail("unsupported value")
    invisible(NULL)
  }
  check(configuration, schema$definitions[[schema$root]], "$")
}

# Lookup keys and columns ----------------------------------------------------

mapping_catalogue_key <- function(specification) {
  mapping_key(unlist(specification[c("configuration", "catalogue")], use.names = FALSE))
}

mapping_lookup_key <- function(specification) {
  mapping_key(unlist(specification[c("configuration", "catalogue", "table", "column")], use.names = FALSE))
}

# Length prefixes avoid collisions when names contain separators.
mapping_key <- function(values) {
  paste0(nchar(values), ":", values, collapse = "|")
}

mapping_column <- function(data, column, table) {
  if (sum(names(data) == column) != 1L)
    stop("Missing mapping column: ", table, ".", column, ".", call. = FALSE)
  invisible(NULL)
}

# Correspondence files -------------------------------------------------------

read_mapping_correspondences <- function(path) {
  if (!file.exists(path) || dir.exists(path))
    stop("Mapping correspondence file does not exist: ", path, ".", call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines)) lines[1L] <- sub("^\ufeff", "", lines[1L])
  data <- withCallingHandlers(utils::read.csv(text = paste(lines, collapse = "\n"),
    colClasses = "character", check.names = FALSE, na.strings = NULL,
    comment.char = "", strip.white = FALSE, row.names = NULL, fill = FALSE),
    warning = function(warning) stop("Malformed correspondence CSV.", call. = FALSE))
  if (!identical(names(data), c("source_value", "target_value")) || anyNA(data) || any(!nzchar(as.matrix(data))))
    stop("Correspondences require source_value,target_value with nonempty text values.", call. = FALSE)
  data <- unique(data)
  list(data = data, index = reference_index(data, "source_value", ignore_case = FALSE),
    provenance = list(path = path, sha256 = digest::digest(file = path, algo = "sha256")))
}
