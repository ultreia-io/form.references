# Public operations ---------------------------------------------------------

#' Build a shared lookup over reference rows
#' @param data Reference table, character vector or list of text values. Rows are retained in order.
#' @param column Lookup column when data is a data frame; otherwise NULL.
#' @param matching "exact" or "name" for optional normalized-name fallback.
#' @param aliases Named character vector mapping aliases to lookup values.
#'   Chains are allowed; cycles are errors. Multiple targets retain all matching rows.
#' @param ignore_case Ignore case in keys, aliases and name fallback? Defaults to TRUE.
#' @return A reference_index retaining the original data, values and row indexes.
#' @details Lookup keys are plain text. Values are never converted to text implicitly.
#'   Reference matching supports text fields only.
#'   Exact matches take precedence over aliases, then name fallback.
#'   Duplicate rows remain separate matches. No catalogue-specific validity policy
#'   is applied: callers decide whether any match or exactly one match is required.
#'   Unknown alias targets remain unmatched. Submitted and reference data are unchanged.
#' @examples
#' # Prepare species.
#' reference_species_table <- data.frame(code = c("001", "002"), label = c("First", "Second"))
#' # Prepare reference index.
#' reference_index <- reference_index(reference_species_table, "code", aliases = c(old = "001"))
#' # Prepare reference rows.
#' reference_rows <- reference_match_rows(reference_index, c("old", "unknown"))
#' # Display the selected reference rows or values.
#' reference_index$data[reference_rows[[1]], , drop = FALSE]
#' # Display the number of matching rows for each submitted value.
#' lengths(reference_rows)
#' @export
reference_index <- function(data, column = NULL, matching = "exact",
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
  if (is.list(aliases)) {
    if (any(!vapply(aliases, reference_text, logical(1))))
      stop("Invalid reference aliases", call. = FALSE)
    aliases <- vapply(aliases, identity, character(1))
  }
  if (!is.character(aliases) || !is.null(dim(aliases)) ||
      (length(aliases) && (is.null(names(aliases)) || anyNA(names(aliases)) ||
      any(!nzchar(names(aliases))) || anyNA(aliases) || any(!nzchar(aliases)))))
    stop("Invalid reference aliases", call. = FALSE)
  exact <- split(seq_along(scalars), vapply(scalars, reference_key, character(1), ignore_case = ignore_case))
  normalized <- if (matching == "name") split(seq_along(scalars),
    normalized_name_key(vapply(scalars, identity, character(1)), ignore_case)) else list()
  pairs <- unique(data.frame(alias = names(aliases), target = unname(aliases)))
  if (ignore_case && nrow(pairs)) {
    pairs <- unique(data.frame(alias = tolower(pairs$alias), target = tolower(pairs$target)))
  }
  lookup_aliases <- if (nrow(pairs)) split(pairs$target, pairs$alias) else list()
  resolve <- function(value, seen = character()) {
    rows <- exact[[reference_key(value, ignore_case)]]
    if (length(rows)) return(rows)
    if (value %in% seen) stop("Cyclic reference alias: ", value, call. = FALSE)
    if (value %in% names(lookup_aliases)) return(sort(unique(as.integer(unlist(
      lapply(lookup_aliases[[value]], resolve, seen = c(seen, value)), use.names = FALSE)))))
    key <- if (matching == "name") normalized_name_key(value, ignore_case) else NA_character_
    if (!is.na(key) && length(normalized[[key]])) normalized[[key]] else integer()
  }
  alias_rows <- lapply(names(lookup_aliases), resolve)
  names(alias_rows) <- names(lookup_aliases)
  structure(list(data = data, column = column, values = values,
    matching = matching, ignore_case = ignore_case, aliases = aliases, exact = exact,
    normalized = normalized, alias_rows = alias_rows), class = "reference_index")
}

#' Find matching reference rows
#' @param index Lookup prepared with reference_index().
#' @param values Character vector or list of text values; NULL and NA indicate missing values.
#' @return One integer vector per input value, containing matching positions in
#'   index$data and index$values. Unknown or missing values return integer(0).
#'   Multiple positions are preserved; no arbitrary canonical row is selected.
#' @details Repeated values share a lookup. Use lengths() for match counts and
#'   the returned positions to retrieve canonical codes or complete entity rows.
#' @export
reference_match_rows <- function(index, values) {
  if (!inherits(index, "reference_index")) stop("Use reference_index() first", call. = FALSE)
  values <- lapply(seq_along(values), function(i) values[[i]])
  present <- !vapply(values, reference_null_value, logical(1))
  if (any(!vapply(values[present], reference_text, logical(1))))
    stop("Malformed submitted reference keys", call. = FALSE)
  result <- rep(list(integer()), length(values))
  values <- values[present]
  keys <- vapply(values, reference_key, character(1), ignore_case = index$ignore_case)
  first <- !duplicated(keys)
  distinct <- keys[first]
  submitted <- values[first]
  rows <- lapply(seq_along(distinct), function(i) {
    found <- index$exact[[distinct[[i]]]]
    if (length(found)) return(found)
    value <- submitted[[i]]
    if (index$ignore_case) value <- tolower(value)
    if (value %in% names(index$alias_rows)) return(index$alias_rows[[value]])
    if (index$matching == "name") {
      key <- normalized_name_key(value, index$ignore_case)
      if (!is.na(key) && length(index$normalized[[key]])) return(index$normalized[[key]])
    }
    integer()
  })
  result[present] <- rows[match(keys, distinct)]
  result
}

#' Build order-independent comparison keys for names
#' @param x Character vector of names.
#' @param ignore_case Ignore letter case? Defaults to TRUE.
#' @return Character keys; missing or empty normalized names return `NA`.
#' @details Removes commas, squishes whitespace, optionally uppercases and transliterates
#'   accents to ASCII, replaces apostrophes, hyphens, dots and parentheses with
#'   spaces, then sorts and concatenates words. This is not fuzzy matching or a
#'   cryptographic hash. Different names can share a key; retain duplicate rows.
#' @export
normalized_name_key <- function(x, ignore_case = TRUE) {
  if (!is.character(x)) stop("Names must be character", call. = FALSE)
  if (!is.logical(ignore_case) || length(ignore_case) != 1L || is.na(ignore_case))
    stop("ignore_case must be TRUE or FALSE", call. = FALSE)
  x <- trimws(gsub("[[:space:]]+", " ", gsub(",", "", x, fixed = TRUE)))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  if (ignore_case) x <- toupper(x)
  x <- gsub("[.'()\\-]", " ", x, perl = TRUE)
  vapply(strsplit(x, "[[:space:]]+"), function(words) {
    if (anyNA(words)) return(NA_character_)
    key <- paste0(sort(words[nzchar(words)]), collapse = "")
    if (nzchar(key)) key else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# Internal helpers ----------------------------------------------------------

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
