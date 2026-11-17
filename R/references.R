# Configuration and sources --------------------------------------------------

#' Reference configuration objects
#'
#' An S3 `reference_configuration` is a named list describing reference catalogues and their lookup policies.
#'
#' @section Construction:
#' Use [read_reference_configuration()] to read and validate a reference YAML file.
#'
#' Relative paths and environment variables are resolved before the object is returned.
#'
#' No catalogue data is downloaded at this stage.
#'
#' @section Structure:
#' - `schema_version`: the reference-configuration format version.
#' - `catalogues`: a named list of catalogue definitions.
#' - Each catalogue contains a `source` object of class [reference_source-class] and a `tables` list.
#' - `tables` maps table IDs to named lookup columns and their `matching` and `ignore_case` settings.
#' - A catalogue may also retain its optional `label`.
#'
#' @section Use:
#' Pass the object to [resolve_reference_lookups()] or [prepare_model_references()] for form-model references.
#'
#' Named lists of these objects supply the catalogues used by [prepare_mapping_references()].
#'
#' Sources can contain authentication headers in memory; do not serialize a configuration containing credentials.
#'
#' There are no class-specific S3 methods.
#'
#' @seealso [read_reference_configuration()], [reference_source-class], [mapping_configuration-class]
#' @name reference_configuration-class
NULL

#' Reference source objects
#'
#' An S3 `reference_source` is a list describing how to retrieve and cache one reference catalogue.
#'
#' @section Construction:
#' Use [reference_source()] directly, or read a source declaration with [read_reference_configuration()].
#'
#' Construction validates the settings but does not download files or create the cache directory.
#'
#' @section Structure:
#' - `url`: a normalized local directory or HTTP(S) base URL.
#' - `cache`: the configured cache root; retrieval uses source-specific subdirectories beneath it.
#' - `remote`: whether the source uses HTTP(S).
#' - `headers`: `NULL`, named HTTP headers, or a function supplying headers for each request.
#' - `timeout`: the positive request timeout in seconds.
#' - `max_connections`: the positive integer limit for simultaneous CSV downloads.
#'
#' @section Cache lifetime:
#' The default cache is a new path in the current R session's temporary directory.
#'
#' An explicitly configured cache path can be reused across sessions.
#'
#' Authentication headers remain in memory and are not written to the cache.
#'
#' @section Use and methods:
#' Pass the object to [read_reference_catalogue()] or [get_reference_tables()].
#'
#' The registered `print.reference_source()` method displays the source URL and cache path without printing headers.
#'
#' This is the only package-defined S3 method for this class.
#'
#' @seealso [reference_source()], [print.reference_source()], [reference_configuration-class]
#' @name reference_source-class
NULL

#' Read the reference-configuration schema
#' @param version Schema version; currently `"1.0.0"`.
#' @return The schema as a nested list. Unsupported versions raise an error.
#' @details Uses the package's `form.references/schema/1` dialect, not JSON Schema.
#' @examples
#' # Inspect the supported schema contract version.
#' reference_schema()$contract_version
#' @export
reference_schema <- function(version = "1.0.0") {
  scalar_text(version, "version")
  if (!identical(version, "1.0.0"))
    stop("Unsupported schema version", call. = FALSE)
  yaml::read_yaml(system.file("schema", "references-1.0.0.yaml",
    package = "form.references", mustWork = TRUE), eval.expr = FALSE)
}

#' Read catalogue sources and lookup definitions
#' @param path YAML configuration path.
#' @param prompt Securely prompt for missing passwords; defaults to FALSE.
#' @return A [reference_configuration-class] object containing named catalogues,
#'   each with a [reference_source-class] object and table lookup definitions. No data is downloaded.
#' @details The packaged schema/references-1.0.0.yaml schema checks the required
#'   schema_version. Each catalogue declares source and tables;
#'   each source directory contains catalogue.yaml and catalogue.csv;
#'   lookup names are column names. Relative source and cache paths resolve from
#'   the YAML directory. The schema uses form.references/schema/1, not JSON Schema.
#'
#'   Sources accept url, cache, timeout and max_connections, plus user and password
#'   for HTTPS Basic authentication. If cache is omitted, each source uses a
#'   temporary directory for the current R session. Set cache explicitly to
#'   retain downloaded files across sessions. Use password: !env VARIABLE to read a secret
#'   from the environment. With prompt = TRUE, missing passwords are requested
#'   through askpass. Credentials remain in memory; do not serialize the result.
#' @examples
#' # Read the catalogue sources and lookup settings.
#' reference_configuration <- read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Inspect the configured catalogue names.
#' names(reference_configuration$catalogues)
#' @export
read_reference_configuration <- function(path, prompt = FALSE) {
  scalar_text(path, "path")
  if (!is.logical(prompt) || length(prompt) != 1L || is.na(prompt))
    stop("prompt must be TRUE or FALSE.", call. = FALSE)
  configuration <- yaml::read_yaml(path, eval.expr = FALSE,
    handlers = list(seq = as.list,
      env = function(variable) structure(variable, class = "reference_env")))
  check_reference_configuration(configuration)
  directory <- dirname(normalizePath(path, winslash = "/", mustWork = TRUE))
  for (name in names(configuration$catalogues)) {
    configuration$catalogues[[name]]$source <- reference_source_configuration(
      configuration$catalogues[[name]]$source, prompt, directory)
  }
  class(configuration) <- "reference_configuration"
  configuration
}

#' Configure a reference source
#'
#' @param url HTTP(S) base URL or local directory containing the selected
#'   catalogue.yaml and catalogue.csv files. URLs must not contain credentials, query strings or fragments.
#' @param cache Local cache directory. Defaults to a new temporary directory
#'   for this source in the current R session. Supply an explicit path to retain
#'   files across sessions. Different sources receive separate
#'   subdirectories. Use separate caches for separate authenticated identities.
#' @param headers Named HTTP header values, or a function returning them at
#'   request time. NULL sends no authentication headers.
#' @param timeout Positive request timeout in seconds.
#' @param max_connections Maximum simultaneous CSV downloads. The default is four.
#' @return A [reference_source-class] object. No files are downloaded.
#' @details Credentials stay in memory and are never written into the cache.
#'   Authenticated requests do not follow redirects; supply the final base URL.
#' @export
#' @examples
#' # Configure the reference catalogue sources.
#' reference_source_configuration <- reference_source(
#'   system.file("examples", "survey", "catalogue", "code_lists", package = "form.references"),
#'                            cache = tempfile("references-"))
#' # Configure the reference catalogue sources.
#' reference_source_configuration
reference_source <- function(url, cache = tempfile("form.references-"), headers = NULL, timeout = 60,
                             max_connections = 4L) {
  scalar_text(url, "url")
  scalar_text(cache, "cache")
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

# Catalogue retrieval --------------------------------------------------------

#' Retrieve selected tables from the configured catalogue
#' @param ids Exact catalogue IDs.
#' @param source Source configuration.
#' @param offline Use verified cache only.
#' @return Verified tables, paths, catalogue rows and publication metadata.
#' @details CSV values remain text, including leading zeros. The source selects
#'   the catalogue; the caller supplies its table IDs. No model interpretation occurs.
#'   When present, the reserved _aliases_ table (domain catalogue) is retrieved too,
#'   through the same source and verified cache. Its columns are table, column,
#'   alias and canonical. Alias targets are checked for the retrieved tables.
#'   Aliases are grouped once by table and column and attached to the retrieved
#'   tables. reference_index() applies them automatically for the selected column.
#'   The reserved alias table is not returned in data and cannot be requested directly.
#' @examples
#' # Choose a temporary location for this example.
#' reference_cache_directory <- tempfile("references-")
#' # Configure the reference catalogue sources.
#' reference_source_configuration <- reference_source(
#'   system.file("examples", "survey", "catalogue", "code_lists", package = "form.references"),
#'   reference_cache_directory)
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
  if ("_aliases_" %in% ids)
    stop("_aliases_ is reserved; request ordinary reference tables.", call. = FALSE)
  ids <- unique(ids)
  catalogue <- read_reference_catalogue(source, offline = offline)
  missing <- setdiff(ids, catalogue$id)
  if (length(missing)) stop("References absent from catalogue: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(ids) && "_aliases_" %in% catalogue$id) ids <- unique(c(ids, "_aliases_"))
  selected <- catalogue[match(ids, catalogue$id), , drop = FALSE]
  paths <- stats::setNames(character(length(ids)), ids)
  tables <- stats::setNames(vector("list", length(ids)), ids)
  directory <- reference_cache(source)
  paths[] <- cached_reference_files(source, selected, directory, offline)
  for (i in seq_along(ids)) {
    tables[[i]] <- read_reference_csv(paths[i])
  }
  if ("_aliases_" %in% names(tables)) {
    grouped_aliases <- validate_reference_aliases(tables[["_aliases_"]], catalogue, tables)
    for (table in names(grouped_aliases))
      attr(tables[[table]], "reference_aliases") <- grouped_aliases[[table]]
  }
  tables[["_aliases_"]] <- NULL
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
#'   system.file("examples", "survey", "catalogue", "code_lists", package = "form.references"),
#'   reference_cache_directory)
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
  metadata_name <- "catalogue.yaml"
  csv_name <- "catalogue.csv"
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
  csv <- cached_reference_file(source, csv_name, metadata$sha256, directory, offline, id = "catalogue")
  catalogue <- read_reference_csv(csv)
  validate_reference_catalogue(catalogue)
  if (!offline) replace_cache_file(staged, pointer)
  attr(catalogue, "publication") <- metadata
  catalogue
}

# Lookup matching ------------------------------------------------------------

#' Reference lookup index objects
#'
#' An S3 `reference_index` is a list retaining reference values, their rows and reusable matching indexes.
#'
#' @section Construction:
#' Use [reference_index()] to index a reference table, character vector or list of text values.
#'
#' [prepare_model_references()] and [prepare_mapping_references()] also create these objects internally.
#'
#' Catalogue aliases attached to the selected lookup column are incorporated automatically.
#'
#' @section Structure:
#' - `data`: the input data retained for this index, in row order.
#' - `column`: the lookup column for a data frame, or `NULL` for a vector or list.
#' - `values`: the reference values used for matching.
#' - `matching` and `ignore_case`: the selected comparison policy.
#' - `aliases`: a named character vector from accepted aliases to canonical values.
#' - `exact`, `normalized` and `alias_rows`: internal lookup structures containing row positions.
#'
#' Preparation may add a `lookup` specification.
#'
#' Model-based preparation also adds `version`, containing the reference table's SHA-256 checksum.
#'
#' @section Use:
#' Pass the object to [reference_match_rows()] to obtain matching row indexes for each supplied value.
#'
#' Those indexes address this object's `data`; they are not necessarily row numbers in the original CSV.
#'
#' Exact matches take precedence over aliases, followed by configured normalized-name matching.
#'
#' Duplicate matches remain separate candidates. Rebuild the index after changing its data or lookup policy.
#'
#' There are no class-specific S3 methods; use the lookup function rather than editing the internal indexes.
#'
#' @seealso [reference_index()], [reference_match_rows()], [mapping_references-class]
#' @name reference_index-class
NULL

#' Build a shared lookup over reference rows
#' @param data Reference table, character vector or list of text values. Rows are retained in order.
#' @param column Lookup column when data is a data frame; otherwise NULL.
#' @param matching "exact" or "name" for optional normalized-name fallback.
#' @param ignore_case Ignore case in keys, aliases and name fallback? Defaults to TRUE.
#' @return A [reference_index-class] object retaining the original data, values and row indexes.
#' @details Lookup keys are plain text. Values are never converted to text implicitly.
#'   Reference matching supports text fields only. Catalogue aliases attached to
#'   the selected column are applied automatically.
#'   Inspect index$aliases for that lookup's alias-to-canonical mappings.
#'   Exact matches take precedence over aliases, then name fallback.
#'   Duplicate rows remain separate matches. No catalogue-specific validity policy
#'   is applied: callers decide whether any match or exactly one match is required.
#'   Submitted and reference values are unchanged.
#' @examples
#' # Prepare species.
#' reference_species_table <- data.frame(code = c("001", "002"), label = c("First", "Second"))
#' # Prepare reference index.
#' reference_index <- reference_index(reference_species_table, "code")
#' # Prepare reference rows.
#' reference_rows <- reference_match_rows(reference_index, c("001", "unknown"))
#' # Display the selected reference rows or values.
#' reference_index$data[reference_rows[[1]], , drop = FALSE]
#' # Display the number of matching rows for each submitted value.
#' lengths(reference_rows)
#' @export
reference_index <- function(data, column = NULL, matching = "exact",
                            ignore_case = TRUE) {
  aliases <- if (is.character(column) && length(column) == 1L && !is.na(column))
    attr(data, "reference_aliases", exact = TRUE)[[column]] else NULL
  if (is.null(aliases)) aliases <- character()
  if (length(aliases)) {
    reference_table_aliases(data.frame(alias = names(aliases), canonical = unname(aliases)),
      "reference", column, data[[column]], ignore_case)
  }
  if (!is.null(attr(data, "reference_aliases", exact = TRUE)))
    attr(data, "reference_aliases") <- if (length(aliases)) stats::setNames(list(aliases), column) else NULL
  build_reference_index(data, column, matching, aliases, ignore_case)
}

#' Find matching reference rows
#' @param index Lookup from reference_index() or prepare_model_references().
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

# Form-model integration -----------------------------------------------------

#' Resolve a model's named reference lookups
#' @param model A form model with named reference bindings.
#' @param configuration Catalogue definitions from read_reference_configuration().
#'   May be NULL only when the model is already resolved.
#' @return A checked form model with explicit lookup settings and a
#'   reference_lookups attribute freezing the resolved definitions.
#' @details Already resolved models retain their frozen definitions. Supplying a
#'   new configuration for such a model is rejected; resolve the original model instead.
#'   No catalogue rows are loaded. Unknown table or lookup names fail immediately.
#' @examples
#' # Load the requested form model.
#' form_model_definition <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
#' # Read the named reference lookup settings.
#' reference_configuration <- read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Resolve reference lookups without changing the original form model.
#' reference_resolved_form_model <- resolve_reference_lookups(
#'   form_model_definition, reference_configuration)
#' # Display the returned values.
#' names(attr(reference_resolved_form_model, "reference_lookups"))
#' @export
resolve_reference_lookups <- function(model, configuration = NULL) {
  frozen <- attr(model, "reference_lookups", exact = TRUE)
  if (!is.null(frozen)) {
    if (!is.null(configuration)) stop("Resolve the original model to change lookup configuration", call. = FALSE)
    return(model)
  }
  if (is.null(configuration)) stop("Supply a lookup configuration for unresolved bindings", call. = FALSE)
  if (!inherits(configuration, "reference_configuration"))
    stop("Expected a reference configuration from read_reference_configuration().", call. = FALSE)
  definition <- form.model::form_definition(model)
  resolved <- list()
  for (sheet in names(definition$sheets)) {
    for (field in names(definition$sheets[[sheet]]$fields)) {
      binding <- definition$sheets[[sheet]]$fields[[field]]$reference
      if (is.null(binding)) next
      if (identical(binding$table, "_aliases_"))
        stop("_aliases_ is reserved for catalogue aliases, not form-model bindings.", call. = FALSE)
      group <- binding$catalogue
      lookup <- configuration$catalogues[[group]]$tables[[binding$table]][[binding$lookup]]
      if (is.null(lookup))
        stop("Unknown reference lookup: ", group, "/", binding$table, "/", binding$lookup, call. = FALSE)
      if (identical(lookup$matching, "name") &&
          definition$sheets[[sheet]]$fields[[field]]$type != "string")
        stop("Name matching requires a string field: ", sheet, ".", field, call. = FALSE)
      key <- paste(group, binding$table, binding$lookup, sep = "/")
      spec <- c(list(catalogue = group, table = binding$table, name = binding$lookup,
                     column = binding$lookup,
                     catalogue_label = if (is.null(configuration$catalogues[[group]]$label)) group else
                       configuration$catalogues[[group]]$label), lookup)
      if (!is.null(resolved[[key]]) && !identical(resolved[[key]], spec))
        stop("Conflicting lookup definitions for snapshot: ", key, call. = FALSE)
      resolved[[key]] <- spec
    }
  }
  model <- form.model::as_form_model(definition)
  attr(model, "reference_lookups") <- resolved
  model
}

#' Prepare reference values selected by a model
#' @param model An unresolved form model with named lookup bindings.
#' @param configuration Combined configuration from read_reference_configuration().
#' @param offline Use verified cached files only; defaults to FALSE.
#' @return A list containing model (the resolved model) and references (named
#'   indexes with values, complete rows, lookup settings and provenance).
#'   The supplied model is unchanged.
#' @details Table IDs and lookup columns come exclusively from the model's resolved
#'   bindings. Each required table is retrieved once per catalogue. Empty lookup
#'   values are excluded; duplicate rows and canonical spelling are retained.
#'   Catalogue aliases are grouped once by table and column. Each required lookup
#'   is indexed with only its own aliases and can be passed to reference_match_rows().
#'   Alias file paths and checksums are recorded in provenance.
#'   Missing sources, tables or columns raise errors.
#'   No validation rules are run.
#' @examples
#' # Load the requested form model.
#' form_model_definition <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
#' # Read the catalogue sources and lookup definitions.
#' reference_configuration <- read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Use a temporary cache for this example.
#' reference_cache_directory <- tempfile("model-references-")
#' # Assign the temporary cache to each catalogue source.
#' for (catalogue in names(reference_configuration$catalogues))
#'   reference_configuration$catalogues[[catalogue]]$source$cache <- reference_cache_directory
#' # Prepare the reference tables required by the form model.
#' reference_preparation <- prepare_model_references(form_model_definition, reference_configuration)
#' # Display the returned values.
#' names(reference_preparation$references)
#' # Display the selected reference rows or values.
#' reference_preparation$references[["registries/contacts/email"]]$data
#' # Remove the temporary files created by this example.
#' unlink(reference_cache_directory, recursive = TRUE)
#' @export
prepare_model_references <- function(model, configuration, offline = FALSE) {
  if (!is.logical(offline) || length(offline) != 1L || is.na(offline))
    stop("offline must be TRUE or FALSE", call. = FALSE)
  resolved_model <- resolve_reference_lookups(model, configuration)
  sources <- lapply(configuration$catalogues, `[[`, "source")
  lookups <- attr(resolved_model, "reference_lookups", exact = TRUE)
  groups <- unique(vapply(lookups, `[[`, character(1), "catalogue"))
  missing <- setdiff(groups, names(sources))
  if (length(missing)) stop("Missing reference sources: ", paste(missing, collapse = ", "), call. = FALSE)
  catalogues <- stats::setNames(lapply(groups, function(group) {
    selected <- Filter(function(lookup) identical(lookup$catalogue, group), lookups)
    ids <- unique(vapply(selected, `[[`, character(1), "table"))
    get_reference_tables(ids, sources[[group]], offline = offline)
  }), groups)
  references <- list()
  provenance <- list()
  for (id in names(lookups)) {
    lookup <- lookups[[id]]
    catalogue <- catalogues[[lookup$catalogue]]
    data <- catalogue$data[[lookup$table]]
    if (!lookup$column %in% names(data))
      stop("Missing lookup column: ", lookup$table, ".", lookup$column, call. = FALSE)
    values <- data[[lookup$column]]
    selected <- !is.na(values) & nzchar(values)
    sha256 <- catalogue$catalogue$sha256[[match(lookup$table, catalogue$catalogue$id)]]
    data <- data[selected, , drop = FALSE]
    attr(data, "reference_aliases") <- attr(data, "reference_aliases", exact = TRUE)[lookup$column]
    references[[id]] <- reference_index(data, lookup$column,
      matching = lookup$matching, ignore_case = lookup$ignore_case)
    references[[id]]$version <- sha256
    references[[id]]$lookup <- lookup
    provenance[[id]] <- list(path = catalogue$paths[[lookup$table]], column = lookup$column,
      sha256 = sha256, status = "available")
    if ("_aliases_" %in% catalogue$catalogue$id) {
      alias_row <- match("_aliases_", catalogue$catalogue$id)
      provenance[[id]]$aliases <- list(path = catalogue$paths[["_aliases_"]],
        sha256 = catalogue$catalogue$sha256[[alias_row]])
    }
  }
  attr(references, "reference_lookups") <- lookups
  attr(references, "provenance") <- provenance
  list(model = resolved_model, references = references)
}
