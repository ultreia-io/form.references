# Configuration --------------------------------------------------------------

#' Mapping configuration objects
#'
#' An S3 `mapping_configuration` is a named list of correspondences between two reference catalogues.
#'
#' @section Construction:
#' Use [read_mapping_configuration()] to read and validate `mapping.yaml`.
#'
#' Lookup defaults and returned properties are made explicit, and correspondence-file paths become absolute.
#'
#' Reading does not load catalogue rows or open correspondence files.
#'
#' @section Structure:
#' - `schema_version`: the mapping format version.
#' - `catalogues`: the `source` and `target` selections, naming reference configurations and their catalogues.
#' - `mappings`: a named list keyed by target table ID, each containing a list of mapping alternatives.
#' - Each alternative declares `source.table`, `source.lookup`, target `lookup` and returned `property`.
#' - Optional `source.property` and `file` select the correspondence source and are mutually exclusive.
#'
#' Alternatives are always stored as lists, including when the YAML uses the compact single-alternative form.
#'
#' Catalogue definitions and their data are not embedded in this object.
#'
#' @section Attributes:
#' - `provenance`: the mapping YAML's absolute `path` and SHA-256 `sha256` checksum.
#' - `mapping_lookups`: added to the returned copy by [resolve_mapping_lookups()], with resolved lookup policies.
#'
#' @section Use:
#' Pass the object and named [reference_configuration-class] objects to [prepare_mapping_references()].
#'
#' Mapping alternatives require an explicit, unambiguous selection; they are not fallbacks.
#'
#' There are no class-specific S3 methods.
#'
#' @seealso [read_mapping_configuration()], [resolve_mapping_lookups()], [mapping_references-class]
#' @name mapping_configuration-class
NULL

#' Read the catalogue-mapping schema
#' @param version Schema version; currently `"1.0.0"`.
#' @return The packaged schema as a nested list. Unsupported versions raise an error.
#' @details Uses the package-specific `form.references/mapping-schema/1` dialect.
#' @examples
#' mapping_schema()$contract_version
#' @export
mapping_schema <- function(version = "1.0.0") {
  scalar_text(version, "version")
  if (!identical(version, "1.0.0"))
    stop("Unsupported mapping schema version.", call. = FALSE)
  yaml::read_yaml(system.file("schema", "mapping-1.0.0.yaml",
    package = "form.references", mustWork = TRUE), eval.expr = FALSE)
}

#' Read catalogue correspondences without loading data
#' @param path Local mapping YAML path.
#' @return A [mapping_configuration-class] object. Each target table contains a list of
#'   alternatives, including when YAML uses the compact single-mapping form.
#'   Effective source and target lookups and the returned property are explicit.
#'   Correspondence file paths are absolute. The `provenance` attribute records
#'   the configuration path and SHA-256 checksum.
#' @details Validates the versioned mapping schema, lookup inheritance and
#'   alternative uniqueness. A selection is identified by its source table,
#'   source lookup, target lookup and returned property. YAML expressions are
#'   never evaluated. Reading does not load reference configurations, open CSV
#'   correspondence files or retrieve catalogue data.
#'
#'   Relative correspondence paths resolve from the YAML directory. File
#'   existence, contents and table columns are checked during preparation.
#' @examples
#' mapping <- read_mapping_configuration(system.file(
#'   "examples", "survey", "mapping.yaml", package = "form.references"))
#' names(mapping$mappings)
#' @export
read_mapping_configuration <- function(path) {
  scalar_text(path, "path")
  configuration <- yaml::read_yaml(path, eval.expr = FALSE, handlers = list(seq = as.list))
  check_mapping_configuration(configuration)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  for (target in names(configuration$mappings)) {
    alternatives <- configuration$mappings[[target]]
    if (!is.null(names(alternatives))) alternatives <- list(alternatives)
    selections <- list()
    for (i in seq_along(alternatives)) {
      entry <- alternatives[[i]]
      location <- paste0("$.mappings.", target, "[", i, "]")
      if (is.null(entry$source$lookup)) entry$source$lookup <- configuration$catalogues$source$lookup
      if (is.null(entry$lookup)) entry$lookup <- configuration$catalogues$target$lookup
      if (is.null(entry$source$lookup) || is.null(entry$lookup))
        stop(location, ": source and target lookups must be declared locally or as defaults.", call. = FALSE)
      if (is.null(entry$property)) entry$property <- entry$lookup
      if (target == "_aliases_" || entry$source$table == "_aliases_")
        stop(location, ": _aliases_ is reserved for catalogue aliases.", call. = FALSE)
      selection <- list(entry$source$table, entry$source$lookup, entry$lookup, entry$property)
      if (any(vapply(selections, identical, logical(1), y = selection)))
        stop(location, ": duplicate mapping selection.", call. = FALSE)
      selections[[i]] <- selection
      if (!is.null(entry$file)) {
        file <- path.expand(entry$file)
        if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", file))
          stop(location, ": file must be a local CSV path.", call. = FALSE)
        if (!grepl("^(/|[A-Za-z]:[/\\\\])", file)) file <- file.path(dirname(path), file)
        entry$file <- file
      }
      alternatives[[i]] <- entry
    }
    configuration$mappings[[target]] <- alternatives
  }
  class(configuration) <- "mapping_configuration"
  attr(configuration, "provenance") <- list(path = path,
    sha256 = digest::digest(file = path, algo = "sha256"))
  configuration
}

# Lookup resolution, preparation and value mapping ---------------------------

#' Prepared mapping reference objects
#'
#' An S3 `mapping_references` is a list holding the catalogue indexes and correspondences needed for value resolution.
#'
#' @section Construction:
#' Use [prepare_mapping_references()] with a [mapping_configuration-class] and named reference configurations.
#'
#' Preparation loads the selected target mappings and their required data, checks columns and reads correspondence files.
#'
#' Only rebound mappings require source catalogue rows.
#'
#' @section Structure:
#' - `mapping`: the mapping configuration, including resolved lookup policies.
#' - `alternatives`: selected target tables and their alternatives, linked to the required reference indexes.
#' - `references`: shared [reference_index-class] objects, each annotated with its lookup specification.
#' - `correspondences`: entries keyed by CSV path, containing `data`, an exact lookup `index` and file `provenance`.
#' - `provenance`: catalogue table metadata, publication metadata, cached paths and any catalogue alias metadata.
#'
#' Each alternative contains its `mapping`, `source_index` and `target_index`.
#'
#' `source_index` is `NULL` for CSV and unchanged-input correspondences.
#'
#' Keys linking alternatives, reference indexes and provenance are internal identifiers.
#'
#' @section Use:
#' Pass the object to [map_reference_values()] for each selected target and its encountered source values.
#'
#' Prepare it once and reuse it for successive calls; prepare it again when configuration or reference data changes.
#'
#' Inspect the returned result rows and their provenance for resolution diagnostics.
#'
#' Mapping results are ordinary data frames, not another custom S3 class.
#'
#' There are no class-specific S3 methods.
#'
#' @seealso [prepare_mapping_references()], [map_reference_values()], [mapping_configuration-class]
#' @name mapping_references-class
NULL

#' Resolve the lookup declarations of a catalogue mapping
#' @param mapping A configuration from read_mapping_configuration().
#' @param configurations Named list of reference configurations from
#'   read_reference_configuration(), keyed by the mapping's configuration names.
#' @return The mapping with a `mapping_lookups` attribute containing the resolved
#'   lookup policies for every alternative. The input is unchanged.
#' @details Checks both catalogue declarations without loading catalogue rows or
#'   correspondence files. Actual property columns are checked during preparation.
#' @export
resolve_mapping_lookups <- function(mapping, configurations) {
  if (!inherits(mapping, "mapping_configuration"))
    stop("Use read_mapping_configuration() first.", call. = FALSE)
  if (!is.list(configurations) || is.null(names(configurations)) ||
      anyNA(names(configurations)) || any(!nzchar(names(configurations))) || anyDuplicated(names(configurations)))
    stop("configurations must be a uniquely named list of reference configurations.", call. = FALSE)
  selected <- lapply(mapping$catalogues, function(selection) {
    configuration <- configurations[[selection$configuration]]
    if (!inherits(configuration, "reference_configuration"))
      stop("Missing reference configuration: ", selection$configuration, ".", call. = FALSE)
    catalogue <- configuration$catalogues[[selection$catalogue]]
    if (is.null(catalogue))
      stop("Unknown catalogue: ", selection$configuration, "/", selection$catalogue, ".", call. = FALSE)
    catalogue
  })
  lookup <- function(side, table, column) {
    policy <- selected[[side]]$tables[[table]][[column]]
    if (is.null(policy))
      stop("Unknown mapping lookup: ", side, "/", table, "/", column, ".", call. = FALSE)
    c(mapping$catalogues[[side]][c("configuration", "catalogue")],
      list(table = table, column = column), policy)
  }
  lookups <- lapply(names(mapping$mappings), function(target) {
    lapply(mapping$mappings[[target]], function(entry) {
      list(source = lookup("source", entry$source$table, entry$source$lookup),
           target = lookup("target", target, entry$lookup))
    })
  })
  attr(mapping, "mapping_lookups") <- stats::setNames(lookups, names(mapping$mappings))
  mapping
}

#' Prepare catalogue tables and explicit correspondences for mappings
#' @inheritParams resolve_mapping_lookups
#' @param targets Target table IDs to prepare; NULL selects every configured target.
#' @param offline Use verified reference caches only. Correspondence CSVs are local files.
#' @return A [mapping_references-class] object retaining the selected alternatives,
#'   shared reference indexes, correspondence tables and provenance.
#' @details Retrieves each required table once per configuration and catalogue.
#'   Source rows are required only for property-based correspondences. File-based
#'   and unchanged-input mappings check source declarations but load target data only.
#'   Different lookups share table retrieval; repeated identical lookups share indexes.
#'
#'   Checks actual lookup and property columns, CSV headers and nonempty text values.
#'   Identical CSV pairs are deduplicated; conflicting targets remain candidates.
#'   Preparation does not require every source value to have a correspondence.
#' @examples
#' local({
#'   directory <- system.file("examples", "survey", package = "form.references")
#'   mapping <- read_mapping_configuration(file.path(directory, "mapping.yaml"))
#'   source <- read_reference_configuration(file.path(directory, "references-source.yaml"))
#'   target <- read_reference_configuration(file.path(directory, "references.yaml"))
#'   configurations <- list(source = source, target = target)
#'   on.exit({
#'     for (configuration in configurations) {
#'       for (catalogue in configuration$catalogues) unlink(catalogue$source$cache, recursive = TRUE)
#'     }
#'   })
#'   prepared <- prepare_mapping_references(mapping, configurations)
#'   map_reference_values(prepared, "stations", c("station-north", "station-unmapped"))[, 1:4]
#' })
#' @export
prepare_mapping_references <- function(mapping, configurations, targets = NULL, offline = FALSE) {
  if (!is.logical(offline) || length(offline) != 1L || is.na(offline))
    stop("offline must be TRUE or FALSE.", call. = FALSE)
  mapping <- resolve_mapping_lookups(mapping, configurations)
  if (is.null(targets)) targets <- names(mapping$mappings)
  if (!is.character(targets) || anyNA(targets) || any(!nzchar(targets)) ||
      any(!targets %in% names(mapping$mappings)))
    stop("targets must identify configured target tables.", call. = FALSE)
  targets <- unique(targets)
  selections <- attr(mapping, "mapping_lookups", exact = TRUE)
  lookups <- list()
  for (target in targets) {
    for (i in seq_along(mapping$mappings[[target]])) {
      entry <- mapping$mappings[[target]][[i]]
      selected <- selections[[target]][[i]]
      lookups[[mapping_lookup_key(selected$target)]] <- selected$target
      if (!is.null(entry$source$property))
        lookups[[mapping_lookup_key(selected$source)]] <- selected$source
    }
  }
  groups <- split(lookups, vapply(lookups, mapping_catalogue_key, character(1)))
  retrieved <- lapply(groups, function(group) {
    specification <- group[[1L]]
    source <- configurations[[specification$configuration]]$catalogues[[specification$catalogue]]$source
    get_reference_tables(unique(vapply(group, `[[`, character(1), "table")), source, offline = offline)
  })
  references <- list()
  provenance <- list()
  for (key in names(lookups)) {
    specification <- lookups[[key]]
    catalogue <- retrieved[[mapping_catalogue_key(specification)]]
    data <- catalogue$data[[specification$table]]
    mapping_column(data, specification$column, specification$table)
    values <- data[[specification$column]]
    selected <- !is.na(values) & nzchar(values)
    aliases <- attr(data, "reference_aliases", exact = TRUE)
    data <- data[selected, , drop = FALSE]
    attr(data, "reference_aliases") <- aliases[specification$column]
    references[[key]] <- reference_index(data, specification$column,
      matching = specification$matching, ignore_case = specification$ignore_case)
    references[[key]]$lookup <- specification
    row <- catalogue$catalogue[catalogue$catalogue$id == specification$table, , drop = FALSE]
    provenance[[key]] <- list(lookup = specification, table = row,
      publication = catalogue$publication, path = catalogue$paths[[specification$table]])
    if ("_aliases_" %in% catalogue$catalogue$id)
      provenance[[key]]$aliases <- list(path = catalogue$paths[["_aliases_"]],
        table = catalogue$catalogue[catalogue$catalogue$id == "_aliases_", , drop = FALSE])
  }
  alternatives <- stats::setNames(vector("list", length(targets)), targets)
  correspondences <- list()
  for (target in targets) {
    alternatives[[target]] <- lapply(seq_along(mapping$mappings[[target]]), function(i) {
      entry <- mapping$mappings[[target]][[i]]
      selected <- selections[[target]][[i]]
      target_key <- mapping_lookup_key(selected$target)
      mapping_column(references[[target_key]]$data, entry$property, target)
      source_key <- NULL
      if (!is.null(entry$source$property)) {
        source_key <- mapping_lookup_key(selected$source)
        mapping_column(references[[source_key]]$data, entry$source$property, entry$source$table)
      }
      if (!is.null(entry$file) && is.null(correspondences[[entry$file]]))
        correspondences[[entry$file]] <<- read_mapping_correspondences(entry$file)
      list(mapping = entry, source_index = source_key, target_index = target_key)
    })
  }
  structure(list(mapping = mapping, alternatives = alternatives, references = references,
    correspondences = correspondences, provenance = provenance), class = "mapping_references")
}

#' Resolve encountered source values to target reference values
#' @param prepared A result from prepare_mapping_references().
#' @param target One prepared target table ID.
#' @param values Character vector or list of scalar text values. NULL elements,
#'   NA and empty text indicate missing input; other types are rejected.
#' @param source_table Optional source table selector.
#' @param source_lookup Optional effective source lookup selector.
#' @param target_lookup Optional effective target lookup selector.
#' @param property Optional returned target property selector.
#' @return A data frame with one row per input in original order. Scalar columns
#'   are `source_value`, `target_value`, `status` and `stage`. List columns retain
#'   `source_rows`, `target_rows`, `target_candidates` and `correspondence_values`.
#'   Row indexes address the corresponding prepared reference index's `data`.
#'   The `provenance` attribute contains the selected mapping, configuration
#'   checksum, catalogue revisions and any correspondence-file checksum.
#' @details Selectors must identify exactly one mapping; alternatives are never
#'   fallbacks. Status is `resolved`, `missing_input`, `missing_source_record`,
#'   `missing_property`, `no_match` or `ambiguous`. Stage is `input`, `source`,
#'   `correspondence` or `target`. Only resolved results receive a target value.
#'   Source and target ambiguities retain all matching rows even if their
#'   requested properties agree. Conflicting CSV targets remain ambiguous even
#'   if target aliases would converge. Repeated inputs and reusable target
#'   lookups are evaluated together. The caller decides whether unresolved
#'   results block a field, row or export.
#' @export
map_reference_values <- function(prepared, target, values, source_table = NULL,
                                 source_lookup = NULL, target_lookup = NULL, property = NULL) {
  if (!inherits(prepared, "mapping_references"))
    stop("Use prepare_mapping_references() first.", call. = FALSE)
  scalar_text(target, "target")
  alternatives <- prepared$alternatives[[target]]
  if (is.null(alternatives)) stop("Target mapping was not prepared: ", target, ".", call. = FALSE)
  selectors <- list(source_table = source_table, source_lookup = source_lookup,
    target_lookup = target_lookup, property = property)
  for (name in names(selectors)) {
    if (!is.null(selectors[[name]])) scalar_text(selectors[[name]], name)
  }
  selected <- vapply(alternatives, function(alternative) {
    entry <- alternative$mapping
    actual <- list(source_table = entry$source$table, source_lookup = entry$source$lookup,
      target_lookup = entry$lookup, property = entry$property)
    all(vapply(names(selectors), function(name)
      is.null(selectors[[name]]) || identical(selectors[[name]], actual[[name]]), logical(1)))
  }, logical(1))
  if (sum(selected) != 1L)
    stop("Mapping selectors must identify exactly one alternative for ", target, ".", call. = FALSE)
  alternative <- alternatives[[which(selected)]]
  entry <- alternative$mapping
  if (!(is.atomic(values) || is.list(values)) || is.data.frame(values) || !is.null(dim(values)))
    stop("values must be a character vector or a list of text values.", call. = FALSE)
  inputs <- vapply(seq_along(values), function(i) {
    value <- values[[i]]
    if (reference_null_value(value)) return(NA_character_)
    if (!reference_text(value)) stop("Malformed mapping input: expected text or a missing value.", call. = FALSE)
    value
  }, character(1))
  distinct <- unique(inputs)
  n <- length(distinct)
  status <- rep("missing_input", n)
  stage <- rep("input", n)
  source_rows <- target_rows <- rep(list(integer()), n)
  candidates <- correspondence_values <- rep(list(character()), n)
  target_values <- submitted <- rep(NA_character_, n)
  present <- which(!is.na(distinct) & nzchar(distinct))
  if (!is.null(entry$source$property)) {
    source_index <- prepared$references[[alternative$source_index]]
    matches <- reference_match_rows(source_index, distinct[present])
    for (j in seq_along(present)) {
      i <- present[j]
      rows <- matches[[j]]
      source_rows[[i]] <- rows
      stage[i] <- "source"
      status[i] <- if (!length(rows)) "missing_source_record" else if (length(rows) > 1L) "ambiguous" else "missing_property"
      if (length(rows) == 1L) {
        value <- source_index$data[[entry$source$property]][rows]
        if (!is.na(value) && nzchar(value)) submitted[i] <- value
      }
    }
  } else if (!is.null(entry$file)) {
    file <- prepared$correspondences[[entry$file]]
    rows <- reference_match_rows(file$index, distinct[present])
    for (j in seq_along(present)) {
      i <- present[j]
      mapped <- file$data$target_value[rows[[j]]]
      correspondence_values[[i]] <- mapped
      stage[i] <- "correspondence"
      status[i] <- if (length(mapped) > 1L) "ambiguous" else "no_match"
      if (length(mapped) == 1L) submitted[i] <- mapped
    }
  } else {
    submitted[present] <- distinct[present]
  }
  ready <- which(!is.na(submitted))
  correspondence_values[ready] <- lapply(submitted[ready], identity)
  target_index <- prepared$references[[alternative$target_index]]
  matches <- reference_match_rows(target_index, submitted[ready])
  for (j in seq_along(ready)) {
    i <- ready[j]
    rows <- matches[[j]]
    target_rows[[i]] <- rows
    candidates[[i]] <- target_index$data[[entry$property]][rows]
    stage[i] <- "target"
    status[i] <- if (!length(rows)) "no_match" else if (length(rows) > 1L) "ambiguous" else "missing_property"
    if (length(rows) == 1L && !is.na(candidates[[i]]) && nzchar(candidates[[i]])) {
      status[i] <- "resolved"
      target_values[i] <- candidates[[i]]
    }
  }
  positions <- match(inputs, distinct)
  result <- data.frame(source_value = inputs, target_value = target_values[positions],
    status = status[positions], stage = stage[positions], stringsAsFactors = FALSE)
  result$source_rows <- I(source_rows[positions])
  result$target_rows <- I(target_rows[positions])
  result$target_candidates <- I(candidates[positions])
  result$correspondence_values <- I(correspondence_values[positions])
  provenance <- list(configuration = attr(prepared$mapping, "provenance", exact = TRUE),
    schema_version = prepared$mapping$schema_version, catalogues = prepared$mapping$catalogues,
    target = target, mapping = entry,
    target_catalogue = prepared$provenance[[alternative$target_index]])
  if (!is.null(alternative$source_index))
    provenance$source_catalogue <- prepared$provenance[[alternative$source_index]]
  if (!is.null(entry$file)) provenance$correspondence <- prepared$correspondences[[entry$file]]$provenance
  attr(result, "provenance") <- provenance
  result
}
