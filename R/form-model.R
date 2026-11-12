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
