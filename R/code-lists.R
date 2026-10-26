#' Read a verified code-list catalogue
#'
#' @param source A configuration from reference_source().
#' @param offline If TRUE, read the last verified cached catalogue only.
#' @return A data.frame with character columns: id, domain, csv_path, version,
#'   updated_at, published_at and sha256. Attribute publication contains the
#'   catalogue's published_at and sha256 metadata.
#' @details Online calls retrieve the selected catalogue's YAML first. A cached catalogue is
#'   reused when its checksum matches; timestamp changes alone do not require
#'   downloading identical CSV bytes. The cached publication pointer changes
#'   only after checksum and catalogue validation succeed.
#' @export
#' @examples
#' cache <- tempfile("references-")
#' source <- reference_source(system.file("examples", package = "form.references"), cache)
#' read_code_list_catalogue(source)[, c("id", "version")]
#' unlink(cache, recursive = TRUE)
read_code_list_catalogue <- function(source, offline = FALSE) {
  if (!inherits(source, "reference_source") || !identical(source$catalogue, "codelists"))
    stop("read_code_list_catalogue requires a codelists source.", call. = FALSE)
  read_catalogue(source, offline)
}


#' Retrieve selected code lists
#'
#' @param ids Exact lowercase catalogue IDs.
#' @param source A configuration from reference_source().
#' @param offline If TRUE, use only verified cached files and make no requests.
#' @return A list with data (tables named by ID), catalogue (selected metadata),
#'   paths (verified local CSV paths), and publication (catalogue metadata).
#' @details CSV columns are read as text, preserving leading zeros, whitespace,
#'   empty strings and literal NA values. Cache files are named by checksum;
#'   earlier verified versions remain available after a source update.
#'   Failed downloads never replace verified files. Offline mode fails explicitly
#'   when a required cached resource is missing or corrupt.
#' @export
#' @examples
#' cache <- tempfile("references-")
#' source <- reference_source(system.file("examples", package = "form.references"), cache)
#' references <- get_code_lists("stations", source)
#' references$data$stations$code
#' unlink(cache, recursive = TRUE)
get_code_lists <- function(ids, source, offline = FALSE) {
  if (!inherits(source, "reference_source") || !identical(source$catalogue, "codelists"))
    stop("get_code_lists requires a codelists source.", call. = FALSE)
  get_catalogue_tables(ids, source, offline)
}


#' Retrieve the code lists required by a form model
#'
#' @param model A checked form.model model.
#' @param source A configuration from reference_source().
#' @param offline If TRUE, use verified cached files without contacting the source.
#' @return The same list as get_code_lists(), with data and paths named by the
#'   model's declared code-list IDs. Registries are not downloaded.
#' @details IDs must match the lowercase catalogue IDs exactly. No domain-specific
#'   prefix removal, case conversion or alias guessing is performed.
#'   Authentication and source locations belong to source, not the model.
#' @export
#' @examples
#' model <- form.model::load_form_model("survey",
#'   system.file("examples", package = "form.model"))
#' cache <- tempfile("references-")
#' source <- reference_source(system.file("examples", package = "form.references"), cache)
#' references <- get_form_code_lists(model, source)
#' names(references$data)
#' unlink(cache, recursive = TRUE)
get_form_code_lists <- function(model, source, offline = FALSE) {
  if (!inherits(source, "reference_source") || !identical(source$catalogue, "codelists"))
    stop("get_form_code_lists requires a codelists source.", call. = FALSE)
  ids <- form.model::form_code_lists(model)
  if (is.null(ids)) ids <- character()
  get_code_lists(ids, source, offline = offline)
}
