#' Read a verified registry catalogue
#' @inheritParams read_code_list_catalogue
#' @return Registry catalogue rows and publication metadata.
#' @export
read_registry_catalogue <- function(source, offline = FALSE) {
  if (!inherits(source, "reference_source") || !identical(source$catalogue, "registries"))
    stop("read_registry_catalogue requires a registries source.", call. = FALSE)
  read_catalogue(source, offline)
}


#' Retrieve selected registry tables
#' @inheritParams get_code_lists
#' @return The same result as `get_code_lists()`, for registry tables.
#' @export
get_registries <- function(ids, source, offline = FALSE) {
  if (!inherits(source, "reference_source") || !identical(source$catalogue, "registries"))
    stop("get_registries requires a registries source.", call. = FALSE)
  get_catalogue_tables(ids, source, offline)
}


#' Retrieve the registries required by a form model
#'
#' @inheritParams get_form_code_lists
#' @return The same list as [`get_registries()`][form.references::get_registries],
#'   with data and paths named by the model's registry table IDs.
#' @details Configure `source` with `catalogue = "registries"`. Authentication
#'   headers can be supplied by [`reference_source()`][form.references::reference_source].
#' @export
get_form_registries <- function(model, source, offline = FALSE) {
  if (!inherits(source, "reference_source") || !identical(source$catalogue, "registries"))
    stop("get_form_registries requires a registries source.", call. = FALSE)
  ids <- form.model::form_registries(model)
  if (is.null(ids)) ids <- character()
  get_registries(ids, source, offline = offline)
}
