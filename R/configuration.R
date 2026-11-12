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

#' Read catalogue sources and lookup definitions
#' @param path YAML configuration path.
#' @param prompt Securely prompt for missing passwords; defaults to FALSE.
#' @return A reference_configuration containing named catalogues, each with a
#'   reference_source object and table lookup definitions. No data is downloaded.
#' @details The packaged schema/references-1.0.0.yaml schema checks the required
#'   schema_version. Each catalogue declares source and tables;
#'   each source directory contains catalogue.yaml and catalogue.csv;
#'   lookup names are column names. Relative source and cache paths resolve from
#'   the YAML directory. The schema uses form.references/schema/1, not JSON Schema.
#'
#'   Sources accept url, cache, timeout and max_connections, plus user and password
#'   for HTTPS Basic authentication. Use password: !env VARIABLE to read a secret
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

reference_source_configuration <- function(config, prompt, directory) {
  for (name in names(config)) {
    if (inherits(config[[name]], "reference_env"))
      config[[name]] <- reference_environment_value(
        unclass(config[[name]]), prompt = prompt && name == "password")
  }
  if (!is.null(directory)) {
    for (name in c("url", "cache")) {
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

#' Configure a reference source
#'
#' @param url HTTP(S) base URL or local directory containing the selected
#'   catalogue.yaml and catalogue.csv files. URLs must not contain credentials, query strings or fragments.
#' @param cache Local cache directory. Different sources receive separate
#'   subdirectories. Use separate caches for separate authenticated identities.
#' @param headers Named HTTP header values, or a function returning them at
#'   request time. NULL sends no authentication headers.
#' @param timeout Positive request timeout in seconds.
#' @param max_connections Maximum simultaneous CSV downloads. The default is four.
#' @return A reference_source configuration. No files are downloaded.
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
reference_source <- function(url, cache, headers = NULL, timeout = 60,
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
