# Copy only mapping inputs; use immutable packaged catalogues and temporary caches.
mapping_test_fixture <- function(envir = parent.frame()) {
  directory <- withr::local_tempdir(.local_envir = envir)
  example <- system.file("examples", "survey", package = "form.references")
  file.copy(file.path(example, c("mapping.yaml", "correspondences")), directory, recursive = TRUE)
  path <- file.path(directory, "mapping.yaml")
  configurations <- stats::setNames(lapply(c("source", "target"), function(name) {
    file <- if (name == "source") "references-source.yaml" else "references.yaml"
    configuration <- read_reference_configuration(file.path(example, file))
    for (catalogue in names(configuration$catalogues))
      configuration$catalogues[[catalogue]]$source$cache <- file.path(directory, "cache", name, catalogue)
    configuration
  }), c("source", "target"))
  list(directory = directory, path = path,
    definition = yaml::read_yaml(path), configurations = configurations,
    csv = file.path(directory, "correspondences", "stations.csv"))
}

mapping_test_read <- function(fixture, definition = fixture$definition) {
  yaml::write_yaml(definition, fixture$path)
  read_mapping_configuration(fixture$path)
}

# Change retrieved rows to exercise mapping diagnostics independently of retrieval checks.
mapping_test_transform_tables <- function(transform, envir = parent.frame()) {
  retrieve <- get_reference_tables
  testthat::local_mocked_bindings(get_reference_tables = function(...) {
    transform(retrieve(...))
  }, .package = "form.references", .env = envir)
}
