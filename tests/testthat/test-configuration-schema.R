configuration_yaml <- function(configuration, envir = parent.frame()) {
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(configuration, path)
  withr::defer(unlink(path), envir = envir)
  path
}

example_configuration <- function() {
  yaml::read_yaml(system.file("examples", "survey", "references.yaml", package = "form.references"))
}

test_that("one configuration validates both sources and lookups", {
  configuration <- example_configuration()
  configuration$catalogues$code_lists$source$url <- "https://example.org/"
  configuration$catalogues$registries$source$url <- "https://example.org/"
  path <- configuration_yaml(configuration)
  prepared <- read_reference_configuration(path)
  expect_s3_class(prepared, "reference_configuration")
  expect_s3_class(prepared$catalogues$code_lists$source, "reference_source")
  expect_identical(prepared$catalogues$registries$tables$contacts$name$matching, "name")
  configuration$catalogues$code_lists$tables$stations$code$ignore_case <- "true"
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "expected boolean")
  configuration$catalogues$code_lists$tables$stations$code$ignore_case <- TRUE
  configuration$catalogues$code_lists$tables$stations$code$matching <- "approximate"
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "unsupported value")
  configuration$catalogues$code_lists$tables$stations$code$matching <- "exact"
  configuration$catalogues$code_lists$source$timeout <- 0
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "must be > 0")
  configuration$catalogues$code_lists$source$timeout <- 60
  configuration$catalogues$code_lists$source$max_connections <- 1.5
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "expected integer")
  configuration$catalogues$code_lists$source <- NULL
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "missing required property source")
})

test_that("the schema checks the declared version", {
  configuration <- example_configuration()
  configuration$schema_version <- NULL
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "missing required property schema_version")
  configuration$schema_version <- "2.0.0"
  expect_error(read_reference_configuration(configuration_yaml(configuration)), "schema_version: unsupported value", fixed = TRUE)
  for (version in list(1, list("1.0.0"), NA_character_)) {
    configuration$schema_version <- version
    expect_error(read_reference_configuration(configuration_yaml(configuration)), "schema_version: expected string", fixed = TRUE)
  }
})

test_that("the schema loader exposes the supported contract", {
  expect_identical(reference_schema()$contract_version, "1.0.0")
  expect_identical(reference_schema("1.0.0"), reference_schema())
  expect_error(reference_schema("2.0.0"), "Unsupported schema version", fixed = TRUE)
  expect_error(reference_schema(1), "one nonempty string", fixed = TRUE)
})

test_that("examples resolve relative sources and private credentials", {
  directory <- system.file("examples", package = "form.references")
  configuration <- read_reference_configuration(file.path(directory, "survey", "references.yaml"))
  expect_named(configuration$catalogues, c("code_lists", "registries"))
  expect_identical(configuration$catalogues$code_lists$source$catalogue, "code_lists")
  expect_identical(configuration$catalogues$registries$source$catalogue, "registries")
  expect_identical(configuration$catalogues$code_lists$source$url,
    normalizePath(file.path(directory, "survey", "catalogue"), winslash = "/"))
  withr::local_envvar(c(REGISTRY_PASSWORD = "example-test-password"))
  private <- read_reference_configuration(file.path(directory, "references.private.example.yaml"))
  expect_match(unname(private$catalogues$registries$source$headers), "^Basic ")
  expect_false(any(grepl("example-test-password", capture.output(print(private)), fixed = TRUE)))
})

test_that("combined configuration prepares references from model bindings", {
  configuration <- read_reference_configuration(system.file("examples", "survey", "references.yaml",
    package = "form.references"))
  cache <- withr::local_tempdir()
  for (catalogue in names(configuration$catalogues)) configuration$catalogues[[catalogue]]$source$cache <- cache
  model <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
  original <- model
  prepared <- prepare_model_references(model, configuration)
  expect_identical(model, original)
  expect_true("registries/contacts/email" %in% names(prepared$references))
  expect_true("001" %in% prepared$references[["code_lists/stations/code"]]$values)
})

test_that("the entire configuration is checked before resolving credentials", {
  path <- tempfile(fileext = ".yaml")
  withr::defer(unlink(path))
  writeLines(c('schema_version: "1.0.0"', 'catalogues:', '  private:',
    '    source:', '      url: https://example.org/', '      cache: cache',
    '      user: reader', '      password: !env SCHEMA_MISSING_PASSWORD',
    '    tables:', '      contacts:', '        email: {matching: wrong, ignore_case: true}'), path)
  expect_error(read_reference_configuration(path), 'unsupported value')
  writeLines(sub('matching: wrong', 'matching: exact', readLines(path), fixed = TRUE), path)
  withr::local_envvar(c(SCHEMA_MISSING_PASSWORD = NA))
  expect_error(read_reference_configuration(path), 'missing or empty')
  writeLines(sub('SCHEMA_MISSING_PASSWORD', 'invalid-name', readLines(path), fixed = TRUE), path)
  expect_error(read_reference_configuration(path), 'valid environment variable')
})
