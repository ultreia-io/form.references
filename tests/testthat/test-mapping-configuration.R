test_that("mapping defaults and paths resolve without reading correspondence data", {
  fixture <- mapping_test_fixture()
  fixture$definition$mappings$stations$file <- "not-created.csv"
  withr::local_dir(withr::local_tempdir())
  local_mocked_bindings(get_reference_tables = function(...) stop("Unexpected catalogue retrieval"))
  mapping <- mapping_test_read(fixture)
  expect_s3_class(mapping, "mapping_configuration")
  expect_length(mapping$mappings$stations, 1L)
  expect_length(mapping$mappings$observers, 2L)
  expect_identical(mapping$mappings$stations[[1L]]$source$lookup, "id")
  expect_identical(mapping$mappings$stations[[1L]]$lookup, "code")
  expect_identical(mapping$mappings$stations[[1L]]$property, "code")
  expect_identical(mapping$mappings$stations[[1L]]$file,
    file.path(normalizePath(dirname(fixture$path), winslash = "/"), "not-created.csv"))
  expect_identical(mapping$mappings$observers[[2L]]$source$lookup, "name")
  expect_identical(attr(mapping, "provenance")$sha256, digest::digest(file = fixture$path, algo = "sha256"))

  fixture$definition$mappings <- list(observers = list(source = list(table = "observers", lookup = "name"),
    lookup = "name"))
  overridden <- mapping_test_read(fixture)
  expect_identical(overridden$mappings$observers[[1L]]$property, "name")
})

test_that("mapping schema rejects unsupported versions and malformed structures", {
  fixture <- mapping_test_fixture()
  expect_identical(mapping_schema()$contract_version, "1.0.0")
  expect_error(mapping_schema("2.0.0"), "Unsupported mapping schema version", fixed = TRUE)
  scenarios <- list(
    missing_version = function(x) { x$schema_version <- NULL; x },
    unsupported_version = function(x) { x$schema_version <- "2.0.0"; x },
    numeric_version = function(x) { x$schema_version <- 1; x },
    unknown_property = function(x) { x$mappings$stations$mode <- "direct"; x },
    conflicting_sources = function(x) { x$mappings$stations$source$property <- "name"; x },
    empty_mappings = function(x) { x$mappings <- list(); x },
    empty_alternatives = function(x) { x$mappings$stations <- list(); x },
    scalar_mapping = function(x) { x$mappings$stations <- "observers"; x },
    missing_table = function(x) { x$mappings$stations$source$table <- NULL; x },
    missing_lookup = function(x) { x$catalogues$source$lookup <- NULL; x }
  )
  expected <- c(missing_version = "missing required property schema_version", unsupported_version = "unsupported value",
    numeric_version = "expected string", unknown_property = "unsupported property mode",
    conflicting_sources = "mutually exclusive", empty_mappings = "requires at least",
    empty_alternatives = "expected exactly one", scalar_mapping = "expected exactly one",
    missing_table = "missing required property table", missing_lookup = "lookups must be declared")
  for (scenario in names(scenarios))
    expect_error(mapping_test_read(fixture, scenarios[[scenario]](fixture$definition)), expected[[scenario]], info = scenario)
})

test_that("duplicate YAML keys and indistinguishable alternatives cannot choose a mapping", {
  fixture <- mapping_test_fixture()
  lines <- readLines(fixture$path)
  writeLines(c(lines, 'schema_version: "1.0.0"'), fixture$path)
  expect_error(read_mapping_configuration(fixture$path), "[Dd]uplicate")

  entry <- fixture$definition$mappings$stations
  other <- entry
  other$file <- "different.csv"
  fixture$definition$mappings$stations <- list(entry, other)
  expect_error(mapping_test_read(fixture), "duplicate mapping selection", fixed = TRUE)

  fixture$definition$mappings <- list(stations = entry)
  fixture$definition$mappings$stations$source$table <- "_aliases_"
  expect_error(mapping_test_read(fixture), "_aliases_ is reserved", fixed = TRUE)
})

test_that("mapping YAML expressions are never evaluated", {
  fixture <- mapping_test_fixture()
  withr::local_options(form.references.mapping.expression = FALSE)
  lines <- readLines(fixture$path)
  lines <- sub('lookup: id',
    "lookup: !expr 'options(form.references.mapping.expression = TRUE)'", lines, fixed = TRUE)
  writeLines(lines, fixture$path)
  mapping <- read_mapping_configuration(fixture$path)
  expect_false(getOption("form.references.mapping.expression"))
  expect_identical(mapping$mappings$stations[[1L]]$source$lookup,
    "options(form.references.mapping.expression = TRUE)")
})

test_that("lookup declarations fail separately from catalogue loading", {
  fixture <- mapping_test_fixture()
  mapping <- mapping_test_read(fixture)
  original <- mapping
  local_mocked_bindings(get_reference_tables = function(...) stop("Unexpected catalogue retrieval"))
  resolved <- resolve_mapping_lookups(mapping, fixture$configurations)
  expect_identical(mapping, original)
  expect_identical(attr(resolved, "mapping_lookups")$observers[[1L]]$target$matching, "name")

  scenarios <- list(
    configuration = function(x) { x$source <- NULL; x },
    catalogue = function(x) { x$source$catalogues$code_lists <- NULL; x },
    source_table = function(x) { x$source$catalogues$code_lists$tables$stations <- NULL; x },
    source_lookup = function(x) { x$source$catalogues$code_lists$tables$stations$id <- NULL; x },
    target_lookup = function(x) { x$target$catalogues$code_lists$tables$stations$code <- NULL; x }
  )
  expected <- c(configuration = "Missing reference configuration", catalogue = "Unknown catalogue",
    source_table = "Unknown mapping lookup: source", source_lookup = "Unknown mapping lookup: source",
    target_lookup = "Unknown mapping lookup: target")
  for (scenario in names(scenarios))
    expect_error(resolve_mapping_lookups(mapping, scenarios[[scenario]](fixture$configurations)),
      expected[[scenario]], info = scenario)
})
