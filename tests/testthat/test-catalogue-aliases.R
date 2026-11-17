alias_fixture <- function(catalogue = "code_lists") {
  configuration <- read_reference_configuration(system.file(
    "examples", "survey", "references.yaml", package = "form.references"))
  source <- configuration$catalogues[[catalogue]]$source
  directory <- withr::local_tempdir(.local_envir = parent.frame())
  origin <- file.path(directory, "source")
  dir.create(origin)
  file.copy(list.files(source$url, full.names = TRUE, all.files = FALSE),
    origin, recursive = TRUE)
  list(directory = origin, source = reference_source(origin,
    file.path(directory, "cache")))
}

publish_alias_fixture <- function(fixture, aliases = NULL, inventory = NULL) {
  catalogue_path <- file.path(fixture$directory, "catalogue.csv")
  if (is.null(inventory)) inventory <- read_reference_csv(catalogue_path)
  if (!is.null(aliases)) {
    alias_path <- file.path(fixture$directory, inventory$csv_path[inventory$id == "_aliases_"])
    utils::write.csv(aliases, alias_path, row.names = FALSE)
    inventory$sha256[inventory$id == "_aliases_"] <- digest::digest(file = alias_path, algo = "sha256")
  }
  utils::write.csv(inventory, catalogue_path, row.names = FALSE)
  metadata_path <- file.path(fixture$directory, "catalogue.yaml")
  metadata <- yaml::read_yaml(metadata_path)
  metadata$sha256 <- digest::digest(file = catalogue_path, algo = "sha256")
  yaml::write_yaml(metadata, metadata_path)
}

test_that("catalogue aliases use verified table retrieval and remain available offline", {
  fixture <- alias_fixture()
  retrieved <- get_reference_tables("stations", fixture$source)
  expect_named(retrieved$data, c("stations"))
  expect_error(get_reference_tables("_aliases_", fixture$source), "reserved")
  expect_identical(retrieved$catalogue$domain[retrieved$catalogue$id == "_aliases_"], "catalogue")
  expect_identical(basename(retrieved$catalogue$csv_path[retrieved$catalogue$id == "_aliases_"]),
    "catalogue_aliases.csv")
  unlink(fixture$directory, recursive = TRUE)
  offline <- get_reference_tables("stations", fixture$source, offline = TRUE)
  expect_identical(offline, retrieved)
  writeLines("corrupt", retrieved$paths[["_aliases_"]])
  expect_error(get_reference_tables("stations", fixture$source, offline = TRUE), "Verified cache unavailable")
})

test_that("alias checksums are required before preparation", {
  fixture <- alias_fixture()
  inventory <- read_reference_catalogue(fixture$source)
  writeLines("corrupt", file.path(fixture$directory, inventory$csv_path[inventory$id == "_aliases_"]))
  expect_error(get_reference_tables("stations", fixture$source), "SHA-256 mismatch")
})

test_that("reserved alias identity cannot be used as an ordinary table", {
  fixture <- alias_fixture()
  inventory <- read_reference_catalogue(fixture$source)
  inventory$domain[inventory$id == "_aliases_"] <- "survey"
  publish_alias_fixture(fixture, inventory = inventory)
  expect_error(get_reference_tables("stations", fixture$source), "domain catalogue")
  inventory$id[inventory$id == "_aliases_"] <- "_ordinary_"
  publish_alias_fixture(fixture, inventory = inventory)
  expect_error(read_reference_catalogue(fixture$source), "unique lowercase")
  configuration <- read_reference_configuration(system.file(
    "examples", "survey", "references.yaml", package = "form.references"))
  model <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
  definition <- form.model::form_definition(model)
  for (sheet in names(definition$sheets))
    for (field in names(definition$sheets[[sheet]]$fields))
      if (!is.null(definition$sheets[[sheet]]$fields[[field]]$reference))
        definition$sheets[[sheet]]$fields[[field]]$reference$table <- "_aliases_"
  expect_error(form.model::as_form_model(definition), "required pattern")
})

test_that("alias rows reject missing targets, missing columns and conflicting values", {
  fixture <- alias_fixture()
  valid <- data.frame(table = "stations", column = "code", alias = "old", canonical = "001")
  invalid <- list(
    columns = valid[c("table", "alias", "canonical")],
    empty = transform(valid, alias = ""),
    duplicate = rbind(valid, valid),
    table = transform(valid, table = "missing"),
    column = transform(valid, column = "missing"),
    target = transform(valid, canonical = "missing"),
    conflict = transform(valid, alias = "001"))
  messages <- c(columns = "require", empty = "require", duplicate = "require",
    table = "absent from the catalogue", column = "column is absent",
    target = "targets are absent", conflict = "conflict with canonical")
  for (name in names(invalid)) {
    publish_alias_fixture(fixture, aliases = invalid[[name]])
    expect_error(get_reference_tables("stations", fixture$source), messages[[name]])
  }
})

test_that("prepared references retain catalogue aliases and their provenance", {
  configuration <- read_reference_configuration(system.file(
    "examples", "survey", "references.yaml", package = "form.references"))
  cache <- withr::local_tempdir()
  for (catalogue in names(configuration$catalogues))
    configuration$catalogues[[catalogue]]$source$cache <- cache
  model <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
  prepared <- prepare_model_references(model, configuration)
  stations <- prepared$references[["code_lists/stations/code"]]
  expect_identical(stations$aliases, c("OLD-STATION" = "001"))
  index <- stations
  expect_identical(reference_match_rows(index, "OLD-STATION")[[1]], 1L)
  expect_length(reference_match_rows(index, "old-station")[[1]], 0L)
  contacts <- prepared$references[["registries/contacts/email"]]
  index <- contacts
  expect_identical(reference_match_rows(index, "OLD-ALICE@EXAMPLE.ORG")[[1]], 1L)
  expect_length(reference_match_rows(stations, "OLD-OBSERVER")[[1]], 0L)
  expect_named(attr(stations$data, "reference_aliases"), "code")
  expect_identical(reference_match_rows(reference_index(contacts$data, contacts$column),
    "old-alice@example.org"), list(1L))
  provenance <- attr(prepared$references, "provenance")
  expect_identical(digest::digest(file = provenance[["code_lists/stations/code"]]$aliases$path,
    algo = "sha256"), provenance[["code_lists/stations/code"]]$aliases$sha256)
  expect_identical(prepare_model_references(model, configuration, offline = TRUE)$references,
    prepared$references)
})

test_that("lookup case policies reject ambiguous aliases", {
  aliases <- data.frame(table = "observers", column = "code",
    alias = c("old", "OLD"), canonical = c("ALICE", "BOB"))
  expect_error(reference_table_aliases(aliases, "observers", "code",
    c("ALICE", "BOB"), ignore_case = TRUE), "Conflicting alias targets")
  expect_identical(reference_table_aliases(aliases, "observers", "code",
    c("ALICE", "BOB"), ignore_case = FALSE), c(old = "ALICE", OLD = "BOB"))
  expect_identical(reference_table_aliases(NULL, "observers", "code", "ALICE"), character())
})

test_that("catalogues without aliases still retrieve ordinary tables", {
  fixture <- alias_fixture()
  inventory <- read_reference_catalogue(fixture$source)
  inventory <- inventory[inventory$id != "_aliases_", ]
  publish_alias_fixture(fixture, inventory = inventory)
  expect_named(get_reference_tables("stations", fixture$source)$data, "stations")
})

test_that("automatic aliases stay within their table and lookup column", {
  fixture <- alias_fixture("registries")
  publish_alias_fixture(fixture, aliases = data.frame(
    table = c("contacts", "contacts"), column = c("email", "name"),
    alias = c("previous", "previous"), canonical = c("alice@example.org", "Bob Example")))
  retrieved <- get_reference_tables("contacts", fixture$source)
  expect_identical(reference_match_rows(reference_index(retrieved$data$contacts, "email"),
    "previous"), list(1L))
  expect_identical(reference_match_rows(reference_index(retrieved$data$contacts, "name"),
    "previous"), list(2L))
  fixture <- alias_fixture()
  publish_alias_fixture(fixture, aliases = data.frame(
    table = c("stations", "observers"), column = c("code", "code"),
    alias = c("previous", "previous"), canonical = c("001", "BOB")))
  retrieved <- get_reference_tables(c("stations", "observers"), fixture$source)
  expect_identical(reference_match_rows(reference_index(retrieved$data$stations, "code"),
    "previous"), list(1L))
  expect_identical(reference_match_rows(reference_index(retrieved$data$observers, "code"),
    "previous"), list(2L))
})
