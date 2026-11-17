test_that("CSV correspondences preserve codes and do not apply source aliases or fallbacks", {
  fixture <- mapping_test_fixture()
  mapping <- mapping_test_read(fixture)
  prepared <- prepare_mapping_references(mapping, fixture$configurations, targets = "stations")
  inputs <- c("station-legacy", "station-south", "station-legacy", "former-north",
    "STATION-NORTH", "001")
  result <- map_reference_values(prepared, "stations", inputs)
  expect_identical(result$source_value, inputs)
  expect_identical(result$target_value, c("001", "002", "001", NA_character_, NA_character_, NA_character_))
  expect_identical(result$status, c(rep("resolved", 3), rep("no_match", 3)))
  expect_identical(result$stage, c(rep("target", 3), rep("correspondence", 3)))
  expect_identical(result$correspondence_values[[1L]], "OLD-STATION")
})

test_that("rebound and unchanged input reuse the configured catalogue matching", {
  fixture <- mapping_test_fixture()
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "observers")
  rebound <- map_reference_values(prepared, "observers",
    c("former-alice", "PERSON-BOB"), source_lookup = "id")
  expect_identical(rebound$target_value, c("ALICE", "BOB"))
  expect_identical(rebound$correspondence_values[[1L]], "Example Alice")
  direct <- map_reference_values(prepared, "observers",
    c("Example, Alice", "  example-bob  ", "Alic Example"), source_lookup = "name")
  expect_identical(direct$target_value, c("ALICE", "BOB", NA_character_))
  expect_identical(direct$status, c("resolved", "resolved", "no_match"))
  expect_length(direct$source_rows[[1L]], 0L)
})

test_that("mapping alternatives require an unambiguous explicit selection", {
  fixture <- mapping_test_fixture()
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "observers")
  expect_error(map_reference_values(prepared, "observers", "person-alice"), "exactly one alternative")
  expect_error(map_reference_values(prepared, "observers", "person-alice", source_table = "observers"),
    "exactly one alternative")
  expect_error(map_reference_values(prepared, "observers", "person-alice", source_lookup = "missing"),
    "exactly one alternative")
  expect_error(map_reference_values(prepared, "stations", "station-north"), "was not prepared")
  result <- map_reference_values(prepared, "observers", "person-alice", source_table = "observers",
    source_lookup = "id", target_lookup = "name", property = "code")
  expect_identical(result$target_value, "ALICE")
})

test_that("missing input, source records and properties have distinct diagnostics", {
  fixture <- mapping_test_fixture()
  mapping_test_transform_tables(function(tables) {
    if ("observers" %in% names(tables$data) && "id" %in% names(tables$data$observers))
      tables$data$observers$name[1L] <- ""
    if ("observers" %in% names(tables$data) && "code" %in% names(tables$data$observers))
      tables$data$observers$code[2L] <- ""
    tables
  })
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "observers")
  result <- map_reference_values(prepared, "observers",
    list(NULL, NA_character_, "", "person-absent", "person-alice", "person-bob", "person-unlisted"),
    source_lookup = "id")
  expect_identical(result$status, c(rep("missing_input", 3), "missing_source_record",
    "missing_property", "missing_property", "no_match"))
  expect_identical(result$stage, c(rep("input", 3), "source", "source", "target", "target"))
  expect_true(all(is.na(result$target_value)))
  expect_identical(result$source_value[1:3], c(NA_character_, NA_character_, ""))
  expect_length(result$source_rows[[4L]], 0L)
  expect_length(result$source_rows[[5L]], 1L)
  expect_length(result$target_rows[[6L]], 1L)
  expect_length(result$target_rows[[7L]], 0L)
})

test_that("duplicate source records remain ambiguous even when their properties agree", {
  fixture <- mapping_test_fixture()
  mapping_test_transform_tables(function(tables) {
    if ("observers" %in% names(tables$data) && "id" %in% names(tables$data$observers)) {
      data <- tables$data$observers
      aliases <- attr(data, "reference_aliases")
      tables$data$observers <- rbind(data, data[1L, , drop = FALSE])
      attr(tables$data$observers, "reference_aliases") <- aliases
    }
    tables
  })
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "observers")
  result <- map_reference_values(prepared, "observers", "former-alice", source_lookup = "id")
  expect_identical(result$status, "ambiguous")
  expect_identical(result$stage, "source")
  expect_length(result$source_rows[[1L]], 2L)
  expect_length(result$target_rows[[1L]], 0L)
  expect_true(is.na(result$target_value))
})

test_that("exact names take precedence and normalized collisions keep all candidates", {
  fixture <- mapping_test_fixture()
  mapping_test_transform_tables(function(tables) {
    if ("observers" %in% names(tables$data) && "code" %in% names(tables$data$observers)) {
      tables$data$observers <- rbind(tables$data$observers,
        data.frame(code = "OTHER", name = "Example Alice"))
    }
    tables
  })
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "observers")
  result <- map_reference_values(prepared, "observers", c("Alice Example", "Alice, Example"), source_lookup = "name")
  expect_identical(result$status, c("resolved", "ambiguous"))
  expect_identical(result$target_value, c("ALICE", NA_character_))
  expect_setequal(result$target_candidates[[2L]], c("ALICE", "OTHER"))
  expect_length(result$target_rows[[2L]], 2L)
})

test_that("matching target rows remain ambiguous even when their returned codes agree", {
  fixture <- mapping_test_fixture()
  mapping_test_transform_tables(function(tables) {
    if ("observers" %in% names(tables$data) && "code" %in% names(tables$data$observers)) {
      data <- tables$data$observers
      tables$data$observers <- rbind(data, data[1L, , drop = FALSE])
    }
    tables
  })
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "observers")
  result <- map_reference_values(prepared, "observers", "Alice Example", source_lookup = "name")
  expect_identical(result$status, "ambiguous")
  expect_identical(result$target_candidates[[1L]], c("ALICE", "ALICE"))
  expect_true(is.na(result$target_value))
})

test_that("CSV duplicates, conflicts and literal values retain their distinct meanings", {
  fixture <- mapping_test_fixture()
  writeLines(c("source_value,target_value", "001,001", "001,001", "1,002", "NA,001", "FALSE,002", "0,001",
    "conflict,001", "conflict,002", "same-canonical,001", "same-canonical,OLD-STATION", "unmatched,absent"), fixture$csv)
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "stations")
  result <- map_reference_values(prepared, "stations", c("001", "1", "NA", "FALSE", "0", "conflict", "same-canonical", "unmatched"))
  expect_identical(result$target_value, c("001", "002", "001", "002", "001", rep(NA_character_, 3)))
  expect_identical(result$status, c(rep("resolved", 5), "ambiguous", "ambiguous", "no_match"))
  expect_identical(result$stage[6:8], c("correspondence", "correspondence", "target"))
  expect_setequal(result$correspondence_values[[6L]], c("001", "002"))
  expect_setequal(result$correspondence_values[[7L]], c("001", "OLD-STATION"))
  expect_length(result$correspondence_values[[1L]], 1L)
  expect_error(map_reference_values(prepared, "stations", 1), "expected text")
  expect_error(map_reference_values(prepared, "stations", FALSE), "expected text")
})

test_that("malformed correspondence files fail during preparation rather than YAML reading", {
  fixture <- mapping_test_fixture()
  mapping <- mapping_test_read(fixture)
  malformed <- list(
    header = c("source,target", "a,001"),
    extra_column = c("source_value,target_value,note", "a,001,extra"),
    duplicate_column = c("source_value,source_value", "a,001"),
    empty_source = c("source_value,target_value", ",001"),
    empty_target = c("source_value,target_value", "a,"),
    short_row = c("source_value,target_value", "a"),
    long_row = c("source_value,target_value", "a,001,extra"),
    unclosed_quote = c("source_value,target_value", 'a,"001')
  )
  for (scenario in names(malformed)) {
    writeLines(malformed[[scenario]], fixture$csv)
    expect_s3_class(read_mapping_configuration(fixture$path), "mapping_configuration")
    expect_error(prepare_mapping_references(mapping, fixture$configurations, targets = "stations"), info = scenario)
  }
  unlink(fixture$csv)
  expect_error(prepare_mapping_references(mapping, fixture$configurations, targets = "stations"), "file does not exist")
})

test_that("CSV byte order marks and missing final newlines preserve textual codes", {
  fixture <- mapping_test_fixture()
  writeChar(paste0("\ufeffsource_value,target_value\n001,001"), fixture$csv, eos = NULL, useBytes = TRUE)
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "stations")
  expect_identical(map_reference_values(prepared, "stations", "001")$target_value, "001")
  writeLines("source_value,target_value", fixture$csv)
  empty <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "stations")
  expect_identical(map_reference_values(empty, "stations", "001")$status, "no_match")
})

test_that("CSV and unchanged-input mappings never load source catalogue data", {
  fixture <- mapping_test_fixture()
  retrieve <- get_reference_tables
  calls <- list()
  source_url <- fixture$configurations$source$catalogues$code_lists$source$url
  local_mocked_bindings(get_reference_tables = function(ids, source, offline = FALSE) {
    if (identical(source$url, source_url)) stop("Unexpected source catalogue retrieval")
    calls[[length(calls) + 1L]] <<- ids
    retrieve(ids, source, offline)
  })
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations, targets = "stations")
  expect_identical(calls, list("stations"))
  expect_identical(map_reference_values(prepared, "stations", "station-north")$target_value, "001")
  expect_false(dir.exists(fixture$configurations$source$catalogues$code_lists$source$cache))

  fixture$definition$mappings <- list(observers = fixture$definition$mappings$observers[[2L]])
  direct <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations)
  expect_identical(calls, list("stations", "observers"))
  expect_identical(map_reference_values(direct, "observers", "Example, Alice")$target_value, "ALICE")
})

test_that("preparation shares source tables and indexes across rebound mappings", {
  fixture <- mapping_test_fixture()
  fixture$definition$mappings$stations <- list(source = list(table = "observers", property = "name"),
    lookup = "name", property = "code")
  fixture$configurations$target$catalogues$code_lists$tables$stations$name <- list(matching = "name", ignore_case = TRUE)
  retrieve <- get_reference_tables
  index <- reference_index
  calls <- indexes <- list()
  local_mocked_bindings(get_reference_tables = function(ids, source, offline = FALSE) {
    calls[[length(calls) + 1L]] <<- ids
    retrieve(ids, source, offline)
  }, reference_index = function(data, column = NULL, matching = "exact", ignore_case = TRUE) {
    indexes[[length(indexes) + 1L]] <<- column
    index(data, column, matching, ignore_case)
  })
  prepared <- prepare_mapping_references(mapping_test_read(fixture), fixture$configurations)
  expect_length(calls, 2L)
  expect_setequal(unlist(calls), c("observers", "stations"))
  expect_equal(sum(unlist(indexes) == "id"), 1L)
  expect_length(indexes, 3L)
  expect_identical(map_reference_values(prepared, "observers", "person-alice", source_lookup = "id")$target_value, "ALICE")
  expect_identical(map_reference_values(prepared, "stations", "person-alice")$status, "no_match")
  expect_length(calls, 2L)
})

test_that("unselected mappings and empty selections need no unnecessary resources", {
  fixture <- mapping_test_fixture()
  fixture$definition$mappings$stations$file <- "unavailable.csv"
  mapping <- mapping_test_read(fixture)
  prepared <- prepare_mapping_references(mapping, fixture$configurations, targets = "observers")
  expect_identical(map_reference_values(prepared, "observers", "person-alice", source_lookup = "id")$status, "resolved")
  empty <- map_reference_values(prepared, "observers", character(), source_lookup = "name")
  expect_equal(nrow(empty), 0L)
  expect_type(empty$target_value, "character")
  local_mocked_bindings(get_reference_tables = function(...) stop("Unexpected catalogue retrieval"))
  none <- prepare_mapping_references(mapping, fixture$configurations, targets = character())
  expect_length(none$references, 0L)
  expect_length(none$correspondences, 0L)
})

test_that("missing source and target property columns are structural preparation errors", {
  fixture <- mapping_test_fixture()
  mapping <- mapping_test_read(fixture)
  for (side in c("source", "target")) local({
    mapping_test_transform_tables(function(tables) {
      if (side == "source" && "observers" %in% names(tables$data) && "id" %in% names(tables$data$observers))
        tables$data$observers$name <- NULL
      if (side == "target" && "observers" %in% names(tables$data) && "code" %in% names(tables$data$observers))
        tables$data$observers$code <- NULL
      tables
    })
    expected <- if (side == "source") "observers.name" else "observers.code"
    expect_error(prepare_mapping_references(mapping, fixture$configurations, targets = "observers"), expected, fixed = TRUE)
  })
})

test_that("results preserve configuration, catalogue, alias and correspondence provenance", {
  fixture <- mapping_test_fixture()
  mapping <- mapping_test_read(fixture)
  prepared <- prepare_mapping_references(mapping, fixture$configurations)
  result <- map_reference_values(prepared, "stations", "station-north")
  provenance <- attr(result, "provenance")
  expect_identical(provenance$configuration$sha256, digest::digest(file = fixture$path, algo = "sha256"))
  expect_identical(provenance$correspondence$sha256, digest::digest(file = fixture$csv, algo = "sha256"))
  expect_identical(provenance$target_catalogue$table$id, "stations")
  expect_identical(provenance$target_catalogue$table$sha256,
    digest::digest(file = provenance$target_catalogue$path, algo = "sha256"))
  expect_identical(provenance$target_catalogue$aliases$table$sha256,
    digest::digest(file = provenance$target_catalogue$aliases$path, algo = "sha256"))
  expect_identical(provenance$catalogues$source$configuration, "source")
  expect_null(provenance$source_catalogue)
  rebound <- map_reference_values(prepared, "observers", "former-alice", source_lookup = "id")
  expect_identical(attr(rebound, "provenance")$source_catalogue$table$id, "observers")
  expect_identical(attr(rebound, "provenance")$source_catalogue$aliases$table$id, "_aliases_")
  reused <- prepare_mapping_references(mapping, fixture$configurations, offline = TRUE)
  expect_identical(map_reference_values(reused, "stations", "station-north"), result)
})
