# Serve packaged catalogue bytes through mocked curl calls, without network access.
mock_reference_downloads <- function(failure = NULL, envir = parent.frame()) {
  fixture_directory <- system.file("examples", "survey", "catalogue", package = "form.references")
  requests <- new.env(parent = emptyenv())
  requests$batches <- list()
  requests$files <- character()
  copy_response <- function(url, destination) {
    resource_path <- sub("https://example.org/", "", url, fixed = TRUE)
    file.copy(file.path(fixture_directory, resource_path), destination, overwrite = TRUE)
    requests$files <- c(requests$files, destination)
    invisible(resource_path)
  }
  local_mocked_bindings(
    curl_fetch_disk = function(url, path, handle) {
      copy_response(url, path)
      if (identical(failure, "single-transport")) stop("simulated connection failure")
      list(status_code = if (identical(failure, "single-http")) 404L else 200L)
    },
    multi_download = function(urls, destfiles, progress) {
      requests$batches <- c(requests$batches, list(vapply(urls, function(handle)
        curl::handle_data(handle)$url, character(1))))
      for (i in seq_along(urls)) copy_response(curl::handle_data(urls[[i]])$url, destfiles[[i]])
      if (identical(failure, "transport")) stop("simulated connection failure")
      if (identical(failure, "checksum")) writeLines("corrupted", destfiles[[length(destfiles)]])
      data.frame(success = rep(!identical(failure, "download"), length(urls)),
        status_code = rep(if (identical(failure, "http")) 503L else 200L, length(urls)))
    }, .package = "curl", .env = envir)
  requests
}

test_that("remote tables retain requested order and can be reused offline", {
  requests <- mock_reference_downloads()
  source <- reference_source("https://example.org/", withr::local_tempdir(), catalogue = "code_lists")
  reference_tables <- get_reference_tables(c("stations", "observers"), source)
  expect_named(reference_tables$data, c("stations", "observers"))
  expect_identical(reference_tables$data$stations$code, c("001", "002", "ST01", "ST02"))
  expect_identical(reference_tables$data$observers$code, c("ALICE", "BOB", "observer-1", "observer-2"))
  expect_length(requests$batches, 1L)
  expect_length(requests$batches[[1L]], 2L)
  expect_false(any(file.exists(requests$files)))
  expect_identical(get_reference_tables(c("stations", "observers"), source, offline = TRUE)$data,
    reference_tables$data)
})

test_that("remote downloads respect the configured batch size", {
  requests <- mock_reference_downloads()
  source <- reference_source("https://example.org/", withr::local_tempdir(),
    catalogue = "code_lists", max_connections = 1L)
  reference_tables <- get_reference_tables(c("observers", "stations"), source)
  expect_identical(lengths(requests$batches), c(1L, 1L))
  expect_named(reference_tables$data, c("observers", "stations"))
})

test_that("failed remote batches do not publish their staged tables", {
  scenarios <- c(transport = "Could not download reference resources",
    download = "Could not download reference resource", http = "HTTP 503", checksum = "SHA-256 mismatch")
  for (failure in names(scenarios)) local({
    requests <- mock_reference_downloads(failure)
    source <- reference_source("https://example.org/", withr::local_tempdir(), catalogue = "code_lists")
    expect_error(get_reference_tables(c("stations", "observers"), source), scenarios[[failure]])
    expect_false(any(file.exists(requests$files)))
    expect_error(get_reference_tables("stations", source, offline = TRUE), "Verified cache unavailable")
    expect_error(get_reference_tables("observers", source, offline = TRUE), "Verified cache unavailable")
  })
})

test_that("failed single HTTP requests clean up temporary files", {
  for (failure in c("single-transport", "single-http")) local({
    requests <- mock_reference_downloads(failure)
    source <- reference_source("https://example.org/", withr::local_tempdir(), catalogue = "code_lists")
    expected <- if (failure == "single-http") "HTTP 404" else "Could not download reference resource"
    expect_error(read_reference_catalogue(source), expected)
    expect_false(any(file.exists(requests$files)))
    expect_error(read_reference_catalogue(source, offline = TRUE), "No cached catalogue")
  })
})

test_that("cache replacement preserves the old file when rename fails", {
  for (failure in c("initial", "backup", "install")) local({
    cache_directory <- withr::local_tempdir()
    staged_path <- file.path(cache_directory, "staged")
    target_path <- file.path(cache_directory, "target")
    writeLines("new", staged_path)
    writeLines("old", target_path)
    original_rename <- base::file.rename
    rename_count <- 0L
    local_mocked_bindings(file.rename = function(from, to) {
      rename_count <<- rename_count + 1L
      if (rename_count == 1L || (failure == "backup" && rename_count == 2L) ||
          (failure == "install" && rename_count == 3L)) return(FALSE)
      original_rename(from, to)
    }, .package = "base")
    if (failure == "initial") {
      replace_cache_file(staged_path, target_path)
      expect_identical(readLines(target_path), "new")
      expect_identical(list.files(cache_directory), "target")
    } else {
      expected <- if (failure == "backup") "Cannot preserve" else "Cannot install"
      expect_error(replace_cache_file(staged_path, target_path), expected)
      expect_identical(readLines(target_path), "old")
      expect_identical(readLines(staged_path), "new")
      expect_setequal(list.files(cache_directory), c("staged", "target"))
    }
  })
})

# Make a mutable copy; corruption must never touch the installed examples.
copy_reference_catalogue <- function(envir = parent.frame()) {
  directory <- withr::local_tempdir(.local_envir = envir)
  fixture_directory <- system.file("examples", "survey", "catalogue", package = "form.references")
  file.copy(fixture_directory, directory, recursive = TRUE)
  file.path(directory, "catalogue")
}

test_that("malformed publication metadata cannot replace the offline catalogue", {
  scenarios <- list(missing = function(metadata) { metadata$sha256 <- NULL; metadata },
    timestamp = function(metadata) { metadata$published_at <- "yesterday"; metadata },
    checksum = function(metadata) { metadata$sha256 <- "invalid"; metadata })
  for (scenario in names(scenarios)) local({
    fixture_directory <- copy_reference_catalogue()
    source <- reference_source(fixture_directory, withr::local_tempdir(), catalogue = "code_lists")
    original_catalogue <- read_reference_catalogue(source)
    metadata_path <- file.path(fixture_directory, "code_lists.yaml")
    yaml::write_yaml(scenarios[[scenario]](yaml::read_yaml(metadata_path)), metadata_path)
    expected <- c(missing = "must declare", timestamp = "UTC timestamp", checksum = "SHA-256")
    expect_error(read_reference_catalogue(source), expected[[scenario]])
    expect_identical(read_reference_catalogue(source, offline = TRUE), original_catalogue)
  })
})

test_that("malformed catalogue columns and IDs cannot replace verified data", {
  scenarios <- list(missing = function(catalogue) { catalogue$domain <- NULL; catalogue },
    duplicate_id = function(catalogue) { catalogue$id[2] <- catalogue$id[1]; catalogue },
    duplicate_column = function(catalogue) { names(catalogue)[2] <- names(catalogue)[1]; catalogue })
  for (scenario in names(scenarios)) local({
    fixture_directory <- copy_reference_catalogue()
    source <- reference_source(fixture_directory, withr::local_tempdir(), catalogue = "code_lists")
    original_catalogue <- read_reference_catalogue(source)
    catalogue_path <- file.path(fixture_directory, "code_lists.csv")
    catalogue_data <- utils::read.csv(catalogue_path, colClasses = "character", check.names = FALSE)
    utils::write.csv(scenarios[[scenario]](catalogue_data), catalogue_path, row.names = FALSE)
    metadata_path <- file.path(fixture_directory, "code_lists.yaml")
    metadata <- yaml::read_yaml(metadata_path)
    metadata$sha256 <- digest::digest(file = catalogue_path, algo = "sha256")
    yaml::write_yaml(metadata, metadata_path)
    expected <- c(missing = "missing columns", duplicate_id = "unique lowercase", duplicate_column = "duplicate column")
    expect_error(read_reference_catalogue(source), expected[[scenario]])
    expect_identical(read_reference_catalogue(source, offline = TRUE), original_catalogue)
  })
})
