test_that('code lists preserve text and remain available offline', {
  cache <- withr::local_tempdir()
  source <- reference_source(system.file('examples', 'survey', 'catalogue', 'code_lists', package = 'form.references'), cache)
  result <- get_reference_tables(c('stations', 'observers', 'stations'), source)
  expect_identical(names(result$data), c('stations', 'observers'))
  expect_identical(result$data$stations$code, c('001', '002', 'ST01', 'ST02'))
  expect_identical(result$data$observers$code, c('ALICE', 'BOB', 'observer-1', 'observer-2'))
  expect_true(all(file.exists(result$paths)))
  expect_identical(get_reference_tables(c('stations', 'observers'), source, offline = TRUE)$data, result$data)
  expect_error(get_reference_tables('missing', source), 'absent from catalogue')
})

test_that('the configured catalogue determines available tables', {
  examples <- system.file('examples', 'survey', 'catalogue', package = 'form.references')
  cache <- withr::local_tempdir()
  registry <- reference_source(file.path(examples, 'registries'), cache)
  code_lists <- reference_source(file.path(examples, 'code_lists'), cache)
  expect_identical(names(get_reference_tables(c("observers", "stations"), code_lists)$data), c('observers', 'stations'))
  contacts <- get_reference_tables("contacts", registry)
  expect_identical(names(contacts$data), c('contacts'))
  expect_identical(contacts$data$contacts$email, c('alice@example.org', 'bob@example.org', 'survey@example.org'))
  expect_identical(get_reference_tables('contacts', registry, offline = TRUE)$data, contacts$data)
  expect_error(get_reference_tables('stations', registry), 'absent from catalogue')
  expect_error(get_reference_tables('contacts', code_lists), 'absent from catalogue')
})

test_that('missing and corrupted cache entries are rejected offline', {
  source <- reference_source(system.file('examples', 'survey', 'catalogue', 'code_lists', package = 'form.references'), withr::local_tempdir())
  expect_error(get_reference_tables('stations', source, offline = TRUE), 'No cached catalogue')
  result <- get_reference_tables('stations', source)
  writeLines('corrupted', result$paths[['stations']])
  expect_error(get_reference_tables('stations', source, offline = TRUE), 'Verified cache unavailable')
  expect_identical(get_reference_tables('stations', source)$data$stations$code, c('001', '002', 'ST01', 'ST02'))
})

test_that('checksum failures do not publish invalid downloaded files', {
  directory <- withr::local_tempdir()
  examples <- system.file('examples', 'survey', 'catalogue', package = 'form.references')
  file.copy(examples, directory, recursive = TRUE)
  copy <- file.path(directory, 'catalogue', 'code_lists')
  writeLines('invalid data', file.path(copy, 'stations.csv'))
  source <- reference_source(copy, file.path(directory, 'cache'))
  expect_error(get_reference_tables('stations', source), 'SHA-256 mismatch')
  expect_error(get_reference_tables('stations', source, offline = TRUE), 'Verified cache unavailable')
})

test_that('source configuration rejects unsafe inputs and hides credentials', {
  cache <- withr::local_tempdir()
  expect_error(reference_source('https://name:secret@example.org/data', cache), 'without credentials')
  expect_error(reference_source('https://example.org/data?secret=x', cache), 'without credentials')
  expect_error(reference_source('https://example.org/', cache, timeout = 0), 'positive')
  expect_error(reference_source('https://example.org/', cache, max_connections = 1.5), 'whole number')
  source <- reference_source('https://example.org/', cache, headers = c(Authorization = 'secret-value'))
  expect_false(any(grepl('secret-value', capture.output(print(source)), fixed = TRUE)))
})

test_that('an invalid catalogue cannot replace the verified offline catalogue', {
  directory <- withr::local_tempdir()
  examples <- system.file('examples', 'survey', 'catalogue', package = 'form.references')
  file.copy(examples, directory, recursive = TRUE)
  copy <- file.path(directory, 'catalogue', 'code_lists')
  source <- reference_source(copy, file.path(directory, 'cache'))
  before <- read_reference_catalogue(source)
  catalogue_path <- file.path(copy, 'catalogue.csv')
  catalogue <- utils::read.csv(catalogue_path, colClasses = 'character', fileEncoding = 'UTF-8-BOM')
  catalogue$csv_path[[1]] <- '../outside.csv'
  utils::write.csv(catalogue, catalogue_path, row.names = FALSE)
  metadata_path <- file.path(copy, 'catalogue.yaml')
  metadata <- yaml::read_yaml(metadata_path)
  metadata$sha256 <- digest::digest(file = catalogue_path, algo = 'sha256')
  yaml::write_yaml(metadata, metadata_path)
  expect_error(read_reference_catalogue(source), 'relative paths without traversal')
  expect_identical(read_reference_catalogue(source, offline = TRUE), before)
})

test_that('YAML authentication uses environment secrets without printing them', {
  directory <- withr::local_tempdir()
  config <- file.path(directory, 'source.yaml')
  withr::local_envvar(c(FORM_TEST_PASSWORD = 'private-test-value'))
  configuration <- yaml::read_yaml(system.file("examples", "survey", "references.yaml", package = "form.references"))
  configuration$catalogues <- configuration$catalogues['registries']
  configuration$catalogues$registries$source <- list(url = 'https://example.org/data/',
    cache = file.path(directory, 'cache'), user = 'reader', password = 'PLACEHOLDER')
  yaml::write_yaml(configuration, config)
  writeLines(sub('password: PLACEHOLDER', 'password: !env FORM_TEST_PASSWORD',
    readLines(config), fixed = TRUE), config)
  source <- read_reference_configuration(config)$catalogues$registries$source
  expect_true(startsWith(source$headers[['Authorization']], 'Basic '))
  expect_false(any(grepl('private-test-value', capture.output(print(source)), fixed = TRUE)))
  writeLines(sub('https://', 'http://', readLines(config), fixed = TRUE), config)
  expect_error(read_reference_configuration(config), 'requires an HTTPS')
})
