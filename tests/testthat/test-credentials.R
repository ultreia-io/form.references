test_that("dynamic headers are refreshed for each request and hidden when printed", {
  header_calls <- 0L
  sent_headers <- list()
  source <- reference_source("https://example.org/", withr::local_tempdir(), headers = function() {
    header_calls <<- header_calls + 1L
    c(Authorization = paste("Bearer private-token", header_calls))
  })
  expect_identical(header_calls, 0L)
  source_output <- capture.output(print(source))
  expect_identical(header_calls, 0L)
  expect_false(any(grepl("private-token", source_output, fixed = TRUE)))
  original_setheaders <- curl::handle_setheaders
  local_mocked_bindings(handle_setheaders = function(handle, ..., .list) {
    sent_headers[[length(sent_headers) + 1L]] <<- .list
    original_setheaders(handle, .list = .list)
  }, .package = "curl")
  reference_request_handle(source, "first.csv")
  reference_request_handle(source, "second.csv")
  expect_identical(sent_headers, list(list(Authorization = "Bearer private-token 1"),
    list(Authorization = "Bearer private-token 2")))
})

test_that("invalid headers and transport errors do not reveal secrets", {
  secret <- "private-secret-value"
  source <- reference_source("https://example.org/", withr::local_tempdir(),
    headers = function() c(Authorization = paste0(secret, "\r\nInjected: value")))
  header_error <- tryCatch(reference_request_handle(source, "table.csv"), error = identity)
  expect_s3_class(header_error, "error")
  expect_match(conditionMessage(header_error), "named HTTP header strings")
  expect_false(grepl(secret, conditionMessage(header_error), fixed = TRUE))
  source <- reference_source("https://example.org/", withr::local_tempdir(),
    headers = c(Authorization = secret))
  local_mocked_bindings(curl_fetch_disk = function(...) stop(paste("Transport failed", secret)),
    .package = "curl")
  transport_error <- tryCatch(read_reference_catalogue(source), error = identity)
  expect_s3_class(transport_error, "error")
  expect_match(conditionMessage(transport_error), "Could not download reference resource")
  expect_false(grepl(secret, conditionMessage(transport_error), fixed = TRUE))
})

test_that("password prompts handle success, cancellation and empty replies", {
  skip_if_not_installed("askpass")
  withr::local_envvar(c(FORM_REFERENCE_PROMPT_PASSWORD = NA))
  configuration_path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c('schema_version: "1.0.0"', 'catalogues:', '  private:',
    '    source:', '      url: https://example.org/', '      cache: cache',
    '      user: reader', '      password: !env FORM_REFERENCE_PROMPT_PASSWORD',
    '    tables:', '      contacts:', '        email: {matching: exact, ignore_case: true}'),
    configuration_path)
  for (reply in list(NULL, "", NA_character_, "private-prompt-secret")) local({
    local_mocked_bindings(askpass = function(...) reply, .package = "askpass")
    if (is.null(reply)) {
      expect_error(read_reference_configuration(configuration_path, prompt = TRUE), "Password entry cancelled")
    } else if (is.na(reply) || !nzchar(reply)) {
      expect_error(read_reference_configuration(configuration_path, prompt = TRUE), "Password must not be empty")
    } else {
      reference_configuration <- read_reference_configuration(configuration_path, prompt = TRUE)
      source <- reference_configuration$catalogues$private$source
      expected_header <- paste("Basic", base64enc::base64encode(charToRaw(paste0("reader:", reply))))
      expect_identical(source$headers[["Authorization"]], expected_header)
      expect_false(any(grepl(reply, capture.output(print(source)), fixed = TRUE)))
    }
  })
})
