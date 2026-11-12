test_that("case matching, aliases and name fallback retain matching rows", {
  values <- c("Alpha", "ALPHA", "Tony Chemit", "old")
  aliases <- c(previous = "Alpha", old = "Alpha")
  index <- build_reference_index(values, aliases = aliases, matching = "name")
  expect_identical(reference_match_rows(index, c("alpha", "previous", "Chemit Tony", "old", "missing")),
    list(c(1L, 2L), c(1L, 2L), 3L, 4L, integer()))
  strict <- reference_index(values, ignore_case = FALSE)
  expect_identical(reference_match_rows(strict, c("alpha", "Alpha")), list(integer(), 1L))
  expect_identical(index$data, values)
})

test_that("text lookup retains leading zeros and missing results", {
  index <- reference_index(c("001", "1", "", "001"))
  expect_identical(reference_match_rows(index, list("001", "1", "", NULL, NA, NA_character_)),
    list(c(1L, 4L), 2L, 3L, integer(), integer(), integer()))
})

test_that("lookup inputs must be plain text", {
  expect_error(reference_index(list(list("A"))), "Malformed reference keys")
  expect_error(reference_index(factor("A")), "Malformed reference keys")
  expect_error(reference_match_rows(reference_index("A"), list(list("A"))),
    "Malformed submitted reference keys")
})

test_that("data-frame lookups select the named column and retain original rows", {
  reference_data <- data.frame(code = c("001", "002", "001"), label = c("First", "Second", "Third"))
  reference_lookup <- reference_index(reference_data, column = "code")
  expect_identical(reference_match_rows(reference_lookup, c("001", "002", "First")),
    list(c(1L, 3L), 2L, integer()))
  expect_identical(reference_lookup$data, reference_data)
  expect_error(reference_index(reference_data), "Select one existing lookup column")
  expect_error(reference_index(reference_data, column = "missing"), "Select one existing lookup column")
  names(reference_data)[2] <- "code"
  expect_error(reference_index(reference_data, column = "code"), "Select one existing lookup column")
})
