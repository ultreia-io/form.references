test_that("case matching, aliases and name fallback retain matching rows", {
  values <- c("Alpha", "ALPHA", "Tony Chemit", "old")
  aliases <- c(previous = "Alpha", old = "Alpha")
  index <- reference_index(values, aliases = aliases, matching = "name")
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
