test_that("missing lookup definitions fail before retrieving tables", {
  form_model <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
  configuration_path <- system.file("examples", "survey", "references.yaml", package = "form.references")
  for (missing in c("catalogue", "table", "lookup")) local({
    reference_configuration <- read_reference_configuration(configuration_path)
    if (missing == "catalogue") reference_configuration$catalogues$code_lists <- NULL
    if (missing == "table") reference_configuration$catalogues$code_lists$tables$stations <- NULL
    if (missing == "lookup") reference_configuration$catalogues$code_lists$tables$stations$code <- NULL
    local_mocked_bindings(get_reference_tables = function(...) stop("Unexpected retrieval"))
    expect_error(prepare_model_references(form_model, reference_configuration),
      "Unknown reference lookup: code_lists/stations/code", fixed = TRUE)
  })
})

test_that("missing table columns produce an explicit binding error", {
  form_model <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
  reference_configuration <- read_reference_configuration(system.file(
    "examples", "survey", "references.yaml", package = "form.references"))
  cache_directory <- withr::local_tempdir()
  for (catalogue in names(reference_configuration$catalogues))
    reference_configuration$catalogues[[catalogue]]$source$cache <- cache_directory
  original_retrieval <- get_reference_tables
  local_mocked_bindings(get_reference_tables = function(...) {
    reference_tables <- original_retrieval(...)
    if ("stations" %in% names(reference_tables$data)) reference_tables$data$stations$code <- NULL
    reference_tables
  })
  expect_error(prepare_model_references(form_model, reference_configuration),
    "Missing lookup column: stations.code", fixed = TRUE)
})

test_that("forms without references need no catalogue retrieval", {
  form_definition <- form.model::form_definition(
    form.model::read_form_model("examples/survey/form.yaml", "form.model"))
  for (sheet in names(form_definition$sheets))
    for (field in names(form_definition$sheets[[sheet]]$fields))
      form_definition$sheets[[sheet]]$fields[[field]]$reference <- NULL
  form_model <- form.model::as_form_model(form_definition)
  reference_configuration <- read_reference_configuration(system.file(
    "examples", "survey", "references.yaml", package = "form.references"))
  local_mocked_bindings(get_reference_tables = function(...) stop("Unexpected retrieval"))
  prepared_references <- prepare_model_references(form_model, reference_configuration)
  expect_length(prepared_references$references, 0L)
  expect_identical(form.model::form_definition(prepared_references$model), form_definition)
  expect_null(attr(form_model, "reference_lookups"))
})
