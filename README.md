# form.references

[![Build](https://github.com/ultreia-io/form.references/actions/workflows/Build.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/form.references/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Test Coverage](https://github.com/ultreia-io/form.references/actions/workflows/TestCoverage.yaml/badge.svg?branch=main)](https://github.com/ultreia-io/form.references/actions/workflows/TestCoverage.yaml?query=branch%3Amain)
[![Coverage](https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fultreia-io.github.io%2Fform.references%2Fcoverage%2Fcoverage.xml&query=round%28%2Fcoverage%2F%40line-rate+%2A+1000%29+div+10&label=coverage&suffix=%25&color=brightgreen)](https://ultreia-io.github.io/form.references/articles/coverage.html)
[![Website](https://img.shields.io/badge/website-documentation-blue)](https://ultreia-io.github.io/form.references/)
[![Latest
release](https://img.shields.io/github/v/release/ultreia-io/form.references?display_name=tag&sort=semver)](https://github.com/ultreia-io/form.references/releases/latest)
[![License](https://img.shields.io/github/license/ultreia-io/form.references)](https://github.com/ultreia-io/form.references/blob/main/LICENSE)

Reference data downloads and verified caches for form models.

Download only the references required by a form model, verify their checksums, and reuse a local cache.

## Installation

Install the latest released version directly from GitHub:

``` r
install.packages("pak") # Once
pak::pak("ultreia-io/form.references@*release")
```

`pak` downloads the package and installs its dependencies automatically.

For the development version:

``` r
pak::pak("ultreia-io/form.references@develop")
```

## Load the survey references

```r
library(form.references)

model <- form.model::load_form_model(
  "survey", system.file("examples", package = "form.model")
)
source <- reference_source(
  url = system.file("examples", package = "form.references"),
  cache = "cache/survey"
)
references <- get_form_code_lists(model, source)
references$data$stations$code # "001" "002"

registry_source <- reference_source(
  url = system.file("examples", package = "form.references"),
  cache = "cache/survey",
  catalogue = "registries"
)
registries <- get_form_registries(model, registry_source)
registries$data$contacts$email # "alice@example.org" "bob@example.org"
```

For HTTP downloads, use the published example endpoint as `url`:

```r
source <- reference_source(
  url = "https://ultreia-io.github.io/form.references/examples/",
  cache = "cache/survey"
)
references <- get_form_code_lists(model, source)
registry_source <- reference_source(
  url = "https://ultreia-io.github.io/form.references/examples/",
  cache = "cache/survey",
  catalogue = "registries"
)
registries <- get_form_registries(model, registry_source)
```

The website must be deployed before using that URL. Installed examples work offline immediately.

Catalogue IDs are lowercase and match the model exactly. File paths and CSV values retain their original case.

Use `get_code_lists(c("stations", "observers"), source)` without a model.

Use `get_registries(ids, registry_source)` without a model.
The public survey source needs no headers. Private sources use the same catalogue format and accept authentication headers.
Each catalogue entry identifies one registry table. The form field's `registry.column` selects a column within it.

Use `offline = TRUE` to load verified cached files without contacting the source.

Cache folders identify the source and catalogue. CSV filenames contain the reference ID and a short checksum suffix.

Missing HTTP code lists download up to four at a time. Set `max_connections` in
`reference_source()` to change that limit.

For Apache Basic authentication, follow the [private registry example](https://ultreia-io.github.io/form.references/articles/form.references.html#authenticate-a-private-registry-source).

## Documentation

- [Get started](https://ultreia-io.github.io/form.references/articles/form.references.html)
- [Function reference](https://ultreia-io.github.io/form.references/reference/index.html)
- [Contributing](https://ultreia-io.github.io/form.references/CONTRIBUTING.html)
- [Maintainer guide](https://ultreia-io.github.io/form.references/DEVELOPMENT.html)
- [Security policy](https://ultreia-io.github.io/form.references/SECURITY.html)
- [Code of conduct](https://ultreia-io.github.io/form.references/CODE_OF_CONDUCT.html)

## License

[LGPL-2.1](https://github.com/ultreia-io/form.references/blob/main/LICENSE).
