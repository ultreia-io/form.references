# form.references

[![Build](https://img.shields.io/github/actions/workflow/status/ultreia-io/form.references/Build.yaml?branch=develop&label=Build&logo=githubactions&logoColor=white)](https://github.com/ultreia-io/form.references/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Website](https://img.shields.io/badge/website-documentation-blue?logo=readthedocs&logoColor=white)](https://ultreia-io.github.io/form.references/index.html)
[![License](https://img.shields.io/badge/license-LGPL--2.1-blue?logo=gnu&logoColor=white)](https://github.com/ultreia-io/form.references/blob/main/LICENSE)

[![Latest release](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fapi.github.com%2Frepos%2Fultreia-io%2Fform.references%2Freleases%2Flatest&query=%24.tag_name&label=Latest%20release&color=blue&logo=github&logoColor=white)](https://github.com/ultreia-io/form.references/releases/latest)
![CRAN release](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fcrandb.r-pkg.org%2Fform.references&query=%24.Version&label=CRAN%20release&color=brightgreen&logo=r&logoColor=white)

[![Coverage develop](https://img.shields.io/codecov/c/github/ultreia-io/form.references/develop?label=coverage%20develop&logo=codecov)](https://app.codecov.io/gh/ultreia-io/form.references/tree/develop)
[![Coverage main](https://img.shields.io/codecov/c/github/ultreia-io/form.references/main?label=coverage%20main&logo=codecov)](https://app.codecov.io/gh/ultreia-io/form.references/tree/main)

## About this project

> `form.references` supplies reference data and shared lookup operations to consumers of form models.

Read the [Terminology](https://ultreia-io.github.io/form.references/articles/terminology.html) for the concepts used throughout this documentation.

A reference catalogue is a collection of tables containing values that form consumers use to interpret or check data.

A form model identifies the catalogue, table and lookup used by each field that needs reference data.

`form.references` retrieves the required tables and matches text values using aliases and configurable case sensitivity.

Exporters, importers and validators can share this configuration and reuse the same reference data.

## Related packages

- [form.model](https://github.com/ultreia-io/form.model) defines forms, their layouts and constraints.
- [form.validation](https://github.com/ultreia-io/form.validation) validates submitted forms and produces reports.
- [form.tools](https://github.com/ultreia-io/form.tools) splits forms using their layouts and keys.

## Installation

The first CRAN submission is pending. Once available, install the latest version from CRAN:

``` r
# Install form.references from CRAN.
install.packages("form.references")
```

Alternatively, install the latest GitHub release:

``` r
# Install the package installer once.
install.packages("pak")
# Install the latest released form.references package.
pak::pak("ultreia-io/form.references@*release")
```

`pak` downloads the package and installs its dependencies automatically.

For the development version:

``` r
# Install form.references from the development branch.
pak::pak("ultreia-io/form.references@develop")
```

## Getting started

Follow the [Getting started guide](https://ultreia-io.github.io/form.references/articles/form.references.html) for the runnable survey example and downloadable files.

## Guides

- [API reference](https://ultreia-io.github.io/form.references/reference/index.html)
- [Contributing](https://ultreia-io.github.io/form.references/CONTRIBUTING.html) and [maintenance](https://ultreia-io.github.io/form.references/DEVELOPMENT.html)
