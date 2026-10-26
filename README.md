# form.references

[![Build](https://github.com/ultreia-io/form.references/actions/workflows/Build.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/form.references/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Test Coverage](https://github.com/ultreia-io/form.references/actions/workflows/TestCoverage.yaml/badge.svg?branch=main)](https://github.com/ultreia-io/form.references/actions/workflows/TestCoverage.yaml?query=branch%3Amain)
[![Coverage](https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fultreia-io.github.io%2Fform.references%2Fcoverage%2Fcoverage.xml&query=round%28%2Fcoverage%2F%40line-rate+%2A+1000%29+div+10&label=coverage&suffix=%25&color=brightgreen)](https://ultreia-io.github.io/form.references/articles/coverage.html)
[![Website](https://img.shields.io/badge/website-documentation-blue)](https://ultreia-io.github.io/form.references/)
[![License](https://img.shields.io/github/license/ultreia-io/form.references)](https://github.com/ultreia-io/form.references/blob/main/LICENSE)

Reference data downloads and verified caches for form models.

This repository currently contains the package skeleton. The download API is not implemented yet.

## Development installation

```r
install.packages("pak")
pak::pak("ultreia-io/form.references@develop")
```

## Planned workflow

1. Read a reference catalogue and verify its checksum.
2. Resolve the references declared by a `form.model` model.
3. Download missing or changed CSVs and verify their checksums before updating the cache.

The package will remain independent of ROS. Callers supply catalogue URLs, reference mappings and authentication.

## Documentation

- [Get started](https://ultreia-io.github.io/form.references/articles/form.references.html)
- [Function reference](https://ultreia-io.github.io/form.references/reference/index.html)
- [Contributing](https://ultreia-io.github.io/form.references/CONTRIBUTING.html)
- [Maintainer guide](https://ultreia-io.github.io/form.references/DEVELOPMENT.html)
- [Security policy](https://ultreia-io.github.io/form.references/SECURITY.html)
- [Code of conduct](https://ultreia-io.github.io/form.references/CODE_OF_CONDUCT.html)

## License

[LGPL-2.1](https://github.com/ultreia-io/form.references/blob/main/LICENSE).
