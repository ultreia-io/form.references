# Survey reference data

These public synthetic references match the `form.model` survey example.

| Catalogue   | ID          | Column  | Values                                  |
|-------------|-------------|---------|-----------------------------------------|
| Code lists  | `stations`  | `code`  | `001`, `002`                            |
| Code lists  | `observers` | `code`  | `ALICE`, `BOB`                          |
| Registries  | `contacts`  | `email` | `alice@example.org`, `bob@example.org` |

Read all CSV columns as text to preserve station codes and their leading zeros.

`codelists.csv` and `registries.csv` list the download paths, versions, dates and SHA-256 checksums.

Each matching YAML file contains its catalogue timestamp and SHA-256 checksum.

Catalogue IDs are lowercase and match the model references exactly; no alias mapping is needed.

Run `Rscript tools/prepare-examples.R` after changing a CSV, its version or its source update date.

The website build copies this directory to `examples/` without rewriting the published files.
