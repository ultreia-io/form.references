# Survey reference data

These synthetic code lists match `observers` and `stations` in the `form.model` survey example.

| Reference   | Code column | Values         |
|-------------|-------------|----------------|
| `stations`  | `code`      | `001`, `002`   |
| `observers` | `code`      | `ALICE`, `BOB` |

Read all CSV columns as text to preserve station codes and their leading zeros.

`codelists.csv` lists the relative download paths, versions, dates and SHA-256 checksums.

`codelists.yaml` contains the catalogue timestamp and its SHA-256 checksum.

Catalogue IDs are lowercase and match the model references exactly; no alias mapping is needed.

Run `Rscript tools/prepare-examples.R` after changing a CSV, its version or its source update date.

The website build copies this directory to `examples/` without rewriting the published files.
