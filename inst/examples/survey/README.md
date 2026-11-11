# Survey reference data

These public synthetic references match the `form.model` survey example.

`catalogue/` contains the deployable source: catalogue metadata and CSV tables.

| Catalogue  | ID          | Column  | Values                                                    |
|------------|-------------|---------|-----------------------------------------------------------|
| Code lists | `stations`  | `code`  | `001`, `002`, `ST01`, `ST02`                                |
| Code lists | `observers` | `code`  | `ALICE`, `BOB`, `observer-1`, `observer-2`                   |
| Registries | `contacts`  | `email` | `alice@example.org`, `bob@example.org`, `survey@example.org` |

`references.yaml` configures catalogue sources and all table lookups, including contacts by email and name.

It is used to prepare references.

The Get started vignette downloads the tables, builds every configured index and retrieves matching rows.

Read all CSV columns as text to preserve station codes and their leading zeros.

`catalogue/code_lists.csv` and `catalogue/registries.csv` list the download paths, versions, dates and SHA-256 checksums.

Each matching YAML file contains its catalogue timestamp and SHA-256 checksum.

Catalogue IDs are lowercase and match the model references exactly; no alias mapping is needed.

If you want to modify the example data, update the affected CSV’s version and source date in its catalogue, then run:

```sh
Rscript tools/update-example-catalogues.R
```

The website build copies only `catalogue/` to `examples/`, preserving published paths and file contents.

The configuration declares `schema_version: "1.0.0"`.

Local source paths resolve from this YAML file. Cache paths use the user's home directory.

Load it with `read_reference_configuration()`, which checks `schema/references-1.0.0.yaml`.

The schema is supplied by `form.references`; invalid configurations fail before reference data is retrieved.
