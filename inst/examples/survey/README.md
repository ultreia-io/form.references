# Survey reference data

These synthetic catalogues accompany the `form.model` survey example.

## Example contents

```text
survey/
├── references.yaml
└── catalogue/
    ├── code_lists/
    │   ├── catalogue.csv
    │   ├── catalogue.yaml
    │   ├── catalogue_aliases.csv
    │   ├── observers.csv
    │   └── stations.csv
    └── registries/
        ├── catalogue.csv
        ├── catalogue.yaml
        ├── catalogue_aliases.csv
        └── contacts.csv
```

Each catalogue directory is a complete source containing these files:

| File                      | Purpose                                             |
|---------------------------|-----------------------------------------------------|
| `catalogue.csv`           | Table IDs, paths, versions, dates and checksums.    |
| `catalogue.yaml`          | Catalogue publication timestamp and checksum.       |
| `catalogue_aliases.csv`   | Alternative values accepted by the table lookups.   |
| Other CSV files           | Reference data, read as text to preserve values.    |

The survey uses the following tables and lookup columns:

| Catalogue    | Table         | Lookup columns    | Example value         |
|--------------|---------------|-------------------|-----------------------|
| Code lists   | `stations`    | `code`            | `001`                 |
| Code lists   | `observers`   | `code`            | `ALICE`               |
| Registries   | `contacts`    | `email`, `name`   | `alice@example.org`   |

## Configuration and use

`references.yaml` connects the named catalogues to their sources and defines their lookup settings.

- `schema_version` selects the configuration schema.
- Source paths are relative to `references.yaml`; the example caches use the user's home directory.
- Table and lookup names match the bindings declared in the form model.

`read_reference_configuration()` checks this file before any reference data is retrieved.

The Getting started guide demonstrates preparation from a form model and matching against the resulting lookups.

## Aliases

Each catalogue declares its alias file through a reserved entry in `catalogue.csv`:

| Column       | Value                     |
|--------------|---------------------------|
| `domain`     | `catalogue`               |
| `id`         | `_aliases_`               |
| `csv_path`   | `catalogue_aliases.csv`   |

The alias file contains four columns:

| Column        | Meaning                                      |
|---------------|----------------------------------------------|
| `table`       | Table containing the canonical value.        |
| `column`      | Lookup column in that table.                 |
| `alias`       | Alternative value accepted during matching.  |
| `canonical`   | Existing value to which the alias refers.    |

Aliases use the catalogue's authentication, checksum verification and cache.

- `prepare_model_references()` builds each required lookup with its own aliases.
- `reference_match_rows()` applies those aliases automatically.
- A lookup's `$aliases` exposes its mappings for inspection; the complete alias table is not returned.

## Updating or publishing the example

If you want to modify the example data:

1. Edit the relevant CSV file.
2. Update its version and source date in the corresponding `catalogue.csv`.
3. Refresh the catalogue checksums from the package root:

```sh
Rscript tools/update-example-catalogues.R
```

The website build copies `catalogue/` to `examples/`, preserving its directory layout and file contents.
