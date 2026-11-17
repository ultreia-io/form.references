# Survey reference data

These synthetic catalogues accompany the `form.model` survey example.

The existing catalogue supplies the codes expected by that model.

A second catalogue describes the same stations and observers using technical IDs.

`mapping.yaml` translates those IDs into the model's existing reference codes.

## Example contents

```text
survey/
├── references.yaml
├── references-source.yaml
├── mapping.yaml
├── correspondences/
│   └── stations.csv
├── catalogue/
│   ├── code_lists/
│   │   ├── catalogue.csv
│   │   ├── catalogue.yaml
│   │   ├── catalogue_aliases.csv
│   │   ├── observers.csv
│   │   └── stations.csv
│   └── registries/
│       ├── catalogue.csv
│       ├── catalogue.yaml
│       ├── catalogue_aliases.csv
│       └── contacts.csv
└── catalogue-source/
    └── code_lists/
        ├── catalogue.csv
        ├── catalogue.yaml
        ├── catalogue_aliases.csv
        ├── observers.csv
        └── stations.csv
```

Each catalogue directory is a complete source containing these files:

| File                    | Purpose                                          |
|-------------------------|--------------------------------------------------|
| `catalogue.csv`         | Table IDs, paths, versions, dates and checksums. |
| `catalogue.yaml`        | Catalogue publication timestamp and checksum.    |
| `catalogue_aliases.csv` | Alternative values accepted by table lookups.    |
| Other CSV files         | Reference data, read as text to preserve codes.  |

The survey uses the following tables and lookup columns:

| Catalogue  | Table       | Lookup columns  | Example value       |
|------------|-------------|-----------------|---------------------|
| Code lists | `stations`  | `code`          | `001`               |
| Code lists | `observers` | `code`, `name`  | `ALICE`             |
| Registries | `contacts`  | `email`, `name` | `alice@example.org` |

## Configuration and use

`references.yaml` connects the named catalogues to their sources and defines their lookup settings.

- `schema_version` selects the configuration schema.
- Source paths are relative to the configuration file.
- Cache paths are omitted, so caches use the R session's temporary directory.
- Runnable examples remove their temporary caches afterwards.
- Table and lookup names match the bindings declared in the form model.

`read_reference_configuration()` checks this file before any reference data is retrieved.

The Getting started guide demonstrates preparation from a form model and matching against the resulting lookups.

## Mapping the second catalogue

`references-source.yaml` declares source station IDs and observer IDs and names.

`mapping.yaml` selects its code lists as `source` and the existing configuration's code lists as `target`.

The existing contact registry remains available to the survey model.

- DIRECT: `station-north` maps through the CSV to station code `001`.
- REBOUND: `person-alice` supplies `Example Alice`, which matches target name `Alice Example` and returns `ALICE`.
- Unchanged input: an observer name goes directly to the target name lookup.

Select the observer alternative with `source_lookup = "id"` or `source_lookup = "name"`.

The station CSV includes a repeated pair, an old target code, an unknown target and conflicting correspondences.

The source observers include a missing name, an unknown person and a duplicated ID with two different names.

These deliberate data issues demonstrate unresolved results; they are not defaults or inferred correspondences.

The mapping vignette loads both catalogues once, then shows each behaviour in a separate runnable statement.

## Aliases

Each catalogue declares its alias file through a reserved entry in `catalogue.csv`:

| Column     | Value                   |
|------------|-------------------------|
| `domain`   | `catalogue`             |
| `id`       | `_aliases_`             |
| `csv_path` | `catalogue_aliases.csv` |

The alias file contains four columns:

| Column      | Meaning                                     |
|-------------|---------------------------------------------|
| `table`     | Table containing the canonical value.       |
| `column`    | Lookup column in that table.                |
| `alias`     | Alternative value accepted during matching. |
| `canonical` | Existing value to which the alias refers.   |

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

It also publishes the complete survey directory under `examples/survey/`, including both catalogues and the mapping.
