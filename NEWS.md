# form.references 0.4.0

* [feature] Read versioned catalogue mappings with independent source and target configurations.
* [feature] Resolve CSV correspondences, source properties and unchanged inputs with explicit unresolved statuses.
* [fix] Stop creating `~/.cache/form.references`; use temporary session caches by default.
* [fix] Preserve explicit persistent caches and clean up temporary caches in examples and vignettes.
* [docs] Rephrase DESCRIPTION to avoid CRAN spelling notes for "checksums" and "lookups".
* [docs] Map two survey catalogues with separate runnable examples for each mapping behaviour and diagnostic.
* [docs] Document all S3 classes and both configuration schemas.
* [docs] Provide individual resource downloads and a ZIP archive named with the package version.

# form.references 0.3.0

* [feature] Configure catalogue sources and lookups in one versioned YAML file.
* [feature] Match text using aliases, case sensitivity and name matching.
* [feature] Load the reference tables required by the form model.
* [docs] Clarify terminology and guides; provide complete reference examples.
* [dependencies] Require form.model >= 0.6.2 from CRAN.
* [build] Add full CRAN checks, shared caches and Codecov coverage.
* [build] Submit GitHub release archives to CRAN with duplicate protection.
* [build] Use standard pkgdown builds with prepared download assets.

# form.references 0.2.1

* Update form.model from 0.5.0 to 0.5.1.
* Test reference retrieval and caching.

# form.references 0.2.0

* Use readable source names and reference IDs in cache paths.
* Configure private sources in YAML with session secrets and HTTP Basic authentication.
* Update dependency to form.model 0.5.0.

# form.references 0.1.0

* Retrieve a form model's code lists and registry tables from checksum-verified catalogues.
* Reuse verified downloads offline and fetch missing tables concurrently, including from private sources.
* Publish a complete survey example with downloadable code lists and registry tables.
