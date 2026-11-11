## Check results

R CMD check --as-cran for form.references on:

* Local computer: Linux (elementary OS 8), R 4.6.2.
* GitHub Actions: Ubuntu, R 4.6.2 (release).
* GitHub Actions: Ubuntu, R-devel (2026-11-09 r90865).
* GitHub Actions: macOS, R 4.6.2 (release).
* GitHub Actions: Windows, R 4.6.2 (release, UCRT).

CI results: 0 errors, 0 warnings, 1 NOTE on all four environments.

The NOTE identifies this as a new submission.

CI run: https://github.com/ultreia-io/form.references/actions/runs/43013096479

## First submission

This is the first CRAN submission of form.references.

The package uses form.model, recently published on CRAN, to identify and retrieve the reference tables a form requires.

It provides configurable text lookups with aliases and case matching for exporters, importers and validators.

Thank you for reviewing this submission and for supporting the R open-source community.
