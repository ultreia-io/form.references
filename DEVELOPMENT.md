# Maintainer guide

Work on `develop`. `main` contains releases. Both branches start at the initial package commit.

## Package structure

| Directory    | Purpose                                  |
|--------------|------------------------------------------|
| `R/`         | Implementation and roxygen comments      |
| `man/`       | Generated R documentation tracked in Git |
| `tests/`     | Offline tests, added with implementation |
| `inst/`      | Installed examples and resources         |
| `vignettes/` | User guides         |
| `tools/`     | Website and verification commands            |

`source.R` configures access; `catalogue.R` resolves metadata.

`download.R` manages cached files; `model.R` selects model reference IDs.

## Survey fixtures

The reference CSVs and catalogue are tracked under `inst/examples/`.

After editing a CSV, update its version and source date in `codelists.csv`, then run:

```sh
Rscript tools/prepare-examples.R
```

Commit both catalogue files with the CSV changes. Website builds copy them unchanged into `docs/examples/`.

## Local verification

Run from the package root with development dependencies and LaTeX installed:

```sh
Rscript tools/check.R
```

This generates documentation, runs tests, builds and checks the archive with `--as-cran`, then builds the website.

Local and CI checks share the CRAN mirror settings in `tools/check.Renviron`. All CRAN checks remain enabled.

The **Build** workflow measures coverage with `covr` on Linux after successful checks and uploads it to Codecov.

The README coverage badges link to the Codecov report.

Archives and check output stay in `build/`.

`roxygen2`, `rcmdcheck`, and `pkgload` are development tools, not runtime dependencies.

Review coverage gaps for missing behaviour. Check installed resources, runnable examples, and website navigation.

## GitHub Release

1. **Prepare the release on `develop`:**

   1. **Prepare the release files:** update the version in `DESCRIPTION`, finalize `NEWS.md`, `cran-comments.md` and documentation.
   2. **Perform the [local checks](#local-verification).**
   3. **Commit the changes:** use `[form.references] Release <version>`.
   4. **Push the release commit.**

2. **Start the release on GitHub:**

   1. Open **Actions → Start Release**.
   2. Click **Run workflow**, select `develop`, then click **Run workflow** to confirm.
   3. Open the release PR created by the workflow.

3. **Merge the release PR:**

   1. **Wait for the checks:** all checks on the release PR must be green.
   2. **Merge:** click **Merge pull request**, then **Confirm merge**.
   3. Merging automatically starts the **Publish Release** workflow.

The **Publish Release** workflow:

1. Publishes the package archive, checksum and documentation.
2. **Submits the same archive to CRAN**.

## Manual CRAN submission

Use this fallback only if the release has not already been submitted by CI.

Start from a clean working tree. Replace `version` with the release version to submit.

```sh
# Fetch the published release tags.
git fetch origin --tags
# Check out the release tag.
git switch --detach version
```

Run these commands in R from the package root, with `devtools` installed.

```r
# Build and run R CMD check --as-cran, including the PDF manual and incoming checks.
devtools::check(args = "--as-cran", manual = TRUE, remote = TRUE, error_on = "warning")

# After resolving errors and warnings and reviewing NOTES, submit the release to CRAN.
devtools::submit_cran()
```

This manual route rebuilds from the release tag. CI submits the existing GitHub archive unchanged.

Confirm CRAN's email, then wait for its checks and response.

## License

LGPL-2.1. See [LICENSE](LICENSE).
