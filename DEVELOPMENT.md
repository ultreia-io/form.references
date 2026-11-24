# Maintainer guide

Work on `develop`. `main` contains releases. Both branches start at the initial package commit.

## Code organization

The files organize functions in one package namespace. You do not source each file separately.

| File                        | Responsibility                                                            |
|-----------------------------|---------------------------------------------------------------------------|
| `references.R`              | Public configuration, retrieval, matching and form-model operations.      |
| `references-internal.R`     | Internal validation, credentials, downloads, caching and lookup helpers.  |
| `mapping.R`                 | Public mapping configuration, preparation and value resolution.           |
| `mapping-internal.R`        | Internal mapping validation, lookup keys and correspondence-file reading. |
| `form.references-package.R` | Package-level documentation.                                              |

Each area has one public API file and one internal helper file, with sections grouping related operations.

Shared helpers are defined once; mapping operations reuse the reference lookup and retrieval functions.

## Survey fixtures

The example catalogues are tracked under `inst/examples/survey/catalogue/` and `survey/catalogue-source/`.

If you want to modify the example data, update the affected CSV’s version and source date in its catalogue, then run:

```sh
Rscript tools/update-example-catalogues.R
```

This refreshes the checksums and publication timestamps so the modified examples remain usable.

Pass catalogue directories as arguments to refresh only the changed catalogues.

The survey mapping and its correspondence CSVs live beside the two reference configurations in `inst/examples/survey/`.

## Local verification

Run from the package root with development dependencies and LaTeX installed:

```sh
Rscript tools/check.R
```

This generates documentation, runs tests, builds and checks the archive with `--as-cran`, then builds the website.

Local and CI checks share the CRAN mirror settings in `tools/check.Renviron`. All CRAN checks remain enabled.

**Build** runs one Linux R-release check for routine changes and uploads coverage to Codecov.

Run **Actions → Check All Platforms** for Linux with R release and R-devel, macOS, and Windows.

The README coverage badges link to the Codecov report.

Archives and check output stay in `build/`.

`roxygen2`, `rcmdcheck`, and `pkgload` are development tools, not runtime dependencies.

Review coverage gaps for missing behaviour. Check installed resources, runnable examples, and website navigation.

## Website

Website builds use `pkgdown` and `zip`, declared in `Config/Needs/website`.

To build only the website, run from the package root:

```r
# Prepare downloadable files from the package sources.
source("tools/prepare-site-assets.R")
# Build the website with pkgdown.
pkgdown::build_site()
```

## GitHub Release

1. **Prepare the release on `develop`:**

   1. **Prepare the release files:** update the version in `DESCRIPTION`, finalize `NEWS.md`, `cran-comments.md` and documentation.
   2. **Perform the [local checks](#local-verification).**
   3. **Commit the changes:** use `[form.references] Release <version>`.
   4. **Push the release commit.**

2. **Start the release on GitHub:**

   1. Open **Actions → Start Release**.
   2. Click **Run workflow**, select `develop`, then click **Run workflow** to confirm.
   3. The workflow checks the selected commit on all four environments and saves the checked Linux archive.
   4. After all four checks pass, it opens the release PR at the checked commit.

3. **Merge the release PR:**

   1. Review the release PR and merge it. Its checks already passed in **Start Release**.
   2. Merging starts **Publish Release**, which verifies that the merged files match the checked candidate.

**Publish Release** publishes the checked archive and checksum, then deploys the documentation.

If the merge changes the checked files, update `develop` and rerun **Start Release** before merging a new PR.

### Enable CRAN submission

The **Submit to CRAN** job is skipped by default. Enable it when the package's required dependencies are on CRAN.

1. Open the repository's **Settings → Secrets and variables → Actions → Variables**.
2. Click **New repository variable** and set `SUBMIT_TO_CRAN` to `true`.

To disable submission, remove the variable or set it to `false`.

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
