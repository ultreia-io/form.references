## Resubmission

Version 0.4.0 fixes both issues reported by CRAN for the rejected 0.3.0 submission.

## Changes made in response to CRAN

* **DESCRIPTION spelling:** reworded the description to resolve the spelling NOTEs for "checksums" and "lookups".
* **User directories:** removed automatic creation of `~/.cache/form.references`, which caused the rejection.

Cache paths are optional. Default caches use `tempfile()` within the current R session's temporary directory.

Bundled examples and vignettes use temporary caches and remove them afterwards.

Persistent caches in user directories are used only when the caller explicitly supplies a path.

## Check results

* Local: 2026-11-17; Linux (elementary OS 8); R 4.6.2; 353 tests passed; 0 errors/warnings; only "New submission" NOTE.
* [ubuntu-latest (release)][linux-release]: Linux x86_64; R 4.6.2; passed.
* [ubuntu-latest (devel)][linux-devel]: Linux x86_64; R-devel (2026-11-15 r90899); passed.
* [windows-latest (release)][windows-release]: Windows x86_64; R 4.6.2 (UCRT); passed.
* [macos-latest (release)][macos-release]: macOS arm64; R 4.6.2; passed.
* [Website deployment][website-results]: passed; 173 URLs and 483 anchors checked; 0 failures.
* [Resources ZIP][resources-zip]: 23 files; contents identical to package sources.

[linux-release]: https://github.com/ultreia-io/form.references/actions/runs/44065803354/job/133979050594
[linux-devel]: https://github.com/ultreia-io/form.references/actions/runs/44065803354/job/133979050864
[windows-release]: https://github.com/ultreia-io/form.references/actions/runs/44065803354/job/133979050720
[macos-release]: https://github.com/ultreia-io/form.references/actions/runs/44065803354/job/133979050136
[website-results]: https://github.com/ultreia-io/form.references/actions/runs/44062280354
[resources-zip]: https://ultreia-io.github.io/form.references/form.references-resources-0.4.0.zip

## Additional changes

Version 0.4.0 adds catalogue mapping configuration, resolution, synthetic examples and regression tests.

Thank you for reviewing this submission and for supporting the R open-source community.
