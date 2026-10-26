# Security policy

Report vulnerabilities privately through [GitHub advisories](https://github.com/ultreia-io/form.references/security/advisories/new).

If private reporting is unavailable, contact the maintainer at `dev@tchemit.fr`.

Do not include credentials or private registry data in public issues or logs.

SHA-256 detects corrupt or inconsistent downloads; authenticity depends on the trusted source and HTTPS.

Cached tables may contain private data. Use a private cache directory and a separate cache per authenticated identity.

Authentication headers are never persisted. Authenticated requests do not follow redirects.

Catalogue paths must remain relative to their configured source. YAML expressions are not evaluated.
