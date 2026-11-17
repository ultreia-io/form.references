# Reference examples

`survey/` contains the survey example, its reference configuration and a second catalogue with different identifiers.

`survey/mapping.yaml` maps stations and observers from the second catalogue to the survey's existing code lists.

`references.private.example.yaml` is a separate template for public code lists and a private HTTPS registry.

Replace its source URLs and registry user, then set `REGISTRY_PASSWORD` or load with `prompt = TRUE` for secure password entry.
