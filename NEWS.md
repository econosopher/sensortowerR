# sensortowerR (development version)

* `st_filter_create()` now sends named `custom_fields` as a JSON array of field records.
  A named list was serialized as an object and the API answered HTTP 422, so filters such as
  `st_filter(custom_fields = list(`Game Sub-genre` = "Card Battler"))` could not be created.

# sensortowerR 2.0.0

* Breaking redesign around eager, data-first tibble pipelines with explicit IDs.
* One metrics function handles sales and DAU/WAU/MAU, retaining native windows.
* Unified sales use the provider's unified endpoint for single and batch calls.
* Missing observations remain missing; failures abort or produce explicit partial statuses.
* Rankings honor measure; store chart positions have a separate function.
* Metadata conversion retains all regional store mappings and input identity.
* Filter construction is local. Creation returns only server-confirmed IDs.
* One httr2 request layer supplies timeouts, bounded read retries and safe errors.
* Cache defaults off, expires and is scoped to credentials.
* Specialist wrappers share data-first conventions; reporting moves to recipes.
* See MIGRATION.md and the audit directory for compatibility and verification.
