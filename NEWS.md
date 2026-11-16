# sensortowerR 2.1.0

* `st_rankings()` gains `tags`, returning requested custom tags (such as
  `"Game Genre"` and `"Game Sub-genre"`) as columns for app sales rankings. One
  unfiltered ranking can then be split by genre locally instead of one filtered
  call per genre.
* New `st_ad_monetization()` returns estimated ad impressions and ad revenue for
  unified apps from 2024 onward, weekly, monthly or quarterly, optionally by ad
  network and/or country. Revenue is converted from cents to USD by default.
* New `st_ad_rankings()` ranks unified apps by ad revenue, ad impressions or
  share of ad impressions, with optional network, category and filter limits.
* Ad monetization coverage is a limited set of countries with no worldwide row.
  The default `countries = "WW"` returns the total over covered regions, labeled
  `covered_regions` rather than `WW`.
* `st_filter_create()` now sends named `custom_fields` as a JSON array of field records.
  A named list was serialized as an object and the API answered HTTP 422, so filters such as
  ``st_filter(custom_fields = list(`Game Sub-genre` = "Card Battler"))`` could not be created.
* Existing functions are otherwise unchanged.

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
