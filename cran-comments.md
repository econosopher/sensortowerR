Update of the existing CRAN package sensortowerR from 1.0.1 to 2.0.0.
The maintainer and MIT license are unchanged.

This major release replaces 73 exports with 25 consistent functions for
ordinary data-frame pipelines. NEWS.md documents the breaking changes;
the source repository contains a migration table for every prior export.
The CRAN index was checked on 2026-09-21 and has no reverse dependencies,
including Depends, Imports, LinkingTo, Suggests and Enhances.

The final source archive was built with R 4.6.1 on Ubuntu 24.04 arm64.
R CMD check --as-cran on that environment: 0 errors, 0 warnings, 0 notes.
R Mac Builder, R 4.6.1 Patched, arm64: 0 errors, 0 warnings, 0 notes.
Windows R-release 4.6.1: 0 errors, 0 warnings, 0 notes.
All 191 test assertions pass without credentials or API network access.
The examples, rebuilt vignettes, PDF manual and HTML manual were checked.
URL checks pass.

Live retrieval requires a user-provided Sensor Tower subscription/API token.
Ordinary package checks do not call the service. The optional authenticated
live audit is excluded from the package archive. HTTP and schema failures
produce informative errors. Caching is opt-in and session-only.
