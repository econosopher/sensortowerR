# Candidate ownership

- Owner: this Codex task.
- Branch: `codex/sensortower-v2`.
- Checkout: `/Users/phillip/.codex/worktrees/sensortower-v2/sensortowerR`.
- Base: `abe3624bbdac5e9cf33efed8c95b3d3cfe290388`.
- Owned surface: package implementation, tests, documentation, recipes and CI.
- Gate: `Rscript tools/check.R`, bounded read-only live audit; other platforms require local checks.
- Execution policy: local checks; GitHub workflows disabled at user request.
- Integration: PR #7 merged at 4e36f082a7762191815a4dbd97cd85cc949997f5.
- Publication/install status: CRAN submission follows the validated merge;
  no installed-library replacement. See cran-builder-status.json.
- Original checkout: staged `tests/test_local.R`, untracked `Rplots.pdf` and
  `genre_yoy_analysis.R` are outside this candidate and are preserved.
