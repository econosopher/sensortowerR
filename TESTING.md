Run `Rscript tools/check.R` for the offline test and package check gate.

Run `SENSORTOWER_RUN_LIVE=true Rscript tools/live-audit.R` separately with a token available.
Live reads are bounded. Filter creation is tested offline only.
