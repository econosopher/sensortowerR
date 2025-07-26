# Cross-Platform Testing Guide for sensortowerR

This package includes comprehensive cross-platform testing capabilities to ensure compatibility across Windows, Linux, and macOS before CRAN submission.

## Overview

The package now includes automated multi-platform testing using R-hub (rhub) that simulates the exact CRAN checking environment across Windows, macOS, and Linux platforms.

## Test Structure

### Standard Tests (Always Run)
- `tests/testthat/test-sensortowerR.R` - Basic package functionality tests
- `tests/testthat/test-rhub-cran-checks.R` - CRAN compliance and multi-platform tests

### Helper Scripts
- `tests/run_rhub_tests.R` - Interactive script for running rhub tests

## Quick Start

### 1. Install Required Packages
```r
install.packages(c("rhub", "rcmdcheck", "pkgbuild"))
```

### 2. Setup R-hub (One-time)
```r
rhub::validate_email("your-email@example.com")
# Check your email and click the validation link
```

### 3. Run Local Tests
```r
# Basic test suite (fast)
testthat::test_dir("tests/testthat")

# Local CRAN check (medium)
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))
```

### 4. Run Multi-Platform Tests
```r
# Enable rhub tests and run
Sys.setenv(RUN_RHUB_TESTS = "true")
testthat::test_file("tests/testthat/test-rhub-cran-checks.R")
```

Or from command line:
```bash
RUN_RHUB_TESTS=true R -e "testthat::test_dir('tests/testthat')"
```

### 5. Interactive Testing
```r
# Run the helper script
source("tests/run_rhub_tests.R")
```

## Test Types

### 1. Local CRAN Check
- **What**: Runs `R CMD check --as-cran` locally
- **When**: Always runs (unless on CRAN)
- **Duration**: 2-5 minutes
- **Platforms**: Your current platform only

### 2. Multi-Platform rhub Check
- **What**: Tests on Windows, macOS, and Linux using CRAN infrastructure
- **When**: Only when `RUN_RHUB_TESTS=true`
- **Duration**: 15-30 minutes
- **Platforms**: Multiple CRAN-like environments

### 3. Package Installation Test
- **What**: Tests clean installation and loading
- **When**: When package tarball exists
- **Duration**: 1-2 minutes
- **Purpose**: Verify installation process

## Environment Variables

### `RUN_RHUB_TESTS`
Controls whether rhub multi-platform tests run:
- `"true"` - Enable rhub tests (slow but comprehensive)
- `""` or unset - Skip rhub tests (fast local tests only)

## Workflow Examples

### Development Workflow (Fast)
```r
# Quick checks during development
testthat::test_dir("tests/testthat")  # ~30 seconds
```

### Pre-Commit Workflow (Medium)
```r
# More thorough local checking
rcmdcheck::rcmdcheck(args = "--as-cran")  # ~5 minutes
```

### Pre-CRAN Workflow (Comprehensive)
```r
# Full multi-platform validation
Sys.setenv(RUN_RHUB_TESTS = "true")
testthat::test_dir("tests/testthat")  # ~30 minutes
```

### CI/CD Integration
```yaml
# In GitHub Actions or similar
- name: Run CRAN checks
  run: |
    if [ "${{ github.event_name }}" = "push" ] && [ "${{ github.ref }}" = "refs/heads/main" ]; then
      export RUN_RHUB_TESTS=true
    fi
    R -e "testthat::test_dir('tests/testthat')"
```

## Interpreting Results

### Success ✅
```
✔ | F W  S  OK | Context
✔ |      3   0 | rhub-cran-checks
[ FAIL 0 | WARN 0 | SKIP 3 | PASS 0 ]
```

### Skipped Tests (Normal) ⏭️
- `"Set RUN_RHUB_TESTS=true"` - rhub tests disabled (expected)
- `"On CRAN"` - Running on CRAN itself (expected)
- `"rhub email not validated"` - Need to setup rhub first

### Failures ❌
- **Local check failures** - Fix before proceeding
- **rhub platform failures** - Platform-specific issues
- **Installation failures** - Package building problems

## Common Issues & Solutions

### rhub Email Not Validated
```r
rhub::validate_email("your-email@example.com")
# Check email and click validation link
```

### Missing Dependencies
```r
install.packages(c("rhub", "rcmdcheck", "pkgbuild"))
```

### Timeout Issues
- rhub tests have 30-minute timeout
- Check rhub status at: https://builder.r-hub.io/

### Platform-Specific Failures
- Windows: Often encoding or path issues
- macOS: Usually dependency-related
- Linux: Typically system library issues

## Manual rhub Usage

For fine-grained control, use rhub directly:

```r
# Build package first
pkg_path <- pkgbuild::build(".")

# Check on all CRAN platforms
rhub::check_for_cran(pkg_path)

# Check specific platforms
rhub::check_on_windows(pkg_path)
rhub::check_on_macos(pkg_path)
rhub::check_on_linux(pkg_path)

# Check results
rhub::list_my_checks()
```

## Best Practices

1. **Always run local checks first** - Faster feedback
2. **Use rhub before CRAN submission** - Catch platform issues
3. **Monitor rhub queue times** - Can vary by demand
4. **Keep email validated** - rhub needs valid email
5. **Check multiple platforms** - Don't assume cross-platform compatibility

## Integration with Package Development

This testing system integrates seamlessly with:
- `devtools::check()` - Local development
- `pkgdown::build_site()` - Documentation building  
- GitHub Actions - Continuous integration
- CRAN submission workflow - Final validation

The goal is to catch CRAN issues **before** submission, reducing rejection rates and development friction. 