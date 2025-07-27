# sensortowerR Project Guidelines

## Version Control Policy

### Minor Version Increments
- Increment version number in DESCRIPTION file with every commit to GitHub
- Use semantic versioning: MAJOR.MINOR.PATCH (e.g., 0.1.6)
- Minor version increments for:
  - New functions added
  - Bug fixes
  - Documentation updates
  - Performance improvements
  - Any meaningful code changes

### Commit Messages
Include a concise version summary with each commit that describes:
- What was added/changed
- Why it was changed (if not obvious)
- Format: "v0.1.6: Add publisher analysis functions and documentation"

### Version Documentation
When incrementing version:
1. Update Version field in DESCRIPTION
2. Add entry to NEWS.md (if exists) with version number and changes
3. Include version summary in commit message
4. Push changes immediately after version increment

### Example Workflow
```bash
# After making changes
# 1. Update DESCRIPTION version from 0.1.5 to 0.1.6
# 2. Stage all changes
git add -A
# 3. Commit with version summary
git commit -m "v0.1.6: Add st_top_publishers function for publisher analytics"
# 4. Push to GitHub
git push origin main
```

## Project-Specific Guidelines

### API Usage
- Always check for API token before making requests
- Minimize API calls during testing
- Use cached/example data when possible

### Documentation
- Keep README.md updated with new functions
- Include examples for all exported functions
- Document all parameters clearly

### Testing
- Run `R CMD check --as-cran` before commits
- Ensure package size stays under 5MB for CRAN
- Test with minimal API calls