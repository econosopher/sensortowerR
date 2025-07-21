# Category Analysis Examples

This folder contains comprehensive analysis examples for different game categories:

## Social Casino Analysis
- **File**: `social_casino_analysis.R`
- **Focus**: US market analysis of Social Casino games
- **Starting point**: Monopoly Go
- **Features**:
  - Discovers category ID from a specific game
  - Creates FiveThirtyEight-styled dashboard
  - Revenue trend analysis
  - User engagement metrics (DAU/MAU ratios)
  - Retention curves
  - Monetization analysis (RPD)

## RPG Analysis
- **Files**: `squad_rpg_analysis_*.R`
- **Focus**: Worldwide market analysis of Role Playing games
- **Starting point**: Marvel Strike Force (in some versions)
- **Features**:
  - Multiple styling options (538 themed, modern, unified)
  - Comprehensive dashboard with retention heatmaps
  - Bar charts for key metrics
  - Country flags and demographic breakdowns

## Usage
Each script can be run independently and will:
1. Find the appropriate category ID
2. Fetch top games data (or use cached data)
3. Generate comprehensive visualizations
4. Save outputs to `inst/images/`