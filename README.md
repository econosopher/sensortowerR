# sensortowerR 📱📊

## Overview
`sensortowerR` is a comprehensive R package that provides a seamless interface to the Sensor Tower API, enabling data scientists and analysts to access mobile app market intelligence programmatically.

## Installation 🔧

```R
# Install from GitHub
devtools::install_github("peterparkerspicklepatch/sensortowerR")

# Load the package
library(sensortowerR)
```

## Authentication 🔑

Before using the package, set up your Sensor Tower API authentication:

```R
# Set environment variable
Sys.setenv(SENSORTOWER_AUTH="your_token_here")

# Or use in individual functions
fetch_sensor_tower_metrics(auth_token = "your_token_here", ...)
```

## Core Functions 🛠️

### fetch_sensor_tower_metrics()

Fetch unified metrics for apps including revenue, downloads, and active users.

#### Parameters:
- `auth_token`: API authentication token
- `app_id`: Unique identifier for the app (e.g., "55c5022c02ac64f9c0001f83" for Spotify)
- `start_date`: Start date for data collection (YYYY-MM-DD)
- `end_date`: End date for data collection (YYYY-MM-DD)
- `app_name`: Name of the app
- `grain`: Time granularity for the data ("daily", "weekly", or "monthly")

#### Returns:
A tibble containing:
- date
- app_name
- app_id
- revenue
- downloads
- active_users (DAU, WAU, or MAU depending on grain)

#### Examples:

```R
# Fetch daily metrics for Spotify
spotify_metrics <- fetch_sensor_tower_metrics(
  auth_token = Sys.getenv("SENSORTOWER_AUTH"),
  app_id = "55c5022c02ac64f9c0001f83",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  app_name = "Spotify",
  grain = "daily"
)

# Fetch monthly metrics for Netflix
netflix_metrics <- fetch_sensor_tower_metrics(
  auth_token = Sys.getenv("SENSORTOWER_AUTH"),
  app_id = "55c500dc02ac64f9c0001f01",
  start_date = "2024-01-01",
  end_date = "2024-03-31",
  app_name = "Netflix",
  grain = "monthly"
)
```

## Best Practices 💡

1. Store API tokens securely using environment variables
2. Use appropriate time grains based on analysis needs
3. Handle rate limits and errors appropriately

## Contributing 🤝

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m "Add amazing feature"`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License 📄

MIT License - see LICENSE file for details

## Support 💬

- Issues: [GitHub Issues](https://github.com/peterparkerspicklepatch/sensortowerR/issues)
- Email: team@julius.ai

---
Made with ❤️ by [Julius AI](https://julius.ai)
