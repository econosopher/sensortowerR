# Test helper functions

skip_if_no_auth <- function() {
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Sensor Tower authentication token not found")
  }
}