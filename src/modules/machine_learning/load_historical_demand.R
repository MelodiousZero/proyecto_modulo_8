
library(EIAapi)
library(dotenv)
library(httr)
library(jsonlite)

library(httr)
library(jsonlite)
library(sf)
library(dplyr)

if (file.exists(".env")) {
  load_dot_env(file = ".env")
}

api_key <- Sys.getenv("eia_key")

offsets <- seq(0, 50000, by = 5000)

for (off in offsets) {
  
  resp <- GET(
    "https://api.eia.gov/v2/electricity/rto/region-data/data/",
    query = list(
      api_key              = api_key,
      frequency            = "hourly",
      `data[0]`            = "value",
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "desc",
      `facets[type][]`     = "D",
      `facets[respondent][]` = "US48",
      start                =  "2021-01-01T00-06:00",
      end                  =  "2026-09-12T00-06:00",
      offset               = off,
      length               = 5000
    )
  )
  datos <- bind_rows(datos, fromJSON(content(resp, "text"))$response$data)
  Sys.sleep(0.3)
}


datos <- datos %>%
  filter(type == "D", respondent == "US48")

write.csv(datos, "machine_learning/ml_data/daily_hourly_historic.csv", row.names = FALSE)
