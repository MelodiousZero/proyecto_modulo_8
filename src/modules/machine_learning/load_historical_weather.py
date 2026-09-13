import openmeteo_requests
import pandas as pd
import requests_cache
from retry_requests import retry
from datetime import datetime, timedelta, timezone
from pathlib import Path

# --- Setup Open-Meteo API client with cache and retry ---
cache_session = requests_cache.CachedSession('.cache', expire_after=3600)
retry_session = retry(cache_session, retries=5, backoff_factor=0.2)
openmeteo = openmeteo_requests.Client(session=retry_session)

# --- Date range: multi-year history, ending ~5 days ago (archive lag) ---
today = datetime.now(timezone.utc).date()
archive_end = today - timedelta(days=5)

start_date = "2018-01-01"
end_date = archive_end.strftime("%Y-%m-%d")

print(f"Today:        {today}")
print(f"Archive end:  {end_date}")
print(f"Fetching:     {start_date} → {end_date}")

# --- Curated hourly variables (fuel-relevant) ---
HOURLY_VARS = [
    # Solar
    "shortwave_radiation", "direct_radiation", "diffuse_radiation",
    "direct_normal_irradiance", "global_tilted_irradiance",
    "cloud_cover", "cloud_cover_low", "cloud_cover_mid", "cloud_cover_high",
    "sunshine_duration", "is_day", "uv_index",
    # Wind
    "wind_speed_10m", "wind_speed_80m", "wind_speed_120m", "wind_speed_180m",
    "wind_direction_10m", "wind_direction_80m",
    "wind_direction_120m", "wind_direction_180m",
    "wind_gusts_10m",
    "temperature_80m", "temperature_120m", "temperature_180m",
    # Thermal
    "temperature_2m", "relative_humidity_2m", "dew_point_2m",
    "apparent_temperature", "wet_bulb_temperature_2m",
    "vapour_pressure_deficit", "surface_pressure", "pressure_msl",
    # Hydro
    "precipitation", "rain", "showers", "snowfall", "snow_depth",
    "et0_fao_evapotranspiration",
    # Context
    "weather_code", "precipitation_probability",
]

# --- Archive endpoint (historical reanalysis, 1940 → ~5 days ago) ---
url = "https://archive-api.open-meteo.com/v1/archive"
params = {
    "latitude": 32.7157,
    "longitude": -117.1611,
    "hourly": HOURLY_VARS,
    "start_date": start_date,
    "end_date": end_date,
    "timezone": "America/Los_Angeles",
}

responses = openmeteo.weather_api(url, params=params)
response = responses[0]

print(f"\nCoordinates: {response.Latitude()}°N {response.Longitude()}°E")
print(f"Elevation:   {response.Elevation()} m asl")
print(f"UTC offset:  {response.UtcOffsetSeconds()}s")

# --- Build hourly DataFrame ---
hourly = response.Hourly()

times = pd.date_range(
    start=pd.to_datetime(hourly.Time(), unit="s", utc=True),
    end=pd.to_datetime(hourly.TimeEnd(), unit="s", utc=True),
    freq=pd.Timedelta(seconds=hourly.Interval()),
    inclusive="left",
).tz_convert("America/Los_Angeles")

hourly_data = {"period": times}
for i, var in enumerate(HOURLY_VARS):
    hourly_data[var] = hourly.Variables(i).ValuesAsNumpy()

hourly_dataframe = pd.DataFrame(data=hourly_data)

# --- Save ---
out_path = Path("src/modules/machine_learning/ml_data/historic_hourly_weather_data.csv")
hourly_dataframe.to_csv(out_path, index=False)

print(f"\nSaved {len(hourly_dataframe)} rows × {len(hourly_dataframe.columns)} cols → {out_path}")
print(hourly_dataframe.head())