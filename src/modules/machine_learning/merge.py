import pandas as pd
from pathlib import Path

BASE = Path("src/modules/machine_learning/ml_data")
weather_path = BASE / "historic_hourly_weather_data.csv"
gen_path     = BASE / "daily_hourly_historic_energy_source.csv"
out_path     = BASE / "merged_generation_weather.csv"

try:
    weather = pd.read_csv(weather_path, engine="pyarrow")
    gen     = pd.read_csv(gen_path, engine="pyarrow")
except ImportError:
    weather = pd.read_csv(weather_path)
    gen     = pd.read_csv(gen_path)

print(f"Weather rows: {len(weather):,}  |  Gen rows: {len(gen):,}")

weather["period_utc"] = pd.to_datetime(weather["period"], utc=True)

gen["period_utc"] = pd.to_datetime(gen["period"] + ":00", utc=True)

weather_slim = weather.drop(columns=["period"])

for col in ["respondent", "respondent-name", "fueltype", "type-name", "value-units"]:
    if col in gen.columns:
        gen[col] = gen[col].astype("category")

merged = gen.merge(weather_slim, on="period_utc", how="inner")

gen_hours     = gen["period_utc"].nunique()
weather_hours = weather["period_utc"].nunique()
matched_hours = merged["period_utc"].nunique()

print(f"Gen unique hours:     {gen_hours:,}")
print(f"Weather unique hours: {weather_hours:,}")
print(f"Matched hours:        {matched_hours:,}")
print(f"Dropped from gen:     {gen_hours - matched_hours:,}")
print(f"Dropped from weather: {weather_hours - matched_hours:,}")
print(f"Merged rows:          {len(merged):,}")
print(f"NA weather cells:     {merged['temperature_2m'].isna().sum():,}")

merged.to_csv(out_path, index=False)
print(f"\nSaved → {out_path}")