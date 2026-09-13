import pandas as pd
from pathlib import Path

# --- Paths ---
BASE = Path("src/modules/machine_learning/ml_data")
weather_path = BASE / "historic_hourly_weather_data.csv"
gen_path     = BASE / "daily_hourly_historic_energy_source.csv"
out_path     = BASE / "merged_generation_weather.csv"

# --- Load (use pyarrow engine if installed — much faster + less RAM) ---
try:
    weather = pd.read_csv(weather_path, engine="pyarrow")
    gen     = pd.read_csv(gen_path, engine="pyarrow")
except ImportError:
    weather = pd.read_csv(weather_path)
    gen     = pd.read_csv(gen_path)

print(f"Weather rows: {len(weather):,}  |  Gen rows: {len(gen):,}")

# --- Parse time columns to UTC ---
# Weather: "2018-01-01 00:00:00-08:00" — has offset, pandas handles it
weather["period_utc"] = pd.to_datetime(weather["period"], utc=True)

# EIA: "2026-09-13T06" — no minutes/seconds, no offset. Append ":00" and mark UTC.
gen["period_utc"] = pd.to_datetime(gen["period"] + ":00", utc=True)

# --- Trim to essentials before the join (saves a lot of RAM) ---
weather_slim = weather.drop(columns=["period"])

# Optional: downcast string columns to categorical
for col in ["respondent", "respondent-name", "fueltype", "type-name", "value-units"]:
    if col in gen.columns:
        gen[col] = gen[col].astype("category")

# --- Inner join on UTC hour ---
merged = gen.merge(weather_slim, on="period_utc", how="inner")

# --- Report ---
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

# --- Save ---
merged.to_csv(out_path, index=False)
print(f"\nSaved → {out_path}")