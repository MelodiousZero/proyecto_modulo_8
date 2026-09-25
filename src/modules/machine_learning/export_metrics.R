library(tidyverse)

MODEL_DIR <- "src/modules/machine_learning/models"
OUT_DIR   <- "src/modules/machine_learning/ml_data"

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1. Renovables — lee todos los xgb_<FUEL>_metrics.rds
# ============================================================
renewable_files <- list.files(
  MODEL_DIR,
  pattern    = "^xgb_\\w+_metrics\\.rds$",
  full.names = TRUE
)

if (length(renewable_files) == 0) {
  warning("No se encontraron archivos xgb_*_metrics.rds en ", MODEL_DIR)
} else {
  renewables_tbl <- map_dfr(renewable_files, function(f) {
    m <- readRDS(f)
    tibble(
      fuel       = m$fuel,
      r2         = round(m$r2,   4),
      rmse       = round(m$rmse, 2),
      mae        = round(m$mae,  2),
      mape       = round(m$mape, 2),
      n_train    = m$n_train,
      n_test     = m$n_test,
      train_end  = m$train_end,
      test_start = m$test_start,
      test_end   = m$test_end,
      trained_at = m$trained_at
    )
  })

  write_csv(
    renewables_tbl,
    file.path(OUT_DIR, "renewables_metrics.csv")
  )
  cat("Escrito:", file.path(OUT_DIR, "renewables_metrics.csv"),
      "| filas:", nrow(renewables_tbl), "\n")
}

# ============================================================
# 2. Demanda — lee demand_metrics.rds
# ============================================================
demand_path <- file.path(MODEL_DIR, "demand_metrics.rds")

if (!file.exists(demand_path)) {
  warning("No se encontró ", demand_path)
} else {
  m <- readRDS(demand_path)

  demand_tbl <- tibble(
    fuel       = m$fuel,
    r2         = round(m$r2,   4),
    rmse       = round(m$rmse, 2),
    mae        = round(m$mae,  2),
    mape       = round(m$mape, 2),
    n_train    = m$n_train,
    n_test     = m$n_test,
    train_end  = m$train_end,
    test_start = m$test_start,
    test_end   = m$test_end,
    trained_at = m$trained_at
  )

  write_csv(
    demand_tbl,
    file.path(OUT_DIR, "demand_metrics.csv")
  )
  cat("Escrito:", file.path(OUT_DIR, "demand_metrics.csv"),
      "| filas:", nrow(demand_tbl), "\n")
}