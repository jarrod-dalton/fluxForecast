# patientSimForecast

Forecasting and validation helpers for the **patientSimCore** ecosystem.

This package is designed for health-services and clinical modeling teams building event-driven, patient-level simulation models. It supports:

- Running forward simulations from a patient’s current state (or snapshot)
- Summarizing **probabilistic forecasts** over a user-supplied time grid
- Memory-friendly “one-shot” summaries (risk curves, state summaries) that scale to large simulation jobs

## Quick start

Typical workflow:

1. Build or load a `Patient` object (from `patientSimCore`)
2. Create an `Engine` + model bundle (e.g., from `patientSimASCVD`)
3. Call `forecast()` or one-shot summary helpers like `risk_forecast()`

## Parallel computing backends

There are two kinds of workflows:

### Memory-light summaries (recommended for large jobs)

Use:

- `risk_forecast()`
- `state_summary_forecast()`
- or `forecast(return = "summary_stats", ...)`

These support:

- `backend = "none"`: serial
- `backend = "mclapply"`: `parallel::mclapply` (macOS/Linux only)
- `backend = "future"`: `future.apply::future_lapply` (recommended for clusters/cloud/Databricks)

Example with **future**:

```r
future::plan(future::multisession, workers = 4)

out <- forecast(
  engine   = eng,
  patients = patients,
  times    = c(0.25, 0.5, 1, 2),
  S        = 500,
  return   = "summary_stats",
  summary_stats = "risk",
  summary_spec  = list(event = "ascvd"),
  backend  = "future"
)
```

### Full forecast objects

`forecast(return = "object")` builds a full `ps_forecast` object (more flexible, more memory).  
This currently supports:

- `backend = "none"`: serial
- `backend = "cluster"`: PSOCK cluster via `patientSimCore::run_cohort(parallel=TRUE)`

For large-scale parallel work, prefer the memory-light summary paths above.
