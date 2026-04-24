# fluxForecast
[![Release](https://img.shields.io/github/v/release/jarrod-dalton/fluxForecast?display_name=tag)](https://github.com/jarrod-dalton/fluxForecast/releases)
[![Downloads](https://img.shields.io/github/downloads/jarrod-dalton/fluxForecast/total)](https://github.com/jarrod-dalton/fluxForecast/releases)
[![License: LGPL-3](https://img.shields.io/badge/license-LGPL--3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Language: R](https://img.shields.io/badge/language-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

Forecasting and validation helpers for the **fluxCore** ecosystem.

This package is designed for health-services and clinical modeling teams building event-driven, entity-level simulation models. It supports:

- Running forward simulations from an entity's current state (or snapshot)
- Summarizing **probabilistic forecasts** over a user-supplied time grid
- Memory-friendly “one-shot” summaries (risk curves, state summaries) that scale to large simulation jobs

## Quick start

Typical workflow:

1. Build or load a `Entity` object (from `fluxCore`)
2. Create an `Engine` + model bundle (e.g., from `fluxASCVD`)
3. Call `forecast()` or one-shot summary helpers like `event_prob_forecast()`

## Parallel computing backends

There are two kinds of workflows:

### Memory-light summaries (recommended for large jobs)

Use:

- `event_prob_forecast()`
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
  entities = entities,
  times    = c(0.25, 0.5, 1, 2),
  S        = 500,
  return   = "summary_stats",
  summary_stats = "event_prob",
  summary_spec  = list(event = "ascvd"),
  backend  = "future"
)
```

### Full forecast objects

`forecast(return = "object")` builds a full `flux_forecast` object (more flexible, more memory).  
This currently supports:

- `backend = "none"`: serial
- `backend = "cluster"`: PSOCK cluster via `fluxCore::run_cohort(parallel=TRUE)`

For large-scale parallel work, prefer the memory-light summary paths above.


## Runtime tips for live forecasts

If you need results in a few seconds (e.g., an interactive dashboard), the goal is to keep work per request small and keep your workers “warm”.

Practical defaults:

- Use `return = "summary_stats"` (risk curves / state summaries). Avoid returning a full `flux_forecast` object in live settings.
- Keep the time grid short (e.g., 4–8 future times), and avoid ultra-fine grids unless you truly need them.
- Start with `S = 100`–`300` simulations per entity. Increase offline for validation, not at request time.
- Prefer `backend = "future"` on servers and clusters. Set the plan once at startup and reuse it:

  ```r
  future::plan(future::multisession, workers = 4)
  ```

- If your model has expensive pieces (e.g., large table lookups), preload them in `ctx` or bundle state so they are not rebuilt each run.
- If you must support cohorts, parallelize across entities first (coarser tasks), then across simulations only if needed.

A useful pattern for APIs: run a small forecast quickly for the response, and queue a larger job (bigger `S`, richer outputs) asynchronously for “download later”.
