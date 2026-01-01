## patientSimForecast 1.0.6

- Fix: parsing error introduced in `state_summary.R`.
- Keep: `risk_forecast(..., by=)` supports run-, patient-, and patient_draw-level summaries.

## patientSimForecast 1.0.4

- Fix: parsing error in `risk()` argument list (escaped quotes).
- Enhancement (from 1.0.3): `risk(by=...)` supports run-, patient-, and patient_draw-level summaries.

# patientSimForecast 1.0.1


## patientSimForecast 1.0.2
- Internal: consolidate NA-safe TRUE-check helpers to avoid scalar/vector pitfalls.
- Made TRUE/FALSE checks more robust in forecast matrices and streaming summaries (preserve NA where appropriate; avoid `isTRUE()` scalar pitfalls).

# patientSimForecast 1.0.0

- `forecast()` API standardized on `backend=` and `ctx=`; legacy `parallel=`/`ctx_base` pathways removed.
- Posterior predictive pooling across multiple parameter sets supported via list-of-ctx semantics (equal-weight by construction when runs per set are balanced).
- `state_summary()` eligibility bug fixed (vectorized alive filtering) and locked down with new unit tests for multi-ctx quantiles and follow-up stopping semantics.


## patientSimForecast 1.0.3
- risk() now supports by = c('run','patient','patient_draw') to compute risk curves within patient or within (patient,draw) groups without changing default behavior.
