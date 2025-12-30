# patientSimForecast 0.5.0

- `forecast()` API standardized on `backend=` and `ctx=`; legacy `parallel=`/`ctx_base` pathways removed.
- Posterior predictive pooling across multiple parameter sets supported via list-of-ctx semantics (equal-weight by construction when runs per set are balanced).
- `state_summary()` eligibility bug fixed (vectorized alive filtering) and locked down with new unit tests for multi-ctx quantiles and follow-up stopping semantics.
