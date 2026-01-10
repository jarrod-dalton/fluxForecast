## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `patientSimCore`.

## 1.2.1

## 1.2.2

- Add LICENSE file to align repository structure with ecosystem standards.

- Guard: forecast/risk/draws/streaming time inputs must be numeric model time. Calendar time inputs (Date/POSIXct) now error with a message that includes ctx$time$unit when available.

## 1.2.0

- Version bump to align with patientSim ecosystem v1.2.0. No functional changes.

# 1.1.2 (2026-01-06)

- Propagate optional Patient$id as patient_tag in forecast run_index and streaming summaries when supplied.
- Documentation cleanup and minor internal refactors.

## patientSimForecast 1.1.0

- Version bump (minor release).

## patientSimForecast 1.0.25

- Fix unit test schema in quantiles-multi-ctx to include age and miles_to_work.

## patientSimForecast 1.0.24

- Update unit tests to build schemas from patientSimCore::default_patient_schema() (engine-level variables).

## 1.0.22
- Remove: forecast() no longer reorders runs to match the run index. patientSimCore now guarantees the alignment invariant (runs[[i]] <-> index[i,]).

## 1.0.19
- Fix by-group binding in state_summary when per-group summaries have different column sets.

# patientSimForecast 1.0.17

- Fix: forecast() now reorders patientSimCore runs to match index row order before populating state/alive/defined matrices. This prevents silent run-index misalignment that caused by="patient" summaries to pool across patients.
- Fix: run_index now includes canonical draw_id column (aliasing param_set_id) for by="patient_draw" grouping.

# patientSimForecast 1.0.16

- Fix: state_summary(by='patient'|'patient_draw') now groups runs using split() on run row indices (not string-key matching), preventing accidental pooling across patients.

## 1.0.15
- Fix: state_summary(by != 'run') subsetting now carries `defined` and `first_event_time` correctly (ps_forecast invariants preserved).

## 1.0.14
- Fix: state_summary(by != 'run') no longer errors due to stray 'probs' argument in internal recursion.

## patientSimForecast 1.0.13

- Fix `state_summary(by='patient'|'patient_draw')` grouping by subsetting runs per group.
- Ensure package zip root folder is `patientSimForecast`.

## 1.0.11

- Fix unit test expectations for state_summary(by='patient') to not assume patient_id labels.
- Ensure DESCRIPTION ends with newline.

## patientSimForecast 1.0.10

- Fix unit test to avoid non-exported patientSimCore::ModelBundle reference.

## patientSimForecast 1.0.9

- Fix parse error in state_summary() categorical level guard.
- Add by= support to state_summary_forecast() streaming summaries.

## patientSimForecast 1.0.8

- Fix: parsing error introduced in 1.0.7 `state_summary.R` (stray brace / malformed stop message).

## patientSimForecast 1.0.7

- Add `by=` option to `state_summary()` for patient-level and patient+draw summaries (default remains run-level pooling).
- Add missing documentation for `state_summary()`.

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

## patientSimForecast 1.0.12
- Fix patient-level grouping in state_summary(by=...).
