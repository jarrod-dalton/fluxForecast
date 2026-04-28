## 1.10.1

- `validate_forecast()` now delegates schema validation to `fluxCore::schema_validate()` — single source of truth. Removed the inline duplicated `allowed_types` list and per-var `$levels` re-check. Future fluxCore type additions/removals propagate automatically.
- Dependency floor bumped to `fluxCore (>= 1.10.1)`.

## 1.10.0

- Updated allowed schema types to include all 14 new types from fluxCore 1.10.0.
- Dependency floor updated to `fluxCore (>= 1.10.0)`.

## 1.10.0

- Coordinated ecosystem release alignment to version 1.10.0.
- Tests no longer depend on `fluxCore:::default_entity_schema()`; Forecast test fixtures now define local schema helpers.
- Dependency floor updated to `fluxCore (>= 1.10.0)`.

## 1.8.0

- Added lifecycle fallback semantics in forecast summaries: supports explicit
  `alive` state when present and robust fallback pathways when lifecycle state
  is represented via terminal-event definitions.
- Added unit tests to lock lifecycle fallback behavior and edge cases.
- Added README release/download badges.

## 1.7.0

- Coordinated ecosystem release alignment to version 1.7.0.
- Dependency floor updated to `fluxCore (>= 1.7.0)`.
- No additional functional changes beyond the 1.5.1 canonical-time API update.

## 1.5.1

- Enforced canonical-time usage across forecast APIs: removed runtime
  `time_unit` argument paths from public functions and docs.
- Forecast, event-probability, draws, and streaming summaries now consume the
  engine/bundle canonical `time_spec` metadata consistently.
- Added tests for canonical time propagation and runtime context conflict
  rejection.

## 1.5.0

- Completed public API transition from risk naming to event-probability naming: event_prob(), event_prob_forecast(), and related docs/tests.

- Summary interfaces and manual Rd pages were aligned to implementation signatures (including by-argument ordering and usage blocks).

- Packaging/license hygiene for coordinated release: LGPL-3 and dependency floor updates.

## 1.4.0

- Internal naming cleanup: removed `flux_` prefixes from internal helper/constructor functions for consistency with current package naming conventions.
- Refactor: centralized `isTRUEorNA()` in `utils_internal.R` and removed duplicate local definitions.
- Documentation and file hygiene: aligned references with current names, removed roxygen-style blocks from `R/`, and standardized filenames to underscore style.

## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `fluxCore`.

## 1.2.1

## 1.2.2

- Add LICENSE file to align repository structure with ecosystem standards.

- Guard: forecast/risk/draws/streaming time inputs must be numeric model time. Calendar time inputs (Date/POSIXct) now error with a message that includes ctx$time$unit when available.

## 1.2.0

- Version bump to align with flux ecosystem v1.2.0. No functional changes.

# 1.1.2 (2026-01-06)

- Propagate optional Entity$id as entity_tag in forecast run_index and streaming summaries when supplied.
- Documentation cleanup and minor internal refactors.

## fluxForecast 1.1.0

- Version bump (minor release).

## fluxForecast 1.0.25

- Fix unit test schema in quantiles-multi-ctx to include age and miles_to_work.

## fluxForecast 1.0.24

- Update unit tests to build schemas from fluxCore::default_entity_schema() (engine-level variables).

## 1.0.22
- Remove: forecast() no longer reorders runs to match the run index. fluxCore now guarantees the alignment invariant (runs[[i]] <-> index[i,]).

## 1.0.19
- Fix by-group binding in state_summary when per-group summaries have different column sets.

# fluxForecast 1.0.17

- Fix: forecast() now reorders fluxCore runs to match index row order before populating state/alive/defined matrices. This prevents silent run-index misalignment that caused by="entity" summaries to pool across entities.
- Fix: run_index now includes canonical `param_draw_id` for by="entity_param_draw" grouping.

# fluxForecast 1.0.16

- Fix: state_summary(by='entity'|'entity_param_draw') now groups runs using split() on run row indices (not string-key matching), preventing accidental pooling across entities.

## 1.0.15
- Fix: state_summary(by != 'run') subsetting now carries `defined` and `first_event_time` correctly (flux_forecast invariants preserved).

## 1.0.14
- Fix: state_summary(by != 'run') no longer errors due to stray 'probs' argument in internal recursion.

## fluxForecast 1.0.13

- Fix `state_summary(by='entity'|'entity_param_draw')` grouping by subsetting runs per group.
- Ensure package zip root folder is `fluxForecast`.

## 1.0.11

- Fix unit test expectations for state_summary(by='entity') to not assume entity_id labels.
- Ensure DESCRIPTION ends with newline.

## fluxForecast 1.0.10

- Fix unit test to avoid non-exported fluxCore::ModelBundle reference.

## fluxForecast 1.0.9

- Fix parse error in state_summary() categorical level guard.
- Add by= support to state_summary_forecast() streaming summaries.

## fluxForecast 1.0.8

- Fix: parsing error introduced in 1.0.7 `state_summary.R` (stray brace / malformed stop message).

## fluxForecast 1.0.7

- Add `by=` option to `state_summary()` for entity-level and entity+draw summaries (default remains run-level pooling).
- Add missing documentation for `state_summary()`.

## fluxForecast 1.0.6

- Fix: parsing error introduced in `state_summary.R`.
- Keep: `risk_forecast(..., by=)` supports run-, entity-, and entity_param_draw-level summaries.

## fluxForecast 1.0.4

- Fix: parsing error in `risk()` argument list (escaped quotes).
- Enhancement (from 1.0.3): `risk(by=...)` supports run-, entity-, and entity_param_draw-level summaries.

# fluxForecast 1.0.1


## fluxForecast 1.0.2
- Internal: consolidate NA-safe TRUE-check helpers to avoid scalar/vector pitfalls.
- Made TRUE/FALSE checks more robust in forecast matrices and streaming summaries (preserve NA where appropriate; avoid `isTRUE()` scalar pitfalls).

# fluxForecast 1.0.0

- `forecast()` API standardized on `backend=` and `ctx=`; legacy `parallel=`/`ctx_base` pathways removed.
- Posterior predictive pooling across multiple parameter sets supported via list-of-ctx semantics (equal-weight by construction when runs per set are balanced).
- `state_summary()` eligibility bug fixed (vectorized alive filtering) and locked down with new unit tests for multi-ctx quantiles and follow-up stopping semantics.


## fluxForecast 1.0.3
- risk() now supports by = c('run','entity','entity_param_draw') to compute risk curves within entity or within (entity,draw) groups without changing default behavior.

## fluxForecast 1.0.12
- Fix entity-level grouping in state_summary(by=...).
