# Posterior summary math (patientSimForecast)

This note describes how **risk**, **event-free/survival**, and **state** summaries are computed from simulated trajectories produced by `patientSimCore`.

These summaries are **Monte Carlo functionals of the simulated process implied by your model**. They are not “estimators” from observed event-time data (so they intentionally do not follow Kaplan–Meier / Aalen–Johansen step-by-step risk-set updates).

## Notation

For a given patient and a given simulation run (a particular draw/parameter set/seed combination), the engine produces a trajectory with:

- a sequence of event times (nondecreasing) and event types
- a time-varying state `X(t)` (stored sparsely at event times, but queryable at arbitrary `t` via `state_at_time()` semantics)

Let the user-requested evaluation times be `times = {t1, t2, ..., tK}` (sorted increasingly). Let the simulation’s natural start time be `t_start` (the engine’s `time0` for the patient/run). In general, `t_start` and `t1` may differ.

## Common ingredients

### Eligibility

All posterior summaries rely on an **eligibility indicator** (a run can contribute to a summary at time `t` only if it is eligible at `t`). Conceptually:

- `alive(t)` must be TRUE
- `in_followup(t)` must be TRUE (unless you intentionally choose to ignore follow-up)
- optionally, additional event-free conditions can be enforced at a baseline time (see below)

The package keeps the eligibility logic explicit because it is easy to build models that stop tracking individuals (terminal events) or have intermittent follow-up. Those design choices directly change what a “risk” curve means.

### Baseline time for denominators

Many “risk” curves are defined with a **fixed baseline denominator** at some baseline time `t0`.

Important nuance: in `patientSimForecast`, `t0` is a *summary baseline*, not automatically the simulation’s `t_start`.

- By default, `t0` is taken as the first element of `times` (i.e., `t0 = t1`).
- You can conceptually think of this as: “Among the runs eligible at the first reporting time, what fraction have had the event by later reporting times?”

Why this default?

- The forecast functions are often used with a reporting grid like `times = 1:10` or `times = c(1, 3, 5, 10)` where the user intends `t1` to be the start of the forecast window.
- Models sometimes initialize at time 0 (or an age) but the analyst only wants forecasts for a later window.

If you want the denominator to be defined at the simulation start, you should pass `times` such that the first element aligns to your intended baseline, or use `start_time` / eligibility arguments in the forecast helpers.

## Risk and event-free/survival summaries

### Event time definitions

For an event type `E` (e.g., `"tx"`, `"death"`, `"stroke"`), define for each run the **first occurrence time**:

- `T_E = inf` if event `E` never occurs in the simulated trajectory
- otherwise `T_E = min{ t : event_type(t) == E }`

Also define a follow-up indicator `F(t)` that is TRUE when the run is considered “observable” at time `t` (e.g., `in_followup(t)`).

### Risk (cumulative incidence) for a single event

With a fixed baseline denominator at `t0`, define baseline eligibility:

- `Elig0 = 1{ alive(t0) & F(t0) & (optional baseline event-free conditions) }`

Then risk at time `t` is estimated as:

- `Risk_E(t) = mean( 1{T_E <= t} | Elig0 == 1 )`

Operationally: among runs eligible at `t0`, what proportion have experienced `E` by `t`.

This is *not* computed as `1 - n(event_free at t) / n(at risk at t)` with a time-varying denominator, and it is not a product-limit estimator. It is simply the Monte Carlo probability implied by your simulation.

### Event-free / survival-style curves

For a single event type `E`, the “event-free” curve is:

- `EventFree_E(t) = mean( 1{T_E > t} | Elig0 == 1 )`

So (up to Monte Carlo error):

- `Risk_E(t) + EventFree_E(t) = 1`.

### Why this differs from Kaplan–Meier / Aalen–Johansen

Kaplan–Meier (and Aalen–Johansen for competing risks) is a **nonparametric estimator from observed, censored event-time data**. Its denominator (“number at risk”) changes at each event time because the dataset is incomplete and censoring must be handled carefully.

Here you already have a **generative model** that produces full trajectories (subject to the model’s own follow-up/terminal rules). The most direct posterior summary is simply:

- probability of the event by time `t` under the model.

If your model includes intermittent follow-up or terminal stops, those features are part of the generative process, and the summary should reflect them.

## Competing risks and whether CIFs sum to 1

Suppose you have mutually exclusive event types `{E1, ..., EJ}` and (crucially) the simulation **continues tracking** until exactly one of these events occurs, or until administrative end-of-horizon.

If:

1. every run that is eligible at `t0` remains in follow-up (`F(t)=TRUE`) up to the horizon or until an event happens, and
2. exactly one of the competing events is guaranteed to occur eventually (or you include an additional terminal state like “none by horizon” as an event type), and
3. the event types are defined as the *first* occurrence (so they are mutually exclusive by construction),

then:

- the CIFs `Risk_Ej(t)` will sum to 1 as `t` approaches the horizon where one of the events is certain.

However, in many simulation models, **other terminal events can stop the trajectory** (e.g., a model “ends” at MI). In that case:

- A stroke that would have occurred after an MI is not recorded because the model did not simulate beyond MI.
- The CIF for stroke, as computed above, is therefore the probability of stroke occurring **before the simulation terminates** (which may be MI).

That can be exactly what you want (it matches the model definition), but it is easy to misinterpret.

### Important warning about terminal events

If your model stops tracking the process at a terminal event `T` (for example, MI), then any event `E` that can occur after `T` in reality cannot be counted after `T` because the trajectory is no longer simulated.

In that situation, `Risk_E(t)` should be interpreted as:

- `P( T_E <= t AND T_E occurs before termination )` among runs eligible at `t0`.

Equivalently, it is the CIF for `E` in a competing risks system that includes the terminal event(s) as competing endpoints.

If you want a CIF for stroke that includes strokes after MI, the simulation must continue after MI (or MI must not be terminal for stroke).

## Handling intermittent follow-up

If a run can drop out of follow-up and later re-enter, you must choose what “risk” means.

The current default pattern in `patientSimForecast` is:

- baseline denominator is fixed at `t0` using `Elig0`
- subsequent times `t` do not change the denominator

This implies you are estimating:

- probability of the event by time `t` among those eligible at `t0`, regardless of later follow-up gaps.

That is coherent as a model-implied probability, but it implicitly treats events during out-of-follow-up periods as “still happening” if the model simulates them.

If instead your model treats out-of-follow-up as “unobservable” (events can occur but would not be recorded), then you should:

- either incorporate that into the simulation itself (so events are not generated/recorded while out of follow-up), or
- define eligibility in a way that conditions on being in follow-up at relevant times (e.g., using `eligible` or baseline conditioning rules).

In short: follow-up behavior should be encoded consistently between the model and the summary.

## State summaries

State summaries are computed from `X(t)` at each requested time `t` across simulation runs.

### Which runs contribute at time t

For state summaries we use time-specific eligibility:

- `Elig(t) = 1{ alive(t) & F(t) }` (plus any user-specified `eligible` filter)

Then state summaries at time `t` use only runs with `Elig(t)==1`.

### Variable typing rules

The state summary logic uses the following pragmatic typing rules:

- treat as **categorical** if the variable is `logical`, `character`, `factor`, or an `integer` with unique values exactly `{0, 1}`
- otherwise treat as **continuous** if the variable is `numeric`, or an `integer` not satisfying the `{0,1}` rule

(If you want stricter behavior, the safest approach is to provide an explicit `summary_spec` so the package doesn’t guess.)

### Summary statistics

For each time `t` and each variable:

- **Categorical**: report proportions by observed level/value among eligible runs at `t`.
  - Missing values are treated as their own category only if they appear explicitly in the data (otherwise they are dropped from the denominator).
- **Continuous**: report `n`, `mean`, `sd`, `min`, `q1`, `median`, `q3`, `max` among eligible runs at `t`.

### Coupling with risk summaries

When `forecast(..., return = "summary_stats", summary_stats = "both")` is used:

- risk and state summaries are computed from the **same set of simulation runs** (same seed, backend plan, parameter set indexing, etc.)
- the only difference is the eligibility logic and the functional applied to each run’s trajectory

This guarantees internal consistency (e.g., if a run is not alive at time `t`, it cannot contribute to state summaries at `t`).
