
# Posterior summary mathematics in `patientSimForecast`

This document describes how `patientSimForecast` constructs posterior summaries from simulated trajectories produced by `patientSimCore`. These summaries are **empirical functionals of simulated runs**, not estimators derived from hazard models.

The emphasis throughout is on:
- clarity of denominators,
- explicit conditioning rules, and
- predictable behavior under terminal events and competing risks.

---

## Notation and setup

Assume a forecast produces:

- `S` simulation runs per patient per parameter set
- indexed by `s = 1, …, S`
- with event histories observed over time

Let:
- `T_e(s)` denote the *first occurrence time* of event `e` in run `s` (or `∞` if it never occurs),
- `A(s, t)` indicate whether run `s` is alive at time `t`,
- `F(s, t)` indicate whether run `s` is in active follow-up at time `t`.

All summaries are evaluated on a user-supplied reporting grid:

```
times = (t₁, t₂, …, t_K)
```

---

## Simulation origin vs summary reference time

A critical distinction in `patientSimForecast` is between:

### Simulation origin
- The time at which the simulation begins (e.g., time 0, age 40, transplant date).
- Determined entirely by the model and engine.
- No events can occur before this time.

### Summary reference time
- The time at which **denominators** for risk and survival summaries are defined.
- This is *not* automatically the simulation origin.

In survival-analysis language, this is **not a “baseline hazard” concept**. It is purely a reference point for conditioning.

---

## Summary reference time for denominators

Risk and survival summaries are defined relative to a **summary reference time**, denoted `t_ref`.

The rules are:

```
t_ref = start_time   if start_time is supplied
t_ref = times[1]     otherwise
```

Eligibility conditions are evaluated **once**, at `t_ref`.

This answers the question:

> “Among runs eligible at the start of the reporting window, what fraction experience the event by later reporting times?”

---

## Why `times[1]` is the default reference

The `times` argument is a **reporting grid**, not a declaration of when the simulation begins.

Typical usage looks like:

```
times = c(1, 3, 5, 10)
```

Here, analysts usually intend:
- the forecast window to begin at time 1, and
- summaries to be interpreted relative to that window.

Requiring `times = c(0, 1, 3, 5, 10)` just to define denominators at time 0 would:
- inflate forecast objects,
- produce redundant zero-risk outputs,
- and obscure the intended reporting window.

Using `times[1]` as the default summary reference keeps outputs compact and semantically aligned with user intent.

---

## When to use `start_time`

If you want denominators defined at the **simulation origin** (or any earlier time) *without* reporting values there, use `start_time`.

Example:

```
times      = c(1, 3, 5, 10)
start_time = 0
```

This defines eligibility at time 0 while reporting summaries only at later times.

---

## Eligibility at the reference time

A run `s` is eligible at `t_ref` if all required conditions hold, for example:

- `A(s, t_ref) = TRUE` (alive)
- `F(s, t_ref) = TRUE` (in follow-up)
- optional event-free conditions (e.g., no prior MI)

Let:

```
E = { s : s is eligible at t_ref }
N = |E|
```

All denominators are based on `N`, unless explicitly stated otherwise.

---

## Risk summaries

### Definition

For an event type `e`, the **risk** at time `t_k` is:

```
Risk_e(t_k) = (1 / N) * Σ_{s ∈ E} I(T_e(s) ≤ t_k)
```

That is:
- the proportion of eligible runs whose *first occurrence* of event `e`
- occurs at or before `t_k`.

### Key properties

- The denominator is **fixed at `t_ref`**
- There is **no redefinition of the risk set** over time
- This is not a Kaplan–Meier or hazard-based estimator

Risk curves are monotone non-decreasing by construction.

---

## Survival summaries

For a single event `e`, survival is defined as the complement:

```
Survival_e(t_k) = 1 − Risk_e(t_k)
```

Equivalently:

```
Survival_e(t_k) = (1 / N) * Σ_{s ∈ E} I(T_e(s) > t_k)
```

This represents the proportion of eligible runs that remain event-free through `t_k`.

---

## Relation to Kaplan–Meier

Kaplan–Meier estimation:
- redefines the risk set at every observed event time,
- conditions on having survived up to that time,
- estimates a survival function via hazards.

`patientSimForecast` does **none** of these.

Instead, it reports **direct empirical proportions** from simulated trajectories, conditioned once at `t_ref`.

This design is intentional:
- it avoids hazard modeling assumptions,
- it aligns naturally with forward simulation,
- and it produces stable denominators for comparison across scenarios.

---

## Competing risks and CIFs

For multiple event types `{e₁, e₂, …}`, the **cumulative incidence function (CIF)** for event `e_j` is:

```
CIF_j(t_k) = (1 / N) * Σ_{s ∈ E} I(T_{e_j}(s) ≤ t_k)
```

### Important consequence

Because each run contributes **at most one first event**:

```
Σ_j CIF_j(t_k) ≤ 1
```

Equality holds only if *every* eligible run experiences *some* modeled event by `t_k`.

This avoids the non-additivity issues seen in hazard-based competing-risks models (e.g., cause-specific Cox or Fine–Gray).

---

## Terminal events and model scope

A crucial modeling choice is that **simulations stop tracking outcomes after terminal events**.

Example:
- If MI is terminal in the model,
- and stroke could occur after MI in reality,
- then stroke events after MI **cannot appear** in the simulation.

As a result:
- Stroke CIFs are interpreted as  
  *“incidence of stroke before MI (or other terminal events)”*.
- CIFs do **not** represent marginal lifetime incidence unless the model explicitly tracks all outcomes indefinitely.

This behavior is correct given the model, but it must be understood by the analyst.

---

## Follow-up gaps and eligibility

If a run leaves follow-up before `t_k`:
- it does **not** contribute an event after that time,
- but it **remains in the denominator** if it was eligible at `t_ref`.

This mirrors a policy-style estimand:
- outcomes are attributed to the baseline-eligible cohort,
- not dynamically reweighted by later censoring.

---

## State variable summaries

In addition to event summaries, `patientSimForecast` can summarize **state variables** at each reporting time.

### Variable classification

Variables are classified as:

**Categorical**
- logical
- character
- factor
- integer with unique values `{0, 1}`

**Continuous**
- numeric
- integer variables not restricted to `{0, 1}`

---

### Categorical state summaries

For a categorical variable `X` with levels `{ℓ₁, …, ℓ_m}`:

```
P(X = ℓ_j at t_k) =
  (1 / M_k) * Σ I(X(s, t_k) = ℓ_j)
```

where `M_k` is the number of runs with observed state at `t_k`.

Outputs are proportions by level.

---

### Continuous state summaries

For continuous variables, summaries include:

- mean
- standard deviation
- minimum
- first quartile (Q1)
- median
- third quartile (Q3)
- maximum

All are computed empirically across runs with observed state at `t_k`.

---

## Summary

`patientSimForecast` produces:
- **fixed-denominator**, policy-style summaries,
- directly from simulated trajectories,
- with explicit conditioning and no hazard modeling.

Key takeaways:
- Denominators are defined once, at a summary reference time.
- Risk and survival are empirical proportions, not estimators.
- CIFs are additive by construction.
- Terminal events define the scope of what can be counted.
- State summaries follow transparent, type-based rules.

Understanding these design choices is essential for correct interpretation.

