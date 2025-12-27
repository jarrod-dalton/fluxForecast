# Posterior predictive summaries in patientSimForecast

This note documents the intended math and conditioning conventions used by
**patientSimForecast** when summarizing forward simulations.

## Posterior predictive interpretation

For a given patient, each simulation run produces:

1. State snapshots evaluated on a finite time grid `times`
2. An event history (event types with times)

When `param_sets` represents draws from a parameter posterior, the collection
of simulation runs is interpreted as draws from the **posterior predictive**
distribution. In the simplest case, each run is equally weighted.

### Pooling over parameter sets

Let `j` index parameter sets and `k` index simulation replicates within a
parameter set. For a fixed time `t` and outcome `Y(t)`, we obtain values
`Y(j,k,t)`.

If each parameter set is used for the same number of runs, then the posterior
predictive mean is estimated by the simple average over all runs:

- mean at time t: average of all `Y(j,k,t)` values

The posterior predictive standard deviation is estimated by the standard sample
standard deviation over all runs.

Quantiles (Q1, median, Q3, etc.) are estimated by pooling all run-level values
and taking the empirical quantiles. Quantiles should not be averaged across
parameter sets.

If different parameter sets are intentionally used with different counts, then
pooling induces a corresponding weighting. The recommended practice is to keep
run counts per parameter set equal when equal posterior weights are desired.

## Conditioning: alive versus active follow-up

The schema variable `alive` is treated as **canonical** (biological truth).
However, whether a run is considered *in follow-up* at a given time can be
model-dependent. Example: a transplant model may stop simulation at transplant
while the patient remains alive.

To support this, summaries distinguish:

- Alive at time t: `alive(t)` from the patient snapshot
- In follow-up at time t: `in_followup(t)` (sometimes operationalized as
  definedness, meaning the simulation provides state at time t)

A run can be alive but not in follow-up. In that case, state variables at that
future time are treated as missing for summarization.

### State summaries

State summaries at each time t are computed among runs satisfying the time
specific eligibility mask. The default mask is:

- in_followup(t) is TRUE
- alive(t) is TRUE

This yields summaries of `Y(t)` among those alive and observed at t.

### Risk and survival summaries

Risk curves are typically defined with a **fixed** denominator set at a
baseline time (often the first element of `times`). Eligibility at baseline can
require:

- in_followup(t0) is TRUE
- alive(t0) is TRUE
- optional event-free conditions at t0

Risk at time t is then estimated as the proportion of eligible runs whose first
occurrence time for the event of interest is less than or equal to t.

## Practical guidance for performance

For most intended uses (for example 100 runs per patient and 3 to 5 times), it
is inexpensive to return run by time matrices for a selected subset of variables
and compute the full five number summary (min, Q1, median, Q3, max) on the
primary node. The dominant performance risks in R tend to be object churn and
repeated list growth, not the final pooling arithmetic.
