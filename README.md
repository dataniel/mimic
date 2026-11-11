# mimic

Synthetic data and output control for register data on **small
populations**.

mimic ties together a remote execution workflow: researchers develop
their R script on a synthetic copy of the data, the script runs on the real
data in a secure environment, and only output that cannot reveal persons is
released. Defaults are stricter than in tools designed for large countries,
because in a small population with many small settlements a cell of three
is often a known household.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/mimic")
```

## The workflow

```r
library(mimic)
cfg <- mimic_config(quasi_ids = c("birth_date", "sex", "settlement"),
                    geo_vars = "settlement", text_vars = "note")

# inside: describe the real data, protection applied before storing
m <- profile_data(list(persons = fake_persons, contacts = fake_contacts),
                  keys = "person_id", config = cfg)
saveRDS(m, "profile.rds")                 # safe to take out

# outside: build a copy and try the script on it. In the script,
# safe_count(), safe_summary() and safe_model() make output that passes,
# and describe_output() tells the checker what each file shows
copy <- mimic(m, seed = 1)
trial_run("analysis.R", copy, config = cfg)   # also checks the output in advance

# inside: run on the real data and check the output
r <- real_run("analysis.R", list(persons = fake_persons, contacts = fake_contacts), "out")
res <- inspect("out", config = cfg, real_data = list(persons = fake_persons),
               register = "register")
write_report(res, "inspection.md")
record_release(res, "register")           # after the approver has decided
```

See `vignette("mimic")` for the full walk-through.

## Functions

| Function | Where | What it does |
|---|---|---|
| `mimic_config()` | | Disclosure rules: `min_count`, dominance, quasi-identifiers, geography, date granularity |
| `profile_data()` | inside | Describes one or more linked tables with protection applied before storing, plus a risk report |
| `mimic()` | outside | Builds a synthetic copy at fidelity 1 (structure), 2 (distributions), 3 (correlations) or 4 (model based, inside only) |
| `mess_options()` | outside | Chooses the realistic mess in the copy: missing values, invalid codes, extremes, empty strings, odd dates, orphaned rows |
| `safe_count()` | script | Counts persons with small cells suppressed, also where a total would give them back, and the rest rounded |
| `safe_summary()` | script | Means, medians, quartiles of a variable with small and dominated groups suppressed, never min or max |
| `safe_model()` | script | The coefficient table instead of the model object, after checking the observations behind it |
| `describe_output()` | script | Says what a file shows, its count and grouping columns, the numbers behind a figure and what changed; `inspect()` uses it and the report shows it |
| `trial_run()` | outside | Runs a script on the copy in an isolated session: does it run, which packages, which files, any row-level output, and what `inspect()` would say about the output |
| `real_run()` | inside | Runs a script on the real data: output only in `output_dir`, a log of the code only (or cleaned output), safe error report |
| `inspect()` | inside | Checks output files and gives each a verdict: approved, needs changes or review by hand |
| `write_report()` | inside | Writes the inspection as a Markdown report for the approver |
| `record_release()` | inside | Keeps released tables in a register, so `inspect(register = )` checks new output against them |
| `check_min_count()` | | Cells and groups with fewer than `min_count` persons |
| `check_k_anonymity()` | | Rare combinations of quasi-identifiers |
| `check_dominance()` | | (n, k) dominance rule and p % rule |
| `check_extremes()` | | Minimum, maximum and range values |
| `check_model()` | | Models with too few observations or degrees of freedom |
| `check_differencing()` | | Tables that can be subtracted to reveal a small group |
| `check_suppression()` | | Suppressed cells that the totals give back |
| `check_group_disclosure()` | | Groups whose persons all share one value |
| `check_keys()` | | Complete keys and linked tables that fit together |
| `check_all()` | | Collects checks into one report |

## Principles

* Data protection comes first. When in doubt, the package chooses the safe
  option and warns.
* No real rows leave the secure environment: not in the copy, the
  profile, error messages or logs.
* Built against mistakes, not malice: a researcher who wants to smuggle data
  out can hide it in allowed output. Contracts, staff who read the script and
  limits on runs must cover that (see `?mimic`).
* Low fidelity by default; higher levels need a deliberate choice.
* Few dependencies: cli and callr. rpart (shipped with R) is only used for
  level 4.
* The synthesis methods are implemented in the package, so they can be
  tuned for small populations.

All example data in the package are invented.
