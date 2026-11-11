# mimic

Synthetic data and output control for register data on **small
populations**.

mimic ties together a remote execution workflow: researchers develop
their R script on a synthetic copy of the data, the script runs on the
real data in a secure environment, and only output that cannot reveal
persons is released. Defaults are stricter than in tools designed for
large countries, because in a small population with many small
settlements a cell of three is often a known household.

## Installation

\
`# install.packages("remotes")`\
`remotes``::`[`install_local`](https://remotes.r-lib.org/reference/install_local.html)`(``"path/to/mimic"``)`

## The workflow

\
[`library`](https://rdrr.io/r/base/library.html)`(``mimic``)`\
`cfg`` ``<-`` `[`mimic_config`](reference/mimic_config.md)`(``quasi_ids ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"birth_date"``, ``"sex"``, ``"settlement"``)``,`\
`                    geo_vars ``=`` ``"settlement"``, text_vars ``=`` ``"note"``)`\
\
`# inside: describe the real data, protection applied before storing`\
`m`` ``<-`` `[`profile_data`](reference/profile_data.md)`(`[`list`](https://rdrr.io/r/base/list.html)`(``persons ``=`` ``fake_persons``, contacts ``=`` ``fake_contacts``)``,`\
`                  keys ``=`` ``"person_id"``, config ``=`` ``cfg``)`\
[`saveRDS`](https://rdrr.io/r/base/readRDS.html)`(``m``, ``"profile.rds"``)``                 ``# safe to take out`\
\
`# outside: build a copy and try the script on it. In the script,`\
`# safe_count(), safe_summary() and safe_model() make output that passes,`\
`# and describe_output() tells the checker what each file shows`\
`copy`` ``<-`` `[`mimic`](reference/mimic.md)`(``m``, seed ``=`` ``1``)`\
[`trial_run`](reference/trial_run.md)`(``"analysis.R"``, ``copy``, config ``=`` ``cfg``)``   ``# also checks the output in advance`\
\
`# inside: run on the real data and check the output`\
`r`` ``<-`` `[`real_run`](reference/real_run.md)`(``"analysis.R"``, `[`list`](https://rdrr.io/r/base/list.html)`(``persons ``=`` ``fake_persons``, contacts ``=`` ``fake_contacts``)``, ``"out"``)`\
`res`` ``<-`` `[`inspect`](reference/inspect.md)`(``"out"``, config ``=`` ``cfg``, real_data ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``persons ``=`` ``fake_persons``)``,`\
`               register ``=`` ``"register"``)`\
[`write_report`](reference/write_report.md)`(``res``, ``"inspection.md"``)`\
[`record_release`](reference/record_release.md)`(``res``, ``"register"``)``           ``# after the approver has decided`

See [`vignette("mimic")`](articles/mimic.md) for the full walk-through.

## Functions

| Function | Where | What it does |
|----|----|----|
| [`mimic_config()`](reference/mimic_config.md) |  | Disclosure rules: `min_count`, dominance, quasi-identifiers, geography, date granularity |
| [`profile_data()`](reference/profile_data.md) | inside | Describes one or more linked tables with protection applied before storing, plus a risk report |
| [`mimic()`](reference/mimic.md) | outside | Builds a synthetic copy at fidelity 1 (structure), 2 (distributions), 3 (correlations) or 4 (model based, inside only) |
| [`mess_options()`](reference/mess_options.md) | outside | Chooses the realistic mess in the copy: missing values, invalid codes, extremes, empty strings, odd dates, orphaned rows |
| [`safe_count()`](reference/safe_count.md) | script | Counts persons with small cells suppressed, also where a total would give them back, and the rest rounded |
| [`safe_summary()`](reference/safe_summary.md) | script | Means, medians, quartiles of a variable with small and dominated groups suppressed, never min or max |
| [`safe_model()`](reference/safe_model.md) | script | The coefficient table instead of the model object, after checking the observations behind it |
| [`describe_output()`](reference/describe_output.md) | script | Says what a file shows, its count and grouping columns, the numbers behind a figure and what changed; [`inspect()`](reference/inspect.md) uses it and the report shows it |
| [`trial_run()`](reference/trial_run.md) | outside | Runs a script on the copy in an isolated session: does it run, which packages, which files, any row-level output, and what [`inspect()`](reference/inspect.md) would say about the output |
| [`real_run()`](reference/real_run.md) | inside | Runs a script on the real data: output only in `output_dir`, a log of the code only (or cleaned output), safe error report |
| [`inspect()`](reference/inspect.md) | inside | Checks output files and gives each a verdict: approved, needs changes or review by hand |
| [`write_report()`](reference/write_report.md) | inside | Writes the inspection as a Markdown report for the approver |
| [`record_release()`](reference/record_release.md) | inside | Keeps released tables in a register, so `inspect(register = )` checks new output against them |
| [`check_min_count()`](reference/check_min_count.md) |  | Cells and groups with fewer than `min_count` persons |
| [`check_k_anonymity()`](reference/check_k_anonymity.md) |  | Rare combinations of quasi-identifiers |
| [`check_dominance()`](reference/check_dominance.md) |  | (n, k) dominance rule and p % rule |
| [`check_extremes()`](reference/check_extremes.md) |  | Minimum, maximum and range values |
| [`check_model()`](reference/check_model.md) |  | Models with too few observations or degrees of freedom |
| [`check_differencing()`](reference/check_differencing.md) |  | Tables that can be subtracted to reveal a small group |
| [`check_suppression()`](reference/check_suppression.md) |  | Suppressed cells that the totals give back |
| [`check_group_disclosure()`](reference/check_group_disclosure.md) |  | Groups whose persons all share one value |
| [`check_keys()`](reference/check_keys.md) |  | Complete keys and linked tables that fit together |
| [`check_all()`](reference/check_all.md) |  | Collects checks into one report |

## Principles

- Data protection comes first. When in doubt, the package chooses the
  safe option and warns.
- No real rows leave the secure environment: not in the copy, the
  profile, error messages or logs.
- Built against mistakes, not malice: a researcher who wants to smuggle
  data out can hide it in allowed output. Contracts, staff who read the
  script and limits on runs must cover that (see
  [`?mimic`](reference/mimic.md)).
- Low fidelity by default; higher levels need a deliberate choice.
- Few dependencies: cli and callr. rpart (shipped with R) is only used
  for level 4.
- The synthesis methods are implemented in the package, so they can be
  tuned for small populations.

All example data in the package are invented.
