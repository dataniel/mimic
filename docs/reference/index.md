# Package index

## The workflow

From the real data to released output, in order.

- [`mimic-package`](mimic-package.md) : mimic: synthetic data and output
  control for small populations
- [`mimic_config()`](mimic_config.md) : Disclosure control settings for
  small populations
- [`profile_data()`](profile_data.md) : Describe a dataset safely
- [`mimic()`](mimic.md) : Build a synthetic copy from a profile
- [`mess_options()`](mess_options.md) : Choose which kinds of mess to
  add
- [`trial_run()`](trial_run.md) : Try a script on the synthetic copy
- [`real_run()`](real_run.md) : Run a script on the real data
- [`inspect()`](inspect.md) : Check output before release
- [`write_report()`](write_report.md) : Write an inspection report for
  the approver
- [`record_release()`](record_release.md) : Record released output in a
  register

## Safe output

For the researcher’s script, so output passes the inspection.

- [`safe_output`](safe_output.md) : Safe output for the researcher's
  script
- [`safe_count()`](safe_count.md) : Count persons with small cells
  suppressed
- [`safe_summary()`](safe_summary.md) : Summarise a numeric variable
  without small or dominated groups
- [`safe_model()`](safe_model.md) : A table of model coefficients that
  is safe to release
- [`describe_output()`](describe_output.md) : Describe an output file
  for the checker

## Checks

The rules behind the inspection, usable on their own.

- [`checks`](checks.md) : Disclosure and data checks
- [`check_min_count()`](check_min_count.md) : Check that every cell has
  enough persons
- [`check_k_anonymity()`](check_k_anonymity.md) : Check combinations of
  quasi-identifiers (k-anonymity)
- [`check_dominance()`](check_dominance.md) : Check that no few
  contributors dominate a sum
- [`check_extremes()`](check_extremes.md) : Check for minimum, maximum
  and other single-person values
- [`check_model()`](check_model.md) : Check that a model rests on enough
  observations
- [`check_differencing()`](check_differencing.md) : Check that two
  tables cannot be subtracted
- [`check_suppression()`](check_suppression.md) : Check that suppressed
  cells cannot be recovered
- [`check_group_disclosure()`](check_group_disclosure.md) : Check that
  no group shares one value of a variable
- [`check_keys()`](check_keys.md) : Check keys of linked tables
- [`check_all()`](check_all.md) : Collect checks into one report

## Example data

Invented data for a small population.

- [`fake_persons`](fake_data.md) [`fake_contacts`](fake_data.md)
  [`fake_admissions`](fake_data.md) : Invented example data for a small
  population
