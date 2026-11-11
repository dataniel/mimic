# Disclosure and data checks

The `check_*()` functions are the building blocks of output control.
Each tests one rule and returns a `mimic_check` object with the same
fields, so results can be printed one by one or collected with
[`check_all()`](check_all.md). [`profile_data()`](profile_data.md) and
[`inspect()`](inspect.md) are built on them.

## Details

|  |  |
|----|----|
| Function | Rule |
| [`check_min_count()`](check_min_count.md) | every cell or group has at least `min_count` persons |
| [`check_k_anonymity()`](check_k_anonymity.md) | combinations of quasi-identifiers are shared by at least `min_count` persons |
| [`check_dominance()`](check_dominance.md) | no few contributors dominate a sum ((n, k) rule and p % rule) |
| [`check_extremes()`](check_extremes.md) | no minimum, maximum or other single-person values |
| [`check_model()`](check_model.md) | models have enough observations and degrees of freedom |
| [`check_differencing()`](check_differencing.md) | two tables cannot be subtracted to reveal a small group |
| [`check_suppression()`](check_suppression.md) | suppressed cells cannot be recovered from totals |
| [`check_group_disclosure()`](check_group_disclosure.md) | no group shares one value of a variable |
| [`check_keys()`](check_keys.md) | keys are complete and linked tables fit together |

## What a check reveals

A check never prints counts below `min_count`; counts are rounded and
small ones are shown as "fewer than 5". It does show *which* groups
fail, using the labels in the table being checked, because the person
approving output needs that to ask for a fix. Results from
[`check_k_anonymity()`](check_k_anonymity.md) only contain totals, never
the rare combinations themselves. Results computed on real data belong
in the secure environment unless reviewed.

## Result fields

- `rule`: name of the rule.

- `target`: what was checked.

- `status`: `"pass"`, `"warn"` or `"fail"`.

- `message`: one sentence for a human.

- `details`: a data frame with failing groups or issues, or `NULL`.
