# Describe a dataset safely

`profile_data()` records what is needed to make a convincing copy and
nothing that identifies a person. It describes one table or a set of
linked tables and applies the protection rules **before** anything is
stored, so the result can be saved with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and taken out of the
secure environment. [`mimic()`](mimic.md) builds the synthetic copy from
it.

## Usage

``` r
profile_data(tables, keys = NULL, config = NULL, main = NULL)
```

## Arguments

- tables:

  A data frame, or a named list of linked data frames.

- keys:

  Key linking the tables: one column name used in every table, or a
  named character vector mapping table names to key columns. Persons are
  counted on this key.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- main:

  Name of the table with one row per person. By default the first table
  whose key is unique.

## Value

An object of class `mimic_profile` with elements `tables` (variable
descriptions per table), `keys`, `n_persons` (rounded), `config` and
`risk` (a risk report, see Details).

## Details

The risk report (`$risk`) lists every variable that protection changed
and how, checks combinations of quasi-identifiers with
[`check_k_anonymity()`](check_k_anonymity.md) (totals only), checks the
keys with [`check_keys()`](check_keys.md), and says whether the
population is too small for synthesis above level 2 (`min_persons`).
Variables that were classified as identifiers or free text automatically
are reported with a warning, so a wrong guess is noticed.

## What is stored

For every variable: name, type, share missing, and depending on type

- **categorical** (character, factor): allowed values and rounded
  frequencies. Categories with fewer than `min_count` persons are merged
  into `other_label`; if the merged group itself is too small, the next
  smallest category is merged too. Factor levels that no one holds are
  dropped: they are labels of fewer than `min_count` persons as well,
  and may come from a list outside the data.

- **geographical** (`geo_vars`): as categorical, flagged so that small
  units (settlements) are reported.

- **numeric**: bottom and top coded bounds, a coarse histogram whose
  bins each hold at least `min_count` persons, and rounded mean, median
  and standard deviation computed after top and bottom coding. Integer
  variables with few values keep their values as bins.

- **dates**: as numeric, with bounds coarsened to `date_unit` and bins
  by year.

- **logical**: rounded frequencies; a value held by fewer than
  `min_count` persons is suppressed.

- **identifiers** (keys, `id_vars`, and columns that look like ids): the
  format only, such as `"A9999999"`. Letters or punctuation shared by
  every value are kept only when at least `min_persons` values share
  them; digits never are.

- **free text** (`text_vars`, and long character columns that are mostly
  unique): only the approximate length.

- **names and addresses**: a categorical variable with at least 20
  categories, more than half of them held by fewer than `min_count`
  persons, is stored as an identifier. Its few common values are
  households or blocks of flats, which is not a category to publish. To
  keep a code like this as categories, coarsen it first (the first
  characters of a code, a district instead of an address). This applies
  to `geo_vars` too: a geographical unit where most units hold fewer
  than `min_count` persons is an address, not a place.

For linked tables it also stores the distribution of rows per person
(top coded), the share of persons with no rows, and the share of rows
that refer to unknown persons.

For level 3 of [`mimic()`](mimic.md) each table also gets a correlation
matrix of the normal scores of its variables on the protected scale,
with the rows per person in linked tables added for the main table. It
is rounded to one decimal and only stored with at least `min_persons`
persons. Correlation coefficients belong to the "safe" output classes in
the Eurostat guidelines for output checking.

All counting is done in **persons**, not rows: a category held by one
person with twenty contacts is still a category of one person.

## Why bounds are not the 1 % quantile

With 40 rows, the 1 % quantile is one person's value. `profile_data()`
therefore places each bound where at least
`max(tail_coding * persons, min_count)` persons lie at or beyond it.

## What is never stored

Rows, identifier values, free text, exact minima and maxima, and labels
of categories held by fewer than `min_count` persons. Counts below
`min_count` are suppressed and other counts rounded to `round_base`.

## Examples

``` r
cfg <- mimic_config(
  quasi_ids = c("birth_date", "sex", "settlement"),
  geo_vars = "settlement",
  text_vars = "note"
)
m <- profile_data(
  list(persons = fake_persons, contacts = fake_contacts,
       admissions = fake_admissions),
  keys = "person_id",
  config = cfg
)
m
#> <mimic_profile> 3 tables, about 2,000 persons
#> • persons (main, key person_id): 2,000 rows; 3 categorical, 1 date, 1 id, 1
#>   logical, 1 numeric, 1 text
#> • contacts (key person_id): 7,725 rows; 2 categorical, 1 date, 1 id; 0 to 17+
#>   rows per person
#> • admissions (key person_id): 480 rows; 1 categorical, 2 date, 1 id, 1 numeric;
#>   0 to 2+ rows per person
#> 
#> ── Risk report ──
#> 
#> Protection changed 12 variables:
#> • persons$person_id: identifier: only the format is stored
#> • persons$birth_date: coarsened to month; bottom and top coded
#> • persons$settlement: 5 small geographical units merged into 'Other' (fewer
#> than 5 persons)
#> • persons$income: bottom and top coded
#> • persons$note: free text: only the approximate length is stored
#> • contacts$person_id: identifier: only the format is stored
#> • contacts$contact_date: coarsened to month; bottom and top coded
#> • contacts$diag_code: 11 categories merged into 'Other' (fewer than 5 persons)
#> • admissions$person_id: identifier: only the format is stored
#> • admissions$admit_date: coarsened to month; bottom and top coded
#> • admissions$discharge_date: coarsened to month; bottom and top coded
#> • admissions$length_of_stay: bottom and top coded
#> ✖ k_anonymity (persons): 2,000 persons (about 100 %) are in combinations of birth_date, sex, settlement shared by fewer than 5 persons.
#>                      variables combinations rare_combinations persons_at_risk
#>  birth_date + sex + settlement        1,782             1,782           2,000
#>             without birth_date           41                11              25
#>                    without sex        1,599             1,597           1,990
#>             without settlement        1,309             1,301           1,960
#> ✔ keys (keys): Keys are complete and all linked tables refer to persons in 'persons'.
#> ℹ Highest recommended fidelity level: 2.
m$tables$persons$variables$settlement
#> $type
#> [1] "categorical"
#> 
#> $class
#> [1] "character"
#> 
#> $ordered
#> [1] FALSE
#> 
#> $geo
#> [1] TRUE
#> 
#> $levels
#>  [1] "Settlement_01" "Settlement_02" "Settlement_03" "Settlement_04"
#>  [5] "Settlement_05" "Settlement_06" "Settlement_07" "Settlement_08"
#>  [9] "Settlement_09" "Settlement_10" "Settlement_11" "Settlement_12"
#> [13] "Settlement_13" "Settlement_14" "Settlement_15" "Town_A"       
#> [17] "Town_B"        "Other"        
#> 
#> $freq
#>  [1] 120  90  80  60  55  40  35  30  25  20  20  15  10  10   5 925 450  10
#> 
#> $n_merged
#> [1] 5
#> 
#> $n_categories
#> [1] 22
#> 
#> $n_rare
#> [1] 5
#> 
#> $actions
#> [1] "5 small geographical units merged into 'Other' (fewer than 5 persons)"
#> [2] "frequencies rounded"                                                  
#> 
#> $missing
#> [1] 0
#> 
#> $empty
#> [1] 0
#> 
```
