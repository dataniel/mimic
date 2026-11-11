# Build a synthetic copy from a profile

`mimic()` builds a synthetic copy of the data from a
[`profile_data()`](profile_data.md) object alone, so it runs outside the
secure environment with no access to the real data. The copy has the
same tables, variable names, types and allowed values, and linked tables
share keys. It contains no real persons.

## Usage

``` r
mimic(
  profile,
  fidelity = 1,
  n = NULL,
  mess = TRUE,
  seed = NULL,
  id_prefix = "S",
  real_data = NULL,
  i_understand_the_risk = FALSE,
  train_rows = 1e+05
)
```

## Arguments

- profile:

  A `mimic_profile` object from [`profile_data()`](profile_data.md).

- fidelity:

  Level 1 to 4, see below.

- n:

  Number of persons in the main table. Defaults to the (rounded) number
  in the real data. Tables without a key are scaled accordingly.

- mess:

  `TRUE` for all types of mess, `FALSE` for a clean copy, or a
  [`mess_options()`](mess_options.md) object.

- seed:

  Optional seed for a reproducible copy. The random number state of the
  session is restored afterwards.

- id_prefix:

  Marks synthetic identifiers, see Linked tables. `NULL` keeps the exact
  format.

- real_data:

  The real data, required for level 4 and optional for level 3 (only
  used to check the copy against them). A data frame or a named list of
  data frames with the same names as in
  [`profile_data()`](profile_data.md).

- i_understand_the_risk:

  Must be `TRUE` for levels 3 and 4.

- train_rows:

  Level 4 only: the trees of each table are fitted on at most this many
  real rows, sampled as whole persons. `NULL` uses all rows, which is
  slower on large registers.

## Value

A data frame if the profile describes one table, otherwise a named list
of data frames. The attribute `"mimic"` records the level, the seed, the
mess that was added and, for levels 3 and 4, the risk report.

## Fidelity levels

1.  **Structure** (default): correct names, types and allowed values,
    but random content: categories uniformly, numbers and dates
    uniformly between the protected bounds. Enough to make most scripts
    run.

2.  **Distributions**: every variable follows its coarse distribution
    from [`profile_data()`](profile_data.md) (rounded frequencies and
    histograms), but variables are independent of each other. Within-row
    logic such as `discharge_date >= admit_date` is not preserved.

3.  **Relationships**: coarse correlations are kept with a Gaussian
    copula. [`profile_data()`](profile_data.md) stores the correlations
    between the normal scores of all variables of a table, rounded to
    one decimal, and for the main table also with the number of rows
    each person has in the linked tables (older persons have more
    contacts). `mimic()` draws correlated normal values and maps them
    through the protected distributions, so the marginals are exactly
    those of level 2. Runs without the real data. Monotone relationships
    are kept best; for categorical variables the order of the categories
    is arbitrary, so their relationships are only approximate. Variables
    in linked tables are related to each other, not to the person's
    variables.

4.  **Model based**: sequential synthesis with classification and
    regression trees, the default method of synthpop, implemented here.
    Each variable is drawn from donors in the leaf of a tree fitted on
    the previous variables of the real data, after the same protection
    as in [`profile_data()`](profile_data.md) (top and bottom coding,
    merged categories). Donors come from at least `min_count` persons,
    also in linked tables where a leaf of many rows can be one person's;
    drawn numbers and dates are smoothed with noise, and discrete
    numbers are drawn within their protected bin. Linked tables are
    modelled with the person's variables as predictors. A table with
    more than `train_rows` rows is modelled on a random sample of whole
    persons, because growing the trees takes time in proportion to the
    rows; 100,000 rows show the same structure as millions of contacts.
    **Requires `real_data` and must run in the secure environment**; the
    copy is output and must be checked before release.

Most researchers only need level 1. Higher levels require a deliberate
choice; levels 3 and 4 require `i_understand_the_risk = TRUE`, and they
always come with a risk report (`attr(x, "mimic")$risk`). With
`real_data` the report says how many real rows and unique combinations
the copy reproduces.

## Linked tables

Persons are created for the main table first, with new identifiers in
the stored format. The number of rows each person gets in a linked table
is drawn from the coarse rows-per-person distribution, so some persons
have no rows, just like in the real data. At level 1 too, because a
script that joins tables must meet persons without contacts.

Identifiers are random values in the real format, marked with
`id_prefix` so they can never equal a real identifier: the constant
first letter of a format like `"P9999999"` is replaced (`"S1234567"`),
otherwise the prefix is put in front. With `id_prefix = NULL` the exact
format is used; an identifier may then coincide with a real one by
chance, but the rest of that row is still synthetic. Numeric identifiers
are never prefixed.

## Mess

Real register data break scripts in predictable ways. With `mess = TRUE`
the copy gets

- missing values in the same proportion as the real data,

- invalid or odd codes in categorical variables (`"999"`, `"UNKNOWN"`,
  padded or lower-case variants),

- extreme values in numeric variables (sentinels such as `999999`,
  negative values),

- empty strings,

- odd dates (`1900-01-01`, `9999-12-31`),

- rows in linked tables whose key is not in the main table.

Each type can be switched off with [`mess_options()`](mess_options.md).
What was added is logged in `attr(x, "mimic")$mess`.

## Examples

``` r
cfg <- mimic_config(geo_vars = "settlement", text_vars = "note")
m <- profile_data(
  list(persons = fake_persons, contacts = fake_contacts),
  keys = "person_id",
  config = cfg
)
copy <- mimic(m, seed = 1)
str(copy$persons)
#> 'data.frame':    2000 obs. of  8 variables:
#>  $ person_id : chr  "S2833981" "S1086505" "S3472554" "S4249864" ...
#>  $ birth_date: Date, format: "1973-02-13" "2013-12-07" ...
#>  $ sex       : chr  "M" "F" "F" "F" ...
#>  $ settlement: chr  "Town_A" "Settlement_09" "Settlement_05" "Settlement_09" ...
#>  $ occupation: chr  "Construction" "Unemployed" "Public sector" "Hunting" ...
#>  $ income    : num  505185 741124 683797 80237 267553 ...
#>  $ smoker    : logi  FALSE TRUE TRUE TRUE FALSE FALSE ...
#>  $ note      : chr  NA "minim labore ipsum elit ipsum et et sit incididunt sed dolore do minim" NA "dolore do minim aliqua tempor magna consectetur veniam sed aliqua ven" ...

# realistic distributions, no mess
clean <- mimic(m, fidelity = 2, mess = FALSE, seed = 1)
table(clean$persons$settlement)
#> 
#>         Other Settlement_01 Settlement_02 Settlement_03 Settlement_04 
#>            13           101           118            79            63 
#> Settlement_05 Settlement_06 Settlement_07 Settlement_08 Settlement_09 
#>            56            49            30            29            29 
#> Settlement_10 Settlement_11 Settlement_12 Settlement_13 Settlement_14 
#>            17            26            13            12            10 
#> Settlement_15        Town_A        Town_B 
#>             8           903           444 

# only missing values and orphaned rows
some <- mimic(m, mess = mess_options(invalid_codes = FALSE, extremes = FALSE,
                                     empty_strings = FALSE, odd_dates = FALSE))
attr(some, "mimic")$mess
#>      table  variable                                 type rows
#> 1  persons    income                       missing values   60
#> 2  persons    smoker                       missing values  110
#> 3  persons      note                       missing values  600
#> 4 contacts person_id rows with keys not in the main table  167

# level 3: coarse correlations, still without the real data
rel <- mimic(m, fidelity = 3, i_understand_the_risk = TRUE, seed = 1)
#> 
#> ── Risk report for the level 3 copy ──
#> 
#> Method: Gaussian copula from correlations rounded to one decimal.
#> ℹ Not compared with the real data (give `real_data` to check replicated rows).

# \donttest{
# level 4: in the secure environment only
close <- mimic(m, fidelity = 4, i_understand_the_risk = TRUE, seed = 1,
               real_data = list(persons = fake_persons, contacts = fake_contacts))
#> 
#> ── Risk report for the level 4 copy ──
#> 
#> Method: sequential classification and regression trees on protected real data.
#> ! The copy reproduces real rows beyond chance, or combinations that are unique in the real data:
#>     table                                                compared
#>   persons birth_date, sex, settlement, occupation, income, smoker
#>  contacts                   contact_date, contact_type, diag_code
#>  identical_rows by_chance replicated_uniques
#>             0 %       0 %                 45
#>           3.7 %       4 %       fewer than 5
#>                           uniques_on
#>  sex, settlement, occupation, smoker
#>              contact_type, diag_code
#> ✖ A level 4 copy is built from the real data: treat it as output and check it before it leaves the secure environment.
attr(close, "mimic")$risk
#> 
#> ── Risk report for the level 4 copy ──
#> 
#> Method: sequential classification and regression trees on protected real data.
#> ! The copy reproduces real rows beyond chance, or combinations that are unique in the real data:
#>     table                                                compared
#>   persons birth_date, sex, settlement, occupation, income, smoker
#>  contacts                   contact_date, contact_type, diag_code
#>  identical_rows by_chance replicated_uniques
#>             0 %       0 %                 45
#>           3.7 %       4 %       fewer than 5
#>                           uniques_on
#>  sex, settlement, occupation, smoker
#>              contact_type, diag_code
#> ✖ A level 4 copy is built from the real data: treat it as output and check it before it leaves the secure environment.
# }
```
