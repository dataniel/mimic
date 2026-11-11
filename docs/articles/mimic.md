# From real data to released output

mimic supports a research service that works with register data on
**small populations**: a country, region or island of some tens of
thousands of people, many settlements of a few hundred, and few persons
in many categories. Researchers never see the real rows. The workflow is
*remote execution*:

1.  We make a synthetic copy of the real data. It contains no real
    persons.
2.  The researcher develops their R script on the copy.
3.  We run the script on the real data in the secure environment.
4.  We check the output and release only results that cannot reveal
    anyone.

The copy mimics the real data without being them. You **profile** the
data ([`profile_data()`](../reference/profile_data.md)), build the copy
([`mimic()`](../reference/mimic.md)), make a **trial run** on it
([`trial_run()`](../reference/trial_run.md)), make the **real run** on
the real data ([`real_run()`](../reference/real_run.md)), and
**inspect** what comes back ([`inspect()`](../reference/inspect.md)).

\
[`library`](https://rdrr.io/r/base/library.html)`(``mimic``)`

## The example data

All data in this vignette are invented. `fake_persons`, `fake_contacts`
and `fake_admissions` imitate an imaginary population of 2,000 persons
in two towns and twenty settlements, some with fewer than five
inhabitants.

\
[`head`](https://rdrr.io/r/utils/head.html)`(``fake_persons``, ``3``)`\
`#>   person_id birth_date sex    settlement   occupation income smoker`\
`#> 1  P3424376 1959-01-31   F Settlement_02        Trade 181300   TRUE`\
`#> 2  P2410400 1940-04-01   M Settlement_12 Construction 147400  FALSE`\
`#> 3  P8850624 1941-06-24   M        Town_A      Tourism 179400  FALSE`\
`#>                                                  note`\
`#> 1                           season radio visit letter`\
`#> 2 storm returned moved net family doctor harbour work`\
`#> 3            letter season school family family moved`\
[`sort`](https://rdrr.io/r/base/sort.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``fake_persons``$``settlement``)``)``[``1``:``6``]`\
`#> `\
`#> Settlement_19 Settlement_20 Settlement_18 Settlement_17 Settlement_16 `\
`#>             1             1             2             3             4 `\
`#> Settlement_15 `\
`#>             7`

## 1. Set the rules

[`mimic_config()`](../reference/mimic_config.md) collects the disclosure
rules. The defaults are stricter than in tools designed for large
countries. Name the quasi-identifiers (variables an outsider could
know), the geographical variables and the free-text variables. Anything
that looks like an identifier or free text is also protected
automatically, with a warning.

\
`cfg`` ``<-`` `[`mimic_config`](../reference/mimic_config.md)`(`\
`  min_count ``=`` ``5``,`\
`  quasi_ids ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"birth_date"``, ``"sex"``, ``"settlement"``)``,`\
`  geo_vars ``=`` ``"settlement"``,`\
`  text_vars ``=`` ``"note"``,`\
`  date_unit ``=`` ``"month"`\
`)`\
`cfg`\
`#> <mimic_config>`\
`#> • min_count: 5`\
`#> • dominance: (n = 2, k = 0.85); p % rule: 0.1`\
`#> • tail coding: 0.01 / 0.99; date unit: month`\
`#> • rounding: counts to 5, values to 2 significant digits`\
`#> • quasi-identifiers: birth_date, sex, settlement`\
`#> • geographical: settlement`\
`#> • identifiers: (none)`\
`#> • free text: note`\
`#> • min persons for synthesis: 100; min model df: 10`\
`#> • merged category label: ``"Other"`\
`#> • descriptions of output: optional`

## 2. Profile the real data (inside)

[`profile_data()`](../reference/profile_data.md) describes the linked
tables and applies protection **before** anything is stored: small
categories and settlements are merged into `"Other"`, numbers are bottom
and top coded where at least five persons lie beyond the bound, dates
are coarsened, identifiers keep only their format, free text only its
length, and all counts are rounded. Persons are counted, not rows.

\
`real`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``persons ``=`` ``fake_persons``, contacts ``=`` ``fake_contacts``,`\
`             admissions ``=`` ``fake_admissions``)`\
`m`` ``<-`` `[`profile_data`](../reference/profile_data.md)`(``real``, keys ``=`` ``"person_id"``, config ``=`` ``cfg``)`\
`m`\
`#> <mimic_profile> 3 tables, about 2,000 persons`\
`#> ``•`` persons (main, key person_id): 2,000 rows; 3 categorical, 1 date, 1 id, 1`\
`#>   logical, 1 numeric, 1 text`\
`#> ``•`` contacts (key person_id): 7,725 rows; 2 categorical, 1 date, 1 id; 0 to 17+`\
`#>   rows per person`\
`#> ``•`` admissions (key person_id): 480 rows; 1 categorical, 2 date, 1 id, 1 numeric;`\
`#>   0 to 2+ rows per person`\
`#> `\
`#> ── ``Risk report`` ──`\
`#> `\
`#> Protection changed 12 variables:`\
`#> • persons$person_id: identifier: only the format is stored`\
`#> • persons$birth_date: coarsened to month; bottom and top coded`\
`#> • persons$settlement: 5 small geographical units merged into 'Other' (fewer`\
`#> than 5 persons)`\
`#> • persons$income: bottom and top coded`\
`#> • persons$note: free text: only the approximate length is stored`\
`#> • contacts$person_id: identifier: only the format is stored`\
`#> • contacts$contact_date: coarsened to month; bottom and top coded`\
`#> • contacts$diag_code: 11 categories merged into 'Other' (fewer than 5 persons)`\
`#> • admissions$person_id: identifier: only the format is stored`\
`#> • admissions$admit_date: coarsened to month; bottom and top coded`\
`#> • admissions$discharge_date: coarsened to month; bottom and top coded`\
`#> • admissions$length_of_stay: bottom and top coded`\
`#> ``✖`` k_anonymity (persons): 2,000 persons (about 100 %) are in combinations of birth_date, sex, settlement shared by fewer than 5 persons.`\
`#>                      variables combinations rare_combinations persons_at_risk`\
`#>  birth_date + sex + settlement        1,782             1,782           2,000`\
`#>             without birth_date           41                11              25`\
`#>                    without sex        1,599             1,597           1,990`\
`#>             without settlement        1,309             1,301           1,960`\
`#> ``✔`` keys (keys): Keys are complete and all linked tables refer to persons in 'persons'.`\
`#> ``ℹ`` Highest recommended fidelity level: 2.`

The risk report says what protection changed, and it is honest about the
data: with birth date by month, sex and settlement, almost everyone is
unique. Without birth date about 25 persons remain at risk. That is
typical of a small population, and the report therefore recommends at
most level 2.

The profile contains no rows and can be saved and taken out:

\
`v`` ``<-`` ``m``$``tables``$``persons``$``variables`\
`v``$``settlement``$``levels`\
`#>  [1] "Settlement_01" "Settlement_02" "Settlement_03" "Settlement_04"`\
`#>  [5] "Settlement_05" "Settlement_06" "Settlement_07" "Settlement_08"`\
`#>  [9] "Settlement_09" "Settlement_10" "Settlement_11" "Settlement_12"`\
`#> [13] "Settlement_13" "Settlement_14" "Settlement_15" "Town_A"       `\
`#> [17] "Town_B"        "Other"`\
`v``$``person_id``$``formats`\
`#>     format share`\
`#> 1 P9999999     1`\
`v``$``income``[`[`c`](https://rdrr.io/r/base/c.html)`(``"lo"``, ``"hi"``, ``"median"``)``]`\
`#> $lo`\
`#> [1] 58000`\
`#> `\
`#> $hi`\
`#> [1] 970000`\
`#> `\
`#> $median`\
`#> [1] 240000`

## 3. Build the copy (outside)

[`mimic()`](../reference/mimic.md) builds the copy from the profile
alone. Level 1, the default, gives correct names, types and allowed
values with random content. That is enough for most scripts. Linked
tables share keys, and some persons have no contacts, just as in the
real data. Synthetic identifiers start with `S`, so they can never be
mistaken for real ones.

\
`copy`` ``<-`` `[`mimic`](../reference/mimic.md)`(``m``, seed ``=`` ``1``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(``copy``$``persons``, ``3``)`\
`#>   person_id birth_date sex    settlement    occupation income smoker`\
`#> 1  S2833981 1973-02-13   M        Town_A  Construction 505185  FALSE`\
`#> 2  S1086505 2013-12-07   F Settlement_09    Unemployed 741124   TRUE`\
`#> 3  S3472554 1956-03-03   F Settlement_05 Public sector 683797   TRUE`\
`#>                                                                     note`\
`#> 1                       labore amet sed eiusmod labore et lorem sit do i`\
`#> 2 minim labore ipsum elit ipsum et et sit incididunt sed dolore do minim`\
`#> 3           labore sit labore sit do adipiscing magna ut veniam dolore a`

By default the copy is messy in the ways real register data are. It has
missing values in the real proportion, invalid codes, extreme values,
empty strings, odd dates, and contacts for persons who are not in the
person table. A script that survives the copy is much more likely to
survive the real run.

\
[`head`](https://rdrr.io/r/utils/head.html)`(`[`attr`](https://rdrr.io/r/base/attr.html)`(``copy``, ``"mimic"``)``$``mess``)`\
`#>     table   variable          type rows`\
`#> 1 persons birth_date     odd dates   20`\
`#> 2 persons        sex invalid codes   20`\
`#> 3 persons        sex empty strings   20`\
`#> 4 persons settlement invalid codes   20`\
`#> 5 persons settlement empty strings   20`\
`#> 6 persons occupation invalid codes   20`\
[`table`](https://rdrr.io/r/base/table.html)`(``copy``$``persons``$``sex``, useNA ``=`` ``"ifany"``)`\
`#> `\
`#>                       F       ?     999       f       F      F        M UNKNOWN `\
`#>      12       8       1       6       2       3     994       6     967       1`

Each kind of mess can be switched off with
[`mess_options()`](../reference/mess_options.md). Level 2 follows the
coarse distributions of every variable:

\
`clean`` ``<-`` `[`mimic`](../reference/mimic.md)`(``m``, fidelity ``=`` ``2``, mess ``=`` ``FALSE``, seed ``=`` ``1``)`\
[`round`](https://rdrr.io/r/base/Round.html)`(`[`prop.table`](https://rdrr.io/r/base/proportions.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``clean``$``persons``$``occupation``)``)``, ``2``)`\
`#> `\
`#>      Aviation  Construction       Fishing       Hunting        Mining `\
`#>          0.01          0.08          0.19          0.04          0.02 `\
`#> Public sector       Retired       Student       Tourism         Trade `\
`#>          0.24          0.12          0.13          0.02          0.09 `\
`#>    Unemployed `\
`#>          0.06`

## 4. Trial run: try the script on the copy

The researcher writes a script that uses the tables by name and writes
its output to the working directory:

\
`script`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"persons$age <- as.numeric(as.Date('2025-01-01') - persons$birth_date) %/% 365.25"``,`\
`  ``"by_sex <- as.data.frame(table(sex = persons$sex), responseName = 'n')"``,`\
`  ``"write.csv(by_sex, 'by_sex.csv', row.names = FALSE)"``,`\
`  ``"fit <- lm(income ~ sex + age, data = persons)"``,`\
`  ``"saveRDS(fit, 'model.rds')"``,`\
`  ``"write.csv(coef(summary(fit)), 'coefficients.csv')"``,`\
`  ``"write.csv(persons[, c('person_id', 'age')], 'ages.csv', row.names = FALSE)"`\
`)`\
`fit`` ``<-`` `[`trial_run`](../reference/trial_run.md)`(``script``, ``copy``, config ``=`` ``cfg``)`\
`fit`\
`#> <mimic_trial> (code)`\
`#> ``✔`` The script ran through in 0.1 seconds.`\
`#> ``ℹ`` Files written to ``C:/Users/danie/AppData/Local/Temp/RtmpiI3TAk/trial_run_879822056fdd``: ages.csv, by_sex.csv, coefficients.csv, and model.rds`\
`#> ``✖`` Row-level data or writes outside the output directory:`\
`#>                     what                                          reason`\
`#>  write.table(): ages.csv                   writes a table with 2000 rows`\
`#>                 ages.csv contains 2000 identifier value(s) from the data`\
`#>                 ages.csv                    holds a table with 2000 rows`\
`#>                model.rds                    holds a table with 1940 rows`\
`#> ``✖`` Output check on the copy: 3 files would be sent back, 1 to review by hand:`\
`#> ages.csv`\
`#> • Has identifier column(s) person_id; output must not contain identifiers.`\
`#> • Has 2000 rows; make sure this is not row-level data.`\
`#> • Table of magnitudes without a count column; add the number of persons per`\
`#> cell so cell sizes can be checked.`\
`#> by_sex.csv`\
`#> • 4 of 10 cells have fewer than 5 persons.`\
`#> model.rds`\
`#> • 2 issue(s) with the observations behind the model.`\
`#> ``ℹ```  The copy has made-up mess (odd codes, empty strings): small cells may come from it. Clean the codes in the script, or try a copy made with `mimic(mess = FALSE)`. ``\
`` #> See `$inspection` for every check. ``

[`trial_run()`](../reference/trial_run.md) runs the script in a fresh R
session. The script runs through, but two outputs hold a row per person:
`ages.csv` with the identifier, and `model.rds`, because a fitted `lm`
object carries the model frame, residuals and fitted values of everyone
it was fitted on.

Then [`trial_run()`](../reference/trial_run.md) runs the written files
through [`inspect()`](../reference/inspect.md), the same check staff
will run on the output from the real data, with the same `config`. It
finds the two files again, and also small cells in `by_sex.csv`: the
copy has odd codes such as `"?"` and `"999"` in `sex`, and the script
tabulates the raw variable. The real data can have such codes too, so
cleaning them in the script is worth it. The check only sees the output
from the copy, so a file it approves is not yet approved; the real check
comes after the run.

### Safe output from the script

Three functions make output that passes the check the first time:

- [`safe_count()`](../reference/safe_count.md) counts persons,
  suppresses small cells, also suppresses the cells that would let a
  total give them back, and rounds the rest,
- [`safe_summary()`](../reference/safe_summary.md) gives means, medians,
  quartiles and the like with small and dominated groups suppressed, and
  never a minimum or maximum,
- [`safe_model()`](../reference/safe_model.md) returns the coefficient
  table instead of the model object, and stops when the model rests on
  too few observations.

In a script on the real data, persons by settlement: the five small
settlements are suppressed. Together they are the total minus the rest,
but no single one can be worked out. Had only one settlement been small,
[`safe_count()`](../reference/safe_count.md) would also have suppressed
the next smallest.

\
[`safe_count`](../reference/safe_count.md)`(``fake_persons``, ``"settlement"``, id ``=`` ``"person_id"``, config ``=`` ``cfg``)`\
`#> Suppressed 5 cells with fewer than 5 persons.`\
`#>       settlement    n`\
`#> 1  Settlement_01  120`\
`#> 2  Settlement_02   90`\
`#> 3  Settlement_03   80`\
`#> 4  Settlement_04   60`\
`#> 5  Settlement_05   55`\
`#> 6  Settlement_06   40`\
`#> 7  Settlement_07   35`\
`#> 8  Settlement_08   30`\
`#> 9  Settlement_09   25`\
`#> 10 Settlement_10   20`\
`#> 11 Settlement_11   20`\
`#> 12 Settlement_12   15`\
`#> 13 Settlement_13   10`\
`#> 14 Settlement_14   10`\
`#> 15 Settlement_15    5`\
`#> 16 Settlement_16   NA`\
`#> 17 Settlement_17   NA`\
`#> 18 Settlement_18   NA`\
`#> 19 Settlement_19   NA`\
`#> 20 Settlement_20   NA`\
`#> 21        Town_A  925`\
`#> 22        Town_B  450`\
`#> 23         Total 2000`

In a script run by [`trial_run()`](../reference/trial_run.md) or
[`real_run()`](../reference/real_run.md) they follow the `config` those
functions were given, so the researcher does not need to know the rules.
On a researcher machine, where researchers run their scripts themselves,
staff set the rules for every session in `Rprofile.site` instead (see
[`?safe_output`](../reference/safe_output.md)).

[`describe_output()`](../reference/describe_output.md) tells the checker
what each file shows: the count and grouping columns of a table, which
[`inspect()`](../reference/inspect.md) then uses instead of guessing,
the numbers behind a figure, and what changed since the last request.
The descriptions are shown in the approver’s report.

The researcher rewrites the script with these functions and cleans the
odd codes:

\
`script`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"persons$sex[!persons$sex %in% c('F', 'M')] <- NA"``,`\
`  ``"persons$age <- as.numeric(as.Date('2025-01-01') - persons$birth_date) %/% 365.25"``,`\
`  ``"by_sex <- mimic::safe_count(persons, 'sex', id = 'person_id')"``,`\
`  ``"write.csv(by_sex, 'by_sex.csv', row.names = FALSE)"``,`\
`  ``"mimic::describe_output('by_sex.csv', 'Persons by sex.', count = 'n', by = 'sex')"``,`\
`  ``"fit <- lm(income ~ sex + age, data = persons)"``,`\
`  ``"write.csv(mimic::safe_model(fit), 'coefficients.csv', row.names = FALSE)"``,`\
`  ``"mimic::describe_output('coefficients.csv', 'Linear model of income on sex and age.')"`\
`)`\
[`trial_run`](../reference/trial_run.md)`(``script``, ``copy``, config ``=`` ``cfg``)`\
`#> <mimic_trial> (code)`\
`#> ``✔`` The script ran through in 0.2 seconds.`\
`#> ``ℹ`` Packages: mimic`\
`#> ``ℹ`` Files written to ``C:/Users/danie/AppData/Local/Temp/RtmpiI3TAk/trial_run_8798316a53d7``: by_sex.csv, coefficients.csv, and output_descriptions.dcf`\
`#> ``✔`` Output check on the copy: nothing needs changes, 1 to review by hand.`

## 5. Real run: run on the real data (inside)

Staff run the script on the real data:

\
`out`` ``<-`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"run_1"``)`\
`r`` ``<-`` `[`real_run`](../reference/real_run.md)`(``script``, ``real``, ``out``, config ``=`` ``cfg``)`\
`r`\
`#> <mimic_run> (code)`\
`#> ``✔`` The script ran through on the real data in under a second.`\
`#> ``ℹ`` Output files: by_sex.csv, coefficients.csv, and output_descriptions.dcf`\
`#> ``ℹ```  Check the output with `inspect()` before release. ``

Output is kept in `out`, writes elsewhere are blocked, and a log of the
code is placed next to the output. Printed output is left out by
default; `log = "cleaned"` keeps it with data values removed, for
scripts staff have read. When a script fails, the researcher gets a safe
error report instead of the data:

\
`r2`` ``<-`` `[`real_run`](../reference/real_run.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"x <- 1"``,`\
`  ``"fit <- lm(income ~ sex + settlment, data = persons)"`\
`)``, ``real``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"run_2"``)``)`\
`r2`\
`#> <mimic_run> (code)`\
`#> ``✖`` The script failed at line 2.`\
`#> Message (data values removed): object 'settlment' not found`\
`#> Variables involved:`\
`#>    table variable     class missing    distinct empty_strings all_missing`\
`#>  persons      sex character     0 %           2         FALSE       FALSE`\
`#>  persons   income   numeric     3 % about 1,615            NA       FALSE`\
`#> ``ℹ`` No output files.`\
`#> ``ℹ```  Check the output with `inspect()` before release. ``

The report keeps the researcher’s own words (`settlment`, misspelled),
the line and a non-revealing description of the variables involved. It
never contains values from the data.

## 6. Inspect before release (inside)

[`inspect()`](../reference/inspect.md) checks every file and gives each
a verdict: *approved*, *needs changes* or *review by hand*. With the
real data it can also check dominance and search text for values.

\
`res`` ``<-`` `[`inspect`](../reference/inspect.md)`(``out``, config ``=`` ``cfg``, real_data ``=`` ``real``)`\
`res`\
`#> <mimic_inspection> 4 files in`\
`#> ``C:/Users/danie/AppData/Local/Temp/RtmpiI3TAk/run_1`\
`#> ``ℹ`` No register: tables were not compared with earlier releases.`\
`#> ``✔`` APPROVED: by_sex.csv`\
`#> ``✔`` APPROVED: coefficients.csv`\
`#> ``✔`` APPROVED: output_descriptions.dcf`\
`#> ``✔`` APPROVED: run_log.txt`\
`#> ────────────────────────────────────────────────────────────────────────────────`\
`#> 4 approved, 0 need changes, 0 to review by hand.`

The table of coefficients from
[`safe_model()`](../reference/safe_model.md) carries its residual
degrees of freedom and number of observations, so
[`inspect()`](../reference/inspect.md) can approve it. The descriptions
are written by the researcher, so they are read like other text: without
the real data they go to a person, with it they are searched for values
that could point to someone.

To show what the inspection catches, here is an output folder with
typical mistakes:

\
`bad`` ``<-`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"run_bad"``)`\
[`dir.create`](https://rdrr.io/r/base/files2.html)`(``bad``, showWarnings ``=`` ``FALSE``)`\
`by_place`` ``<-`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``settlement ``=`` ``fake_persons``$``settlement``)``, responseName ``=`` ``"n"``)`\
[`write.csv`](https://rdrr.io/r/utils/write.table.html)`(``by_place``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``bad``, ``"by_settlement.csv"``)``, row.names ``=`` ``FALSE``)`\
`inc`` ``<-`` `[`aggregate`](https://rdrr.io/r/stats/aggregate.html)`(``income`` ``~`` ``settlement``, data ``=`` ``fake_persons``, FUN ``=`` ``mean``)`\
[`names`](https://rdrr.io/r/base/names.html)`(``inc``)``[``2``]`` ``<-`` ``"mean_income"`\
`inc``$``n`` ``<-`` ``100`\
[`write.csv`](https://rdrr.io/r/utils/write.table.html)`(``inc``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``bad``, ``"income.csv"``)``, row.names ``=`` ``FALSE``)`\
[`saveRDS`](https://rdrr.io/r/base/readRDS.html)`(`[`summary`](https://rdrr.io/r/base/summary.html)`(``fake_persons``$``income``)``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``bad``, ``"summary.rds"``)``)`\
\
[`inspect`](../reference/inspect.md)`(``bad``, config ``=`` ``cfg``, real_data ``=`` ``real``)`\
`#> <mimic_inspection> 3 files in`\
`#> ``C:/Users/danie/AppData/Local/Temp/RtmpiI3TAk/run_bad`\
`#> ``ℹ`` No register: tables were not compared with earlier releases.`\
`#> ``✖`` NEEDS CHANGES: by_settlement.csv`\
`#> • 5 of 22 cells have fewer than 5 persons.`\
`#> • Contains 5 value(s) that could point to a person (free text, or a date, code`\
`#> or label held by fewer than 5 persons).`\
`#> ``✖`` NEEDS CHANGES: income.csv`\
`#> • Contains 5 value(s) that could point to a person (free text, or a date, code`\
`#> or label held by fewer than 5 persons).`\
`#> • 3 of 22 groups of income are dominated by few contributors (n = 2, k = 0.85)`\
`#> and p = 0.1.`\
`#> • 2 number(s) equal a value held by fewer than 5 persons (often a median or`\
`#> extreme of a small group).`\
`#> ``✖`` NEEDS CHANGES: summary.rds`\
`#> • Found 2 minimum, maximum or range value(s). These are often one person's`\
`#> value.`\
`#> • 3 number(s) equal a value held by fewer than 5 persons (often a median or`\
`#> extreme of a small group).`\
`#> ────────────────────────────────────────────────────────────────────────────────`\
`#> 0 approved, 3 need changes, 0 to review by hand.`

A researcher who suppresses a small cell must also make sure the total
does not give it back, and a table of counts can reveal something
without a single small cell, when everyone in a group shares one value:

\
`suppressed`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sex ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"F"``, ``"M"``, ``"Total"``)``, n ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"<5"``, ``"1015"``, ``"1018"``)``)`\
[`check_suppression`](../reference/check_suppression.md)`(``suppressed``)`\
`#> ``✖`` suppression (suppressed): 1 of 1 suppressed cell(s) can be recovered from the totals; suppress a second cell in each line or remove the totals.`\
`#>  sex`\
`#>    F`\
\
`smokers`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``settlement ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Settlement_03"``, ``"Town_A"``)``, each ``=`` ``2``)``,`\
`                      smoker ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"yes"``, ``"no"``)``, ``2``)``, n ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``12``, ``210``, ``640``)``)`\
[`check_group_disclosure`](../reference/check_group_disclosure.md)`(``smokers``)`\
`#> ``!`` group_disclosure (smokers): In 2 group(s) everyone shares one value of settlement, smoker; knowing that a person is in the group reveals it. Fine if the zeros are structural.`\
`#>                       group     shared`\
`#>                smoker = yes settlement`\
`#>  settlement = Settlement_03     smoker`

The approver gets a report that can be saved with
[`write_report()`](../reference/write_report.md), outside the output
directory:

\
`report`` ``<-`` `[`write_report`](../reference/write_report.md)`(``res``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"inspection.md"``)``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`readLines`](https://rdrr.io/r/base/readLines.html)`(``report``)``[``1``:``12``]``, sep ``=`` ``"\n"``)`\
`#> # Output inspection`\
`#> `\
`` #> - Directory: `C:/Users/danie/AppData/Local/Temp/RtmpiI3TAk/run_1` ``\
`#> - Inspected: 2026-11-11 21:03`\
`#> - Rules: min_count 5, dominance (n = 2, k = 0.85), p % rule 0.1`\
`#> - Real data used for checking: yes`\
`#> - Earlier releases compared: no register`\
`#> - Result: 4 approved, 0 need changes, 0 to review by hand`\
`#> `\
`#> ## Verdict per file`\
`#> `\
`#> | file | verdict | reasons |`

## 7. Keep a register of releases

Each run has its own output directory, but released tables add up over
time. A table of persons by sex released in January and the same table
in February, after one small settlement was left out, differ by exactly
that settlement. [`record_release()`](../reference/record_release.md)
keeps what was released, and [`inspect()`](../reference/inspect.md) with
the same `register` compares every new table with it:

\
`reg`` ``<-`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"register"``)`\
[`record_release`](../reference/record_release.md)`(``res``, ``reg``)`\
`#> ``✔`` Recorded 4 files with 2 tables as release 20261111-210340.`\
\
`out_2`` ``<-`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"run_3"``)`\
`most`` ``<-`` ``real`\
`most``$``persons`` ``<-`` ``fake_persons``[``fake_persons``$``settlement`` ``!=`` ``"Settlement_20"``, ``]`\
`r3`` ``<-`` `[`real_run`](../reference/real_run.md)`(``script``, ``most``, ``out_2``)`\
[`inspect`](../reference/inspect.md)`(``out_2``, config ``=`` ``cfg``, real_data ``=`` ``most``, register ``=`` ``reg``)`\
`#> <mimic_inspection> 4 files in`\
`#> ``C:/Users/danie/AppData/Local/Temp/RtmpiI3TAk/run_3`\
`#> ``ℹ`` Compared with 1 earlier release.`\
`#> ``✔`` APPROVED: by_sex.csv`\
`#> ``✔`` APPROVED: coefficients.csv`\
`#> ``✔`` APPROVED: output_descriptions.dcf`\
`#> ``✔`` APPROVED: run_log.txt`\
`#> ────────────────────────────────────────────────────────────────────────────────`\
`#> 4 approved, 0 need changes, 0 to review by hand.`

## The checks on their own

The rules behind [`inspect()`](../reference/inspect.md) are separate
functions that can be used on any table or dataset. They never print
counts below `min_count`.

\
[`check_min_count`](../reference/check_min_count.md)`(``fake_persons``, by ``=`` ``"settlement"``, id ``=`` ``"person_id"``)`\
`#> ``✖`` min_count (fake_persons): 5 of 22 cells have fewer than 5 persons.`\
`#>     settlement`\
`#>  Settlement_17`\
`#>  Settlement_16`\
`#>  Settlement_19`\
`#>  Settlement_18`\
`#>  Settlement_20`\
[`check_dominance`](../reference/check_dominance.md)`(``fake_persons``, value ``=`` ``"income"``, by ``=`` ``"settlement"``, id ``=`` ``"person_id"``)`\
`#> ``✖`` dominance (fake_persons): 3 of 22 groups of income are dominated by few contributors (n = 2, k = 0.85) and p = 0.1.`\
`#>     settlement    rule_broken`\
`#>  Settlement_19 (n, k) and p %`\
`#>  Settlement_18 (n, k) and p %`\
`#>  Settlement_20 (n, k) and p %`\
[`check_k_anonymity`](../reference/check_k_anonymity.md)`(``fake_persons``, `[`c`](https://rdrr.io/r/base/c.html)`(``"sex"``, ``"settlement"``)``, id ``=`` ``"person_id"``)`\
`#> ``✖`` k_anonymity (fake_persons): 25 persons (about 1.2 %) are in combinations of sex, settlement shared by fewer than 5 persons.`\
`#>           variables combinations rare_combinations persons_at_risk`\
`#>    sex + settlement           41                11              25`\
`#>         without sex           22                 5              10`\
`#>  without settlement            2                 0               0`

The others are [`check_extremes()`](../reference/check_extremes.md)
(minimum and maximum values),
[`check_model()`](../reference/check_model.md) (models with too few
observations, or objects that carry the data),
[`check_differencing()`](../reference/check_differencing.md) (two tables
that can be subtracted) and [`check_keys()`](../reference/check_keys.md)
(complete keys in linked tables).
[`check_all()`](../reference/check_all.md) collects any of them into one
report:

\
`all_sex`` ``<-`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``sex ``=`` ``fake_persons``$``sex``)``, responseName ``=`` ``"n"``)`\
`some_sex`` ``<-`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``sex ``=`` ``fake_persons``$``sex``[``fake_persons``$``settlement`` ``!=`` ``"Settlement_20"``]``)``,`\
`                          responseName ``=`` ``"n"``)`\
[`check_all`](../reference/check_all.md)`(`\
`  `[`check_keys`](../reference/check_keys.md)`(``real``, keys ``=`` ``"person_id"``)``,`\
`  `[`check_extremes`](../reference/check_extremes.md)`(`[`summary`](https://rdrr.io/r/base/summary.html)`(``fake_persons``$``income``)``)``,`\
`  `[`check_differencing`](../reference/check_differencing.md)`(``all_sex``, ``some_sex``)`\
`)`\
`#> ``✔`` keys (keys): Keys are complete and all linked tables refer to persons in 'persons'.`\
`#> ``✖`` extremes (summary(fake_persons$income)): Found 2 minimum, maximum or range value(s). These are often one person's value.`\
`#>  location name`\
`#>      name Min.`\
`#>      name Max.`\
`#> ``✖`` differencing (all_sex vs some_sex): Subtracting the tables reveals fewer than 5 persons in 1 cell(s) and the grand totals.`\
`#>  sex`\
`#>    F`\
`#> ────────────────────────────────────────────────────────────────────────────────`\
`#> 1 passed, 0 warnings, 2 failed.`

## Higher fidelity

Most researchers only need level 1. Levels 3 and 4 need a deliberate
choice and always come with a risk report.

- **Level 3** keeps coarse correlations with a Gaussian copula. It uses
  only the correlation matrix in the profile, rounded to one decimal,
  and runs without the real data.
- **Level 4** fits classification and regression trees to the protected
  real data, one variable after another, and draws from leaves of at
  least `min_count` persons. It runs inside, and the copy counts as
  output.

\
`l3`` ``<-`` `[`mimic`](../reference/mimic.md)`(``m``, fidelity ``=`` ``3``, i_understand_the_risk ``=`` ``TRUE``, seed ``=`` ``1``, n ``=`` ``500``)`\
`#> Warning: The risk report recommends at most level 2 for these data.`\
`#> ``ℹ```  Consider `fidelity = 2`. ``\
`#> `\
`#> ── ``Risk report for the level 3 copy`` ──`\
`#> `\
`#> Method: Gaussian copula from correlations rounded to one decimal.`\
`#> ``✖`` The profile recommends at most level 2.`\
`#> ``!`` The real data have rare combinations of quasi-identifiers: 2,000 persons (about 100 %) are in combinations of birth_date, sex, settlement shared by fewer than 5 persons.`\
`#> ``ℹ```  Not compared with the real data (give `real_data` to check replicated rows). ``

The example data were generated with independent variables, so they have
no relationships to keep. On real data, level 3 and 4 keep, for example,
the relationship between age and income.

## Why it is built this way

- **Strict by default.** In a small population a cell of three is often
  a household that everybody knows. `min_count` cannot be set below 3,
  bounds never rest on fewer than `min_count` persons, and when
  something cannot be checked automatically it is not approved.
- **Persons, not rows.** A category held by one person with twenty
  contacts is still a category of one person.
- **Low fidelity first.** A copy that only has the right structure
  reveals the least. Most scripts that run on level 1 also run on the
  real data, especially when the copy is messy.
- **Own methods.** The synthesis methods are well known (Gaussian
  copulas, sequential CART as in synthpop), but implemented here so they
  can be tuned for small populations and the package keeps few
  dependencies.
- **Safety net, not sandbox.** [`real_run()`](../reference/real_run.md)
  blocks the common ways of writing outside the output directory, but a
  determined script can find other ways. File permissions in the secure
  environment must enforce where data may go, and staff review remains
  the final check. The log keeps only the code by default, because
  cleaning printed output is a search for known values and cannot find
  what a script has transformed.
- **Mistakes, not malice.** The checks catch what an honest researcher
  gets wrong. A researcher who wants to smuggle data out can hide it in
  what is allowed to leave: the shape of a figure, the last digit of a
  number, the line where the script fails. Against that only contracts,
  staff who read the script, and limits on the number of runs help. See
  [`?mimic`](../reference/mimic.md).
