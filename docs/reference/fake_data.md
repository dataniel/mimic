# Invented example data for a small population

Three linked tables describing an imaginary population of 2,000 persons
living in two towns and twenty settlements. **Everything is invented**:
identifiers, dates, place labels and code lists have no relation to any
real person, place or classification. The script that generates them is
in `data-raw/make_example_data.R` in the package source.

## Usage

``` r
fake_persons

fake_contacts

fake_admissions
```

## Format

`fake_persons`, one row per person:

- person_id:

  Identifier, `"P"` followed by 7 digits.

- birth_date:

  Date of birth.

- sex:

  `"F"` or `"M"`.

- settlement:

  Place of residence, `"Town_A"`, `"Town_B"` or `"Settlement_01"` to
  `"Settlement_20"`.

- occupation:

  Main occupation, with rare categories.

- income:

  Annual income, skewed, with missing values and one outlier.

- smoker:

  Logical with missing values.

- note:

  Free text.

`fake_contacts`, zero or more rows per person:

- person_id:

  Key to `fake_persons`.

- contact_date:

  Date of contact.

- contact_type:

  Type of contact, with a rare category.

- diag_code:

  Invented diagnosis code `"X00"` to `"X60"`.

`fake_admissions`, zero or more rows per person:

- person_id:

  Key to `fake_persons`.

- admit_date, discharge_date:

  Dates of admission and discharge.

- length_of_stay:

  Days.

- ward:

  Ward type.

An object of class `data.frame` with 7726 rows and 4 columns.

An object of class `data.frame` with 482 rows and 5 columns.

## Details

The tables are built to exercise the protection rules: several
settlements have fewer than 5 inhabitants, some occupations and contact
types are rare, income has one dominating outlier, `note` is free text,
and many persons have no contacts or admissions.

## Examples

``` r
head(fake_persons)
#>   person_id birth_date sex    settlement   occupation income smoker
#> 1  P3424376 1959-01-31   F Settlement_02        Trade 181300   TRUE
#> 2  P2410400 1940-04-01   M Settlement_12 Construction 147400  FALSE
#> 3  P8850624 1941-06-24   M        Town_A      Tourism 179400  FALSE
#> 4  P1822493 2011-06-16   M        Town_B      Fishing 128400  FALSE
#> 5  P5812956 1966-03-18   F        Town_B      Fishing 358600  FALSE
#> 6  P2052804 2007-11-17   F Settlement_04      Student 276000   TRUE
#>                                                              note
#> 1                                       season radio visit letter
#> 2             storm returned moved net family doctor harbour work
#> 3                        letter season school family family moved
#> 4                                                            <NA>
#> 5                     letter letter ice ice ice work moved family
#> 6 storm winter doctor family season family boat net doctor family
table(fake_persons$settlement)
#> 
#> Settlement_01 Settlement_02 Settlement_03 Settlement_04 Settlement_05 
#>           120            90            80            60            55 
#> Settlement_06 Settlement_07 Settlement_08 Settlement_09 Settlement_10 
#>            40            35            30            25            20 
#> Settlement_11 Settlement_12 Settlement_13 Settlement_14 Settlement_15 
#>            18            15            12             9             7 
#> Settlement_16 Settlement_17 Settlement_18 Settlement_19 Settlement_20 
#>             4             3             2             1             1 
#>        Town_A        Town_B 
#>           923           450 
head(fake_contacts)
#>   person_id contact_date contact_type diag_code
#> 1  P1014639   2018-07-04        Nurse       X19
#> 2  P1017587   2018-09-22 Telemedicine       X02
#> 3  P1017587   2019-08-06        Phone       X39
#> 4  P1017587   2024-01-29    Emergency       X11
#> 5  P1035233   2016-03-19        Nurse       X08
#> 6  P1035233   2016-09-18        Nurse       X01
```
