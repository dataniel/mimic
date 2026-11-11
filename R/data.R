#' Invented example data for a small population
#'
#' Three linked tables describing an imaginary population of 2,000 persons
#' living in two towns and twenty settlements. **Everything is invented**:
#' identifiers, dates, place labels and code lists have no relation to any
#' real person, place or classification. The script that generates them is
#' in `data-raw/make_example_data.R` in the package source.
#'
#' The tables are built to exercise the protection rules: several
#' settlements have fewer than 5 inhabitants, some occupations and contact
#' types are rare, income has one dominating outlier, `note` is free text,
#' and many persons have no contacts or admissions.
#'
#' @format
#' `fake_persons`, one row per person:
#' \describe{
#'   \item{person_id}{Identifier, `"P"` followed by 7 digits.}
#'   \item{birth_date}{Date of birth.}
#'   \item{sex}{`"F"` or `"M"`.}
#'   \item{settlement}{Place of residence, `"Town_A"`, `"Town_B"` or
#'     `"Settlement_01"` to `"Settlement_20"`.}
#'   \item{occupation}{Main occupation, with rare categories.}
#'   \item{income}{Annual income, skewed, with missing values and one outlier.}
#'   \item{smoker}{Logical with missing values.}
#'   \item{note}{Free text.}
#' }
#'
#' `fake_contacts`, zero or more rows per person:
#' \describe{
#'   \item{person_id}{Key to `fake_persons`.}
#'   \item{contact_date}{Date of contact.}
#'   \item{contact_type}{Type of contact, with a rare category.}
#'   \item{diag_code}{Invented diagnosis code `"X00"` to `"X60"`.}
#' }
#'
#' `fake_admissions`, zero or more rows per person:
#' \describe{
#'   \item{person_id}{Key to `fake_persons`.}
#'   \item{admit_date, discharge_date}{Dates of admission and discharge.}
#'   \item{length_of_stay}{Days.}
#'   \item{ward}{Ward type.}
#' }
#' @name fake_data
#' @aliases fake_persons fake_contacts fake_admissions
#' @examples
#' head(fake_persons)
#' table(fake_persons$settlement)
#' head(fake_contacts)
"fake_persons"

#' @rdname fake_data
"fake_contacts"

#' @rdname fake_data
"fake_admissions"
