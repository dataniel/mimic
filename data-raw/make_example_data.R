# Generates the example data shipped with mimic.
#
# Everything here is invented. No real data, real persons, real place names
# or real code lists are used. Settlement names are labels ("Settlement_07")
# so they cannot be mistaken for real places.
#
# The data imitate the shape of register data for a small population:
# two towns, many small settlements (some with fewer than 5 inhabitants),
# rare categories, skewed incomes with an outlier, free text, missing values
# and repeated observations per person.
#
# Run from the package root: Rscript --no-init-file data-raw/make_example_data.R

set.seed(20261109)

n_persons <- 2000

# --- persons ----------------------------------------------------------------

place_sizes <- c(Town_A = 900, Town_B = 450,
                 stats::setNames(c(120, 90, 80, 60, 55, 40, 35, 30, 25, 20,
                                   18, 15, 12, 9, 7, 4, 3, 2, 1, 1),
                                 sprintf("Settlement_%02d", 1:20)))
place_sizes["Town_A"] <- place_sizes["Town_A"] + (n_persons - sum(place_sizes))
settlement <- sample(rep(names(place_sizes), place_sizes))

# Identifiers: invented format "P" + 7 digits, not related to any real scheme
person_id <- sprintf("P%07d", sample(1e6:9e6, n_persons))

birth_date <- as.Date("1935-01-01") +
  sample(0:as.integer(as.Date("2024-12-31") - as.Date("1935-01-01")),
         n_persons, replace = TRUE)
sex <- sample(c("F", "M"), n_persons, replace = TRUE, prob = c(0.48, 0.52))

occupation <- sample(
  c("Fishing", "Hunting", "Public sector", "Trade", "Construction",
    "Student", "Retired", "Unemployed", "Mining", "Tourism", "Aviation"),
  n_persons, replace = TRUE,
  prob = c(0.18, 0.05, 0.25, 0.10, 0.08, 0.12, 0.12, 0.06, 0.02, 0.015, 0.005)
)

income <- round(stats::rlnorm(n_persons, meanlog = 12.4, sdlog = 0.6), -2)
income[sample(n_persons, 1)] <- 9800000           # one dominating outlier
income[sample(n_persons, 60)] <- NA

smoker <- sample(c(TRUE, FALSE, NA), n_persons, replace = TRUE,
                 prob = c(0.45, 0.50, 0.05))

words <- c("boat", "net", "snow", "visit", "family", "moved", "returned",
           "school", "work", "season", "house", "dog", "ice", "radio",
           "letter", "doctor", "storm", "harbour", "winter", "summer")
note <- vapply(seq_len(n_persons), function(i) {
  if (stats::runif(1) < 0.3) return(NA_character_)
  paste(sample(words, sample(3:12, 1), replace = TRUE), collapse = " ")
}, character(1))

fake_persons <- data.frame(
  person_id = person_id,
  birth_date = birth_date,
  sex = sex,
  settlement = settlement,
  occupation = occupation,
  income = income,
  smoker = smoker,
  note = note,
  stringsAsFactors = FALSE
)

# --- contacts (0 to many per person) ---------------------------------------

n_contacts <- stats::rnbinom(n_persons, size = 1.2, mu = 4)
contact_person <- rep(person_id, n_contacts)
n_c <- length(contact_person)

fake_contacts <- data.frame(
  person_id = contact_person,
  contact_date = as.Date("2015-01-01") +
    sample(0:(365 * 10 - 1), n_c, replace = TRUE),
  contact_type = sample(c("Clinic", "Nurse", "Phone", "Emergency", "Telemedicine"),
                        n_c, replace = TRUE,
                        prob = c(0.40, 0.30, 0.20, 0.09, 0.01)),
  diag_code = sprintf("X%02d", pmin(stats::rgeom(n_c, 0.12), 60)),
  stringsAsFactors = FALSE
)
fake_contacts <- fake_contacts[order(fake_contacts$person_id,
                                     fake_contacts$contact_date), ]
rownames(fake_contacts) <- NULL

# --- admissions (most persons have none) ------------------------------------

n_adm <- stats::rpois(n_persons, 0.25)
adm_person <- rep(person_id, n_adm)
n_a <- length(adm_person)
admit_date <- as.Date("2015-01-01") + sample(0:(365 * 10 - 1), n_a, replace = TRUE)
length_of_stay <- 1L + stats::rgeom(n_a, 0.25)

fake_admissions <- data.frame(
  person_id = adm_person,
  admit_date = admit_date,
  discharge_date = admit_date + length_of_stay,
  length_of_stay = length_of_stay,
  ward = sample(c("Medical", "Surgical", "Maternity", "Psychiatric"),
                n_a, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05)),
  stringsAsFactors = FALSE
)
fake_admissions <- fake_admissions[order(fake_admissions$person_id,
                                         fake_admissions$admit_date), ]
rownames(fake_admissions) <- NULL

usethis::use_data(fake_persons, fake_contacts, fake_admissions,
                  overwrite = TRUE, compress = "xz")
