m <- profile_data(example_tables(), keys = "person_id", config = example_config())

test_that("level 1 has the right structure", {
  copy <- mimic(m, seed = 1, mess = FALSE)
  expect_named(copy, c("persons", "contacts", "admissions"))
  for (tn in names(copy)) {
    real <- example_tables()[[tn]]
    expect_named(copy[[tn]], names(real))
    expect_equal(vapply(copy[[tn]], function(v) class(v)[1], character(1)),
                 vapply(real, function(v) class(v)[1], character(1)))
  }
  p <- copy$persons
  expect_equal(nrow(p), 2000)
  expect_true(all(p$settlement %in% m$tables$persons$variables$settlement$levels))
  expect_true(all(grepl("^S[0-9]{7}$", p$person_id)))
  expect_false(anyDuplicated(p$person_id) > 0)
  inc <- m$tables$persons$variables$income
  expect_true(all(p$income >= inc$lo & p$income <= inc$hi))
  expect_false(anyNA(p$income))
})

test_that("linked tables share keys and follow rows per person", {
  copy <- mimic(m, fidelity = 2, seed = 2, mess = FALSE)
  expect_true(all(copy$contacts$person_id %in% copy$persons$person_id))
  expect_true(all(copy$admissions$person_id %in% copy$persons$person_id))
  # persons without contacts exist, as in the real data
  share_with <- mean(copy$persons$person_id %in% copy$contacts$person_id)
  expect_equal(share_with, m$tables$contacts$share_with_rows, tolerance = 0.05)
  rpp <- table(factor(copy$contacts$person_id, levels = copy$persons$person_id))
  expect_lte(max(rpp), m$tables$contacts$rows_per_person$hi)
})

test_that("level 2 follows the coarse distributions", {
  copy <- mimic(m, fidelity = 2, seed = 3, mess = FALSE)
  v <- m$tables$persons$variables$settlement
  share_real <- v$freq / sum(v$freq)
  share_copy <- as.numeric(table(factor(copy$persons$settlement, levels = v$levels))) / 2000
  expect_lt(max(abs(share_real - share_copy)), 0.03)
  expect_equal(stats::median(copy$persons$income), m$tables$persons$variables$income$median,
               tolerance = 0.15)
  # level 1 is uniform instead
  flat <- mimic(m, fidelity = 1, seed = 3, mess = FALSE)
  expect_lt(abs(mean(flat$persons$settlement == "Town_A") - 1 / length(v$levels)), 0.02)
})

test_that("seed gives a reproducible copy and restores the session seed", {
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  a <- mimic(m, seed = 5)
  after <- stats::runif(1)
  expect_equal(before, after)
  expect_equal(mimic(m, seed = 5), a)
})

test_that("n scales the copy", {
  copy <- mimic(m, n = 100, seed = 1, mess = FALSE)
  expect_equal(nrow(copy$persons), 100)
  expect_lt(nrow(copy$contacts), 1000)
})

test_that("a single table gives a data frame", {
  m1 <- profile_data(fake_persons, keys = "person_id", config = example_config())
  copy <- mimic(m1, seed = 1)
  expect_s3_class(copy, "data.frame")
  expect_equal(nrow(copy), 2000)
  expect_type(attr(copy, "mimic"), "list")
})

# --- mess ---------------------------------------------------------------------

test_that("mess adds each kind of problem and logs it", {
  copy <- mimic(m, fidelity = 2, seed = 4)
  log <- attr(copy, "mimic")$mess
  expect_true(all(c("missing values", "invalid codes", "extreme values", "empty strings",
                    "odd dates", "rows with keys not in the main table") %in% log$type))
  p <- copy$persons
  expect_equal(mean(is.na(p$income)), m$tables$persons$variables$income$missing, tolerance = 0.01)
  expect_true(any(p$income == 999999, na.rm = TRUE))
  expect_true(any(p$sex %in% c("999", "UNKNOWN", "?"), na.rm = TRUE))
  expect_true(any(p$occupation %in% c("", " "), na.rm = TRUE))
  expect_true(any(p$birth_date == as.Date("1900-01-01") | p$birth_date == as.Date("9999-12-31"), na.rm = TRUE))
  expect_true(any(!copy$contacts$person_id %in% p$person_id))
})

test_that("each kind of mess can be switched off", {
  opts <- mess_options(missing = FALSE, invalid_codes = FALSE, extremes = TRUE,
                       empty_strings = FALSE, odd_dates = FALSE, orphans = FALSE)
  copy <- mimic(m, seed = 6, mess = opts)
  log <- attr(copy, "mimic")$mess
  expect_equal(unique(log$type), "extreme values")
  expect_true(all(copy$contacts$person_id %in% copy$persons$person_id))

  clean <- mimic(m, seed = 6, mess = FALSE)
  expect_equal(nrow(attr(clean, "mimic")$mess), 0)
  expect_false(anyNA(clean$persons$income))
  expect_error(mimic(m, mess = "yes"), "mess")
  expect_error(mess_options(rate = 2), "rate")
})

# --- no real values -----------------------------------------------------------

test_that("the copy contains no real text, rare labels or extreme values", {
  copy <- mimic(m, fidelity = 2, seed = 7)
  s <- strings_in(copy)
  expect_false(any(stats::na.omit(fake_persons$note) %in% s))
  small <- names(which(table(fake_persons$settlement) < 5))
  expect_false(any(small %in% s))
  expect_false(max(fake_persons$income, na.rm = TRUE) %in% numbers_in(copy))
})

test_that("synthetic ids are marked and never equal real ids", {
  copy <- mimic(m, seed = 8)
  ids <- unique(c(copy$persons$person_id, copy$contacts$person_id, copy$admissions$person_id))
  expect_false(any(ids %in% fake_persons$person_id))
  expect_true(all(grepl("^S[0-9]{7}$", stats::na.omit(ids))))

  # a format without a constant first character gets the prefix in front
  expect_equal(apply_prefix("9999", "S"), "S9999")
  expect_equal(apply_prefix("S999", "S"), "SS999")
  expect_error(mimic(m, id_prefix = "A"), "id_prefix")
})

test_that("without a prefix, an id that collides by chance carries no real values", {
  copy <- mimic(m, fidelity = 2, seed = 8, mess = FALSE, id_prefix = NULL)
  expect_true(all(grepl("^P[0-9]{7}$", copy$persons$person_id)))
  hit <- intersect(copy$persons$person_id, fake_persons$person_id)
  for (id in hit) {
    real <- fake_persons[fake_persons$person_id == id, ]
    syn <- copy$persons[copy$persons$person_id == id, ]
    expect_false(identical(real$birth_date, syn$birth_date) &&
                 identical(real$settlement, syn$settlement) &&
                 identical(real$income, syn$income))
  }
  succeed()
})

# --- levels 3 and 4 -----------------------------------------------------------

test_that("levels 3 and 4 need an explicit choice", {
  expect_error(mimic(m, fidelity = 3), "i_understand_the_risk")
  expect_error(mimic(m, fidelity = 4), "i_understand_the_risk")
  expect_error(mimic(m, fidelity = 5), "1, 2, 3 or 4")
  expect_error(mimic(list(), fidelity = 1), "profile_data")
  expect_error(suppressWarnings(mimic(m, fidelity = 4, i_understand_the_risk = TRUE)), "real_data")
})
