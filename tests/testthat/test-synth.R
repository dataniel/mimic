# Example data with real structure: income rises with age, and women have
# more nurse contacts. The shipped fake data are independent by design.
structured <- function() {
  set.seed(11)
  p <- fake_persons
  age <- as.numeric(as.Date("2025-01-01") - p$birth_date) / 365.25
  p$income <- round(pmax(20000, 100000 + 4000 * pmin(age, 65) + stats::rnorm(nrow(p), 0, 60000)), -2)
  p$income[sample(nrow(p), 60)] <- NA
  c <- fake_contacts
  female <- p$sex[match(c$person_id, p$person_id)] == "F"
  c$contact_type <- ifelse(female & stats::runif(nrow(c)) < 0.7, "Nurse",
                           ifelse(!female & stats::runif(nrow(c)) < 0.7, "Clinic", c$contact_type))
  list(persons = p, contacts = c)
}
age_of <- function(d) as.numeric(as.Date("2025-01-01") - d) / 365.25
spearman <- function(x, y) stats::cor(x, y, method = "spearman", use = "complete.obs")

real <- structured()
cfg <- mimic_config(quasi_ids = c("sex", "settlement"), geo_vars = "settlement", text_vars = "note")
m <- profile_data(real, keys = "person_id", config = cfg)
quiet_dress <- function(...) suppressMessages(suppressWarnings(mimic(...)))

# --- correlations in profile_data() ----------------------------------------------------

test_that("profile_data stores coarse correlations", {
  r <- m$tables$persons$cor
  expect_true(is.matrix(r))
  expect_equal(r, t(r))
  expect_true(all(abs(r * 10 - round(r * 10)) < 1e-9))
  expect_true(".rows_contacts" %in% colnames(r))
  expect_lt(r["birth_date", "income"], -0.5)
  expect_false("person_id" %in% colnames(r))
  expect_false("note" %in% colnames(r))
})

test_that("correlations are not stored for small populations", {
  small <- profile_data(list(persons = real$persons[1:60, ]), keys = "person_id", config = cfg)
  expect_null(small$tables$persons$cor)
  expect_true(any(grepl("correlations not stored", small$risk$changes$action)))
  expect_warning(mimic(small, fidelity = 3, i_understand_the_risk = TRUE, seed = 1), "falls back to level 2")
})

# --- level 3 ---------------------------------------------------------------------

test_that("level 3 keeps relationships that level 2 loses", {
  l2 <- mimic(m, fidelity = 2, mess = FALSE, seed = 1)$persons
  l3 <- quiet_dress(m, fidelity = 3, mess = FALSE, seed = 1, i_understand_the_risk = TRUE)$persons
  expect_lt(abs(spearman(age_of(l2$birth_date), l2$income)), 0.15)
  expect_gt(spearman(age_of(l3$birth_date), l3$income), 0.6)
})

test_that("level 3 keeps the marginals of level 2 and the structure", {
  l3 <- quiet_dress(m, fidelity = 3, mess = FALSE, seed = 2, i_understand_the_risk = TRUE)
  v <- m$tables$persons$variables$settlement
  share_real <- v$freq / sum(v$freq)
  share_copy <- as.numeric(table(factor(l3$persons$settlement, levels = v$levels))) / nrow(l3$persons)
  expect_lt(max(abs(share_real - share_copy)), 0.03)
  inc <- m$tables$persons$variables$income
  expect_true(all(l3$persons$income >= inc$lo & l3$persons$income <= inc$hi))
  expect_true(all(l3$contacts$person_id %in% l3$persons$person_id))
  expect_lte(max(table(l3$contacts$person_id)), m$tables$contacts$rows_per_person$hi)
})

test_that("level 3 runs without the real data and always reports risk", {
  expect_message(l3 <- suppressWarnings(mimic(m, fidelity = 3, seed = 3, i_understand_the_risk = TRUE)),
                 "Risk report")
  risk <- attr(l3, "mimic")$risk
  expect_s3_class(risk, "mimic_copy_risk")
  expect_null(risk$comparison)
  with_real <- quiet_dress(m, fidelity = 3, seed = 3, i_understand_the_risk = TRUE, real_data = real)
  expect_s3_class(attr(with_real, "mimic")$risk$comparison, "data.frame")
})

# --- level 4 ---------------------------------------------------------------------

test_that("level 4 keeps relationships, also across linked tables", {
  l3 <- quiet_dress(m, fidelity = 3, mess = FALSE, seed = 4, i_understand_the_risk = TRUE)
  l4 <- quiet_dress(m, fidelity = 4, mess = FALSE, seed = 4, i_understand_the_risk = TRUE, real_data = real)
  expect_gt(spearman(age_of(l4$persons$birth_date), l4$persons$income), 0.6)

  nurse_share <- function(copy) {
    sex <- copy$persons$sex[match(copy$contacts$person_id, copy$persons$person_id)]
    mean(copy$contacts$contact_type[sex == "F"] == "Nurse") -
      mean(copy$contacts$contact_type[sex == "M"] == "Nurse")
  }
  expect_gt(nurse_share(real), 0.4)
  expect_gt(nurse_share(l4), 0.3)
  # level 3 relates linked variables to each other, not to the person
  expect_lt(abs(nurse_share(l3)), 0.1)
})

test_that("level 4 values are protected", {
  l4 <- quiet_dress(m, fidelity = 4, mess = FALSE, seed = 5, i_understand_the_risk = TRUE, real_data = real)
  p <- l4$persons
  inc <- m$tables$persons$variables$income
  expect_true(all(p$income >= inc$lo & p$income <= inc$hi))
  # smoothing: synthetic incomes rarely hit a real value exactly
  expect_lt(mean(p$income %in% real$persons$income), 0.05)
  # no labels of small settlements, no real ids, no free text
  s <- strings_in(l4)
  small <- names(which(table(real$persons$settlement) < 5))
  expect_false(any(small %in% s))
  expect_false(any(real$persons$person_id %in% s))
  expect_false(any(stats::na.omit(real$persons$note) %in% s))
  # rows per person stay within the top code
  expect_lte(max(table(l4$contacts$person_id)), m$tables$contacts$rows_per_person$hi)
})

test_that("level 4 reports how close the copy is to the real data", {
  expect_message(l4 <- suppressWarnings(mimic(m, fidelity = 4, seed = 6, i_understand_the_risk = TRUE,
                                              real_data = real)),
                 "treat it as output")
  risk <- attr(l4, "mimic")$risk
  expect_equal(risk$fidelity, 4L)
  expect_true(risk$stays_inside)
  expect_named(risk$comparison, c("table", "compared", "identical_rows", "by_chance",
                                  "replicated_uniques", "uniques_on"))
})

test_that("level 4 checks its input", {
  expect_error(quiet_dress(m, fidelity = 4, i_understand_the_risk = TRUE,
                           real_data = list(persons = real$persons)), "lacks table")
  expect_error(quiet_dress(m, fidelity = 4, i_understand_the_risk = TRUE, real_data = real,
                           train_rows = 0), "train_rows")
})

test_that("level 4 trees are fitted on a sample of whole persons", {
  pid <- rep(c("a", "b", "c", "d"), c(3, 1, 4, 2))
  set.seed(1)
  rows <- training_rows(length(pid), 5, pid)
  expect_lte(length(rows), 5)
  # every person sampled comes with all their rows
  kept <- unique(pid[rows])
  expect_setequal(rows, which(pid %in% kept))
  expect_equal(training_rows(10, NULL, pid), 1:10)
  expect_equal(training_rows(10, 20, pid), 1:10)
  expect_length(training_rows(10, 4), 4)

  # a sampled level 4 copy keeps relationships and protection
  l4 <- quiet_dress(m, fidelity = 4, mess = FALSE, seed = 4, i_understand_the_risk = TRUE,
                    real_data = real, train_rows = 1000)
  expect_gt(spearman(age_of(l4$persons$birth_date), l4$persons$income), 0.5)
  inc <- m$tables$persons$variables$income
  expect_true(all(l4$persons$income >= inc$lo & l4$persons$income <= inc$hi))
  expect_equal(nrow(l4$persons), 2000)
})

test_that("a copy of a single table works on levels 3 and 4", {
  m1 <- profile_data(real$persons, keys = "person_id", config = cfg)
  l3 <- quiet_dress(m1, fidelity = 3, seed = 7, i_understand_the_risk = TRUE)
  l4 <- quiet_dress(m1, fidelity = 4, seed = 7, i_understand_the_risk = TRUE, real_data = real$persons)
  expect_s3_class(l3, "data.frame")
  expect_s3_class(l4, "data.frame")
  expect_equal(nrow(l4), 2000)
})

test_that("level 4 donors come from enough persons in linked tables", {
  # one person has ten rows that a tree can split off: x is 1 only for them
  cfg <- mimic_config()
  pid <- c(rep("P_ONE", 10), sprintf("P%03d", rep(1:100, each = 2)))
  x <- c(rep(1, 10), rep(0, 200))
  y <- factor(c(rep("own", 10), rep("common", 200)), levels = c("common", "own"))
  spec <- list(type = "categorical", levels = c("common", "own"), freq = c(200, 10), class = "character")
  set.seed(3)
  syn_pred <- data.frame(x = rep(1, 200))
  by_rows <- cart_draw(y, data.frame(x = x), syn_pred, spec, cfg)
  by_persons <- cart_draw(y, data.frame(x = x), syn_pred, spec, cfg, pid = pid)
  # without persons the leaf of one person gives only that person's value
  expect_true(all(by_rows == "own"))
  expect_lt(mean(by_persons == "own"), 0.5)
})

test_that("take_rows gives the rows of a data frame, repeated and missing ones too", {
  df <- data.frame(a = 1:4, b = factor(c("x", "y", "x", "z")), d = as.Date("2020-01-01") + 0:3,
                   stringsAsFactors = FALSE)
  i <- c(2L, 2L, NA, 4L, 1L)
  ref <- df[i, , drop = FALSE]
  rownames(ref) <- NULL
  expect_identical(take_rows(df, i), ref)
  expect_identical(take_rows(df, integer()), df[0, , drop = FALSE])
})

test_that("row_keys are equal for equal rows and for no others", {
  a <- data.frame(x = c(1, 2, NA, 2), y = c("u", "v", "u", "v"), d = as.Date("2020-01-01") + c(0, 1, 0, 1),
                  stringsAsFactors = FALSE)
  b <- data.frame(x = c(2, NA, 3), y = c("v", "u", "v"), d = as.Date("2020-01-01") + c(1, 0, 1),
                  stringsAsFactors = FALSE)
  k <- row_keys(list(a, b), c("x", "y", "d"))
  expect_equal(k[[1]] %in% k[[2]], c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(k[[2]] %in% k[[1]], c(TRUE, TRUE, FALSE))
  expect_equal(k[[1]][2], k[[1]][4])
})
