# --- safe_count ------------------------------------------------------------------

test_that("safe_count counts persons, rounds and adds a total", {
  out <- safe_count(fake_persons, "sex")
  expect_equal(names(out), c("sex", "n"))
  expect_equal(out$sex, c("F", "M", "Total"))
  expect_true(all(out$n %% 5 == 0))
  expect_equal(out$n[3], 5 * round(nrow(fake_persons) / 5))
})

test_that("safe_count suppresses a small cell and the cell that would give it back", {
  d <- data.frame(sex = rep(c("F", "M"), c(3, 50)))
  expect_message(out <- safe_count(d, "sex"),
                 "Suppressed 1 cell with fewer than 5 persons, and 1 more so it cannot be worked out")
  expect_equal(out$n, c(NA, NA, 55))
  expect_equal(check_suppression(out)$status, "pass")

  # without totals there is nothing to give it back
  expect_message(out <- safe_count(d, "sex", total = FALSE), "Suppressed 1 cell")
  expect_equal(out$n, c(NA, 50))
})

test_that("safe_count on two variables leaves no small or recoverable cell", {
  out <- suppressMessages(safe_count(fake_persons, c("settlement", "sex")))
  expect_equal(check_suppression(out, by = c("settlement", "sex"), count = "n")$status, "pass")
  known <- out[!is.na(out$n), ]
  expect_equal(check_min_count(known, by = c("settlement", "sex"), count = "n")$status, "pass")
  expect_true(all(out$n %% 5 == 0, na.rm = TRUE))
  expect_true(any(out$settlement == "Total" & out$sex == "Total"))
  # a settlement too small for any count is suppressed in full
  small <- names(which(table(fake_persons$settlement) < 5))
  expect_true(all(is.na(out$n[out$settlement %in% small])))
})

test_that("safe_count counts persons, not rows", {
  out <- safe_count(fake_contacts, "contact_type", id = "person_id")
  persons <- length(unique(fake_contacts$person_id))
  expect_equal(out$n[out$contact_type == "Total"], 5 * round(persons / 5))
  rows <- safe_count(fake_contacts, "contact_type")
  expect_gt(rows$n[rows$contact_type == "Total"], out$n[out$contact_type == "Total"])
})

test_that("safe_count output passes inspect()", {
  out_dir <- file.path(tempfile("safe_"), "out")
  dir.create(out_dir, recursive = TRUE)
  utils::write.csv(safe_count(fake_persons, "sex"), file.path(out_dir, "sex.csv"), row.names = FALSE)
  d <- data.frame(sex = rep(c("F", "M"), c(3, 50)))
  utils::write.csv(suppressMessages(safe_count(d, "sex")), file.path(out_dir, "small.csv"), row.names = FALSE)
  res <- inspect(out_dir)
  expect_true(all(res$files$verdict != "NEEDS CHANGES"))
})

test_that("safe_count follows the configuration in the session", {
  d <- data.frame(g = rep(c("a", "b", "c"), c(7, 30, 40)))
  expect_equal(safe_count(d, "g")$n[1], 5)
  old <- options(mimic.config = mimic_config(min_count = 10))
  on.exit(options(old))
  out <- suppressMessages(safe_count(d, "g"))
  expect_true(is.na(out$n[1]))
  # an explicit config wins
  expect_equal(safe_count(d, "g", config = mimic_config())$n[1], 5)
})

test_that("safe_count gives clear errors", {
  expect_error(safe_count(fake_persons, "nope"), "nope")
  expect_error(safe_count(fake_persons, character()), "by")
  expect_error(safe_count(list(a = 1), "a"), "data frame")
  d <- data.frame(g = rep(c("Total", "a"), each = 10))
  expect_error(safe_count(d, "g"), "reads as a total")
  expect_silent(safe_count(d, "g", total = FALSE))
})

# --- safe_summary ----------------------------------------------------------------

test_that("safe_summary suppresses small groups and names columns after the value", {
  expect_message(out <- safe_summary(fake_persons, "income", by = "settlement"), "Suppressed")
  expect_equal(names(out), c("settlement", "n", "mean_income", "sd_income", "median_income"))
  small <- names(which(table(fake_persons$settlement[!is.na(fake_persons$income)]) < 5))
  expect_true(length(small) > 0)
  expect_true(all(is.na(out[out$settlement %in% small, -1])))
  expect_true(all(out$n %% 5 == 0, na.rm = TRUE))
})

test_that("safe_summary suppresses means of dominated groups but keeps the median", {
  d <- data.frame(g = rep(c("a", "b"), each = 10),
                  income = c(rep(100, 9), 1e7, seq(100, 1000, 100)))
  expect_message(out <- safe_summary(d, "income", by = "g"), "dominated")
  expect_true(is.na(out$mean_income[1]))
  expect_true(is.na(out$sd_income[1]))
  expect_equal(out$median_income[1], 100)
  expect_equal(out$mean_income[2], 550)
})

test_that("safe_summary adds up values per person for dominance", {
  # one person with many rows dominates, though no row does
  d <- data.frame(id = c(rep("p1", 20), paste0("q", 1:10)), cost = c(rep(1000, 20), rep(10, 10)))
  out <- suppressMessages(safe_summary(d, "cost", id = "id", stats = "sum"))
  expect_true(is.na(out$sum_cost))
  expect_equal(out$n, 10)
})

test_that("safe_summary refuses minimum and maximum", {
  expect_error(safe_summary(fake_persons, "income", stats = c("mean", "max")),
               "one person's value")
  expect_error(safe_summary(fake_persons, "income", stats = "mode"), "must be among")
  expect_error(safe_summary(fake_persons, "sex"), "numeric")
})

# --- safe_model ------------------------------------------------------------------

test_that("safe_model returns the coefficients without row-level data", {
  fit <- lm(income ~ sex, data = fake_persons)
  out <- safe_model(fit)
  expect_equal(names(out), c("term", "estimate", "std_error", "statistic", "p_value",
                             "observations", "df_residual"))
  expect_equal(out$term, c("(Intercept)", "sexM"))
  expect_equal(out$estimate, unname(coef(fit)))
  expect_equal(out$observations[1] %% 5, 0)
})

test_that("safe_model stops on a model with too few observations", {
  d <- fake_persons[1:8, ]
  fit <- lm(income ~ sex, data = d)
  expect_error(safe_model(fit), "too few observations")
  expect_error(safe_model(structure(list(), class = "odd_model")), "Cannot check")
})

# --- in the script's session -----------------------------------------------------

test_that("trial_run gives the script the configuration", {
  skip_on_cran()
  m <- profile_data(list(persons = fake_persons), keys = "person_id", config = example_config())
  copy <- mimic(m, seed = 1)
  fit <- trial_run("writeLines(format(getOption('mimic.config')$min_count), 'cfg.txt')",
                   copy, config = mimic_config(min_count = 10))
  expect_equal(readLines(file.path(fit$output_dir, "cfg.txt")), "10")
})

test_that("safe_count in a script passes the output check in trial_run", {
  skip_on_cran()
  installed <- tryCatch(callr::r(function() "safe_count" %in% getNamespaceExports("mimic")),
                        error = function(e) FALSE)
  skip_if_not(installed, "the installed mimic has no safe_count()")
  m <- profile_data(list(persons = fake_persons), keys = "person_id", config = example_config())
  copy <- mimic(m, seed = 1)
  # a copy of one table is a data frame, called `data` in the script
  fit <- trial_run(c("tab <- mimic::safe_count(data, 'sex', id = 'person_id')",
                     "write.csv(tab, 'sex.csv', row.names = FALSE)"), copy)
  expect_equal(fit$inspection$files$verdict, "APPROVED")
})
