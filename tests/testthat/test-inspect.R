real <- list(persons = fake_persons, contacts = fake_contacts)

out_dir <- function() {
  d <- tempfile("inspect_")
  dir.create(d)
  d
}
put_csv <- function(d, name, df) utils::write.csv(df, file.path(d, name), row.names = FALSE)
verdict <- function(res, file) res$files$verdict[res$files$file == file]

test_that("a safe count table is approved", {
  d <- out_dir()
  put_csv(d, "by_sex.csv", data.frame(sex = c("F", "M"), n = c(985, 1015)))
  res <- inspect(d)
  expect_s3_class(res, "mimic_inspection")
  expect_equal(verdict(res, "by_sex.csv"), "APPROVED")
})

test_that("small cells, extremes and identifiers need changes", {
  d <- out_dir()
  put_csv(d, "small.csv", data.frame(g = c("a", "b"), n = c(100, 3)))
  put_csv(d, "extremes.csv", data.frame(g = "a", mean = 1, max_income = 2, n = 50))
  put_csv(d, "rows.csv", data.frame(person_id = c("P1", "P2"), x = 1:2))
  res <- inspect(d)
  expect_equal(verdict(res, "small.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "extremes.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "rows.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "small.csv"], "fewer than 5")
})

test_that("cross tables are read as counts", {
  d <- out_dir()
  xt <- data.frame(occupation = c("Fishing", "Aviation"), F = c(150, 2), M = c(200, 8))
  put_csv(d, "xt.csv", xt)
  res <- inspect(d)
  expect_equal(verdict(res, "xt.csv"), "NEEDS CHANGES")
  det <- res$checks[["xt.csv"]][[which(vapply(res$checks[["xt.csv"]], `[[`, "", "rule") == "min_count")]]$details
  expect_equal(det$occupation, "Aviation")
  expect_equal(det$column, "F")
})

test_that("tables of magnitudes without counts go to review by hand", {
  d <- out_dir()
  put_csv(d, "means.csv", data.frame(sex = c("F", "M"), mean_income = c(280000.5, 300000.25)))
  res <- inspect(d)
  expect_equal(verdict(res, "means.csv"), "REVIEW BY HAND")
  expect_match(res$files$reasons, "count column")
})

test_that("dominance is checked against the real data", {
  d <- out_dir()
  tab <- aggregate(income ~ settlement, data = fake_persons, FUN = sum)
  names(tab)[2] <- "sum_income"
  tab$n <- 1000
  put_csv(d, "sums.csv", tab)
  res <- inspect(d, real_data = real)
  rules <- vapply(res$checks[["sums.csv"]], `[[`, "", "rule")
  dom <- res$checks[["sums.csv"]][[which(rules == "dominance")]]
  expect_equal(dom$status, "fail")
  expect_true(all(dom$details$settlement %in% names(which(table(fake_persons$settlement) < 5))))
})

test_that("differencing between two tables is found", {
  d <- out_dir()
  put_csv(d, "all.csv", data.frame(sex = c("F", "M"), n = c(985, 1015)))
  put_csv(d, "most.csv", data.frame(sex = c("F", "M"), n = c(983, 1015)))
  res <- inspect(d)
  expect_equal(verdict(res, "all.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "most.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[1], "Subtracting")
})

test_that("R objects: models, summaries and lists", {
  d <- out_dir()
  saveRDS(stats::lm(income ~ sex, data = fake_persons), file.path(d, "good_model.rds"))
  saveRDS(stats::lm(income ~ sex, data = fake_persons[1:6, ]), file.path(d, "small_model.rds"))
  saveRDS(summary(fake_persons$income), file.path(d, "summary.rds"))
  saveRDS(list(tab = data.frame(sex = c("F", "M"), n = c(985, 1015)), note = "fine"),
          file.path(d, "list.rds"))
  utils::write.csv(coef(summary(stats::lm(income ~ sex, data = fake_persons))), file.path(d, "coef.csv"))
  res <- inspect(d)
  # a fitted model object carries the data it was fitted on
  expect_equal(verdict(res, "good_model.rds"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "good_model.rds"], "issue")
  expect_equal(verdict(res, "small_model.rds"), "NEEDS CHANGES")
  expect_equal(verdict(res, "coef.csv"), "REVIEW BY HAND")
  expect_match(res$files$reasons[res$files$file == "coef.csv"], "coefficients")
  expect_equal(verdict(res, "summary.rds"), "NEEDS CHANGES")
  # the character element cannot be checked, so the file is not approved
  expect_equal(verdict(res, "list.rds"), "REVIEW BY HAND")
})

test_that("images and unknown files are never approved automatically", {
  d <- out_dir()
  grDevices::png(file.path(d, "plot.png"))
  graphics::plot(1:3)
  grDevices::dev.off()
  writeLines("x", file.path(d, "data.xyz"))
  res <- inspect(d)
  expect_equal(verdict(res, "plot.png"), "REVIEW BY HAND")
  expect_equal(verdict(res, "data.xyz"), "REVIEW BY HAND")
})

test_that("text is searched for values with the real data", {
  d <- out_dir()
  writeLines(c("All good", "mean income about 290000"), file.path(d, "notes.txt"))
  writeLines(c("look at", fake_persons$person_id[1]), file.path(d, "leak.txt"))
  writeLines(c("note:", stats::na.omit(fake_persons$note)[1]), file.path(d, "leak2.md"))
  res <- inspect(d, real_data = real)
  expect_equal(verdict(res, "notes.txt"), "APPROVED")
  expect_equal(verdict(res, "leak.txt"), "NEEDS CHANGES")
  expect_equal(verdict(res, "leak2.md"), "NEEDS CHANGES")
  # without real data text needs a human
  expect_equal(verdict(inspect(d), "notes.txt"), "REVIEW BY HAND")
})

test_that("row-level output is found with the real data", {
  d <- out_dir()
  put_csv(d, "copy.csv", fake_persons[, c("sex", "settlement", "income")])
  res <- inspect(d, real_data = real)
  expect_equal(verdict(res, "copy.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons, "row-level")
})

test_that("an extract without identifiers and below the row threshold is not approved", {
  d <- out_dir()
  ext <- fake_persons[fake_persons$settlement == "Town_B", c("sex", "settlement", "occupation")]
  ext$birth_year <- as.integer(format(fake_persons$birth_date[fake_persons$settlement == "Town_B"], "%Y"))
  put_csv(d, "extract.csv", ext)
  # aggregated tables, also with a whole-number grouping column, are fine
  put_csv(d, "by_year.csv", data.frame(year = rep(2019:2020, each = 2), sex = c("F", "M"), n = c(10, 12, 11, 9)))
  for (rd in list(NULL, real)) {
    res <- inspect(d, real_data = rd)
    expect_equal(verdict(res, "extract.csv"), "NEEDS CHANGES")
    expect_match(res$files$reasons[res$files$file == "extract.csv"], "repeat a combination")
    expect_equal(verdict(res, "by_year.csv"), "APPROVED")
  }
})

test_that("tables of values without numbers are not approved", {
  d <- out_dir()
  ext <- fake_persons[1:20, c("birth_date", "sex", "settlement")]
  put_csv(d, "extract.csv", ext)
  put_csv(d, "notes.csv", data.frame(note = stats::na.omit(fake_persons$note)[1:5]))
  res <- inspect(d)
  expect_equal(verdict(res, "extract.csv"), "REVIEW BY HAND")
  expect_equal(verdict(res, "notes.csv"), "REVIEW BY HAND")
  expect_match(res$files$reasons[res$files$file == "extract.csv"], "without counts or numbers")
  # with the real data the day-precise dates and the notes are found
  res <- inspect(d, config = example_config(), real_data = real)
  expect_equal(verdict(res, "extract.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "notes.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "notes.csv"], "could point to a person")
})

test_that("a rare label is found whatever its count says", {
  d <- out_dir()
  rare <- names(which(table(fake_persons$settlement) < 5))[1]
  # 20 contacts may be one person
  put_csv(d, "contacts.csv", data.frame(settlement = c("Town_A", rare), n = c(500, 20)))
  res <- inspect(d, real_data = real)
  expect_equal(verdict(res, "contacts.csv"), "NEEDS CHANGES")
})

test_that("row names written by write.csv() do not hide an extract", {
  d <- out_dir()
  ext <- fake_persons[fake_persons$settlement == "Town_B", c("sex", "settlement", "occupation")]
  utils::write.csv(ext, file.path(d, "extract.csv"))
  res <- inspect(d)
  expect_equal(verdict(res, "extract.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons, "repeat a combination")
  expect_false("row_names" %in% names(res$tables[["extract.csv"]]))
})

test_that("counts are checked per year, and suppression and groups are checked", {
  d <- out_dir()
  put_csv(d, "by_year.csv", data.frame(year = rep(2019:2020, each = 2), sex = c("F", "M"), n = c(3, 12, 11, 9)))
  put_csv(d, "suppressed.csv", data.frame(sex = c("F", "M", "Total"), n = c("<5", "1015", "1018")))
  put_csv(d, "smokers.csv", data.frame(place = rep(c("A", "B"), each = 2), smoker = rep(c("yes", "no"), 2),
                                       n = c(0, 12, 210, 640)))
  res <- inspect(d)
  expect_equal(verdict(res, "by_year.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "suppressed.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "suppressed.csv"], "recovered")
  expect_equal(verdict(res, "smokers.csv"), "REVIEW BY HAND")
  expect_match(res$files$reasons[res$files$file == "smokers.csv"], "shares one value")
})

test_that("write_report writes a markdown report outside the output", {
  d <- out_dir()
  put_csv(d, "small.csv", data.frame(g = c("a", "b"), n = c(100, 3)))
  res <- inspect(d)
  f <- tempfile(fileext = ".md")
  write_report(res, f)
  md <- readLines(f)
  expect_true(any(grepl("^# Output inspection", md)))
  expect_true(any(grepl("small.csv \\| NEEDS CHANGES", md)))
  expect_true(any(grepl("Approved by", md)))
  expect_error(write_report(res, file.path(d, "report.md")), "outside")
  expect_error(write_report(list(), f), "inspect")
})

test_that("inspect gives clear errors", {
  expect_error(inspect(file.path(tempdir(), "does_not_exist_dir")), "existing directory")
})

test_that("a share next to a suppressed count is found", {
  d <- out_dir()
  put_csv(d, "pct.csv", data.frame(settlement = c("Town_A", "Settlement_20"), n = c("1997", "<5"),
                                   pct = c(99.85, 0.15)))
  put_csv(d, "ok.csv", data.frame(settlement = c("Town_A", "Settlement_20"), n = c("1997", "<5"),
                                  pct = c(99.85, NA)))
  res <- inspect(d)
  expect_equal(verdict(res, "pct.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "pct.csv"], "pct")
  expect_false(grepl("still show", res$files$reasons[res$files$file == "ok.csv"]))
})

test_that("a count of part of the group next to a suppressed count is found", {
  d <- out_dir()
  put_csv(d, "sub.csv", data.frame(settlement = c("Town_A", "Settlement_20"), n = c("1997", "<5"),
                                   n_female = c(1000, 2)))
  res <- inspect(d)
  expect_equal(verdict(res, "sub.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons, "n_female")
})

test_that("a small count of part of the group is found", {
  d <- out_dir()
  put_csv(d, "sub.csv", data.frame(settlement = c("Town_A", "Town_B"), n = c(1997, 300),
                                   n_female = c(1000, 2)))
  res <- inspect(d)
  expect_equal(verdict(res, "sub.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons, "fewer than .*column n_female")
  # n_female is a count, not a grouping column
  expect_false(grepl("shares one value", res$files$reasons))
})

test_that("a suppressed part that is the count minus the other parts is found", {
  d <- out_dir()
  put_csv(d, "back.csv", data.frame(settlement = c("Town_A", "Town_B"), n = c(1997, 300),
                                    n_female = c("1000", "<5"), n_male = c(997, 298)))
  # two suppressed parts protect each other
  put_csv(d, "two.csv", data.frame(settlement = c("Town_A", "Town_B"), n = c("1997", "<5"),
                                   n_female = c("1000", "<5"), n_male = c("997", "<5")))
  res <- inspect(d)
  expect_equal(verdict(res, "back.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "back.csv"], "only n_female is suppressed")
  expect_false(grepl("subtraction", res$files$reasons[res$files$file == "two.csv"]))
})

test_that("a count-like category next to a suppressed count is not a count of part of the group", {
  d <- out_dir()
  put_csv(d, "kids.csv", data.frame(antal_boern = 0:4, n = c("900", "600", "400", "<5", "<5")))
  res <- inspect(d)
  expect_false(grepl("still show", res$files$reasons))
})

test_that("a file named after a person is found with the real data", {
  d <- out_dir()
  put_csv(d, paste0(fake_persons$person_id[1], ".csv"), data.frame(sex = c("F", "M"), n = c(985, 1015)))
  res <- inspect(d, real_data = real)
  expect_equal(res$files$verdict, "NEEDS CHANGES")
  expect_match(res$files$reasons, "file name")
})
