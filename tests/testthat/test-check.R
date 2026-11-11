test_that("check_min_count works on microdata and counts persons", {
  ch <- check_min_count(fake_persons, by = "settlement", id = "person_id")
  expect_s3_class(ch, "mimic_check")
  expect_equal(ch$status, "fail")
  small <- names(which(table(fake_persons$settlement) < 5))
  expect_setequal(ch$details$settlement, small)

  # one person with many rows is still one person
  df <- data.frame(id = c(rep(1, 20), 2:30), g = c(rep("a", 20), rep("b", 29)))
  expect_equal(check_min_count(df, by = "g", id = "id")$status, "fail")
  expect_equal(check_min_count(df, by = "g")$status, "pass")
})

test_that("check_min_count works on aggregated tables and allows zeros", {
  tab <- data.frame(sex = c("F", "M", "X"), n = c(100, 3, 0))
  ch <- check_min_count(tab)
  expect_equal(ch$status, "fail")
  expect_equal(ch$details$sex, "M")
  expect_equal(check_min_count(table(c(rep("a", 10), rep("b", 6))))$status, "pass")
  expect_error(check_min_count(data.frame(a = 1:3)), "Give `by`")
})

test_that("check results never contain the small counts", {
  tab <- data.frame(sex = c("F", "M"), n = c(100, 3))
  ch <- check_min_count(tab)
  expect_false(any(grepl("\\b3\\b", c(ch$message, unlist(ch$details)))))
})

test_that("check_k_anonymity reports totals, never the rare combinations", {
  ch <- check_k_anonymity(fake_persons, c("birth_date", "sex", "settlement"), id = "person_id")
  expect_equal(ch$status, "fail")
  expect_true("without birth_date" %in% ch$details$variables)
  s <- strings_in(ch)
  expect_false(any(fake_persons$settlement %in% s))
  expect_false(any(format(fake_persons$birth_date, "%Y-%m") %in% s))

  ok <- check_k_anonymity(fake_persons, "sex")
  expect_equal(ok$status, "pass")
  expect_error(check_k_anonymity(fake_persons), "No quasi-identifiers")
})

test_that("check_dominance applies the (n, k) and p % rules", {
  df <- data.frame(g = rep(c("dominated", "balanced", "two"), c(10, 10, 2)),
                   v = c(1000, rep(1, 9), rep(10, 10), 5, 5))
  ch <- check_dominance(df, value = "v", by = "g")
  expect_equal(ch$status, "fail")
  expect_setequal(ch$details$g, c("dominated", "two"))
  expect_match(ch$details$rule_broken[ch$details$g == "two"], "p %")

  # contributions are summed per contributor first
  df2 <- data.frame(id = c(rep(1, 100), 2:10), v = 1)
  expect_equal(check_dominance(df2, value = "v", id = "id")$status, "fail")
  expect_equal(check_dominance(df2, value = "v")$status, "pass")
})

test_that("check_extremes finds min and max", {
  expect_equal(check_extremes(summary(1:10))$status, "fail")
  expect_equal(check_extremes(data.frame(g = "a", max_income = 1))$status, "fail")
  expect_equal(check_extremes(data.frame(stat = c("mean", "Max."), v = 1:2))$status, "fail")
  expect_equal(check_extremes(data.frame(g = "a", mean_income = 1, maximal = 2))$status, "pass")
})

test_that("check_model fails on model objects that carry the data", {
  ch <- check_model(lm(income ~ sex, data = fake_persons))
  expect_equal(ch$status, "fail")
  expect_match(ch$details$issue[1], "row-level data")
})

test_that("check_model fails on small models and rare factor levels", {
  fit <- lm(income ~ sex, data = fake_persons)
  fit[c("model", "residuals", "fitted.values", "effects", "qr", "na.action")] <- NULL
  expect_equal(check_model(fit)$status, "pass")
  expect_equal(check_model(lm(income ~ sex, data = fake_persons[1:8, ]))$status, "fail")
  ch <- check_model(lm(income ~ settlement, data = fake_persons))
  expect_equal(ch$status, "fail")
  expect_true(any(grepl("settlement", ch$details$issue)))
  expect_equal(check_model(list(a = 1))$status, "warn")
})

test_that("check_differencing finds tables that can be subtracted", {
  all <- data.frame(sex = c("F", "M"), n = c(100, 120))
  minus_two <- data.frame(sex = c("F", "M"), n = c(98, 120))
  minus_many <- data.frame(sex = c("F", "M"), n = c(80, 100))
  ch <- check_differencing(all, minus_two)
  expect_equal(ch$status, "fail")
  expect_equal(ch$details$sex, "F")
  expect_equal(check_differencing(all, all)$status, "pass")
  expect_equal(check_differencing(all, minus_many)$status, "pass")
})

test_that("check_keys finds missing, duplicated and orphaned keys", {
  tabs <- example_tables()
  expect_equal(check_keys(tabs, "person_id")$status, "pass")

  orphan <- tabs
  orphan$contacts$person_id[1] <- "P0000000"
  expect_equal(check_keys(orphan, "person_id", main = "persons")$status, "warn")

  dup <- tabs
  dup$persons <- rbind(dup$persons, dup$persons[1, ])
  ch <- check_keys(dup, "person_id", main = "persons")
  expect_equal(ch$status, "fail")
  expect_match(ch$details$issue, "duplicated", all = FALSE)

  expect_error(check_keys(tabs, "pid"), "Key column missing")
  expect_error(check_keys(tabs, c(nope = "person_id")), "unknown table")
})

test_that("check_suppression finds cells the totals give back", {
  tab <- data.frame(sex = c("F", "M", "Total"), n = c(NA, 120, 123))
  ch <- check_suppression(tab)
  expect_equal(ch$status, "fail")
  expect_equal(ch$details$sex, "F")
  tab$n[2] <- NA
  expect_equal(check_suppression(tab)$status, "pass")
  expect_match(check_suppression(data.frame(sex = c("F", "M"), n = c(5, 6)))$message, "No cells")

  # markers as text, and a recovered cell opens the next line
  two <- expand.grid(sex = c("F", "M", "Total"), place = c("A", "B", "Total"), stringsAsFactors = FALSE)
  two$n <- c("<5", "<5", "6",  "10", "20", "30",  "13", "<5", "36")
  ch <- check_suppression(two)
  expect_equal(ch$status, "fail")
  expect_equal(nrow(ch$details), 3)
  # without totals nothing can be recovered
  expect_equal(check_suppression(two[two$sex != "Total" & two$place != "Total", ])$status, "pass")
  expect_error(check_suppression(data.frame(a = "x")), "count column")
})

test_that("check_group_disclosure warns when a group shares one value", {
  tab <- data.frame(settlement = rep(c("A", "B"), each = 2), smoker = rep(c("yes", "no"), 2),
                    n = c(12, 0, 30, 41))
  ch <- check_group_disclosure(tab)
  expect_equal(ch$status, "warn")
  # everyone in A smokes, and every non-smoker lives in B
  expect_equal(ch$details$group, c("smoker = no", "settlement = A"))
  expect_equal(ch$details$shared, c("settlement", "smoker"))
  # a missing combination is a zero; a suppressed cell may hold persons
  expect_equal(check_group_disclosure(tab[-2, ])$status, "warn")
  tab$n[2] <- NA
  expect_equal(check_group_disclosure(tab)$status, "pass")
  # totals are not a group, and one grouping variable cannot disclose
  expect_equal(check_group_disclosure(data.frame(sex = c("F", "M", "Total"), n = c(0, 5, 5)))$status, "pass")
  # microdata are counted in persons
  df <- data.frame(id = 1:20, place = rep(c("A", "B"), each = 10), smoker = c(rep("yes", 10), rep(c("yes", "no"), 5)))
  expect_true("place = A" %in% check_group_disclosure(df, by = c("place", "smoker"), id = "id")$details$group)
})

test_that("check_all collects results into one report", {
  rep <- check_all(
    check_min_count(fake_persons, by = "sex"),
    list(check_extremes(summary(1:3)), check_keys(example_tables(), "person_id"))
  )
  expect_s3_class(rep, "mimic_report")
  expect_equal(nrow(rep), 3)
  expect_equal(rep$status, c("pass", "fail", "pass"))
  expect_output(print(rep), NA)
  expect_error(check_all(1), "mimic_check")
})

test_that("date_period gives the year or month as format() does", {
  d <- as.Date(c("1959-01-31", NA, "2000-02-29", "1959-01-31", "2024-12-01"))
  expect_identical(date_period(d, "year"), format(d, "%Y"))
  expect_identical(date_period(d, "month"), format(d, "%Y-%m"))
  odd <- as.Date(c("0999-03-01", "2020-05-17", NA))
  expect_identical(date_period(odd, "year"), format(odd, "%Y"))
  expect_identical(date_period(as.Date(character()), "year"), character())
  expect_identical(date_period(as.Date(NA), "month"), NA_character_)
})

test_that("check_dominance takes each group on its own, also one without values", {
  d <- data.frame(g = c("a", "a", "a", "b", "b", "c"), v = c(10, 11, 12, NA, NA, 100), id = 1:6)
  ch <- check_dominance(d, "v", by = "g", id = "id")
  expect_equal(ch$status, "fail")
  expect_equal(ch$details$g, "c")
  expect_equal(ch$details$rule_broken, "(n, k) and p %")
})
