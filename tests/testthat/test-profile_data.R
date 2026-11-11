m <- profile_data(example_tables(), keys = "person_id", config = example_config())

# --- no real values leave profile_data() ------------------------------------------

test_that("no identifier value is stored or printed", {
  s <- strings_in(m)
  out <- printed(m)
  ids <- unique(c(fake_persons$person_id, fake_contacts$person_id))
  expect_false(any(ids %in% s))
  expect_false(any(vapply(ids[1:200], grepl, logical(1), x = out, fixed = TRUE)))
})

test_that("no free text is stored", {
  s <- strings_in(m)
  notes <- unique(stats::na.omit(fake_persons$note))
  expect_false(any(notes %in% s))
  # not even a substring of a note survives
  expect_false(any(vapply(s, function(z) nchar(z) > 15 && any(grepl(z, notes, fixed = TRUE)), logical(1))))
})

test_that("labels of small settlements and rare categories are not stored", {
  s <- strings_in(m)
  persons_per_place <- table(fake_persons$settlement)
  small <- names(persons_per_place)[persons_per_place < 5]
  expect_true(length(small) > 0)
  expect_false(any(small %in% s))

  diag_persons <- tapply(fake_contacts$person_id, fake_contacts$diag_code,
                         function(z) length(unique(z)))
  rare <- names(diag_persons)[diag_persons < 5]
  expect_true(length(rare) > 0)
  expect_false(any(rare %in% s))
  expect_false(any(rare %in% strings_in(printed(m))))
})

test_that("exact minimum and maximum values are not stored", {
  set.seed(1)
  df <- data.frame(
    weight = c(stats::rnorm(300, 70, 12) + stats::runif(300) / 1000, 251.3377),
    visit = as.Date("2019-03-17") + c(sample(1:900, 300), 2000)
  )
  mm <- profile_data(df)
  nums <- numbers_in(mm)
  expect_false(min(df$weight) %in% nums)
  expect_false(max(df$weight) %in% nums)
  expect_false(as.numeric(min(df$visit)) %in% nums)
  expect_false(as.numeric(max(df$visit)) %in% nums)

  # the outlier in the example data is gone, not just rounded
  nums_m <- numbers_in(m)
  expect_false(max(fake_persons$income, na.rm = TRUE) %in% nums_m)
  expect_true(m$tables$persons$variables$income$hi < max(fake_persons$income, na.rm = TRUE))
})

test_that("tail bounds never rest on fewer than min_count persons", {
  set.seed(2)
  df <- data.frame(x = stats::rnorm(40))
  mm <- profile_data(df)
  v <- mm$tables$df$variables$x
  # at least 5 persons at or below lo and at or above hi, also after rounding
  expect_gte(sum(df$x <= v$lo), 5)
  expect_gte(sum(df$x >= v$hi), 5)
  expect_equal(v$lo, signif(v$lo, 2))
})

test_that("dates are coarsened to the configured unit", {
  d <- m$tables$persons$variables$birth_date
  expect_equal(format(d$lo, "%d"), "01")
  expect_equal(format(d$median, "%d"), "01")
  my <- profile_data(example_tables(), keys = "person_id", config = example_config(date_unit = "year"))
  dy <- my$tables$persons$variables$birth_date
  expect_equal(format(dy$lo, "%m-%d"), "01-01")
  expect_equal(format(dy$hi, "%m-%d"), "12-31")
})

# --- protection rules ---------------------------------------------------------

test_that("small categories are merged and the merged group is large enough", {
  v <- m$tables$persons$variables$settlement
  expect_true("Other" %in% v$levels)
  expect_true(v$geo)
  expect_true(all(v$freq >= 5))
})

test_that("merging counts persons, not rows", {
  persons <- data.frame(id = sprintf("P%03d", 1:100), stringsAsFactors = FALSE)
  rows <- data.frame(id = c(rep("P001", 30), rep(persons$id[2:100], each = 2)),
                     type = c(rep("Rare", 30), rep(c("A", "B"), 99)),
                     stringsAsFactors = FALSE)
  mm <- profile_data(list(persons = persons, rows = rows), keys = "id")
  v <- mm$tables$rows$variables$type
  expect_false("Rare" %in% v$levels)
  expect_false("Rare" %in% strings_in(mm))
})

test_that("a merged group that is still too small absorbs the next category", {
  df <- data.frame(g = c(rep("big", 50), rep("mid", 6), rep("tiny", 2)))
  v <- profile_data(df)$tables$df$variables$g
  expect_equal(v$levels, c("big", "Other"))
  expect_false("tiny" %in% strings_in(v))
})

test_that("counts are rounded to round_base", {
  v <- m$tables$persons$variables$occupation
  expect_true(all(v$freq %% 5 == 0))
  expect_true(m$tables$contacts$n_rows %% 5 == 0)
})

test_that("a stricter min_count merges more", {
  m10 <- profile_data(example_tables(), keys = "person_id", config = example_config(min_count = 10))
  expect_gt(m10$tables$persons$variables$settlement$n_merged,
            m$tables$persons$variables$settlement$n_merged)
})

test_that("identifiers keep only their format", {
  v <- m$tables$persons$variables$person_id
  expect_equal(v$type, "id")
  expect_equal(v$formats$format, "P9999999")
  # digits are never kept, even when shared by all values
  df <- data.frame(id = sprintf("AB12%04d", 1:200), stringsAsFactors = FALSE)
  v2 <- profile_data(df, keys = "id")$tables$df$variables$id
  expect_equal(v2$formats$format, "AB999999")
})

test_that("rows per person are top coded", {
  r <- m$tables$contacts$rows_per_person
  true_max <- max(table(fake_contacts$person_id))
  expect_lt(r$hi, true_max)
  expect_false(true_max %in% c(r$dist$from, r$dist$to))
  expect_true(r$dist$from[1] == 0)
  expect_true(m$tables$contacts$share_with_rows < 1)
})

test_that("variables that look like ids or free text are protected automatically", {
  expect_warning(mm <- profile_data(fake_persons), "automatically")
  expect_equal(mm$tables$fake_persons$variables$person_id$type, "id")
  expect_equal(mm$tables$fake_persons$variables$note$type, "text")
})

test_that("typos in the configuration are reported", {
  expect_warning(profile_data(fake_persons, keys = "person_id",
                              config = mimic_config(geo_vars = "setlement", text_vars = "note")),
                 "setlement")
})

# --- risk report --------------------------------------------------------------

test_that("risk report lists changes, rare combinations and size", {
  r <- m$risk
  expect_s3_class(r, "mimic_risk")
  expect_true(all(c("settlement", "income", "note") %in% r$changes$variable))
  expect_equal(r$quasi$status, "fail")
  expect_equal(r$keys$status, "pass")
  expect_false(r$too_small)
  expect_equal(r$max_level, 2L)
})

test_that("small datasets are flagged as too small for synthesis", {
  small <- fake_persons[1:60, ]
  mm <- profile_data(small, keys = "person_id", config = example_config())
  expect_true(mm$risk$too_small)
  expect_match(printed(mm), "too few for safe synthesis")
})

test_that("factor levels that no one holds are not stored", {
  df <- data.frame(person_id = sprintf("P%07d", 1:200),
                   grp = factor(rep(c("a", "b"), 100), levels = c("a", "b", "Jens Hansen, Village 3")))
  mm <- suppressWarnings(profile_data(df, keys = "person_id"))
  spec <- mm$tables[[1]]$variables$grp
  expect_equal(spec$levels, c("a", "b"))
  expect_false("Jens Hansen, Village 3" %in% strings_in(mm))
  expect_match(printed(mm), "1 unused factor level not stored")
})

test_that("bounds of tightly packed values are rounded, never stored exactly", {
  set.seed(2)
  df <- data.frame(person_id = sprintf("P%07d", 1:200), x = stats::runif(200, 1235, 1239))
  spec <- suppressWarnings(profile_data(df, keys = "person_id"))$tables[[1]]$variables$x
  expect_false(spec$lo %in% df$x)
  expect_false(spec$hi %in% df$x)
  expect_equal(c(spec$lo, spec$hi), c(1200, 1300))
  expect_equal(signif_inward(1235.37, 1238.91, 2), c(1200, 1300))
  expect_equal(signif_inward(1234, 5678, 2), c(1300, 5600))
})

test_that("the profile survives saving and loading", {
  f <- tempfile(fileext = ".rds")
  saveRDS(m, f)
  expect_equal(readRDS(f), m)
})

test_that("bad input gives clear errors", {
  expect_error(profile_data(list(fake_persons)), "unique, non-empty names")
  expect_error(profile_data(list(a = fake_persons, b = 1)), "not a data frame")
  expect_error(profile_data(example_tables(), keys = "pid"), "Key column missing")
  expect_error(profile_data(example_tables(), keys = "person_id", main = "nope"), "main")
})

test_that("identifier formats are counted in persons, not rows", {
  ch <- data.frame(person_id = c(rep("X12", 12), sprintf("P%07d", rep(1:50, each = 2))),
                   v = 1)
  mm <- profile_data(ch, keys = "person_id")
  fmts <- mm$tables[[1]]$variables$person_id$formats$format
  expect_false(any(grepl("^A99$", fmts)))
  expect_true(any(nchar(fmts) == 8))
})

test_that("addresses are stored as identifiers, not categories", {
  set.seed(1)
  p <- fake_persons
  p$address <- sprintf("B-%04d, %s", sample(1:400, nrow(p), replace = TRUE), p$settlement)
  p$address[1:300] <- "B-0001, Town_A"
  expect_warning(mm <- profile_data(list(persons = p), keys = "person_id", config = example_config()),
                 "most categories")
  v <- mm$tables$persons$variables$address
  expect_equal(v$type, "id")
  expect_false(any(unique(p$address) %in% strings_in(mm)))
  # codes with some small categories stay categories
  expect_equal(m$tables$contacts$variables$diag_code$type, "categorical")
  expect_equal(m$tables$persons$variables$settlement$type, "categorical")
})

test_that("average_rank is rank() with average ties", {
  set.seed(1)
  for (x in list(sample(5, 200, replace = TRUE), round(rnorm(300), 1), c(3, 3, 3), 7, c(-Inf, 0, Inf, 0),
                 numeric())) {
    expect_identical(average_rank(x), rank(x, ties.method = "average"))
  }
})

test_that("identifier formats are the same with and without letters outside ASCII", {
  id <- c(sprintf("K%04d-x", 1:30), sprintf("Æ%04dø", 1:30))
  spec <- describe_id(id, seq_along(id), mimic_config())
  expect_setequal(spec$formats$format, c("A9999-A", "A9999A"))
})
