test_that("defaults are strict and complete", {
  cfg <- mimic_config()
  expect_s3_class(cfg, "mimic_config")
  expect_equal(cfg$min_count, 5)
  expect_equal(cfg$dominance, list(n = 2, k = 0.85))
  expect_equal(cfg$date_unit, "month")
  expect_equal(cfg$tail_coding, c(0.01, 0.99))
  expect_equal(cfg$other_label, "Other")
})

test_that("settings can be tightened", {
  cfg <- mimic_config(min_count = 10, date_unit = "year",
                      quasi_ids = c("sex", "settlement", "sex"),
                      other_label = "Merged")
  expect_equal(cfg$min_count, 10)
  expect_equal(cfg$date_unit, "year")
  expect_equal(cfg$quasi_ids, c("sex", "settlement"))
  expect_equal(cfg$other_label, "Merged")
})

test_that("unsafe or malformed settings are rejected with clear messages", {
  expect_error(mimic_config(min_count = 1), "min_count.*at least 3")
  expect_error(mimic_config(min_count = NA), "min_count")
  expect_error(mimic_config(tail_coding = c(0.99, 0.01)), "tail_coding")
  expect_error(mimic_config(dominance = list(n = 2)), "dominance")
  expect_error(mimic_config(dominance = list(n = 2, k = 1.5)), "dominance\\$k")
  expect_error(mimic_config(round_base = 10, min_count = 5), "round_base")
  expect_error(mimic_config(quasi_ids = 1:3), "quasi_ids")
  expect_error(mimic_config(other_label = ""), "other_label")
  expect_error(mimic_config(date_unit = "day"))
})

test_that("as_config accepts NULL and rejects other objects", {
  expect_s3_class(as_config(NULL), "mimic_config")
  expect_error(as_config(list(min_count = 5)), "mimic_config")
})

test_that("print shows the settings", {
  expect_snapshot(print(mimic_config(geo_vars = "settlement")))
})
