put <- function(name, df) {
  d <- tempfile("release_")
  dir.create(d)
  utils::write.csv(df, file.path(d, name), row.names = FALSE)
  d
}
verdict <- function(res, file) res$files$verdict[res$files$file == file]

test_that("a later release that can be subtracted from an earlier one is found", {
  reg <- tempfile("register_")
  first <- inspect(put("by_sex.csv", data.frame(sex = c("F", "M"), n = c(985, 1015))), register = reg)
  expect_equal(first$n_releases, 0L)
  expect_equal(verdict(first, "by_sex.csv"), "APPROVED")
  path <- record_release(first, reg)
  expect_true(file.exists(path))

  # the same population again is fine; one without a small settlement is not
  same <- inspect(put("by_sex.csv", data.frame(sex = c("F", "M"), n = c(985, 1015))), register = reg)
  expect_equal(verdict(same, "by_sex.csv"), "APPROVED")
  later <- inspect(put("by_sex.csv", data.frame(sex = c("F", "M"), n = c(983, 1014))), register = reg)
  expect_equal(later$n_releases, 1L)
  expect_equal(verdict(later, "by_sex.csv"), "NEEDS CHANGES")
  expect_match(later$files$reasons, "Subtracting.*release")

  # a finer table is summed up before it is compared
  finer <- data.frame(sex = c("F", "F", "M", "M"), settlement = c("A", "B", "A", "B"),
                      n = c(500, 483, 600, 414))
  res <- inspect(put("by_sex_place.csv", finer), register = reg)
  expect_equal(verdict(res, "by_sex_place.csv"), "NEEDS CHANGES")
})

test_that("only released files are recorded, never files that need changes", {
  reg <- tempfile("register_")
  d <- put("small.csv", data.frame(g = c("a", "b"), n = c(100, 3)))
  utils::write.csv(data.frame(g = c("a", "b"), n = c(100, 30)), file.path(d, "ok.csv"), row.names = FALSE)
  res <- inspect(d)
  expect_error(record_release(res, reg, files = "small.csv"), "must not be released")
  expect_error(record_release(res, reg, files = "nope.csv"), "not in the inspection")
  expect_error(record_release(res, file.path(d, "reg")), "outside the output directory")
  expect_error(record_release(list(), reg), "inspect")
  record_release(res, reg)
  rec <- readRDS(list.files(reg, full.names = TRUE))
  expect_equal(rec$files, "ok.csv")
  expect_named(rec$tables, "ok.csv")
})

test_that("the register only accepts releases", {
  reg <- tempfile("register_")
  dir.create(reg)
  saveRDS(list(a = 1), file.path(reg, "release_bad.rds"))
  expect_error(inspect(put("x.csv", data.frame(g = "a", n = 10)), register = reg), "not a release")
})

test_that("is_inside compares directories, not prefixes", {
  expect_true(is_inside("C:/a/out", "C:/a/out"))
  expect_true(is_inside("c:/a/out/sub", "C:/a/out/"))
  expect_false(is_inside("C:/a/out2", "C:/a/out"))
})

test_that("releases are read oldest first, also within one second", {
  reg <- tempfile("register_")
  dir.create(reg)
  for (id in c("20260101-120000", "20260101-120000-2", "20260101-120000-10", "20251231-235959")) {
    saveRDS(list(id = id, tables = list()), file.path(reg, paste0("release_", id, ".rds")))
  }
  ids <- vapply(read_register(reg), `[[`, "", "id")
  expect_equal(ids, c("20251231-235959", "20260101-120000", "20260101-120000-2", "20260101-120000-10"))
})

test_that("files for review by hand are named when they are not recorded", {
  d <- tempfile("release_")
  dir.create(d)
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(d, "ok.csv"), row.names = FALSE)
  writeLines("x", file.path(d, "notes.txt"))
  res <- inspect(d)
  expect_warning(record_release(res, tempfile("register_")), "notes.txt")
  # the approved files are recorded before the warning, so a warning
  # turned into an error does not stop that
  reg <- tempfile("register_")
  w <- tryCatch(record_release(res, reg), warning = function(w) w)
  expect_s3_class(w, "warning")
  expect_length(list.files(reg, pattern = "^release_.*\\.rds$"), 1)
})
