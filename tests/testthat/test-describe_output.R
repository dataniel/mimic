new_out <- function() {
  d <- file.path(tempfile("desc_"), "out")
  dir.create(d, recursive = TRUE)
  d
}
verdict <- function(res, f) res$files$verdict[res$files$file == f]

test_that("describe_output records a description and replaces it when run again", {
  out <- new_out()
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(out, "sex.csv"), row.names = FALSE)
  p <- describe_output("sex.csv", "Persons by sex.", count = "n", by = "sex", dir = out)
  expect_equal(basename(p), "output_descriptions.dcf")
  describe_output("sex.csv", "Persons by sex, 2024.\nAll ages.", count = "n", dir = out,
                  changes = "Year added")
  d <- read_descriptions(out)
  expect_named(d, "sex.csv")
  expect_equal(d$sex.csv$note, "Persons by sex, 2024. All ages.")
  expect_null(d$sex.csv$by)
  expect_equal(d$sex.csv$changes, "Year added")
})

test_that("describe_output checks the file and the columns", {
  out <- new_out()
  expect_error(describe_output("nope.csv", "x", dir = out), "Write the file first")
  utils::write.csv(data.frame(sex = "F", n = 10), file.path(out, "sex.csv"), row.names = FALSE)
  expect_error(describe_output("sex.csv", "x", count = "antal", dir = out), "antal")
  expect_error(describe_output("sex.csv", "", dir = out), "note")
  expect_error(describe_output("sex.csv", "x", data = "gone.csv", dir = out), "gone.csv")
  expect_error(describe_output("output_descriptions.dcf", "x", dir = out), "description file")
})

test_that("inspect() uses the count column named in the description", {
  out <- new_out()
  # 'persons' is not a name inspect() would guess as a count, and next to a
  # mean the table reads as magnitudes without counts
  tab <- data.frame(settlement = c("A", "B", "C"), persons = c(40, 3, 60),
                    mean_income = c(250000.5, 310000.2, 270000.8))
  utils::write.csv(tab, file.path(out, "places.csv"), row.names = FALSE)
  res <- inspect(out)
  expect_equal(verdict(res, "places.csv"), "REVIEW BY HAND")
  expect_match(res$files$reasons, "without a count column")

  describe_output("places.csv", "Persons per settlement.", count = "persons", by = "settlement", dir = out)
  res <- inspect(out)
  expect_equal(verdict(res, "places.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "places.csv"], "fewer than 5 persons")
  expect_equal(res$files$description[res$files$file == "places.csv"], "Persons per settlement.")
})

test_that("inspect() sends back a table whose described columns are gone", {
  out <- new_out()
  utils::write.csv(data.frame(sex = "F", n = 10), file.path(out, "sex.csv"), row.names = FALSE)
  describe_output("sex.csv", "Persons by sex.", count = "n", dir = out)
  utils::write.csv(data.frame(sex = "F", count = 10), file.path(out, "sex.csv"), row.names = FALSE)
  res <- inspect(out)
  expect_match(res$files$reasons[res$files$file == "sex.csv"], "names column\\(s\\) n")
})

test_that("a figure takes the verdict of the numbers behind it", {
  skip_if_not(capabilities("png"))
  out <- new_out()
  grDevices::png(file.path(out, "fig.png"))
  graphics::plot(1:3)
  grDevices::dev.off()
  res <- inspect(out)
  expect_match(res$files$reasons[res$files$file == "fig.png"], "describe_output\\(data = \\)")

  describe_output("fig.png", "Persons by settlement.",
                  data = data.frame(settlement = c("A", "B"), n = c(40, 2)), dir = out)
  expect_true(file.exists(file.path(out, "fig_data.csv")))
  res <- inspect(out)
  expect_equal(verdict(res, "fig_data.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "fig.png"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "fig.png"], "need changes, so the figure does too")

  # good numbers: the figure is still a person's call
  describe_output("fig.png", "Persons by settlement.",
                  data = data.frame(settlement = c("A", "B"), n = c(40, 20)), dir = out)
  res <- inspect(out)
  expect_equal(verdict(res, "fig_data.csv"), "APPROVED")
  expect_equal(verdict(res, "fig.png"), "REVIEW BY HAND")
})

test_that("require_description sends back files without a description", {
  out <- new_out()
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(out, "a.csv"), row.names = FALSE)
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(out, "b.csv"), row.names = FALSE)
  describe_output("a.csv", "Persons by sex.", dir = out)
  cfg <- mimic_config(require_description = TRUE)
  res <- inspect(out, config = cfg)
  expect_equal(verdict(res, "a.csv"), "APPROVED")
  expect_equal(verdict(res, "b.csv"), "NEEDS CHANGES")
  expect_match(res$files$reasons[res$files$file == "b.csv"], "No description")
  # the description file needs no description of its own
  expect_false(grepl("No description", res$files$reasons[res$files$file == "output_descriptions.dcf"]))
  expect_equal(verdict(inspect(out), "b.csv"), "APPROVED")
})

test_that("the description file is checked as text and notes files that are gone", {
  out <- new_out()
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(out, "a.csv"), row.names = FALSE)
  describe_output("a.csv", "Persons by sex.", dir = out)
  unlink(file.path(out, "a.csv"))
  res <- inspect(out)
  expect_equal(res$files$type, "description")
  expect_match(res$files$reasons, "Describes a.csv, which is not in the output")

  # with the real data it is searched for values of persons
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(out, "a.csv"), row.names = FALSE)
  id <- fake_persons$person_id[1]
  describe_output("a.csv", paste("Leaves out person", id), dir = out)
  res <- inspect(out, real_data = list(persons = fake_persons))
  expect_equal(verdict(res, "output_descriptions.dcf"), "NEEDS CHANGES")
})

test_that("write_report shows the descriptions", {
  out <- new_out()
  utils::write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)), file.path(out, "a.csv"), row.names = FALSE)
  describe_output("a.csv", "Persons by sex.", count = "n", by = "sex", changes = "New file", dir = out)
  rep <- write_report(inspect(out), tempfile(fileext = ".md"))
  txt <- readLines(rep)
  expect_true(any(grepl("## What the files show", txt, fixed = TRUE)))
  expect_true(any(grepl("`a.csv`: Persons by sex. (count: `n`; grouped by: `sex`)", txt, fixed = TRUE)))
  expect_true(any(grepl("Changes since last request: New file", txt, fixed = TRUE)))
})

test_that("a coefficient table from safe_model() is approved, a thin one is not", {
  out <- new_out()
  utils::write.csv(safe_model(lm(income ~ sex, data = fake_persons)), file.path(out, "coef.csv"), row.names = FALSE)
  thin <- data.frame(term = "x", estimate = 1, std_error = 1, df_residual = 4, observations = 10)
  utils::write.csv(thin, file.path(out, "thin.csv"), row.names = FALSE)
  plain <- data.frame(term = "x", estimate = 1.5, std_error = 0.2)
  utils::write.csv(plain, file.path(out, "plain.csv"), row.names = FALSE)
  res <- inspect(out)
  expect_equal(verdict(res, "coef.csv"), "APPROVED")
  expect_equal(verdict(res, "thin.csv"), "NEEDS CHANGES")
  expect_equal(verdict(res, "plain.csv"), "REVIEW BY HAND")
})
