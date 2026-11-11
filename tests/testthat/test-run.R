skip_on_cran()

cfg <- example_config()
m <- profile_data(list(persons = fake_persons, contacts = fake_contacts), keys = "person_id", config = cfg)
copy <- mimic(m, seed = 1)
clean <- mimic(m, mess = FALSE, seed = 1)
real <- list(persons = fake_persons, contacts = fake_contacts)
new_dir <- function() file.path(tempfile("out_"), "output")

# --- trial_run ----------------------------------------------------------------

test_that("trial_run runs a good script and reports packages and files", {
  fit <- trial_run(c(
    "library(stats)",
    "tab <- table(persons$sex)",
    "n <- utils::head(contacts, 3)",
    "write.csv(as.data.frame(tab), 'sex.csv', row.names = FALSE)"
  ), clean)
  expect_s3_class(fit, "mimic_trial")
  expect_equal(fit$status, "ok")
  expect_setequal(fit$packages$package, c("stats", "utils"))
  expect_equal(fit$files$file, "sex.csv")
  expect_true(file.exists(file.path(fit$output_dir, "sex.csv")))
  expect_equal(nrow(fit$row_level), 0)
})

test_that("trial_run flags row-level output and writes outside the output directory", {
  fit <- trial_run(c(
    "write.csv(persons, 'everyone.csv')",
    "saveRDS(persons[, c('person_id', 'sex')], file.path(tempdir(), 'away.rds'))",
    "keep <- persons$person_id[1:3]",
    "writeLines(keep, 'ids.txt')"
  ), copy)
  expect_equal(fit$status, "warn")
  r <- paste(fit$row_level$what, fit$row_level$reason)
  expect_true(any(grepl("everyone.csv.*rows", r)))
  expect_true(any(grepl("away.rds.*outside", r)))
  expect_true(any(grepl("ids.txt.*identifier", r)))
})

test_that("trial_run reports the failing line", {
  fit <- trial_run(c("x <- 1", "y <- 2", "z <- persons$nope + undefined_thing"), copy)
  expect_equal(fit$status, "error")
  expect_equal(fit$error$line, 3L)
  expect_match(fit$error$message, "undefined_thing")
})

test_that("trial_run runs the output check on what the script writes", {
  fit <- trial_run(c(
    "write.csv(data.frame(group = c('a', 'b'), n = c(2, 40)), 'small.csv', row.names = FALSE)",
    "saveRDS(lm(income ~ sex, data = persons), 'model.rds')",
    "write.csv(data.frame(sex = c('F', 'M'), n = c(985, 1015)), 'fine.csv', row.names = FALSE)"
  ), copy, config = cfg)
  expect_s3_class(fit$inspection, "mimic_inspection")
  v <- setNames(fit$inspection$files$verdict, fit$inspection$files$file)
  expect_equal(unname(v["small.csv"]), "NEEDS CHANGES")
  expect_equal(unname(v["model.rds"]), "NEEDS CHANGES")
  expect_equal(unname(v["fine.csv"]), "APPROVED")
  expect_equal(fit$status, "warn")
  expect_false(fit$inspection$with_real_data)

  out <- cli::cli_fmt(print(fit))
  expect_true(any(grepl("Output check on the copy: 2 files would be sent back", out)))
  expect_true(any(grepl("small.csv", out)))
  expect_false(any(grepl("fine.csv", out[grepl("^small|^model|^fine", out)])))
  expect_true(any(grepl("made-up mess", out)))
})

test_that("trial_run points to the mess when odd codes give small cells", {
  script <- c("tab <- table(persons$sex)",
              "write.csv(as.data.frame(tab), 'sex.csv', row.names = FALSE)")
  messy <- trial_run(script, copy)
  expect_equal(messy$inspection$files$verdict, "NEEDS CHANGES")
  expect_true(any(grepl("made-up mess", cli::cli_fmt(print(messy)))))
  tidy <- trial_run(script, clean)
  expect_equal(tidy$inspection$files$verdict, "APPROVED")
  expect_false(tidy$copy_mess)
})

test_that("trial_run skips the output check when asked or when nothing is written", {
  fit <- trial_run("write.csv(data.frame(group = 'a', n = 2), 'small.csv', row.names = FALSE)",
                   copy, check_output = FALSE)
  expect_null(fit$inspection)
  expect_equal(fit$status, "ok")
  expect_null(trial_run("x <- 1", copy)$inspection)
  expect_error(trial_run("x <- 1", copy, check_output = NA), "check_output")
})

test_that("trial_run prints one line when the output check finds nothing", {
  fit <- trial_run("write.csv(data.frame(sex = c('F', 'M'), n = c(985, 1015)), 'fine.csv', row.names = FALSE)",
                   copy)
  out <- cli::cli_fmt(print(fit))
  expect_true(any(grepl("Output check on the copy: nothing needs changes.", out, fixed = TRUE)))
})

test_that("trial_run notices packages that are not installed", {
  fit <- trial_run("ok <- requireNamespace('notapackage123', quietly = TRUE)", copy)
  expect_equal(fit$status, "warn")
  expect_false(fit$packages$installed[fit$packages$package == "notapackage123"])
})

# --- real_run ---------------------------------------------------------------

test_that("real_run keeps output, cleans the log and leaks no values", {
  out <- new_dir()
  r <- real_run(c(
    "print(head(persons))",
    "print(summary(persons$income))",
    "tab <- as.data.frame(table(sex = persons$sex))",
    "print(tab)",
    "write.csv(tab, 'sex.csv', row.names = FALSE)"
  ), real, out, log = "cleaned")
  expect_s3_class(r, "mimic_run")
  expect_equal(r$status, "ok")
  expect_equal(r$files, "sex.csv")
  expect_true(file.exists(r$log_file))

  log <- readLines(r$log_file)
  ids <- fake_persons$person_id
  toks <- unlist(regmatches(log, gregexpr("[A-Za-z0-9_]+", log)))
  expect_false(any(toks %in% ids))
  expect_false(any(vapply(stats::na.omit(fake_persons$note)[1:300], grepl, logical(1),
                          x = paste(log, collapse = "\n"), fixed = TRUE)))
  expect_false(any(grepl(max(fake_persons$income, na.rm = TRUE), log)))
  expect_gt(r$lines_removed, 0)
  # the aggregated table is still readable
  expect_true(any(grepl("^ *1 +F +[0-9]+", log)))
  # nothing in the returned object either
  expect_false(any(ids %in% strings_in(r)))
})

test_that("the cleaned log finds values written in other ways", {
  r <- real_run(c(
    "inc <- sort(persons$income, decreasing = TRUE)[1:3]",
    "print(format(inc, big.mark = ',', nsmall = 1))",
    "print(format(inc, big.mark = ' '))",
    "print(substr(persons$note[!is.na(persons$note)][1:3], 1, 25))",
    "print(format(persons$birth_date[1:3], '%d.%m.%Y'))",
    "print(format(persons$birth_date[1:3], '%d/%m/%Y'))"
  ), real, new_dir(), log = "cleaned", config = cfg)
  log <- paste(readLines(r$log_file), collapse = "\n")
  inc <- sort(fake_persons$income, decreasing = TRUE)[1:3]
  for (s in c(format(inc, big.mark = ",", nsmall = 1), format(inc, big.mark = " "))) {
    expect_false(grepl(s, log, fixed = TRUE))
  }
  notes <- stats::na.omit(fake_persons$note)[1:3]
  for (s in substr(notes, 1, 25)) expect_false(grepl(s, log, fixed = TRUE))
  for (f in c("%d.%m.%Y", "%d/%m/%Y")) {
    for (s in format(fake_persons$birth_date[1:3], f)) expect_false(grepl(s, log, fixed = TRUE))
  }
})

test_that("rare small numbers in quasi-identifiers are found", {
  cfg_age <- mimic_config(quasi_ids = c("age", "sex"))
  d <- data.frame(age = c(97, rep(40:60, each = 10)), sex = "F")
  sens <- sensitive_values(list(p = d), cfg_age)
  expect_equal(sens$small, 97)
  expect_equal(redact("oldest is 97, most are 50", sens)$lines, "oldest is <value>, most are 50")
  # outside the quasi-identifiers small numbers stay
  expect_length(sensitive_values(list(p = d), mimic_config())$small, 0)
})

test_that("the log keeps only the code by default", {
  out <- new_dir()
  r <- real_run(c("x <- 1", "print(head(persons))", "print(table(persons$sex))"), real, out)
  log <- readLines(r$log_file)
  expect_match(log[1], "code only")
  body <- log[-1]
  expect_true(all(grepl("^[>+]( |$)", body) | body == "<output not kept>"))
  expect_true(any(grepl("print(head(persons))", body, fixed = TRUE)))
  expect_false(any(grepl("Town_A", body)))
  expect_equal(sum(body == "<output not kept>"), 2)
  expect_error(real_run("x <- 1", real, new_dir(), log = "none"), "should be one of")
})

test_that("real_run blocks writes outside the output directory", {
  out <- new_dir()
  away <- file.path(tempdir(), "should_not_exist.rds")
  unlink(away)
  r <- real_run(sprintf("saveRDS(persons, '%s')", gsub("\\\\", "/", away)), real, out)
  expect_false(file.exists(away))
  expect_equal(r$status, "error")
  expect_equal(nrow(r$blocked), 1)
})

test_that("a failing script gives a safe error report", {
  out <- new_dir()
  rare_place <- names(which(table(fake_persons$settlement) < 5))[1]
  script <- c(
    "x <- 1",
    sprintf("who <- persons[persons$settlement == '%s', ]", rare_place),
    "stop('person ', who$person_id[1], ' earns ', who$income[1], ' and wrote: ', who$note[1], ' in ', who$settlement[1])"
  )
  r <- real_run(script, real, out)
  e <- r$error
  expect_equal(r$status, "error")
  expect_equal(e$line, 3L)
  who <- fake_persons[fake_persons$settlement == rare_place, ][1, ]
  expect_false(grepl(who$person_id, e$message, fixed = TRUE))
  expect_false(grepl(format(who$income, scientific = FALSE), e$message, fixed = TRUE))
  if (!is.na(who$note)) expect_false(grepl(who$note, e$message, fixed = TRUE))
  expect_false(grepl("[0-9]", e$message))
  expect_true(all(c("person_id", "income", "settlement", "note") %in% e$variables$variable))
  expect_named(e$variables, c("table", "variable", "class", "missing", "distinct",
                              "empty_strings", "all_missing"))
  expect_false(any(fake_persons$person_id %in% strings_in(r)))
})

test_that("the error report keeps the researcher's own words", {
  r <- real_run("fit <- lm(income ~ settlment, data = persons)", real, new_dir())
  expect_match(r$error$message, "settlment")
  expect_equal(r$error$line, 1L)
  expect_true("income" %in% r$error$variables$variable)
})

test_that("a script path that does not exist is not read as code", {
  expect_error(real_run("C:/nowhere (1)/analysis.R", real, new_dir()), "not found")
})

test_that("real_run refuses unsafe set-ups", {
  out <- new_dir()
  dir.create(out, recursive = TRUE)
  writeLines("old", file.path(out, "old.txt"))
  expect_error(real_run("x <- 1", real, out), "must be empty")
  out2 <- new_dir()
  expect_error(real_run("x <- 1", real, out2, raw_log = file.path(out2, "raw.txt")), "outside")
})

test_that("real_run stops scripts that run too long", {
  r <- real_run("Sys.sleep(20)", real, new_dir(), timeout = 2)
  expect_equal(r$status, "error")
  expect_equal(r$error$class, "timeout")
})

# --- sanitising, without a child session ----------------------------------------

test_that("redact removes rare values and printed rows but keeps common ones", {
  sens <- sensitive_values(list(p = fake_persons), cfg)
  id <- fake_persons$person_id[1]
  lines <- c(paste("person", id), "sex F is common", paste(id, fake_persons$income[1], "F"))
  out <- redact(lines, sens, drop_rows = TRUE)
  expect_equal(out$lines[1], "person <value>")
  expect_equal(out$lines[2], "sex F is common")
  expect_match(out$lines[3], "line removed")
})

test_that("redact finds rare numbers at the precision they are printed with", {
  set.seed(1)
  inc <- round(stats::rlnorm(300, 12, 0.5), 2)
  sens <- sensitive_values(list(p = data.frame(income = inc)), cfg)
  log <- utils::capture.output(print(summary(inc)))
  out <- redact(log, sens)$lines
  # summary() rounds the minimum and maximum, which are single persons' values
  expect_false(grepl(format(signif(min(inc), 4), scientific = FALSE), out[2], fixed = TRUE))
  expect_false(grepl(format(round(max(inc)), scientific = FALSE), out[2], fixed = TRUE))
  expect_equal(out[1], log[1])
  # rounded and exact forms are found; fewer than three significant digits are left alone
  one <- sensitive_values(list(p = data.frame(x = c(38389.6, rep(50, 10)))), cfg)
  expect_equal(rare_number_hits(c("38000", "38390", "38389.6", "50", "x"), one),
               c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(written_digits(c("38390", "72.30", "0.012", "-1.5e+05")), c(4L, 4L, 2L, 2L))
})

test_that("rare values are counted in persons, not rows", {
  ch <- data.frame(person_id = c(rep("P0000001", 12), sprintf("P%07d", 2:200)),
                   diag = c(rep("RARE_X99", 12), rep("COMMON", 199)),
                   cost = c(rep(123456.7, 12), rep(50, 199)))
  sens <- sensitive_values(list(contacts = ch), cfg)
  expect_true("RARE_X99" %in% sens$strings)
  expect_false("COMMON" %in% sens$strings)
  expect_true(123456.7 %in% sens$numbers)
  expect_equal(redact("diag RARE_X99 cost 123456.7", sens)$lines, "diag <value> cost <value>")
})

test_that("sanitize_message removes quoted values and numbers", {
  sens <- sensitive_values(list(p = fake_persons), cfg)
  msg <- sanitize_message("object 'abc' and 'income' at row 1234: value \"P1234567\"", sens,
                          known = c("income"))
  expect_equal(msg, "object '<value>' and 'income' at row <n>: value \"<value>\"")
})

test_that("file names the script makes are cleaned in the result", {
  r <- real_run(c(
    "for (id in persons$person_id[1:2]) writeLines('x', paste0(id, '.txt'))",
    "saveRDS(1, paste0(tempdir(), '/', persons$person_id[3], '.rds'))"
  ), real, new_dir(), config = cfg)
  ids <- fake_persons$person_id[1:3]
  shown <- paste(c(r$files, r$blocked$path, printed(r)), collapse = "\n")
  for (id in ids) expect_false(grepl(id, shown, fixed = TRUE))
  expect_length(r$files, 2)
  expect_equal(nrow(r$blocked), 1)
})

test_that("names and addresses count as identifiers in the log", {
  set.seed(1)
  p <- data.frame(person_id = sprintf("P%04d", 1:500),
                  address = sprintf("B-%03d", sample(1:200, 500, replace = TRUE)))
  p$address[1:100] <- "B-999"
  sens <- sensitive_values(list(p = p), cfg)
  expect_true("B-999" %in% sens$strings)
})

test_that("short values of an address-like column stay out of the deny list", {
  set.seed(1)
  h <- data.frame(person_id = sprintf("P%04d", 1:2000),
                  household = as.character(sample(1:1500, 2000, replace = TRUE)))
  h$household[1:400] <- "1"
  h$household[401:800] <- "2"
  sens <- sensitive_values(list(h = h), cfg)
  # common and rare alike
  expect_false(any(nchar(sens$strings) < 3))
  code <- c("> y <- f(2, 1)", "> z <- f(3, 4)", "x[7]", "table_3.csv", "2020-01-15")
  expect_equal(redact(code, sens, drop_rows = TRUE)$lines, code)
  # longer values are still identifiers: most are rare, so all are
  long <- unique(h$household[nchar(h$household) >= 3])
  expect_true(all(long %in% sens$strings))
})

test_that("a rare short value of a quasi-identifier stays on the deny list", {
  p <- data.frame(person_id = sprintf("P%04d", 1:100),
                  settlement = c(rep("Town_A", 99), "7"))
  sens <- sensitive_values(list(p = p), cfg)
  expect_true("7" %in% sens$strings)
})

test_that("ids inside compound file names are found", {
  p <- fake_persons
  # a numeric key; named lbnr it would need id_vars, see real_run()
  p$pnr <- seq_len(nrow(p)) + 10000000
  sens <- sensitive_values(list(persons = p), cfg)
  id <- p$person_id[1]
  for (f in c(paste0(id, ".csv"), paste0("result_", id, ".csv"), paste0(id, "_tab.csv"),
              paste0(id, "-tab.csv"), "10000001.csv", "res_10000001.rds")) {
    expect_gt(redact(f, sens)$n, 0)
  }
  expect_equal(redact(c("by_sex.csv", "table_2.csv", "model_2019.rds"), sens)$n, 0L)
})

test_that("long numbers are counted on their exact value", {
  # a numeric key the package does not recognise: still one person per value
  p <- fake_persons
  p$lbnr <- seq_len(nrow(p)) + 10000000
  sens <- sensitive_values(list(persons = p), cfg)
  expect_true(all(c(10000001, 10000009) %in% sens$numbers))
  expect_gt(redact("res_10000001.rds", sens)$n, 0)
})

test_that("the planned search of redact finds what a search of every line finds", {
  sens <- sensitive_values(list(p = fake_persons), cfg)
  expect_gt(length(sens$multi_index$groups), 0)
  # the same deny list, with every key searched for in every line
  everywhere <- function(n) list(n = n, all = rep(TRUE, n), groups = list())
  slow <- sens
  slow$multi_index <- everywhere(length(sens$multi))
  slow$prefix_index <- everywhere(length(sens$prefixes))

  notes <- unique(fake_persons$note[!is.na(fake_persons$note)])
  long <- notes[nchar(notes) >= 30][1:5]
  lines <- c(
    paste("note:", notes[1:40]),
    paste0(notes[41], " and ", notes[42], " and ", notes[41]),
    paste("cut short:", substr(long, 1, 22)),
    paste0("x", notes[50], "x"),
    notes[51:60],
    "", "ab", "> tab <- table(persons$sex)", "nothing to find here at all",
    paste("<value>", notes[61]), paste(fake_persons$person_id[1:5], collapse = " "),
    "blåbærgrød på en ø", paste("æ", notes[62], "ø")
  )
  fast <- redact(lines, sens, drop_rows = TRUE)
  expect_identical(fast, redact(lines, slow, drop_rows = TRUE))
  expect_gt(fast$n, 50)
  expect_identical(redact(lines, sens), redact(lines, slow))
  # one line at a time: file names and single cells
  for (l in lines[c(1, 42, 44, 50, 60)]) expect_identical(redact(l, sens), redact(l, slow))

  # a key is searched for in every line that holds it
  plan <- search_plan(sens$multi_index, lines)
  for (j in seq_along(sens$multi)[1:300]) {
    holds <- which(grepl(sens$multi[j], lines, fixed = TRUE))
    if (!plan$all[j]) expect_true(all(holds %in% plan$lines[[j]]))
  }
})

test_that("a deny list without the search index is searched all the same", {
  sens <- sensitive_values(list(p = fake_persons), cfg)
  bare <- sens[setdiff(names(sens), c("multi", "single", "multi_index", "prefix_index"))]
  lines <- c(paste("person", fake_persons$person_id[1]), paste("note:", fake_persons$note[!is.na(fake_persons$note)][1]))
  expect_identical(redact(lines, bare), redact(lines, sens))
})

test_that("keys that could match across an inserted marker are searched for everywhere", {
  keys <- c(sprintf("free text number %02d", 1:30), "a <b> c", "value", "alu")
  idx <- search_index(keys)
  expect_true(all(idx$all[31:33]))
  expect_false(any(idx$all[1:30]))
  expect_true(all(search_index(keys[1:5])$all))
})

test_that("rare dates are written as format() writes them", {
  d <- as.Date(c("1959-01-31", "2000-02-29", "2024-12-01", "1001-07-04"))
  slow <- c(format(d), unlist(lapply(date_formats, function(f) format(d, f))))
  expect_identical(written_dates(d), slow)
  expect_identical(written_dates(as.Date(character())), character())
  # years that format() pads differently between platforms are left to it
  odd <- as.Date(c("0999-01-01", "2020-05-17"))
  expect_identical(written_dates(odd), c(format(odd), unlist(lapply(date_formats, function(f) format(odd, f)))))
})
