# Helpers that search an object for leaked values.

# Every character string anywhere in x, including names and attributes.
strings_in <- function(x) {
  out <- character()
  walk <- function(y) {
    if (is.null(y)) return()
    if (is.list(y)) {
      out <<- c(out, names(y))
      for (el in unclass(y)) walk(el)
    } else if (is.character(y) || is.factor(y)) {
      out <<- c(out, as.character(y), names(y))
    } else {
      out <<- c(out, names(y))
    }
    a <- attributes(y)
    a$names <- NULL
    a$class <- NULL
    a$row.names <- NULL
    for (el in a) walk(el)
  }
  walk(x)
  unique(out[!is.na(out)])
}

# Every number anywhere in x; dates as days since 1970-01-01.
numbers_in <- function(x) {
  out <- numeric()
  walk <- function(y) {
    if (is.null(y)) return()
    if (is.list(y)) {
      for (el in unclass(y)) walk(el)
    } else if (is.numeric(y) || inherits(y, "Date")) {
      out <<- c(out, as.numeric(unclass(y)))
    }
  }
  walk(x)
  unique(out[!is.na(out)])
}

# Everything a user would see when printing x.
printed <- function(x) {
  paste(c(utils::capture.output(print(x), type = "output"),
          utils::capture.output(print(x), type = "message")),
        collapse = "\n")
}

example_tables <- function() {
  list(persons = fake_persons, contacts = fake_contacts, admissions = fake_admissions)
}

example_config <- function(...) {
  mimic_config(quasi_ids = c("birth_date", "sex", "settlement"),
               geo_vars = "settlement", text_vars = "note", ...)
}
