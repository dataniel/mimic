test_that("example data are linked and shaped like a small population", {
  expect_equal(nrow(fake_persons), 2000)
  expect_false(anyDuplicated(fake_persons$person_id) > 0)
  expect_true(all(grepl("^P[0-9]{7}$", fake_persons$person_id)))

  # keys link
  expect_true(all(fake_contacts$person_id %in% fake_persons$person_id))
  expect_true(all(fake_admissions$person_id %in% fake_persons$person_id))

  # some persons are missing from the linked tables
  expect_true(any(!fake_persons$person_id %in% fake_contacts$person_id))
  expect_true(any(!fake_persons$person_id %in% fake_admissions$person_id))

  # tiny settlements exist, so merging is exercised
  sizes <- table(fake_persons$settlement)
  expect_true(any(sizes < 5))

  # rare categories and missing values exist
  expect_true(any(table(fake_contacts$contact_type) < 0.02 * nrow(fake_contacts)))
  expect_true(anyNA(fake_persons$income))
  expect_true(anyNA(fake_persons$note))
})
