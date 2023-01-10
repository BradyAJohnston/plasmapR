test_that("get features", {
  testthat::expect_equal(
    .get_label('                      /regulatory_class="terminator"'),
    "regulatory_class"
  )
  testthat::expect_equal(
    .get_value('                      /regulatory_class="terminator"'),
    "terminator"
  )
  expect_equal(
    .get_label('                     /note="efficient rho-independent terminator B1006"'),
    "note"
  )
  expect_equal(
    .get_value('                     /note="efficient rho-independent terminator B1006"'),
    'efficient rho-independent terminator B1006'
  )

  expect_equal(
    .get_start_end("      protein_bind    82..86"),
    c(82, 86)
  )

  expect_equal(
    .get_start_end("      primer_bind     complement(170..186)"),
    c(170, 186)
  )
  expect_equal(
    .get_feature_type("      primer_bind     complement(170..186)"),
    "primer_bind"
  )

  expect_equal(
    .get_direction("      primer_bind     complement(170..186)"),
    -1
  )
  expect_equal(
    .get_direction("      protein_bind    82..86"),
    1
  )
})

test_that('expections', {
  expect_true(
    .is_label_start('                     /regulatory_class="ribosome_binding_site"')
  )
  expect_false(
    .is_feature_start('                     /regulatory_class="ribosome_binding_site"')
  )
  expect_true(
    .is_feature_start('     regulatory      70..81')
  )
  expect_false(
    .is_label_start('     regulatory      70..81')
  )
})

test_that("Extract Lines", {
  lines <- readLines("../../inst/extdata/sequence.gb")
  expect_equal(
    sum(.get_line_types(lines) == "FEATURES"),
    59
  )
})
