test_that("testing rectangle generation", {
  expect_snapshot_value(
    .points_rectangle(0, 5, 4, 0.5),
    style = 'deparse'
  )
  expect_snapshot_value(
    .points_arrow(
      base = 0,
      tip = 5,
      midpoint = 4,
      phlange = 0.3,
      phlange_width = 0.5,
      width = 0.15
      ),
    style = 'deparse'
  )
})
