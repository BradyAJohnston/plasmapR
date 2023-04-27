test_that("Plot a data.frame object", {
  expect_equal({
    fl <- system.file('extdata', 'petm20.gb', package = "plasmapR")

    fl |>
      read_gb() |>
      as.data.frame() |>
      plot_plasmid(name = "pETM-20")

    TRUE
  }, TRUE)
  expect_snapshot({
    fl <- system.file('extdata', 'petm20.gb', package = "plasmapR")

    plasmid <- fl |>
      read_gb()

    dat <- plasmid |>
      as.data.frame()

    dat[dat$type == "CDS", ]
  })
})
