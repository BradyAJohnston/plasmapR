test_that("Transform to Non-Polar", {
  expect_equal(
    {
      fl <- system.file('extdata', 'petm20.gb', package = "plasmapR")

      plasmid <- fl |> read_gb()

      dat <- plasmid |> as.data.frame()

      dat[dat$type == "CDS", ] |>
        plot_plasmid(name = "pETM-20") +
        ggplot2::coord_cartesian()
      TRUE
    },
    TRUE
  )
})
