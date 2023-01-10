.arrow_points <- function(base,
                          tip,
                          midpoint,
                          phlange,
                          phlange_width,
                          width) {
  phlange_outer <- midpoint + width
  phlange_inner <- midpoint - width
  phlange_inner_tip <- phlange_inner - width * phlange_width
  phlange_outer_tip <- phlange_outer + width * phlange_width

  data.frame(
    x = c(base,
          base,
          phlange,
          phlange,
          tip,
          phlange,
          phlange,
          base),
    y = c(
      phlange_outer,
      phlange_inner,
      phlange_inner,
      phlange_inner_tip,
      midpoint,
      phlange_outer_tip,
      phlange_outer,
      phlange_outer
    )
  )

}
