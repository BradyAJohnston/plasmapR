#' Create single arrow points df
#'
#' @export
create_arrow_positions <- function(start,
                                   end,
                                   middle,
                                   width,
                                   plasmid_length,
                                   arrowhead_size = 8) {
  length <- end - start

  arrow_width <- plasmid_length / 360 * arrowhead_size

  phlange <- start + length - arrow_width

  if (length < arrow_width) {
    phlange <- start
  }

  phlange_outer <- middle + width
  phlange_inner <- middle - width

  df <- data.frame(
    x = c(start,
          start,
          phlange,
          phlange,
          end,
          phlange,
          phlange,
          start),
    y = c(
      phlange_outer,
      phlange_inner,
      phlange_inner,
      phlange_inner - width,
      middle,
      phlange_outer + width,
      phlange_outer,
      phlange_outer
    )
  )

  df
}
