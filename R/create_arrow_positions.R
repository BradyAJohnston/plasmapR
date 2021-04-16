#' Create Arrow Points
#'
#' Calculates arrow vertices based on parameters for width, size of arrowhead
#' and the overall plasmid size.
#'
#' @param start Start position in bp.
#' @param end End position in bp.
#' @param middle Middle of the plasmid arrows (y position).
#' @param width Width of the plasmid arrows.
#' @param plasmid_length Total length of the plasmid in bp.
#' @param arrowhead_size Size (in degrees) of the arrowheads. Sets how much of
#'   the circle they will take up (total of circle being 360 degrees).
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
    x = c(
      start,
      start,
      phlange,
      phlange,
      end,
      phlange,
      phlange,
      start
    ),
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
