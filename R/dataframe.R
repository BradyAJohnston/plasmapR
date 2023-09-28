.feature_list_to_df <- function(x, bp = NULL, ...) {

  feats <- lapply(seq(length(x)), \(i) {
    feat <- x[[i]]

    data.frame(
      index = i,
      name = feat$name,
      type = feat$type,
      start = feat$start_end[1],
      end = feat$start_end[2],
      direction = as.numeric(feat$direction)
    )
  })

  dat <- do.call(rbind, feats)

  # turn certain features in to numeric columns
  dat$start <- as.numeric(dat$start)
  dat$end <- as.numeric(dat$end)
  dat$direction <- as.numeric(dat$direction)

  over_origin <- dat$start > dat$end & dat$direction == 1

  if (any(over_origin)) {
    offset <- dat$end[over_origin] + 1

    if (is.null(bp)) {
      bp <- max(c(dat$start, dat$end))
    }

    dat$start <- dat$start - offset
    dat$end <- dat$end - offset

    dat$end[over_origin] <- bp

  }

# only return features where a start was successfully parsed
  # dat[!is.na(dat$start), ]
  dat
}

#' Extract Features of a Plasmid as a DataFrame
#'
#' @param x A list of class 'plasmid' from `read_gb()`
#' @param row.names Ignored.
#' @param optional Ignored.
#' @param ... Ignored.
#'
#' @return a DataFrame
#' @rdname as.data.frame.plasmid
#' @export
as.data.frame.plasmid <- function(x, row.names, optional, ...) {
  .feature_list_to_df(x$features, bp = x$length)
}
