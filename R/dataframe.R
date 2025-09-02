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
    if (is.null(bp)) {
      bp <- max(c(dat$start, dat$end))
    }
    
    # For origin-spanning features like join(4891..5096,1..751):
    # - We need to offset the coordinate system so this becomes continuous
    # - The offset should be chosen so that the feature becomes [new_start..new_end]
    # - All other features get shifted by the same offset
    
    # Find the origin-spanning feature with the smallest end coordinate
    # This determines our offset
    min_end <- min(dat$end[over_origin])
    offset <- min_end
    
    # Apply offset to all coordinates
    dat$start <- dat$start - offset
    dat$end <- dat$end - offset
    
    # For origin-spanning features, calculate the correct end position
    # Original: join(4891..5096, 1..751) with bp=5096
    # After offset by 751: start=4140, end=0
    # Correct end should be: start + ((5096-4891+1) + 751 - 1) = 4140 + 956 = 5096
    for (i in which(over_origin)) {
      original_start <- dat$start[i] + offset  # Restore original start
      original_end <- dat$end[i] + offset      # Restore original end
      
      # Calculate total feature length: (bp - start + 1) + end
      part1_length <- bp - original_start + 1  # From start to end of plasmid
      part2_length <- original_end              # From beginning to end position
      total_length <- part1_length + part2_length
      
      # Set the new end position
      dat$end[i] <- dat$start[i] + total_length - 1
    }
    
    # Handle any negative coordinates by wrapping them around
    negative_coords <- dat$start < 0 | dat$end < 0
    dat$start[negative_coords & dat$start < 0] <- 
      dat$start[negative_coords & dat$start < 0] + bp
    dat$end[negative_coords & dat$end < 0] <- 
      dat$end[negative_coords & dat$end < 0] + bp
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
