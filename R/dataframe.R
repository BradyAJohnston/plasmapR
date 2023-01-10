.feature_list_to_df <- function(x) {
  dat <- data.frame(
    index = numeric(),
    name = character(),
    type = character(),
    start = numeric(),
    end = numeric(),
    # note = character(),
    direction = numeric()
  )

  i <- 1
  for (feat in x) {
    if (stringr::str_detect(feat$type, "(CDS)")) next

    dat[i, ] <- c(
      i,
      feat$name, # name
      feat$type, # type
      feat$start_end[1],
      feat$start_end[2],
      feat$direction

    )
    i <- i + 1
  }

  dat$start <- as.numeric(dat$start)
  dat$end <- as.numeric(dat$end)
  dat$direction <- as.numeric(dat$direction)

  dat[!is.na(dat$start), ]
}


#' Extract Features of a Plasmid as a DataFrame
#'
#' @param plasmid Plasmid
#'
#' @return a DataFrame
#' @export
as.data.frame.plasmid <- function(plasmid) {
  .feature_list_to_df(plasmid$features)
}
