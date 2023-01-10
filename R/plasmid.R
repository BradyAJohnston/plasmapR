print.plasmid <- function(plasmid) {
  name <- plasmid$features$source$name
  cli::cli_h1("Plasmid Details")
  cli::cli_ul()
  cli::cli_li(stringr::str_glue("{plasmid$length} bp"))
  cli::cli_li(stringr::str_glue("{length(plasmid$features)} Features"))
}
