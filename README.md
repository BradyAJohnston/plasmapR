plasmapR
================
Brady Johnston

## About

Extension on the {ggplot2} graphics library for generating quick plasmid
maps from FASTA / genbank files.

Example below:

``` r
#devtools::install_github("bradyajohnston/plasmapR")
library(plasmapR)

plasmid <- parse_plasmid("data/petm20.gb")

p <-
  render_plasmap(
    plasmid,
    rotation = 45,
    repelBox = 0.2,
    plasmid_name = "pETM20-dsnPPR10",
    nameSize = 6, 
    labelNudge = 1, 
    curve = 10
  )

p
```

<img src="README_files/figure-gfm/example-1.png" style="display: block; margin: auto;" />
