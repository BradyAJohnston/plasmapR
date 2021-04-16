plasmapR
================
Brady Johnston

## About

Extension on the `{ggplot2}` graphics library for generating quick
plasmid maps from FASTA / genbank files.

The result of the `render_plasmid()` function is just a `ggplot` object,
so you can add themes and treat it as you would any other `ggplot`
object.

``` r
#devtools::install_github("bradyajohnston/plasmapR")
library(plasmapR)

plasmid <- parse_plasmid("data/petm20.gb")

p <- render_plasmap(plasmid,
                    rotation = 45,
                    plasmid_name = "pETM20-avi-dsnPPR10-C2")

p + ggplot2::scale_fill_brewer(palette = 8, type = "qual")
```

<img src="README_files/figure-gfm/example-1.png" style="display: block; margin: auto;" />
