# Plot a data.frame object

    Code
      fl <- system.file("extdata", "petm20.gb", package = "plasmapR")
      plasmid <- read_gb(fl)
      dat <- as.data.frame(plasmid)
      dat[dat$type == "CDS", ]
    Output
         index        name type start  end direction
      4      4        AmpR  CDS   599 1459         1
      7      7         rop  CDS  2648 2839        -1
      8      8        lacI  CDS  3648 4730         1
      12    12        TrxA  CDS  5209 5535         1
      13    13       6xHis  CDS  5557 5574         1
      15    15    TEV Site  CDS  5584 5604         1
      16    16  AviTag(TM)  CDS  5611 5655         1
      18    18 dsnPPR10-C2  CDS  5659 7503         1
      19    19       6xHis  CDS  7544 7561         1

