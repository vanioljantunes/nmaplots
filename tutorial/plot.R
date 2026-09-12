# Renders the example network shown in the tutorial image.
# Run from the package root: Rscript tutorial/plot.R

suppressMessages({
  library(netmeta)
  devtools::load_all(quiet = TRUE)
})

d <- read.csv("inst/extdata/recurrence_pairwise.csv", sep = ";",
              fileEncoding = "UTF-8")
d <- nma_clean(d)
net <- netmeta(TE, seTE, treat1, treat2, studlab, data = d, sm = "RR",
               n1 = n1, n2 = n2, event1 = event1, event2 = event2)

nmaplot(net, outcome = "Patient recurrence", layout = "multi",
        file = "man/figures/tutorial-plot.png",
        width = 9, height = 9.5, dpi = 200)
cat("man/figures/tutorial-plot.png written\n")
