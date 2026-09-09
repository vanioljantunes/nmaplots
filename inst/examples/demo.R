# Demo: renders the README figures into man/figures (and PDF/TIFF into
# inst/examples/out). Run from the package root: Rscript inst/examples/demo.R

suppressPackageStartupMessages({
  library(netmeta)
  library(nmaplots)
})

out <- "inst/examples/out"
fig <- "man/figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
dir.create(fig, showWarnings = FALSE, recursive = TRUE)

# Binary outcome with sample sizes and multi-arm studies (netmeta example)
data(smokingcessation)
p <- pairwise(treat = list(treat1, treat2, treat3),
              event = list(event1, event2, event3),
              n = list(n1, n2, n3),
              data = smokingcessation, sm = "OR")
net <- netmeta(p, reference.group = "A")

labs <- c(A = "No contact", B = "Self-help", C = "Individual counselling",
          D = "Group counselling")

# 1. Default: node area = n, edge width + boxed count = studies, boxed legend
nmaplot(net, labels = labs, outcome = "Smoking cessation at 6 to 12 months",
        file = c(file.path(fig, "default.png"), file.path(out, "default.pdf"),
                 file.path(out, "default.tiff")),
        width = 9, height = 9)

# 2. Outer ring with a subgroup composition (here study design: RCT vs PSM)
design <- data.frame(treatment = rep(net$trts, each = 2),
                     group = rep(c("RCT", "PSM"), 4),
                     value = c(3, 0,  2, 1,  11, 4,  3, 1))
nmaplot(net, labels = labs, ring = design, ring_name = "Study design",
        outcome = "Smoking cessation at 6 to 12 months",
        file = c(file.path(fig, "ring.png"), file.path(out, "ring.pdf")),
        width = 9, height = 9.5)

# 3. Same ring plot in the other two layouts (kept in inst/examples/out)
nmaplot(net, labels = labs, ring = design, ring_name = "Study design",
        outcome = "Smoking cessation at 6 to 12 months", layout = "star",
        file = file.path(out, "ring_star.png"), width = 9, height = 9.5)
nmaplot(net, labels = labs, ring = design, ring_name = "Study design",
        outcome = "Smoking cessation at 6 to 12 months", layout = "circle",
        file = file.path(out, "ring_circle.png"), width = 9, height = 9.5)

cat("Figures written to", fig, "and", out, "\n")
