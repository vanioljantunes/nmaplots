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

data(mash)
d <- mash$fib_improvement_alldoses
p <- pairwise(treat = treatment, event = responders, n = sampleSize,
              studlab = study, data = d, sm = "RR")
net <- netmeta(p, reference.group = "Placebo")
outcome <- "Fibrosis improvement without worsening of MASH"

# 1. README figure: binary network, so the event-rate ring is drawn by
#    default (events over participants per treatment)
nmaplot(net, outcome = outcome,
        file = c(file.path(fig, "default.png"), file.path(out, "default.pdf"),
                 file.path(out, "default.tiff")),
        width = 10, height = 10.5)

# 2. Same network in the other two layouts (kept in inst/examples/out)
for (lay in c("circle", "star")) {
  nmaplot(net, outcome = outcome, layout = lay,
          file = file.path(out, paste0("default_", lay, ".png")),
          width = 10, height = 10.5)
}

# 3. Subgroup ring from the design column (RCT vs PSM, illustrative);
#    not shown in the README
nmaplot(net, ring = table(d$treatment, d$design), ring_name = "Study design",
        outcome = outcome,
        file = c(file.path(out, "ring.png"), file.path(out, "ring.pdf")),
        width = 10, height = 10.5)

cat("Figures written to", fig, "and", out, "\n")
