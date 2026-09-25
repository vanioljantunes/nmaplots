# Demo: renders the README figures into man/figures (and PDF/TIFF into
# inst/examples/out). Run from the package root: Rscript inst/examples/demo.R

# load_all() uses the source in this repository, so the figures always match
# the current code (library(nmaplots) would draw with the installed version)
suppressPackageStartupMessages({
  library(netmeta)
  devtools::load_all(quiet = TRUE)
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

# 4. Forest of the relative effects, fitted with gemtc. Needs JAGS, so it is
#    skipped when gemtc or rjags is missing.
if (requireNamespace("gemtc", quietly = TRUE) &&
    requireNamespace("rjags", quietly = TRUE)) {
  network <- nma_gemtc(d[, c("study", "treatment", "responders", "sampleSize")])
  model <- gemtc::mtc.model(network, likelihood = "binom", link = "log",
                            linearModel = "random")
  set.seed(1)
  result <- gemtc::mtc.run(model, n.adapt = 1000, n.iter = 10000, thin = 2)

  nmaforest(result, comparisons = "reference", reference = "Placebo",
            outcome = outcome, title = "Relative effects",
            favours = c("Favours placebo", "Favours treatment"),
            file = file.path(fig, "forest.png"), width = 10)

  nmaforest(result, comparisons = "reference", reference = "Placebo",
            style = "revman", columns = c("studies", "events"),
            outcome = outcome, title = "Relative effects",
            favours = c("Favours placebo", "Favours treatment"),
            file = file.path(out, "forest_revman.png"), width = 10)

  # every pairwise comparison, one block per comparator
  nmaforest(result, comparisons = "panels", reference = "Placebo",
            outcome = outcome, title = "All pairwise comparisons",
            file = file.path(out, "forest_panels.png"), width = 10)
} else {
  cat("gemtc or rjags missing: the forest figures were skipped\n")
}

cat("Figures written to", fig, "and", out, "\n")
