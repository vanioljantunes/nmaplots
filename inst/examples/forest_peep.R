# Forest figures for the PEEP network (mortality, primary outcome).
# Data: inst/extdata/peep_mortality.csv, one row per study arm.
# Run from the package root: Rscript inst/examples/forest_peep.R

suppressPackageStartupMessages({
  library(gemtc)
  devtools::load_all(quiet = TRUE)
})

out <- "inst/examples/out/peep"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

d <- read.csv(system.file("extdata", "peep_mortality.csv", package = "nmaplots"),
              stringsAsFactors = FALSE)
if (!nrow(d)) d <- read.csv("inst/extdata/peep_mortality.csv", stringsAsFactors = FALSE)

network <- nma_gemtc(d[, c("study", "treatment", "responders", "sampleSize")])
model <- mtc.model(network, likelihood = "binom", link = "log",
                   linearModel = "random")
set.seed(1)
result <- mtc.run(model, n.adapt = 5000, n.iter = 20000, thin = 2)

outcome <- "Mortality, primary outcome"
ref <- "Conventional_low_PEEP"
favours <- c("Favours the treatment", "Favours conventional low PEEP")

# 1. reference block: the layout gemtc::forest() draws, in the JAMA style
nmaforest(result, comparisons = "reference", reference = ref,
          outcome = outcome, title = "PEEP strategies in ARDS",
          favours = favours,
          file = file.path(out, "1_reference_jama.png"), width = 12)

# 2. same block in the RevMan style, events over totals
nmaforest(result, comparisons = "reference", reference = ref, style = "revman",
          columns = c("studies", "events"),
          outcome = outcome, title = "PEEP strategies in ARDS",
          favours = favours,
          file = file.path(out, "2_reference_revman.png"), width = 12)

# 3. every pairwise comparison, one block per comparator
nmaforest(result, comparisons = "panels", reference = ref,
          outcome = outcome, title = "All pairwise comparisons",
          columns = c("studies", "n"),
          file = file.path(out, "3_panels_jama.png"), width = 12)

# 4. one flat row per unordered pair, sorted by effect
nmaforest(result, comparisons = "pairs", sort = "effect",
          outcome = outcome, title = "All pairwise comparisons",
          columns = "studies",
          file = file.path(out, "4_pairs_jama.png"), width = 13)

# the network itself, for the same set
nmaplot(network, reference = ref, outcome = outcome,
        file = file.path(out, "5_network.png"), width = 11, height = 11)

cat("Figures written to", out, "\n")
