# Builds data/mash.rda from the MASH network meta-analysis extraction sheet
# (four outcome tabs). Run from the package root: Rscript data-raw/mash.R
#
# The `design` column is illustrative only: every trial in the sheet is a
# randomised trial; three of them are labelled "PSM" so that the outer-ring
# examples have two groups to show.

read_tab <- function(txt) {
  d <- read.csv(text = txt, sep = "|", strip.white = TRUE, stringsAsFactors = FALSE,
                check.names = FALSE)
  d <- d[, c("study", "treatment", "responders", "sampleSize", "rob", "incrr")]
  d$responders <- as.integer(d$responders)
  d$sampleSize <- as.integer(d$sampleSize)
  d$rob <- as.integer(d$rob)
  d$incrr <- as.integer(d$incrr)
  psm <- c("Phase 3 REVERSE trial (not published)",
           "Rinella et al. (2024) (ALPINE-4)",
           "Harrison et al. et al. (2018) (GS-US-321-0106)")
  d$design <- ifelse(d$study %in% psm, "PSM", "RCT")
  rownames(d) <- NULL
  d
}

fib_improvement <- read_tab("study|treatment|responders|sampleSize|rob|incrr
Alkhouri et al. (2025) (WAYFIND)|Semaglutide 2.4 mg/week + Cilofexor 30 mg/day + Firsocostat 20 mg/day|17|124|1|1
Alkhouri et al. (2025) (WAYFIND)|Semaglutide 2.4 mg/week|19|122|1|1
Alkhouri et al. (2025) (WAYFIND)|Cilofexor 30 mg/day + Firsocostat 20 mg/day|25|123|1|1
Alkhouri et al. (2025) (WAYFIND)|Placebo|7|84|1|1
Noureddin et al. (2025) (SYMMETRY)|Efruxifermin 50 mg/week|18|63|1|1
Noureddin et al. (2025) (SYMMETRY)|Placebo|7|61|1|1
Abdelmalek et al. (2024) (FALCON 2)|Pegbelfermin 40 mg/week|11|39|1|1
Abdelmalek et al. (2024) (FALCON 2)|Placebo|12|39|1|1
Rinella et al. (2024) (ALPINE-4)|Aldafermin 3.0 mg/day|11|55|2|1
Rinella et al. (2024) (ALPINE-4)|Placebo|7|56|2|1
Loomba et al. (2023) (NN9931-4492)|Semaglutide 2.4 mg/week|5|47|1|1
Loomba et al. (2023) (NN9931-4492)|Placebo|7|24|1|1
Phase 3 REVERSE trial (not published)|Obeticholic acid 10 mg/day (first 3 months) titrated to 25 mg/day|37|310|1|1
Phase 3 REVERSE trial (not published)|Placebo|31|313|1|1
Loomba et al. (2021) (ATLAS)|Firsocostat 20 mg/day|3|22|2|1
Loomba et al. (2021) (ATLAS)|Cilofexor 30 mg/day|3|22|2|1
Loomba et al. (2021) (ATLAS)|Firsocostat 20 mg/day + Selonsertib 18 mg/day|10|46|2|1
Loomba et al. (2021) (ATLAS)|Cilofexor 30 mg/day + Selonsertib 18 mg/day|8|46|2|1
Loomba et al. (2021) (ATLAS)|Cilofexor 30 mg/day + Firsocostat 20 mg/day|12|42|2|1
Loomba et al. (2021) (ATLAS)|Placebo|3|22|2|1
Harrison et al. (2020) (STELLAR-4)|Selonsertib 18 mg/day|51|354|1|1
Harrison et al. (2020) (STELLAR-4)|Placebo|22|172|1|1")

mash_resolution <- read_tab("study|treatment|responders|sampleSize|rob|incrr
Alkhouri et al. (2025) (WAYFIND)|Semaglutide 2.4 mg/week + Cilofexor 30 mg/day + Firsocostat 20 mg/day|47|82|1|1
Alkhouri et al. (2025) (WAYFIND)|Semaglutide 2.4 mg/week|40|81|1|1
Alkhouri et al. (2025) (WAYFIND)|Cilofexor 30 mg/day + Firsocostat 20 mg/day|28|88|1|1
Alkhouri et al. (2025) (WAYFIND)|Placebo|11|49|1|1
Noureddin et al. (2025) (SYMMETRY)|Efruxifermin 50 mg/week|22|52|1|1
Noureddin et al. (2025) (SYMMETRY)|Placebo|6|45|1|1
Abdelmalek et al. (2024) (FALCON 2)|Pegbelfermin 40 mg/week|1|39|1|1
Abdelmalek et al. (2024) (FALCON 2)|Placebo|0|39|1|1
Loomba et al. (2023) (NN9931-4492)|Semaglutide 2.4 mg/week|16|47|1|1
Loomba et al. (2023) (NN9931-4492)|Placebo|5|24|1|1
Harrison et al. (2020) (STELLAR-4)|Selonsertib 18 mg/day|8|354|1|1
Harrison et al. (2020) (STELLAR-4)|Placebo|7|172|1|1
Harrison et al. et al. (2018) (GS-US-321-0106)|Simtuzumab 700 mg/week|6|86|2|1
Harrison et al. et al. (2018) (GS-US-321-0106)|Placebo|3|85|2|1")

fib_improvement_alldoses <- read_tab("study|treatment|responders|sampleSize|rob|incrr
Noureddin et al. (2025) (SYMMETRY)|Efruxifermin|30|120|1|1
Noureddin et al. (2025) (SYMMETRY)|Placebo|7|61|1|1
Abdelmalek et al. (2024) (FALCON 2)|Pegbelfermin|31|115|1|1
Abdelmalek et al. (2024) (FALCON 2)|Placebo|12|39|1|1
Rinella et al. (2024) (ALPINE-4)|Aldafermin|22|97|2|1
Rinella et al. (2024) (ALPINE-4)|Placebo|7|56|2|1
Loomba et al. (2023) (NN9931-4492)|Semaglutide|5|47|1|1
Loomba et al. (2023) (NN9931-4492)|Placebo|7|24|1|1
Loomba et al. (2021) (ATLAS)|Firsocostat|3|22|2|1
Loomba et al. (2021) (ATLAS)|Cilofexor|3|22|2|1
Loomba et al. (2021) (ATLAS)|Firsocostat + Selonsertib|10|46|2|1
Loomba et al. (2021) (ATLAS)|Cilofexor + Selonsertib|8|46|2|1
Loomba et al. (2021) (ATLAS)|Cilofexor + Firsocostat|12|42|2|1
Loomba et al. (2021) (ATLAS)|Placebo|3|22|2|1
Harrison et al. (2020) (STELLAR-4)|Selonsertib|96|705|1|1
Harrison et al. (2020) (STELLAR-4)|Placebo|22|172|1|1")

mash_resolution_alldoses <- read_tab("study|treatment|responders|sampleSize|rob|incrr
Noureddin et al. (2025) (SYMMETRY)|Efruxifermin|41|97|1|1
Noureddin et al. (2025) (SYMMETRY)|Placebo|6|45|1|1
Abdelmalek et al. (2024) (FALCON 2)|Pegbelfermin|4|115|1|1
Abdelmalek et al. (2024) (FALCON 2)|Placebo|0|39|1|1
Loomba et al. (2023) (NN9931-4492)|Semaglutide|16|47|1|1
Loomba et al. (2023) (NN9931-4492)|Placebo|5|24|1|1
Harrison et al. (2020) (STELLAR-4)|Selonsertib|21|705|1|1
Harrison et al. (2020) (STELLAR-4)|Placebo|7|172|1|1
Harrison et al. et al. (2018) (GS-US-321-0106)|Simtuzumab|7|173|2|1
Harrison et al. et al. (2018) (GS-US-321-0106)|Placebo|3|85|2|1")

mash <- list(fib_improvement = fib_improvement,
             mash_resolution = mash_resolution,
             fib_improvement_alldoses = fib_improvement_alldoses,
             mash_resolution_alldoses = mash_resolution_alldoses)

dir.create("data", showWarnings = FALSE)
save(mash, file = "data/mash.rda", compress = "bzip2", version = 2)
cat("data/mash.rda written:", paste(names(mash), sapply(mash, nrow), collapse = ", "), "\n")
