#' MASH drug trials: arm-level data for four outcomes
#'
#' Arm-level results of randomised trials of drugs for metabolic
#' dysfunction-associated steatohepatitis (MASH) with fibrosis, extracted for
#' a network meta-analysis. One data frame per outcome, in the long format
#' that [meta::pairwise()] takes directly (one row per study arm).
#'
#' @format A named list of four data frames:
#' \describe{
#'   \item{`fib_improvement`}{Fibrosis improvement without worsening of MASH;
#'     highest-dose arms, dose in the treatment name. 9 trials, 15 treatments.}
#'   \item{`mash_resolution`}{MASH resolution without worsening of fibrosis;
#'     highest-dose arms. 7 trials, 8 treatments.}
#'   \item{`fib_improvement_alldoses`}{Fibrosis improvement, all dose arms
#'     pooled per drug. 6 trials, 9 treatments.}
#'   \item{`mash_resolution_alldoses`}{MASH resolution, all dose arms pooled
#'     per drug. 5 trials, 6 treatments.}
#' }
#' Each data frame has the columns:
#' \describe{
#'   \item{`study`}{Study label: first author, year and trial acronym.}
#'   \item{`treatment`}{Treatment arm; `"Placebo"` is the reference.}
#'   \item{`responders`}{Number of participants reaching the outcome.}
#'   \item{`sampleSize`}{Participants in the arm.}
#'   \item{`rob`}{Risk of bias of the study: 1 = low, 2 = some concerns.}
#'   \item{`incrr`}{Inclusion flag from the extraction sheet (all 1).}
#'   \item{`design`}{Study design label used by the ring examples, `"RCT"` or
#'     `"PSM"`. Illustrative only: all trials in this set are randomised;
#'     three are labelled PSM so the outer-ring examples show two groups.}
#' }
#'
#' @source MetaHub MASH network meta-analysis extraction sheet (2026).
#' @examples
#' data(mash)
#' d <- mash$fib_improvement_alldoses
#' table(d$treatment, d$design)
#' if (requireNamespace("netmeta", quietly = TRUE)) {
#'   library(netmeta)
#'   p <- pairwise(treat = treatment, event = responders, n = sampleSize,
#'                 studlab = study, data = d, sm = "RR")
#'   net <- netmeta(p, reference.group = "Placebo")
#'   nmaplot(net, outcome = "Fibrosis improvement without worsening of MASH",
#'           ring = table(d$treatment, d$design), ring_name = "Study design")
#' }
"mash"
