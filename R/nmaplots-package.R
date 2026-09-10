#' nmaplots: Publication-quality network plots for netmeta objects
#'
#' Draws the evidence network of a network meta-analysis fitted with
#' \pkg{netmeta}. One function, [nmaplot()], takes the same object as
#' [netmeta::netgraph()] and renders it with \pkg{ggplot2}: node area by
#' sample size, edge width and a circled count by number of studies, the
#' treatment name and `n = ...` at every node, an optional outer ring per
#' node showing a subgroup composition (study design, risk of bias, region,
#' anything you can count per treatment), a boxed legend under the network,
#' and export to PNG, PDF or TIFF. Nothing statistical is recomputed; every
#' number comes from the `netmeta` object.
#'
#' @section Guides (read in order):
#' \enumerate{
#'   \item [nmaplots-1-workflow] -- from a `netmeta` object to a saved figure.
#'   \item [nmaplots-2-ring] -- adding an outer ring with a subgroup composition.
#'   \item [nmaplots-3-style] -- layouts, colours, labels, legend and titles.
#'   \item [nmaplots-4-netgraph] -- moving from `netgraph()`: argument map.
#' }
#'
#' @section Main function:
#' \describe{
#'   \item{[nmaplot()]}{Draw (and optionally save) the network plot. Returns a
#'     `ggplot` object with the node, edge and ring tables in `$nmaplot`.}
#' }
#'
#' @section Example data:
#' [mash]: arm-level results of trials of drugs for MASH with fibrosis, four
#' outcomes, with a `design` column for the ring examples.
#'
#' @examples
#' if (requireNamespace("netmeta", quietly = TRUE)) {
#'   library(netmeta)
#'   data(mash)
#'   d <- mash$fib_improvement_alldoses
#'   p <- pairwise(treat = treatment, event = responders, n = sampleSize,
#'                 studlab = study, data = d, sm = "RR")
#'   net <- netmeta(p, reference.group = "Placebo")
#'   nmaplot(net, outcome = "Fibrosis improvement without worsening of MASH")
#' }
#'
#' @keywords internal
"_PACKAGE"
