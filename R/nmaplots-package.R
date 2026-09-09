#' nmaplots: Publication-quality network plots for netmeta objects
#'
#' Draws the evidence network of a network meta-analysis fitted with
#' \pkg{netmeta}. One function, [nmaplot()], takes the same object as
#' [netmeta::netgraph()] and renders it with \pkg{ggplot2}: node area by
#' sample size, edge width and a boxed count by number of studies, the
#' treatment name and `n = ...` at every node, an optional outer ring per
#' node showing a subgroup composition (risk of bias, study design, region,
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
#' The examples use the data sets shipped with \pkg{netmeta}:
#' `Franchini2012` (Parkinson's disease, continuous outcome, the netmeta
#' vignette example) and `smokingcessation` (binary outcome with multi-arm
#' trials).
#'
#' @examples
#' if (requireNamespace("netmeta", quietly = TRUE)) {
#'   library(netmeta)
#'   data(Franchini2012)
#'   p1 <- pairwise(list(Treatment1, Treatment2, Treatment3),
#'                  n = list(n1, n2, n3), mean = list(y1, y2, y3),
#'                  sd = list(sd1, sd2, sd3), data = Franchini2012,
#'                  studlab = Study)
#'   net1 <- netmeta(p1, sm = "MD", reference.group = "plac")
#'   nmaplot(net1, outcome = "Change in UPDRS motor score")
#' }
#'
#' @keywords internal
"_PACKAGE"
