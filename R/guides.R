# Long-form help pages, in the style of easyTSA.

#' Guide 1: workflow, from a netmeta object to a saved figure
#'
#' @description
#' `nmaplots` draws what \pkg{netmeta} already computed. Fit the network
#' meta-analysis as usual, hand the object to [nmaplot()], and save.
#'
#' @section Step 1, fit the network meta-analysis:
#' ```r
#' library(netmeta); library(nmaplots)
#' data(Franchini2012)
#' p1 <- pairwise(list(Treatment1, Treatment2, Treatment3),
#'                n = list(n1, n2, n3), mean = list(y1, y2, y3),
#'                sd = list(sd1, sd2, sd3), data = Franchini2012, studlab = Study)
#' net1 <- netmeta(p1, sm = "MD", reference.group = "plac")
#' ```
#' Any `netmeta` object works. Sample sizes appear on the plot when the
#' object carries them (`n1`/`n2`, which `pairwise()` supplies when you give
#' `n`); otherwise the number of studies per treatment is shown as `k = ...`.
#'
#' @section Step 2, draw:
#' ```r
#' nmaplot(net1, outcome = "Change in UPDRS motor score")
#' ```
#' Defaults: treatments on a polygon (`layout = "multi"`), node area
#' proportional to the sample size, reference treatment in grey and the
#' others in muted red, black edges whose width follows the number of direct
#' studies with that number in a white circle on the edge, bold treatment name
#' with `n = ...` underneath, centred title "Network of Interventions" with
#' the outcome as subtitle, and a boxed legend under the network.
#'
#' The result is a `ggplot`; `+` works as usual to add layers or change the
#' theme. The tables used for drawing are in `p$nmaplot$nodes`,
#' `p$nmaplot$edges` and `p$nmaplot$rings`.
#'
#' @section Step 3, save:
#' ```r
#' nmaplot(net1, outcome = "Change in UPDRS motor score",
#'         file = c("network.png", "network.pdf", "network.tiff"),
#'         width = 9, height = 9, dpi = 300)
#' ```
#' The format follows the extension. PDF is vector (Cairo), PNG and TIFF use
#' \pkg{ragg} when installed. `width` and `height` are inches; a square or
#' slightly tall figure suits the legend below the network.
#'
#' @section Next:
#' [nmaplots-2-ring] adds a subgroup ring around each node;
#' [nmaplots-3-style] covers layouts, colours and text.
#'
#' @name nmaplots-1-workflow
NULL

#' Guide 2: the outer ring, a subgroup composition per treatment
#'
#' @description
#' The ring around each node shows how the studies (or patients) of that
#' treatment split across the levels of a subgroup: study design (RCT versus
#' propensity-score matched cohorts), risk of bias, region, dose class,
#' funding source, anything you can count per treatment.
#'
#' @section What to pass:
#' A long data frame with one row per treatment and group:
#' ```r
#' design <- data.frame(treatment = rep(net1$trts, each = 2),
#'                      group = rep(c("RCT", "PSM"), 5),
#'                      value = c(4, 2,  3, 2,  2, 1,  3, 0,  2, 2))
#' nmaplot(net1, ring = design, ring_name = "Study design",
#'         outcome = "Change in UPDRS motor score")
#' ```
#' The first three columns are used whatever their names: treatment, group,
#' value. Values can be counts or proportions; each treatment is normalised
#' to 100 percent. A wide matrix or data frame (rows = treatments, columns =
#' groups, row names = treatment names) is accepted too. Treatments absent
#' from `ring` are drawn without a ring. Group order follows the factor
#' levels, or the order of first appearance.
#'
#' @section What is drawn:
#' Segments start at the top and run clockwise, in group order, separated by
#' thin white lines. Each segment gets its percentage printed just outside
#' the ring, at the point of the segment farthest from any edge so the
#' number does not sit on a line. Segments below `ring_label_min` (5 percent)
#' get no label. `ring_width` sets the ring thickness relative to the node
#' radius; small nodes get a minimum absolute thickness so the ring stays
#' readable.
#'
#' @section Naming and colours:
#' `ring_name` (e.g. `"Study design"`) is appended to the subtitle as
#' "Outer ring: study design" and names the legend section
#' "Outer ring = Study design". `ring_title` overrides the legend text only.
#' `ring_colors` takes a named vector (names = groups) or a vector in group
#' order; the default is muted blue, red, green, then yellow, purple, orange.
#'
#' @name nmaplots-2-ring
NULL

#' Guide 3: layouts, colours, labels, legend and titles
#'
#' @section Layouts:
#' \describe{
#'   \item{`layout = "multi"`}{Default. Treatments evenly spaced on a polygon
#'     in the order of `order` (defaults to the order in the `netmeta`
#'     object).}
#'   \item{`layout = "circle"`}{Treatments on a circle, drawn as a light
#'     dashed guide (`circle_guide`, `circle_color`); the most connected
#'     treatment sits at the top and the others follow clockwise by
#'     decreasing number of direct comparisons.}
#'   \item{`layout = "star"`}{The reference treatment in the centre, the
#'     others on a polygon around it. `reference` overrides the reference
#'     group stored in the object.}
#'   \item{a matrix}{Two columns, one row per treatment, row names = treatment
#'     names: your own coordinates.}
#' }
#'
#' @section Nodes:
#' Node area is proportional to the sample size (`node_size = "n"`), or to
#' the number of studies (`"studies"`), or equal. `node_size_range` gives the
#' smallest and largest radius in layout units (the network spans about -1 to
#' 1). `node_fill` is one colour, one colour per treatment, or `"auto"` with
#' `palette` (any [grDevices::hcl.colors()] palette name). The reference
#' treatment is filled with `reference_fill` (grey); `highlight` recolours
#' chosen treatments with `highlight_color`.
#'
#' @section Edges:
#' Width follows the number of direct studies (`edge_width = "studies"`)
#' within `edge_width_range` (mm); edges are black (`edge_color`). The count
#' is printed in a white circle with a black border on each edge
#' (`edge_labels`, `edge_label_size`); `edge_label_fill = NA` prints plain
#' numbers nudged off the line instead. `edge_style = "multi"` draws one thin line
#' per study; `min_studies` hides comparisons with fewer studies.
#' `multiarm = TRUE` shades the polygon of each multi-arm trial.
#'
#' @section Labels:
#' Treatment names are bold with `n = ...` in a lighter line underneath
#' (`show_n = FALSE` drops it). Names longer than `label_wrap` characters
#' are wrapped. `labels` supplies display names (a named vector is safest).
#' Each label is placed in the largest empty angle around its node so it
#' does not cross an edge; `label_offset` moves it further out.
#'
#' @section Legend and titles:
#' The legend (`legend = TRUE` by default) is one framed box under the
#' network with sections for node size, the circled study count on the
#' edges and, when a ring is drawn, the ring groups. `title` defaults to "Network of Interventions",
#' centred (`title_align = "left"` for the other style); `outcome` becomes
#' the subtitle; `caption` is printed in italics at the bottom right.
#' `font_family = "serif"` gives the reference look, `"sans"` a plainer one.
#' `grid = TRUE` adds a light background grid; `background` sets the page
#' colour (pair it with `label_color` and `title_color` for dark themes).
#'
#' @name nmaplots-3-style
NULL

#' Guide 4: moving from netgraph()
#'
#' @description
#' [nmaplot()] accepts the object you already pass to
#' [netmeta::netgraph()], and the most common `netgraph()` arguments are
#' mapped so old calls keep working.
#'
#' @section Argument map:
#' \tabular{ll}{
#'   `netgraph()` \tab `nmaplot()` \cr
#'   `col.points` \tab `node_fill` (and the reference keeps that colour too) \cr
#'   `col` \tab `edge_color` \cr
#'   `number.of.studies` \tab `edge_labels` \cr
#'   `thickness = "number.of.studies"` \tab `edge_width = "studies"` \cr
#'   `cex` \tab scales `label_size` \cr
#'   `cex.points` \tab scales `node_size_range` \cr
#'   `seq` \tab `order` \cr
#'   `start.layout` \tab `layout` \cr
#'   `labels` \tab `labels` \cr
#'   `plastic`, `points`, `lwd`, `points.min`, `points.max` \tab accepted and ignored
#' }
#' Any other argument is ignored with a message naming it.
#'
#' @section Example:
#' ```r
#' # old
#' netgraph(net1, plastic = FALSE, points = TRUE, col.points = "darkblue",
#'          number.of.studies = TRUE, thickness = "number.of.studies")
#' # same call, new engine
#' nmaplot(net1, plastic = FALSE, points = TRUE, col.points = "darkblue",
#'         number.of.studies = TRUE, thickness = "number.of.studies")
#' ```
#'
#' @section What is different:
#' Node size is by area, not by diameter. Labels carry the sample size on a
#' second line. The plot is a `ggplot`, so it composes with other ggplot
#' layers and saves through `file =` in one call instead of `png()` /
#' `dev.off()` pairs.
#'
#' @name nmaplots-4-netgraph
NULL
