#' Network plot for a netmeta or gemtc object
#'
#' Draws the evidence network of a network meta-analysis fitted with
#' [netmeta::netmeta()] or set up with \pkg{gemtc}. The function takes the
#' same object as [netmeta::netgraph()] (or a gemtc network, model or result)
#' and renders it with 'ggplot2': node area by sample
#' size, edge width by number of studies, study counts on the edges, treatment
#' name and sample size at every node, an optional outer ring showing a
#' subgroup composition per treatment (for example study design), several
#' layouts, a bottom legend panel, and direct export to PNG, PDF or TIFF.
#'
#' @param x An object of class `netmeta` (from [netmeta::netmeta()]), or a
#'   \pkg{gemtc} object: `mtc.network` (`gemtc::mtc.network()`), `mtc.model`
#'   (`gemtc::mtc.model()`) or `mtc.result` (`gemtc::mtc.run()`). For gemtc the
#'   network is read from `data.ab` and `data.re`: sample sizes come from
#'   `sampleSize` and events from `responders` when every arm has them; the
#'   `description` column of the treatments table, when it differs from `id`,
#'   gives the default `labels`; `reference` defaults to the most connected
#'   treatment. Pairwise data also works: the output of [meta::pairwise()], or
#'   any data frame with `treat1`, `treat2` and `studlab` (sample sizes from
#'   `n1`/`n2`, events from `event1`/`event2`). Text columns holding numbers,
#'   as after reading a decimal-comma file, are converted by [nma_clean()].
#' @param layout Node arrangement. `"multi"` (default): treatments evenly
#'   spaced on a polygon, in the order of `order`. `"circle"`: treatments on
#'   a circle (drawn as a light guide line), ordered by number of direct
#'   comparisons, most connected treatment at the top and going clockwise.
#'   `"star"`: reference treatment in the centre, the others on a polygon
#'   around it. A numeric matrix with two columns and
#'   one row per treatment (row names = treatment names) gives a custom
#'   layout.
#' @param order Character vector giving the order of treatments around the
#'   polygon (used by `"multi"` and `"star"`). Defaults to the order stored in
#'   the `netmeta` object. Partial vectors are completed with the remaining
#'   treatments.
#' @param reference Treatment placed in the centre for `layout = "star"` and
#'   filled with `reference_fill`. Defaults to the reference group of the
#'   `netmeta` object.
#' @param labels Optional character vector of display names for the
#'   treatments, in the order of `x$trts` (or a named vector).
#' @param show_n Logical. Print the sample size and its share of all
#'   randomised participants (`n = 1,234 (18%)`) under each treatment name. When the `netmeta` object has no sample sizes the number
#'   of studies is printed instead (`k = 4`).
#' @param ring Subgroup composition drawn as a ring around each node. The
#'   default (`NULL`) draws the event rate of every treatment when the network
#'   is binary (events over participants, taken from the `netmeta` object) and
#'   nothing otherwise; `FALSE` never draws a ring. Supply your own as a long
#'   data frame with three columns (treatment, group, value) or a wide matrix /
#'   data frame with one row per treatment (row names) and one column per
#'   group. Values are counts or proportions; each treatment is normalised to
#'   100 percent. Treatments missing from `ring` get no ring.
#' @param ring_colors Colours for the ring groups: a named vector (names =
#'   groups) or a vector in the order of the groups. Defaults to a muted
#'   blue / red / green set.
#' @param ring_width Thickness of the ring as a fraction of the node radius
#'   (never thinner than 0.035 layout units so small nodes stay readable).
#' @param ring_labels Logical. Print the percentage of each ring segment
#'   outside the ring.
#' @param ring_label_min Segments below this proportion get no percentage
#'   label.
#' @param ring_name Short name of what the ring shows (e.g. `"Risk of bias"`).
#'   Used in the subtitle and in the legend ("Outer ring = Risk of bias").
#' @param ring_title Title of the ring section in the legend. Defaults to
#'   `"Outer ring = <ring_name>"`.
#' @param outcome Name of the outcome. Becomes the subtitle (with the ring
#'   name appended when a ring is drawn) unless `subtitle` is given.
#' @param summary_line Logical. Add a line under the subtitle with the number
#'   of studies, treatments and patients in the network.
#' @param font_family Font family for all text. `"serif"` reproduces the
#'   reference look; use `"sans"` for Helvetica-like output.
#' @param edge_font_family Font family for the circled study counts only.
#'   `"sans"` (Arial-like) keeps the digits compact inside their circle.
#' @param label_wrap Integer. Wrap treatment names longer than this many
#'   characters. `NULL` disables wrapping.
#' @param label_size Font size (points) of the treatment names and of the
#'   sample-size line under them.
#' @param label_color Colour of the treatment names.
#' @param label_offset Gap between the node (or its ring) and the label, in
#'   layout units (the network spans roughly -1 to 1).
#' @param label_box Container behind each node label. `FALSE` (default) for
#'   none, `TRUE` for a white box with a light border, or a colour (e.g.
#'   `"#F3F4F6"`) for a box filled with that colour.
#' @param node_size What drives the node area: `"n"` (sample size),
#'   `"studies"` (number of studies including the treatment) or `"equal"`.
#'   A numeric vector (length one or one per treatment) gives radii directly.
#' @param node_size_range Smallest and largest node radius, in layout units.
#' @param node_fill Fill colour of the nodes: a single colour, one colour per
#'   treatment (in the order of `x$trts` or named), or `"auto"` to use
#'   `palette`.
#' @param reference_fill Fill colour of the reference treatment (`NULL` to
#'   use `node_fill`).
#' @param node_color Border colour of the nodes.
#' @param node_stroke Border width of the nodes (mm).
#' @param palette Colours used when `node_fill = "auto"`: a colour vector or
#'   the name of a [grDevices::hcl.colors()] palette.
#' @param highlight Optional treatments to emphasise with `highlight_color`.
#' @param highlight_color Fill colour for highlighted treatments.
#' @param edge_width What drives the edge width: `"studies"` or `"equal"`, or
#'   a single number (mm).
#' @param edge_width_range Thinnest and thickest edge in mm.
#' @param edge_color Edge colour (single, or one per direct comparison).
#' @param edge_alpha Edge opacity.
#' @param edge_style `"single"` draws one line per comparison with width by
#'   number of studies; `"multi"` draws one parallel line per study (up to
#'   `max_lines`).
#' @param max_lines Maximum parallel lines per comparison for
#'   `edge_style = "multi"`.
#' @param min_studies Hide direct comparisons with fewer studies than this.
#' @param edge_labels Logical. Print the number of studies on each edge.
#' @param edge_label_size Font size (points) of the edge labels.
#' @param edge_label_color Text and border colour of the edge labels.
#' @param edge_label_fill Fill of the circle that carries the study count on
#'   each edge. `NA` prints plain text nudged off the line instead.
#' @param edge_label_offset Nudge (layout units) for plain-text edge labels.
#' @param multiarm Logical. Shade the polygon of each multi-arm study.
#' @param multiarm_fill,multiarm_alpha Fill and opacity of those polygons.
#' @param title,subtitle,caption Title, subtitle and caption. `title`
#'   defaults to "Network of Interventions"; `subtitle` defaults to `outcome`.
#'   The caption is printed in italics at the bottom right.
#' @param title_size Font size of the title. Subtitle, caption and legend
#'   text scale from it.
#' @param title_color Colour of the title.
#' @param title_align `"left"` or `"center"`.
#' @param circle_guide Logical. For `layout = "circle"`, draw the circle the
#'   nodes sit on as a light dashed line.
#' @param circle_color Colour of that guide line.
#' @param grid Logical. Light background grid.
#' @param grid_color Colour of the grid lines.
#' @param background Background colour.
#' @param legend Logical. Draw the boxed legend under the network (node size,
#'   edge width, ring groups). On by default.
#' @param legend_size Font size (points) of the legend text.
#' @param margin Extra space around the network as a fraction of its radius.
#' @param file Optional path(s) to save the plot; format from the extension
#'   (`.png`, `.pdf`, `.tiff`/`.tif`).
#' @param width,height Size of the saved figure in inches.
#' @param dpi Resolution of raster output.
#' @param ... [netmeta::netgraph()] arguments accepted for compatibility:
#'   `col.points`, `col`, `number.of.studies`, `cex`, `cex.points`, `seq`,
#'   `start.layout`, `thickness`, `labels`; `plastic`, `points`, `lwd`,
#'   `points.min`, `points.max` are ignored. Unknown arguments are ignored
#'   with a message.
#'
#' @return A `ggplot` object with an extra element `nmaplot` holding the node,
#'   edge and ring data frames used for drawing. Returned invisibly when
#'   `file` is given.
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
#'
#'   # outer ring from a column of the data (here study design)
#'   nmaplot(net, ring = table(d$treatment, d$design), ring_name = "Study design",
#'           outcome = "Fibrosis improvement without worsening of MASH")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_polygon geom_text geom_label geom_rect coord_equal labs theme theme_void element_rect element_line element_text element_blank margin unit ggsave scale_fill_identity scale_colour_identity scale_linewidth_identity .data
#' @export
nmaplot <- function(x,
                    layout = c("multi", "circle", "star"),
                    order = NULL,
                    reference = NULL,
                    labels = NULL,
                    show_n = TRUE,
                    ring = NULL,
                    ring_colors = NULL,
                    ring_width = 0.45,
                    ring_labels = TRUE,
                    ring_label_min = 0.05,
                    ring_name = NULL,
                    ring_title = NULL,
                    outcome = NULL,
                    summary_line = TRUE,
                    font_family = "serif",
                    edge_font_family = "sans",
                    label_wrap = 18,
                    label_size = 14,
                    label_color = "#1F2A44",
                    label_offset = 0.06,
                    label_box = FALSE,
                    node_size = c("n", "studies", "equal"),
                    node_size_range = c(0.07, 0.2),
                    node_fill = "#A94A47",
                    reference_fill = "#8A939B",
                    node_color = "white",
                    node_stroke = 0.8,
                    palette = "Dark 3",
                    highlight = NULL,
                    highlight_color = "#E4572E",
                    edge_width = c("studies", "equal"),
                    edge_width_range = c(0.3, 2),
                    edge_color = "black",
                    edge_alpha = 1,
                    edge_style = c("single", "multi"),
                    max_lines = 6,
                    min_studies = 1,
                    edge_labels = TRUE,
                    edge_label_size = 9,
                    edge_label_color = "black",
                    edge_label_fill = "white",
                    edge_label_offset = 0.05,
                    multiarm = FALSE,
                    multiarm_fill = "#8A939B",
                    multiarm_alpha = 0.08,
                    title = "Network of Interventions",
                    subtitle = NULL,
                    caption = NULL,
                    title_size = 22,
                    title_color = "#1F2A44",
                    title_align = c("center", "left"),
                    circle_guide = TRUE,
                    circle_color = "#D5D8DC",
                    grid = FALSE,
                    grid_color = "grey88",
                    background = "white",
                    legend = TRUE,
                    legend_size = 11,
                    margin = 0.06,
                    file = NULL,
                    width = 9,
                    height = 8,
                    dpi = 300,
                    ...) {

  x <- as_nma_input(x)

  # ---- netgraph() compatibility arguments -----------------------------------
  dots <- list(...)
  if (length(dots)) {
    nm <- names(dots)
    if (is.null(nm)) nm <- rep("", length(dots))
    if ("col.points" %in% nm) { node_fill <- dots$col.points; reference_fill <- NULL }
    if ("col" %in% nm) edge_color <- dots$col
    if ("number.of.studies" %in% nm) edge_labels <- isTRUE(dots$number.of.studies)
    if ("cex" %in% nm) label_size <- label_size * dots$cex
    if ("cex.points" %in% nm) node_size_range <- node_size_range * dots$cex.points
    if ("seq" %in% nm) order <- dots$seq
    if ("start.layout" %in% nm) layout <- dots$start.layout
    if ("thickness" %in% nm) {
      edge_width <- if (identical(dots$thickness, "number.of.studies")) "studies" else
        if (identical(dots$thickness, "equal")) "equal" else edge_width
    }
    if ("labels" %in% nm && is.null(labels)) labels <- dots$labels
    known <- c("col.points", "col", "number.of.studies", "cex", "cex.points",
               "seq", "start.layout", "thickness", "points.min", "points.max",
               "plastic", "points", "lwd", "labels")
    unknown <- setdiff(nm, known)
    if (length(unknown)) {
      message("nmaplot(): ignoring unsupported argument(s): ",
              paste(unknown, collapse = ", "))
    }
  }

  if (is.null(labels) && !is.null(x[["labels"]])) labels <- x[["labels"]]

  edge_style <- match.arg(edge_style)
  title_align <- match.arg(title_align)
  if (is.character(node_size)) node_size <- match.arg(node_size)
  if (is.character(edge_width)) edge_width <- match.arg(edge_width)

  # ---- network ---------------------------------------------------------------
  net <- nma_network(x)
  trts <- net$trts
  n_trt <- length(trts)

  order <- complete_order(order, trts)
  if (is.null(reference)) {
    reference <- x$reference.group
    if (is.null(reference) || !nzchar(reference) || !(reference %in% trts)) {
      # netmeta accepts abbreviated reference groups; resolve them
      hit <- if (!is.null(reference) && nzchar(reference))
        trts[startsWith(tolower(trts), tolower(reference))] else character(0)
      reference <- if (length(hit) == 1) hit else trts[which.max(net$degree)]
    }
  }
  if (is.character(layout)) layout <- match.arg(layout, c("multi", "circle", "star"))
  coords <- nma_layout(layout, trts, order, reference, net)

  nodes <- data.frame(
    trt = trts,
    x = coords[trts, 1],
    y = coords[trts, 2],
    n = unname(net$n.trts[trts]),
    k = unname(net$k.trts[trts]),
    degree = unname(net$degree[trts]),
    stringsAsFactors = FALSE
  )
  rownames(nodes) <- NULL

  # ---- node radius (layout units) -------------------------------------------
  size_label <- NULL
  if (is.numeric(node_size)) {
    if (length(node_size) == 1) {
      nodes$size <- rep(node_size, n_trt)
    } else {
      if (length(node_size) != n_trt)
        stop("Numeric `node_size` must have length 1 or one value per treatment.",
             call. = FALSE)
      nodes$size <- node_size
    }
  } else if (node_size == "equal") {
    nodes$size <- rep(mean(node_size_range), n_trt)
  } else {
    use_n <- node_size == "n" && net$has.n
    driver <- if (use_n) nodes$n else nodes$k
    size_label <- if (use_n) "Node size = number of participants" else
      "Node size = number of studies"
    # area proportional to the driver: radius ~ sqrt
    nodes$size <- rescale_range(sqrt(driver), node_size_range * sqrt(layout_radius(n_trt)))
    nodes$size_driver <- driver
  }

  # ---- node fill -------------------------------------------------------------
  nodes$fill <- resolve_node_fill(node_fill, trts, palette)
  if (!is.null(reference_fill) && !identical(node_fill, "auto") &&
      length(node_fill) == 1) {
    nodes$fill[nodes$trt == reference] <- reference_fill
  }
  if (!is.null(highlight)) {
    miss <- setdiff(highlight, trts)
    if (length(miss))
      warning("`highlight` treatments not in the network: ",
              paste(miss, collapse = ", "), call. = FALSE)
    nodes$fill[nodes$trt %in% highlight] <- highlight_color
  }

  # ---- ring ------------------------------------------------------------------
  rings <- NULL
  ring_groups <- character(0)
  auto_ring <- FALSE
  if (is.null(ring) && net$has.events && net$has.n) {
    # binary network: show each treatment's event rate by default
    auto_ring <- TRUE
    ev <- unname(net$e.trts[trts])
    ring <- data.frame(
      treatment = rep(trts, 2),
      group = factor(rep(c("Events", "No event"), each = n_trt),
                     levels = c("Events", "No event")),
      value = c(ev, unname(net$n.trts[trts]) - ev),
      stringsAsFactors = FALSE)
    if (is.null(ring_colors))
      ring_colors <- c("Events" = "#4E7CA8", "No event" = "#DFE3E7")
    if (is.null(ring_name)) ring_name <- "Event rate"
  }
  if (!is.null(ring) && !isFALSE(ring)) {
    rg <- normalise_ring(ring, trts)
    if (!is.null(rg$data) && nrow(rg$data) > 0) {
      ring_groups <- rg$groups
      ring_cols <- resolve_ring_colors(ring_colors, ring_groups)
      rings <- rg$data
      rings$fill <- unname(ring_cols[rings$group])
      # the automatic ring prints one percentage: the event rate
      if (auto_ring) rings$show_pct <- rings$group == "Events"
    }
  }
  nodes$has_ring <- if (!is.null(rings)) nodes$trt %in% rings$treatment else FALSE
  nodes$r_out <- ifelse(nodes$has_ring,
                        pmax(nodes$size * (1 + ring_width), nodes$size * 1.06 + 0.035),
                        nodes$size)

  # ---- titles from outcome / ring_name -------------------------------------
  if (is.null(subtitle)) {
    parts <- c(outcome, if (!is.null(rings) && !is.null(ring_name))
      paste0("Outer ring: ", tolower(substr(ring_name, 1, 1)),
             substr(ring_name, 2, nchar(ring_name))))
    if (length(parts)) subtitle <- paste(parts, collapse = ". ")
  }
  if (isTRUE(summary_line)) {
    k_studies <- length(unique(as.character(x$studlab)))
    line <- paste0(k_studies, ifelse(k_studies == 1, " study, ", " studies, "),
                   n_trt, " treatments",
                   if (net$has.n) paste0(", ", format_int(sum(net$n.trts, na.rm = TRUE)),
                                         " patients") else "",
                   if (net$has.events) paste0(", ", format_int(sum(net$e.trts, na.rm = TRUE)),
                                              " events") else "")
    subtitle <- if (is.null(subtitle)) line else paste0(subtitle, "\n", line)
  }
  if (is.null(ring_title)) {
    ring_title <- if (!is.null(ring_name)) paste0("Outer ring = ", ring_name) else
      "Outer ring = subgroup composition"
  }

  # ---- labels ----------------------------------------------------------------
  if (is.null(labels)) {
    name <- trts
  } else {
    name <- if (!is.null(names(labels))) unname(labels[trts]) else as.character(labels)
    if (length(name) != n_trt || anyNA(name))
      stop("`labels` must have one entry per treatment.", call. = FALSE)
  }
  if (!is.null(label_wrap)) name <- wrap_labels(name, label_wrap)
  nodes$name <- name
  nodes$nline <- if (isTRUE(show_n)) {
    if (net$has.n) {
      share <- round(100 * nodes$n / sum(nodes$n, na.rm = TRUE))
      paste0("n = ", format_int(nodes$n), " (", share, "%)")
    } else paste0("k = ", nodes$k)
  } else ""
  nodes$label <- ifelse(nzchar(nodes$nline), paste0(nodes$name, "\n", nodes$nline),
                        nodes$name)

  centre <- c(mean(nodes$x), mean(nodes$y))
  dx <- nodes$x - centre[1]
  dy <- nodes$y - centre[2]
  # built-in layouts put the treatments on a circle: labels go straight
  # outward. A custom coordinate matrix uses the emptiest angle instead.
  ang <- if (is.character(layout)) atan2(dy, dx) else
    label_direction(nodes, net$edges, atan2(dy, dx))
  ux <- cos(ang)
  uy <- sin(ang)
  # percent labels sit just outside the ring; the name goes beyond them
  pct_pad <- if (!is.null(rings) && isTRUE(ring_labels)) 0.045 else 0
  off <- nodes$r_out + ifelse(nodes$has_ring, pct_pad, 0) + label_offset
  nodes$lx <- nodes$x + ux * off
  nodes$ly <- nodes$y + uy * off
  # snap the anchor to the corner of the text block that faces the node, so
  # the block never overlaps the disc whatever the direction
  nodes$hjust <- ifelse(ux > 0.15, 0, ifelse(ux < -0.15, 1, 0.5))
  nodes$vjust <- ifelse(uy > 0.15, 0, ifelse(uy < -0.15, 1, 0.5))
  # a node sitting in the middle of the network (star layout) has spokes all
  # around it: write its label inside the disc when the disc is wide enough
  upi0 <- 2.9 / max(width, 1)
  chars <- vapply(strsplit(nodes$label, "\n", fixed = TRUE),
                  function(v) max(nchar(v)), numeric(1))
  half_w <- chars * label_size * 0.55 / 72 * upi0 / 2
  centre_node <- sqrt(dx^2 + dy^2) < 0.15 & nodes$degree >= 3
  is_star <- is.character(layout) && identical(layout, "star")
  # star layout: the centre disc is described in the legend instead of on the
  # plot (there is never room for text between the spokes)
  nodes$in_legend <- is_star & centre_node
  nodes$tscale <- pmin(1, nodes$size * 0.92 / pmax(half_w, 1e-9))
  nodes$inside <- centre_node & !nodes$in_legend & nodes$tscale >= 0.65
  nodes$tscale[!nodes$inside] <- 1
  nodes$lx[nodes$inside] <- nodes$x[nodes$inside]
  nodes$ly[nodes$inside] <- nodes$y[nodes$inside]
  nodes$hjust[nodes$inside] <- 0.5
  nodes$vjust[nodes$inside] <- 0.5

  # ---- edges -----------------------------------------------------------------
  edges <- net$edges
  edges <- edges[edges$studies >= min_studies, , drop = FALSE]
  rownames(edges) <- NULL
  edges$x <- nodes$x[match(edges$treat1, nodes$trt)]
  edges$y <- nodes$y[match(edges$treat1, nodes$trt)]
  edges$xend <- nodes$x[match(edges$treat2, nodes$trt)]
  edges$yend <- nodes$y[match(edges$treat2, nodes$trt)]
  edges$mx <- (edges$x + edges$xend) / 2
  edges$my <- (edges$y + edges$yend) / 2

  if (is.numeric(edge_width)) {
    edges$width <- rep(edge_width[1], nrow(edges))
  } else if (edge_width == "equal") {
    edges$width <- rep(mean(edge_width_range), nrow(edges))
  } else if (length(unique(edges$studies)) < 2) {
    edges$width <- rep(edge_width_range[1], nrow(edges))
  } else {
    edges$width <- rescale_range(edges$studies, edge_width_range)
  }
  if (length(edge_color) == 1) {
    edges$color <- rep(edge_color, nrow(edges))
  } else {
    if (length(edge_color) != nrow(edges))
      stop("`edge_color` must be a single colour or one colour per comparison (",
           nrow(edges), ").", call. = FALSE)
    edges$color <- edge_color
  }

  # ---- multi-arm polygons ----------------------------------------------------
  polys <- NULL
  if (isTRUE(multiarm) && length(net$multiarm)) {
    polys <- do.call(rbind, lapply(seq_along(net$multiarm), function(i) {
      tr <- net$multiarm[[i]]
      tr <- tr[tr %in% nodes$trt]
      if (length(tr) < 3) return(NULL)
      px <- nodes$x[match(tr, nodes$trt)]
      py <- nodes$y[match(tr, nodes$trt)]
      o <- order(atan2(py - mean(py), px - mean(px)))
      data.frame(study = names(net$multiarm)[i], x = px[o], y = py[o],
                 stringsAsFactors = FALSE)
    }))
  }

  # ---- limits ----------------------------------------------------------------
  # two passes: the text extent depends on the scale, which depends on the
  # window, which depends on the text extent. Second pass uses the limiting
  # dimension of the figure (coord_equal letterboxes the other one).
  upi <- 2.9 / max(width, 1)                # layout units per inch, first guess
  win <- plot_window(nodes, label_size, upi, margin)
  leg_h <- if (isTRUE(legend)) 0.5 * win$hw else 0
  panel_w <- max(width - 0.4, 1)
  panel_h <- max(height - 0.4 - (if (!is.null(title)) 0.8 else 0) -
                   (if (!is.null(caption)) 0.4 else 0), 1)
  upi <- max(diff(win$xlim) / panel_w, (diff(win$ylim) + leg_h) / panel_h)
  win <- plot_window(nodes, label_size, upi, margin)
  lim <- win$hw
  ylo <- win$ylim[1]
  leg <- NULL
  if (isTRUE(legend)) {
    leg <- build_legend(nodes, edges, rings, ring_groups, size_label,
                        edge_width_range, ring_title, win, has_n = net$has.n,
                        rc = edge_label_size * 0.95 / 72 * upi,
                        ring_name = ring_name, reference_fill = reference_fill)
    ylo <- leg$ylo
  }

  # ---- draw ------------------------------------------------------------------
  p <- ggplot()

  if (is.character(layout) && identical(layout[1], "circle") && isTRUE(circle_guide)) {
    guide <- circle_poly(mean(nodes$x), mean(nodes$y), layout_radius(n_trt), n = 180)
    guide <- rbind(guide, guide[1, ])
    p <- p + ggplot2::geom_path(
      data = guide, aes(x = .data$x, y = .data$y),
      colour = circle_color, linewidth = 0.5, linetype = "22"
    )
  }

  if (!is.null(polys) && nrow(polys)) {
    p <- p + geom_polygon(
      data = polys, aes(x = .data$x, y = .data$y, group = .data$study),
      fill = multiarm_fill, alpha = multiarm_alpha, colour = NA
    )
  }

  if (nrow(edges)) {
    if (edge_style == "multi") {
      seg <- expand_multi_edges(edges, max_lines, spacing = 0.018)
      p <- p + geom_segment(
        data = seg,
        aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            colour = .data$color),
        linewidth = min(edge_width_range), alpha = edge_alpha, lineend = "round"
      )
    } else {
      p <- p + geom_segment(
        data = edges,
        aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            linewidth = .data$width, colour = .data$color),
        alpha = edge_alpha, lineend = "round"
      )
    }
  }

  # rings (annulus sectors) under the node discs
  if (!is.null(rings)) {
    rings$x0 <- nodes$x[match(rings$treatment, nodes$trt)]
    rings$y0 <- nodes$y[match(rings$treatment, nodes$trt)]
    rings$r0 <- nodes$size[match(rings$treatment, nodes$trt)] * 1.06
    rings$r1 <- nodes$r_out[match(rings$treatment, nodes$trt)]
    sect <- do.call(rbind, lapply(seq_len(nrow(rings)), function(i) {
      d <- sector_poly(rings$x0[i], rings$y0[i], rings$r0[i], rings$r1[i],
                       rings$a0[i], rings$a1[i])
      d$id <- i
      d$fill <- rings$fill[i]
      d
    }))
    p <- p + geom_polygon(
      data = sect, aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
      colour = "white", linewidth = 0.5
    )
    if (isTRUE(ring_labels)) {
      pl <- rings[rings$prop >= ring_label_min & rings$show_pct, , drop = FALSE]
      if (nrow(pl)) {
        # put each percentage at the angle of its sector farthest from any
        # edge leaving that node, so the number never sits on a line
        am <- vapply(seq_len(nrow(pl)), function(i) {
          t <- pl$treatment[i]
          j <- which(nodes$trt == t)
          nb <- c(edges$treat2[edges$treat1 == t], edges$treat1[edges$treat2 == t])
          k <- match(nb, nodes$trt)
          ea <- atan2(nodes$y[k] - nodes$y[j], nodes$x[k] - nodes$x[j])
          # keep the percentage away from the edges and from the whole
          # width of the label block, not just its anchor direction.
          # A node showing a single percentage (the event rate) may put it
          # anywhere on its ring, so a thin segment still lands in the clear.
          avoid <- c(ea, ang[j] + c(-0.5, -0.25, 0, 0.25, 0.5))
          if (sum(pl$treatment == t) == 1) pct_angle(-pi, pi, avoid)
          else pct_angle(pl$a0[i], pl$a1[i], avoid)
        }, numeric(1))
        rr <- pl$r1 + 0.02
        pl$px <- pl$x0 + rr * cos(am)
        pl$py <- pl$y0 + rr * sin(am)
        pl$ph <- (1 - cos(am)) / 2
        pl$pv <- (1 - sin(am)) / 2
        pl$txt <- paste0(round(100 * pl$prop), "%")
        p <- p + geom_text(
          data = pl,
          aes(x = .data$px, y = .data$py, label = .data$txt,
              hjust = .data$ph, vjust = .data$pv),
          size = label_size * 0.8 / ggplot2::.pt, colour = label_color,
          family = font_family
        )
      }
    }
  }

  # node discs
  discs <- do.call(rbind, lapply(seq_len(n_trt), function(i) {
    d <- circle_poly(nodes$x[i], nodes$y[i], nodes$size[i])
    d$id <- i
    d$fill <- nodes$fill[i]
    d
  }))
  p <- p + geom_polygon(
    data = discs, aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
    colour = node_color, linewidth = node_stroke
  )

  # edge labels
  if (nrow(edges) && isTRUE(edge_labels)) {
    if (is.na(edge_label_fill)) {
      ex <- edges$xend - edges$x
      ey <- edges$yend - edges$y
      el <- sqrt(ex^2 + ey^2)
      el[el == 0] <- 1
      nx <- -ey / el
      ny <- ex / el
      flip <- (nx * (edges$mx - centre[1]) + ny * (edges$my - centre[2])) < 0
      nx[flip] <- -nx[flip]
      ny[flip] <- -ny[flip]
      edges$lx <- edges$mx + nx * edge_label_offset
      edges$ly <- edges$my + ny * edge_label_offset
      edges$lh <- (1 - nx) / 2
      edges$lv <- (1 - ny) / 2
      p <- p + geom_text(
        data = edges,
        aes(x = .data$lx, y = .data$ly, label = .data$studies,
            hjust = .data$lh, vjust = .data$lv),
        size = edge_label_size / ggplot2::.pt, colour = edge_label_color,
        fontface = "bold", family = font_family
      )
    } else {
      # white disc with a black border, number inside, sized to the font
      rc <- edge_label_size * 0.95 / 72 * upi * (1 + 0.06 * (nchar(edges$studies) - 1))
      edges <- spread_edge_labels(edges, rc)
      dots_df <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
        d <- circle_poly(edges$mx[i], edges$my[i], rc[i], n = 48)
        d$id <- i
        d
      }))
      p <- p +
        geom_polygon(data = dots_df,
                     aes(x = .data$x, y = .data$y, group = .data$id),
                     fill = edge_label_fill, colour = edge_label_color,
                     linewidth = 0.45) +
        geom_text(data = edges,
                  aes(x = .data$mx, y = .data$my, label = .data$studies),
                  size = edge_label_size / ggplot2::.pt, colour = edge_label_color,
                  fontface = "bold", family = edge_font_family)
    }
  }

  # treatment names (bold) with the sample size line underneath (lighter)
  line_h <- label_size * 1.15 / 72 * upi * nodes$tscale
  two <- nzchar(nodes$nline)
  nl <- vapply(strsplit(nodes$name, "\n", fixed = TRUE), length, integer(1))
  nn <- ifelse(two, vapply(strsplit(nodes$nline, "\n", fixed = TRUE), length, integer(1)), 0L)
  blk <- (nl + nn) * line_h                          # text block height
  chars_all <- vapply(strsplit(nodes$label, "\n", fixed = TRUE),
                      function(v) max(nchar(v)), numeric(1))
  tw <- chars_all * label_size * 0.5 / 72 * upi * nodes$tscale   # block width
  yb <- nodes$ly - nodes$vjust * blk                 # block bottom
  # every row is centred on the block; the block itself sits on the far side
  # of the anchor so it never touches the disc
  nodes$cx <- nodes$lx + (0.5 - nodes$hjust) * tw
  nodes$n_y <- yb + nn * line_h / 2
  nodes$name_y <- yb + nn * line_h + nl * line_h / 2
  nodes$name_v <- 0.5
  nodes$tsize <- label_size * nodes$tscale / ggplot2::.pt
  nodes$box_xmin <- nodes$cx - tw / 2 - 0.3 * line_h
  nodes$box_xmax <- nodes$cx + tw / 2 + 0.3 * line_h
  nodes$box_ymin <- yb - 0.25 * line_h
  nodes$box_ymax <- yb + blk + 0.25 * line_h
  nodes$name_col <- ifelse(nodes$inside, contrast_text(nodes$fill), label_color)
  nodes$n_col <- ifelse(nodes$inside, contrast_text(nodes$fill), label_color)
  shown <- !nodes$in_legend
  boxed <- shown & !nodes$inside
  if (!isFALSE(label_box) && any(boxed)) {
    box_fill <- if (isTRUE(label_box)) "white" else label_box
    p <- p + geom_rect(
      data = nodes[boxed, ],
      aes(xmin = .data$box_xmin, xmax = .data$box_xmax,
          ymin = .data$box_ymin, ymax = .data$box_ymax),
      fill = box_fill, colour = "#C5C9CE", linewidth = 0.35
    )
  }
  p <- p + geom_text(
    data = nodes[shown, ],
    aes(x = .data$cx, y = .data$name_y, label = .data$name,
        vjust = .data$name_v, colour = .data$name_col, size = .data$tsize),
    hjust = 0.5, fontface = "bold", family = font_family, lineheight = 0.9
  )
  if (any(two & shown)) {
    p <- p + geom_text(
      data = nodes[two & shown, ],
      aes(x = .data$cx, y = .data$n_y, label = .data$nline,
          colour = .data$n_col, size = .data$tsize),
      hjust = 0.5, vjust = 0.5, family = font_family, lineheight = 0.9
    )
  }
  p <- p + ggplot2::scale_size_identity()

  # legend panel
  if (!is.null(leg)) {
    p <- p +
      geom_rect(data = leg$boxes,
                aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                    ymax = .data$ymax),
                fill = "white", colour = "#9AA0A6", linewidth = 0.5) +
      geom_rect(data = leg$headers,
                aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                    ymax = .data$ymax),
                fill = "#EDEFF2", colour = NA) +
      geom_segment(data = leg$dividers,
                   aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                   colour = "#C5C9CE", linewidth = 0.4) +
      geom_polygon(data = leg$circles,
                   aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
                   colour = "white", linewidth = 0.4) +
      geom_segment(data = leg$lines,
                   aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
                       linewidth = .data$width),
                   colour = edge_color, lineend = "round") +
      geom_polygon(data = leg$dot,
                   aes(x = .data$x, y = .data$y, group = .data$id),
                   fill = edge_label_fill, colour = edge_label_color, linewidth = 0.45) +
      geom_text(data = leg$dot_text,
                aes(x = .data$x, y = .data$y, label = .data$label),
                size = edge_label_size / ggplot2::.pt, colour = edge_label_color,
                fontface = "bold", family = edge_font_family)
    if (!is.null(leg$ringex) && nrow(leg$ringex)) {
      p <- p + geom_polygon(data = leg$ringex,
                            aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
                            colour = "white", linewidth = 0.4)
    }
    if (!is.null(leg$squares) && nrow(leg$squares)) {
      p <- p + geom_rect(data = leg$squares,
                         aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                             ymax = .data$ymax, fill = .data$fill),
                         colour = "white", linewidth = 0.3)
    }
    p <- p +
      geom_text(data = leg$text,
                aes(x = .data$x, y = .data$y, label = .data$label,
                    fontface = .data$face, hjust = .data$hjust,
                    size = .data$sz * legend_size * (if (leg$n_sec >= 4) 0.9 else 1) / ggplot2::.pt),
                vjust = 0.5, colour = label_color, family = font_family, lineheight = 0.9)
  }

  p <- p +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_linewidth_identity() +
    coord_equal(xlim = win$xlim, ylim = c(ylo, win$ylim[2]), expand = FALSE) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    nmaplot_theme(grid = grid, grid_color = grid_color, background = background,
                  title_size = title_size, title_color = title_color,
                  title_align = title_align, font_family = font_family)

  p$nmaplot <- list(nodes = nodes, edges = edges, rings = rings, multiarm = polys)

  if (!is.null(file)) {
    for (f in file) save_nmaplot(p, f, width, height, dpi, background)
    return(invisible(p))
  }
  p
}
