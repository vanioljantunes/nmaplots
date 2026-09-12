# Internal helpers for nmaplot(). Nothing here is exported.

# Classes nmaplot() accepts.
nma_classes <- c("netmeta", "mtc.network", "mtc.model", "mtc.result")

# Normalise the input to the netmeta fields nma_network() reads: trts, treat1,
# treat2, studlab, n.trts, events.trts, k.trts, reference.group, plus labels
# (display names, gemtc only). netmeta objects pass through unchanged.
as_nma_input <- function(x) {
  if (inherits(x, "netmeta")) return(x)
  if (is.data.frame(x) && all(c("treat1", "treat2", "studlab") %in% names(x))) {
    return(pairwise_input(x))
  }
  if (inherits(x, "mtc.result")) x <- x$model
  if (inherits(x, "mtc.model")) x <- x$network
  if (!inherits(x, "mtc.network")) {
    stop("`x` must be a 'netmeta' object (netmeta::netmeta()), pairwise data ",
         "(meta::pairwise(), or a data frame with treat1, treat2 and studlab) ",
         "or a 'gemtc' network, model or result (gemtc::mtc.network(), ",
         "mtc.model(), mtc.run()).", call. = FALSE)
  }
  gemtc_input(x)
}

# gemtc stores one row per study arm in data.ab (arm-level) and/or data.re
# (relative effects). Arms become the pairwise rows netmeta would hold:
# choose(k, 2) rows per study.
gemtc_input <- function(network) {
  arm_cols <- function(d) {
    if (is.null(d) || !nrow(d)) return(NULL)
    get <- function(col) if (col %in% names(d)) as.numeric(d[[col]]) else
      rep(NA_real_, nrow(d))
    data.frame(study = as.character(d$study),
               treatment = as.character(d$treatment),
               n = get("sampleSize"), events = get("responders"),
               stringsAsFactors = FALSE)
  }
  arms <- rbind(arm_cols(network[["data.ab"]]), arm_cols(network[["data.re"]]))
  if (is.null(arms) || !nrow(arms)) {
    stop("The gemtc network has no data (`data.ab` or `data.re`).", call. = FALSE)
  }

  tr <- network$treatments
  trts <- if (!is.null(tr$id)) as.character(tr$id) else
    sort(unique(arms$treatment))

  rows <- lapply(split(arms$treatment, factor(arms$study, unique(arms$study))),
                 function(t) {
    t <- unique(t)
    if (length(t) < 2) return(NULL)
    cm <- utils::combn(t, 2)
    data.frame(treat1 = cm[1, ], treat2 = cm[2, ], stringsAsFactors = FALSE)
  })
  studlab <- rep(names(rows), vapply(rows, function(r) NROW(r), integer(1)))
  pw <- do.call(rbind, rows)

  totals <- arm_totals(arms, trts)

  labels <- NULL
  if (!is.null(tr$description)) {
    desc <- as.character(tr$description)
    if (!all(is.na(desc)) && !identical(desc, trts)) {
      desc[is.na(desc) | !nzchar(desc)] <- trts[is.na(desc) | !nzchar(desc)]
      labels <- stats::setNames(desc, trts)
    }
  }

  list(trts = trts, treat1 = pw$treat1, treat2 = pw$treat2, studlab = studlab,
       n.trts = totals$n.trts, events.trts = totals$events.trts, k.trts = NULL,
       reference.group = NULL, labels = labels)
}

# Per-treatment sample size and event totals from one row per study arm
# (columns treatment, n, events). Totals only when every arm reports them, so
# events / n stay consistent.
arm_totals <- function(arms, trts) {
  per_trt <- function(v) {
    if (anyNA(v)) return(NULL)
    tot <- tapply(v, factor(arms$treatment, trts), sum)
    stats::setNames(as.numeric(ifelse(is.na(tot), 0, tot)), trts)
  }
  n.trts <- per_trt(arms$n)
  list(n.trts = n.trts,
       events.trts = if (is.null(n.trts)) NULL else per_trt(arms$events))
}

# Extract nodes, edges, sample sizes and multi-arm designs from a netmeta object.
nma_network <- function(x) {
  trts <- x$trts
  if (is.null(trts)) trts <- sort(unique(c(x$treat1, x$treat2)))
  trts <- as.character(trts)

  t1 <- as.character(x$treat1)
  t2 <- as.character(x$treat2)
  studlab <- as.character(x$studlab)

  # unordered pair key so A:B and B:A count as the same comparison
  a <- pmin(t1, t2)
  b <- pmax(t1, t2)
  key <- paste(a, b, sep = "\r")
  pair_studies <- tapply(studlab, key, function(s) length(unique(s)))
  keys <- names(pair_studies)
  parts <- strsplit(keys, "\r", fixed = TRUE)
  edges <- data.frame(
    treat1 = vapply(parts, `[`, character(1), 1),
    treat2 = vapply(parts, `[`, character(1), 2),
    studies = as.integer(pair_studies),
    stringsAsFactors = FALSE
  )
  # keep treatment order of the netmeta object inside each pair
  swap <- match(edges$treat1, trts) > match(edges$treat2, trts)
  tmp <- edges$treat1[swap]
  edges$treat1[swap] <- edges$treat2[swap]
  edges$treat2[swap] <- tmp
  edges <- edges[order(match(edges$treat1, trts), match(edges$treat2, trts)), ,
                 drop = FALSE]
  rownames(edges) <- NULL

  # sample size per treatment: netmeta stores n.trts when n1/n2 were given
  n.trts <- x$n.trts
  has.n <- !is.null(n.trts) && !all(is.na(n.trts)) && any(n.trts > 0, na.rm = TRUE)
  if (has.n) {
    n.trts <- stats::setNames(as.numeric(n.trts), names(x$n.trts))
    if (is.null(names(n.trts)) || !all(trts %in% names(n.trts))) {
      n.trts <- stats::setNames(as.numeric(x$n.trts), trts)
    }
    n.trts <- n.trts[trts]
  } else {
    n.trts <- stats::setNames(rep(NA_real_, length(trts)), trts)
  }

  # events per treatment: netmeta stores events.trts for binary outcomes
  e.trts <- x$events.trts
  has.events <- !is.null(e.trts) && !all(is.na(e.trts)) &&
    any(e.trts > 0, na.rm = TRUE)
  if (has.events) {
    e.trts <- stats::setNames(as.numeric(e.trts), names(x$events.trts))
    if (is.null(names(e.trts)) || !all(trts %in% names(e.trts))) {
      e.trts <- stats::setNames(as.numeric(x$events.trts), trts)
    }
    e.trts <- e.trts[trts]
  } else {
    e.trts <- stats::setNames(rep(NA_real_, length(trts)), trts)
  }

  # number of studies per treatment
  k.trts <- x$k.trts
  if (is.null(k.trts) || is.null(names(k.trts)) || !all(trts %in% names(k.trts))) {
    k.trts <- vapply(trts, function(t) {
      length(unique(studlab[t1 == t | t2 == t]))
    }, numeric(1))
  }
  k.trts <- stats::setNames(as.numeric(k.trts[trts]), trts)

  # degree: number of distinct direct comparators
  degree <- vapply(trts, function(t) {
    sum(edges$treat1 == t | edges$treat2 == t)
  }, numeric(1))

  # multi-arm designs: studies with more than one row (choose(k, 2) rows)
  arms <- tapply(c(t1, t2), c(studlab, studlab), function(v) unique(v))
  multiarm <- Filter(function(v) length(v) > 2, as.list(arms))

  list(trts = trts, edges = edges, n.trts = n.trts, k.trts = k.trts,
       e.trts = e.trts, has.events = has.events,
       has.n = has.n, degree = degree, multiarm = multiarm)
}

# Complete a partial ordering with the remaining treatments.
complete_order <- function(order, trts) {
  if (is.null(order)) return(trts)
  order <- as.character(order)
  bad <- setdiff(order, trts)
  if (length(bad)) {
    stop("`order` contains treatments not in the network: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  c(order, setdiff(trts, order))
}

# Radius of the polygon / circle the treatments sit on. Grows with the number
# of treatments so that labels keep room; 1 for up to six treatments.
layout_radius <- function(n) 1 + 0.22 * max(0, n - 6)

# Points evenly spaced on a circle, starting at the top and going clockwise.
circle_coords <- function(n, radius = 1) {
  if (n == 0) return(matrix(numeric(0), ncol = 2))
  ang <- pi / 2 - 2 * pi * (seq_len(n) - 1) / n
  cbind(radius * cos(ang), radius * sin(ang))
}

# Compute node coordinates. Returns a matrix with row names = treatments.
nma_layout <- function(layout, trts, order, reference, net) {
  if (is.matrix(layout) || is.data.frame(layout)) {
    m <- as.matrix(layout)
    if (ncol(m) != 2 || nrow(m) != length(trts)) {
      stop("A custom `layout` must be a matrix with two columns and one row per treatment.",
           call. = FALSE)
    }
    if (is.null(rownames(m))) rownames(m) <- trts
    if (!all(trts %in% rownames(m))) {
      stop("Row names of a custom `layout` must be the treatment names.",
           call. = FALSE)
    }
    m <- m[trts, , drop = FALSE]
    # normalise to roughly the unit square so label offsets stay sensible
    m <- sweep(m, 2, colMeans(m))
    r <- max(abs(m))
    if (r > 0) m <- m / r
    return(m)
  }

  layout <- match.arg(layout, c("multi", "circle", "star"))
  n <- length(trts)

  if (layout == "multi") {
    m <- circle_coords(n, layout_radius(n))
    rownames(m) <- order
    return(m[trts, , drop = FALSE])
  }

  if (layout == "circle") {
    # most connected treatment at the top, then clockwise by decreasing degree
    ord <- trts[order(-net$degree, -net$k.trts, trts)]
    m <- circle_coords(n, layout_radius(n))
    rownames(m) <- ord
    return(m[trts, , drop = FALSE])
  }

  others <- setdiff(order, reference)
  m <- rbind(c(0, 0), circle_coords(length(others), layout_radius(length(others) + 1)))
  rownames(m) <- c(reference, others)
  m[trts, , drop = FALSE]
}

# Angle inside the sector [a0, a1] farthest from any of `edge_angles`.
pct_angle <- function(a0, a1, edge_angles) {
  mid <- (a0 + a1) / 2
  if (!length(edge_angles)) return(mid)
  cand <- seq(a0, a1, length.out = 13)[3:11]
  gap <- vapply(cand, function(a) {
    min(abs(((a - edge_angles + pi) %% (2 * pi)) - pi))
  }, numeric(1))
  # small preference for the middle of the sector when several are as good
  cand[which.max(gap - 0.02 * abs(cand - mid))]
}

# Linear rescale of v into [lo, hi]; constant vectors map to the midpoint.
rescale_range <- function(v, range) {
  v <- as.numeric(v)
  if (length(v) == 0) return(v)
  v[is.na(v)] <- min(v, na.rm = TRUE)
  lo <- range[1]
  hi <- range[2]
  if (diff(range(v)) == 0) return(rep((lo + hi) / 2, length(v)))
  lo + (v - min(v)) / (max(v) - min(v)) * (hi - lo)
}

# Resolve node_fill into one colour per treatment.
resolve_node_fill <- function(node_fill, trts, palette) {
  n <- length(trts)
  if (identical(node_fill, "auto")) {
    return(resolve_palette(palette, n))
  }
  if (!is.null(names(node_fill)) && all(trts %in% names(node_fill))) {
    return(unname(node_fill[trts]))
  }
  if (length(node_fill) == 1) return(rep(node_fill, n))
  if (length(node_fill) == n) return(unname(node_fill))
  stop("`node_fill` must be a single colour, \"auto\", or one colour per treatment (",
       n, ").", call. = FALSE)
}

# A palette name (hcl.colors) or a colour vector -> n colours.
resolve_palette <- function(palette, n) {
  if (length(palette) == 1 && palette %in% grDevices::hcl.pals()) {
    return(grDevices::hcl.colors(n, palette))
  }
  if (length(palette) >= n) return(palette[seq_len(n)])
  grDevices::colorRampPalette(palette)(n)
}

format_int <- function(v) {
  format(round(v), big.mark = ",", trim = TRUE, scientific = FALSE)
}

wrap_labels <- function(x, width) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

# One parallel segment per study (up to max_lines), offset perpendicular to the edge.
expand_multi_edges <- function(edges, max_lines, spacing) {
  out <- lapply(seq_len(nrow(edges)), function(i) {
    e <- edges[i, ]
    k <- min(e$studies, max_lines)
    dx <- e$xend - e$x
    dy <- e$yend - e$y
    len <- sqrt(dx^2 + dy^2)
    if (len == 0) len <- 1
    nx <- -dy / len
    ny <- dx / len
    off <- (seq_len(k) - (k + 1) / 2) * spacing
    data.frame(
      treat1 = e$treat1, treat2 = e$treat2, studies = e$studies,
      x = e$x + nx * off, y = e$y + ny * off,
      xend = e$xend + nx * off, yend = e$yend + ny * off,
      color = e$color, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

pretty_size_breaks <- function(size) {
  r <- range(size)
  if (diff(r) == 0) return(r[1])
  seq(r[1], r[2], length.out = 3)
}

# Angle (radians) at which to place each node label: the bisector of the
# largest gap between the directions of the node's edges. Nodes without edges
# use the radial fallback.
label_direction <- function(nodes, edges, fallback) {
  vapply(seq_len(nrow(nodes)), function(i) {
    t <- nodes$trt[i]
    nb <- c(edges$treat2[edges$treat1 == t], edges$treat1[edges$treat2 == t])
    nb <- nb[nb %in% nodes$trt]
    if (!length(nb)) return(fallback[i])
    j <- match(nb, nodes$trt)
    a <- sort(atan2(nodes$y[j] - nodes$y[i], nodes$x[j] - nodes$x[i]))
    if (length(a) == 1) return(a + pi)
    gaps <- diff(c(a, a[1] + 2 * pi))
    k <- which.max(gaps)
    a[k] + gaps[k] / 2
  }, numeric(1))
}

# Rectangular plotting window fitted to the nodes (with rings) and the
# estimated extent of their labels. `upi` = layout units per inch.
plot_window <- function(nodes, label_size, upi, margin) {
  chars <- vapply(strsplit(nodes$label, "\n", fixed = TRUE),
                  function(v) max(nchar(v)), numeric(1))
  tw <- chars * label_size * 0.55 / 72 * upi
  nlines <- vapply(strsplit(nodes$label, "\n", fixed = TRUE), length, numeric(1))
  th <- (nlines + 0.3) * 1.15 * label_size / 72 * upi
  left <- pmin(nodes$x - nodes$r_out, nodes$lx - tw * nodes$hjust)
  right <- pmax(nodes$x + nodes$r_out, nodes$lx + tw * (1 - nodes$hjust))
  bottom <- pmin(nodes$y - nodes$r_out, nodes$ly - th * nodes$vjust)
  top <- pmax(nodes$y + nodes$r_out, nodes$ly + th * (1 - nodes$vjust))
  xr <- range(c(left, right))
  yr <- range(c(bottom, top))
  padx <- margin * diff(xr)
  pady <- margin * diff(yr)
  xlim <- c(xr[1] - padx, xr[2] + padx)
  ylim <- c(yr[1] - pady, yr[2] + pady)
  list(xlim = xlim, ylim = ylim, hw = diff(xlim) / 2)
}

# Theme used by nmaplot().
nmaplot_theme <- function(grid, grid_color, background, title_size,
                          title_color, title_align, font_family) {
  hj <- if (title_align == "left") 0 else 0.5
  th <- theme_void(base_size = 12, base_family = font_family) +
    theme(
      plot.background = element_rect(fill = background, colour = NA),
      panel.background = element_rect(fill = background, colour = NA),
      plot.title = element_text(size = title_size, face = "bold", hjust = hj,
                                colour = title_color, family = font_family,
                                margin = margin(b = 3)),
      plot.subtitle = element_text(size = title_size * 0.68, hjust = hj,
                                   colour = title_color, lineheight = 1.05,
                                   family = font_family, margin = margin(b = 8)),
      plot.caption = element_text(size = title_size * 0.55, hjust = 1,
                                  face = "italic", colour = title_color,
                                  family = font_family, margin = margin(t = 4)),
      plot.margin = margin(14, 14, 10, 14),
      legend.position = "none"
    )
  if (isTRUE(grid)) {
    th <- th + theme(
      panel.grid.major = element_line(colour = grid_color, linewidth = 0.3),
      panel.grid.minor = element_line(colour = grid_color, linewidth = 0.15)
    )
  }
  th
}

# Write the plot to disk. Format from the file extension.
save_nmaplot <- function(p, file, width, height, dpi, background) {
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("png", "pdf", "tiff", "tif")) {
    stop("`file` must end in .png, .pdf, .tiff or .tif (got '", ext, "').",
         call. = FALSE)
  }
  dir <- dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (ext == "pdf") {
    ggsave(file, plot = p, width = width, height = height, units = "in",
           device = grDevices::cairo_pdf, bg = background)
  } else if (ext == "png") {
    dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
    ggsave(file, plot = p, width = width, height = height, units = "in",
           dpi = dpi, device = dev, bg = background)
  } else {
    dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_tiff else "tiff"
    ggsave(file, plot = p, width = width, height = height, units = "in",
           dpi = dpi, device = dev, bg = background, compression = "lzw")
  }
  invisible(file)
}

# Blend a colour towards mid grey by `amount` (0 = unchanged, 1 = grey).
soften <- function(col, amount) {
  rgb <- grDevices::col2rgb(col)
  mid <- c(128, 128, 128)
  out <- rgb * (1 - amount) + mid * amount
  grDevices::rgb(out[1], out[2], out[3], maxColorValue = 255)
}

# Black or white, whichever reads better on `fill`.
contrast_text <- function(fill) {
  rgb <- grDevices::col2rgb(fill)
  lum <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
  ifelse(lum > 0.6, "#1F2A44", "white")
}

# Edge-label circles that would overlap (typically two diagonals crossing at
# the centre) are pushed apart along their own edges.
spread_edge_labels <- function(edges, rc) {
  n <- nrow(edges)
  if (n < 2) return(edges)
  ex <- edges$xend - edges$x
  ey <- edges$yend - edges$y
  el <- sqrt(ex^2 + ey^2)
  el[el == 0] <- 1
  for (it in 1:3) {
    moved <- FALSE
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      d <- sqrt((edges$mx[i] - edges$mx[j])^2 + (edges$my[i] - edges$my[j])^2)
      need <- (rc[i] + rc[j]) * 1.15
      if (d < need) {
        shift <- (need - d) / 2 + 0.5 * rc[i]
        edges$mx[i] <- edges$mx[i] + ex[i] / el[i] * shift
        edges$my[i] <- edges$my[i] + ey[i] / el[i] * shift
        edges$mx[j] <- edges$mx[j] - ex[j] / el[j] * shift
        edges$my[j] <- edges$my[j] - ey[j] / el[j] * shift
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  edges
}
