# Geometry and legend helpers for nmaplot(): node discs, subgroup rings and
# the bottom legend panel. Nothing here is exported.

# Closed polygon approximating a circle.
circle_poly <- function(cx, cy, r, n = 96) {
  a <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  data.frame(x = cx + r * cos(a), y = cy + r * sin(a))
}

# Annulus sector between radii r0 < r1 and angles a0 -> a1 (radians).
sector_poly <- function(cx, cy, r0, r1, a0, a1, n = 64) {
  k <- max(3, ceiling(n * abs(a1 - a0) / (2 * pi)))
  a <- seq(a0, a1, length.out = k)
  data.frame(x = c(cx + r1 * cos(a), cx + r0 * cos(rev(a))),
             y = c(cy + r1 * sin(a), cy + r0 * sin(rev(a))))
}

# Turn the `ring` argument into one row per (treatment, group) with proportions
# and start/end angles (clockwise from the top, like a pie chart).
normalise_ring <- function(ring, trts) {
  wide <- is.matrix(ring) ||
    (is.data.frame(ring) && all(vapply(ring, is.numeric, logical(1))) &&
       !is.null(rownames(ring)) && any(rownames(ring) %in% trts))
  if (wide) {
    m <- as.matrix(ring)
    groups <- colnames(m)
    if (is.null(groups)) groups <- paste0("Group ", seq_len(ncol(m)))
    long <- data.frame(treatment = rep(rownames(m), times = ncol(m)),
                       group = rep(groups, each = nrow(m)),
                       value = as.numeric(m), stringsAsFactors = FALSE)
  } else {
    if (!is.data.frame(ring) || ncol(ring) < 3)
      stop("`ring` must be a long data frame (treatment, group, value) or a wide matrix.",
           call. = FALSE)
    long <- data.frame(treatment = as.character(ring[[1]]),
                       group = ring[[2]],
                       value = as.numeric(ring[[3]]), stringsAsFactors = FALSE)
    groups <- if (is.factor(long$group)) levels(long$group) else
      unique(as.character(long$group))
    long$group <- as.character(long$group)
  }
  bad <- setdiff(unique(long$treatment), trts)
  if (length(bad)) warning("`ring` treatments not in the network: ",
                           paste(bad, collapse = ", "), call. = FALSE)
  long <- long[long$treatment %in% trts & !is.na(long$value), , drop = FALSE]
  if (!nrow(long)) return(list(data = NULL, groups = groups))
  long$group <- factor(long$group, levels = groups)
  long <- long[order(match(long$treatment, trts), long$group), , drop = FALSE]
  out <- do.call(rbind, lapply(split(long, long$treatment), function(d) {
    tot <- sum(d$value)
    d$prop <- if (tot > 0) d$value / tot else 0
    cum <- cumsum(c(0, d$prop))
    d$a0 <- pi / 2 - 2 * pi * cum[-length(cum)]
    d$a1 <- pi / 2 - 2 * pi * cum[-1]
    d
  }))
  rownames(out) <- NULL
  out <- out[out$prop > 0, , drop = FALSE]
  out$group <- as.character(out$group)
  list(data = out, groups = groups)
}

resolve_ring_colors <- function(ring_colors, groups) {
  default <- c("#6F93B8", "#E39A99", "#9FC29A", "#E8C36A", "#B39DDB", "#F4A261")
  if (is.null(ring_colors)) {
    cols <- if (length(groups) <= length(default)) default[seq_along(groups)] else
      grDevices::hcl.colors(length(groups), "Set 2")
    return(stats::setNames(cols, groups))
  }
  if (!is.null(names(ring_colors)) && all(groups %in% names(ring_colors)))
    return(ring_colors[groups])
  if (length(ring_colors) < length(groups))
    stop("`ring_colors` needs one colour per ring group (", length(groups), ").",
         call. = FALSE)
  stats::setNames(ring_colors[seq_along(groups)], groups)
}

# Legend panel drawn in data space under the network: one framed box with
# centred sections for node size, the circled study count and (if present)
# the ring groups.
build_legend <- function(nodes, edges, rings, ring_groups, size_label,
                         edge_width_range, ring_title, win, has_n, rc) {
  centre <- if (!is.null(nodes$in_legend) && any(nodes$in_legend)) nodes[which(nodes$in_legend)[1], ] else NULL
  n_sec <- 2 + (!is.null(centre)) + (!is.null(rings))
  lim <- win$hw
  gap <- 0.04 * lim
  top <- win$ylim[1] - gap
  hgt <- 0.34 * lim
  bottom <- top - hgt
  wsec <- diff(win$xlim) / n_sec
  xs <- win$xlim[1] + (seq_len(n_sec) - 1) * wsec
  cxs <- xs + wsec / 2
  boxes <- data.frame(xmin = win$xlim[1], xmax = win$xlim[2], ymin = bottom, ymax = top)
  dividers <- data.frame(x = xs[-1], xend = xs[-1], y = bottom, yend = top)
  pad <- 0.04 * lim
  ty <- top - pad - 0.025 * lim            # title line
  cy <- (bottom + ty - 0.04 * lim) / 2     # middle of the item area
  text <- list()
  circles <- list()
  lines <- list()
  squares <- NULL

  # section 1: node size (two discs with their n, centred as a group)
  txt1 <- if (is.null(size_label)) "Node size" else size_label
  txt1 <- sub("number of participants", "participants", txt1, fixed = TRUE)
  text[[1]] <- data.frame(x = cxs[1], y = ty, label = txt1,
                          face = "bold", hjust = 0.5, stringsAsFactors = FALSE)
  sc <- min(1, 0.075 * wsec / max(nodes$size))
  rmax <- max(nodes$size) * sc
  rmin <- min(nodes$size) * sc
  pre <- if (isTRUE(has_n)) "n = " else "k = "
  lab_big <- if (!is.null(nodes$size_driver)) paste0(pre, format_int(max(nodes$size_driver))) else "largest"
  lab_small <- if (!is.null(nodes$size_driver)) paste0(pre, format_int(min(nodes$size_driver))) else "smallest"
  tw <- 0.22 * wsec                        # room for the text after each disc
  grp <- 2 * rmax + 0.02 * lim + tw + (if (rmin < rmax) 0.06 * wsec + 2 * rmin + 0.02 * lim + tw else 0)
  x0 <- cxs[1] - grp / 2
  cx1 <- x0 + rmax
  c1 <- circle_poly(cx1, cy, rmax)
  c1$id <- "L1"
  c1$fill <- "#C9CDD2"
  circles[[1]] <- c1
  text[[2]] <- data.frame(x = cx1 + rmax + 0.02 * lim, y = cy,
                          label = paste0(lab_big, "\n(larger node)"),
                          face = "plain", hjust = 0, stringsAsFactors = FALSE)
  if (rmin < rmax) {
    cx2 <- cx1 + rmax + 0.02 * lim + tw + 0.06 * wsec + rmin
    c2 <- circle_poly(cx2, cy, rmin)
    c2$id <- "L2"
    c2$fill <- "#C9CDD2"
    circles[[2]] <- c2
    text[[3]] <- data.frame(x = cx2 + rmin + 0.02 * lim, y = cy,
                            label = paste0(lab_small, "\n(smaller node)"),
                            face = "plain", hjust = 0, stringsAsFactors = FALSE)
  }

  # section 2: one line with the circled study count, centred as a group
  text[[4]] <- data.frame(x = cxs[2], y = ty,
                          label = "Circled number = direct studies",
                          face = "bold", hjust = 0.5, stringsAsFactors = FALSE)
  llen <- 0.34 * wsec
  grp2 <- llen + 0.03 * lim + 0.36 * wsec
  lx0 <- cxs[2] - grp2 / 2
  lx1 <- lx0 + llen
  lines[[1]] <- data.frame(x = lx0, xend = lx1, y = cy, yend = cy,
                           width = if (nrow(edges)) mean(range(edges$width)) else 1)
  kex <- if (nrow(edges)) stats::median(edges$studies) else 1
  dot <- circle_poly((lx0 + lx1) / 2, cy, rc, n = 48)
  dot$id <- "L"
  dot_text <- data.frame(x = (lx0 + lx1) / 2, y = cy, label = round(kex),
                         stringsAsFactors = FALSE)
  text[[5]] <- data.frame(x = lx1 + 0.03 * lim, y = cy,
                          label = "studies comparing\nthe two treatments",
                          face = "plain", hjust = 0, stringsAsFactors = FALSE)

  # section 3 (star layout): the centre node, described here instead of on the plot
  isec <- 3
  if (!is.null(centre)) {
    text[[8]] <- data.frame(x = cxs[isec], y = ty, label = "Centre node = reference",
                            face = "bold", hjust = 0.5, stringsAsFactors = FALSE)
    rcen <- min(centre$size, 0.075 * wsec)
    ctxt <- if (nzchar(centre$nline)) paste(centre$name, centre$nline, sep = "\n") else centre$name
    grp3 <- 2 * rcen + 0.03 * lim + 0.45 * wsec
    cx3 <- cxs[isec] - grp3 / 2 + rcen
    c3 <- circle_poly(cx3, cy, rcen)
    c3$id <- "L3"
    c3$fill <- centre$fill
    circles[[3]] <- c3
    text[[9]] <- data.frame(x = cx3 + rcen + 0.03 * lim, y = cy, label = ctxt,
                            face = "plain", hjust = 0, stringsAsFactors = FALSE)
    isec <- 4
  }

  # ring groups, stacked and centred as a block
  if (!is.null(rings)) {
    text[[6]] <- data.frame(x = cxs[isec], y = ty, label = ring_title,
                            face = "bold", hjust = 0.5, stringsAsFactors = FALSE)
    cols <- unique(rings[, c("group", "fill")])
    cols <- cols[match(ring_groups, cols$group), , drop = FALSE]
    cols <- cols[!is.na(cols$group), , drop = FALSE]
    k <- nrow(cols)
    step <- min(0.10 * lim, (ty - 0.05 * lim - bottom - pad) / max(k, 1))
    ys <- cy + (k - 1) * step / 2 - (seq_len(k) - 1) * step
    sq <- 0.028 * lim
    wtxt <- max(nchar(cols$group)) * 0.55 * 11 / 72 * (2 * lim) / 9
    blk <- 2 * sq + 0.03 * lim + wtxt
    bx0 <- cxs[isec] - blk / 2
    squares <- data.frame(xmin = bx0, xmax = bx0 + 2 * sq,
                          ymin = ys - sq, ymax = ys + sq, fill = cols$fill,
                          stringsAsFactors = FALSE)
    text[[7]] <- data.frame(x = bx0 + 2 * sq + 0.03 * lim, y = ys,
                            label = cols$group, face = "plain", hjust = 0,
                            stringsAsFactors = FALSE)
  }

  list(boxes = boxes, dividers = dividers, circles = do.call(rbind, circles),
       lines = do.call(rbind, lines), dot = dot, dot_text = dot_text,
       squares = squares, text = do.call(rbind, text), ylo = bottom - gap,
       n_sec = n_sec)
}
