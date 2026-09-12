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
  out$show_pct <- TRUE
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
# centred sections (node size, circled study count, centre node for the star
# layout, ring groups). Every section has a shaded header carrying a bold name
# and a lighter descriptor.
build_legend <- function(nodes, edges, rings, ring_groups, size_label,
                         edge_width_range, ring_title, win, has_n, rc,
                         ring_name = NULL, reference_fill = "#8A939B",
                         auto_ring = FALSE) {
  centre <- if (!is.null(nodes$in_legend) && any(nodes$in_legend)) nodes[which(nodes$in_legend)[1], ] else NULL
  n_sec <- 2 + (!is.null(centre)) + (!is.null(rings))
  lim <- win$hw
  gap <- 0.04 * lim
  top <- win$ylim[1] - gap
  hgt <- 0.46 * lim
  bottom <- top - hgt
  wsec <- diff(win$xlim) / n_sec
  xs <- win$xlim[1] + (seq_len(n_sec) - 1) * wsec
  cxs <- xs + wsec / 2
  boxes <- data.frame(xmin = win$xlim[1], xmax = win$xlim[2], ymin = bottom, ymax = top)
  dividers <- data.frame(x = xs[-1], xend = xs[-1], y = bottom, yend = top)
  pad <- 0.04 * lim
  ty <- top - 0.055 * lim                    # section name
  ty2 <- ty - 0.05 * lim                     # descriptor, close under it
  hb <- ty2 - 0.03 * lim                     # bottom of the shaded header band
  headers <- data.frame(xmin = xs, xmax = xs + wsec, ymin = hb, ymax = top,
                        stringsAsFactors = FALSE)
  cy <- (bottom + hb) / 2                    # middle of the item area
  text <- list()
  circles <- list()
  lines <- list()
  squares <- NULL
  ringex <- NULL

  hdr <- function(i, name, what) {
    rbind(data.frame(x = cxs[i], y = ty, label = name, face = "bold", hjust = 0.5,
                     sz = 1.45, stringsAsFactors = FALSE),
          data.frame(x = cxs[i], y = ty2, label = what, face = "plain", hjust = 0.5,
                     sz = 0.95, stringsAsFactors = FALSE))
  }
  item <- function(x, y, label) {
    data.frame(x = x, y = y, label = label, face = "plain", hjust = 0,
               sz = 1, stringsAsFactors = FALSE)
  }

  # section 1: one node, labelled the way the plot labels them
  what1 <- if (is.null(size_label)) "" else
    sub("^Node size = (number of )?", "", size_label)
  what1 <- paste0(toupper(substr(what1, 1, 1)), substr(what1, 2, nchar(what1)))
  text[[1]] <- hdr(1, "Node size", what1)
  by_patients <- !is.null(size_label) && grepl("participants", size_label, fixed = TRUE)
  sc <- min(1, 0.085 * wsec / max(nodes$size))
  rex1 <- max(nodes$size) * sc
  pre <- if (isTRUE(has_n)) "n = " else "k = "
  big <- if (!is.null(nodes$size_driver)) max(nodes$size_driver) else NA
  lab_big <- if (!is.na(big)) {
    if (by_patients) paste0(pre, format_int(big), " patients")
    else paste0(pre, format_int(big))
  } else "largest node"
  note <- if (by_patients) "% inside: share of\nall patients" else
    "largest node in the network"
  grp <- 2 * rex1 + 0.03 * lim + 0.44 * wsec
  cx1 <- cxs[1] - grp / 2 + rex1
  c1 <- circle_poly(cx1, cy, rex1)
  c1$id <- "L1"
  c1$fill <- dominant_fill(nodes$fill)
  circles[[1]] <- c1
  text[[2]] <- item(cx1 + rex1 + 0.03 * lim, cy, paste0(lab_big, "\n", note))
  # the example disc carries its share inside, as the plot's nodes do
  inside <- if (by_patients && !is.na(big)) {
    data.frame(x = cx1, y = cy,
               label = paste0(round(100 * big / sum(nodes$size_driver, na.rm = TRUE)), "%"),
               colour = share_text_color(c1$fill[1]), stringsAsFactors = FALSE)
  } else NULL

  # section 2: one line with the circled study count
  text[[3]] <- hdr(2, "Circled number", "Direct studies")
  llen <- 0.30 * wsec
  grp2 <- llen + 0.03 * lim + 0.38 * wsec
  lx0 <- cxs[2] - grp2 / 2
  lx1 <- lx0 + llen
  lines[[1]] <- data.frame(x = lx0, xend = lx1, y = cy, yend = cy,
                           width = if (nrow(edges)) mean(range(edges$width)) else 1)
  kex <- if (nrow(edges)) stats::median(edges$studies) else 1
  dot <- circle_poly((lx0 + lx1) / 2, cy, rc, n = 48)
  dot$id <- "L"
  dot_text <- data.frame(x = (lx0 + lx1) / 2, y = cy, label = round(kex),
                         stringsAsFactors = FALSE)
  text[[4]] <- item(lx1 + 0.03 * lim, cy,
                    paste0("studies comparing", "\n", "the two treatments"))

  # section 3 (star layout): the centre node
  isec <- 3
  if (!is.null(centre)) {
    text[[5]] <- hdr(isec, "Centre node", "Reference")
    rcen <- min(centre$size, 0.07 * wsec)
    ctxt <- if (nzchar(centre$nline)) paste0(centre$name, "\n", centre$nline) else centre$name
    grp3 <- 2 * rcen + 0.03 * lim + 0.45 * wsec
    cx3 <- cxs[isec] - grp3 / 2 + rcen
    c3 <- circle_poly(cx3, cy, rcen)
    c3$id <- "L3"
    c3$fill <- centre$fill
    circles[[3]] <- c3
    text[[6]] <- item(cx3 + rcen + 0.03 * lim, cy, ctxt)
    isec <- 4
  }

  # ring groups: a small ringed disc showing every colour, then the squares
  if (!is.null(rings)) {
    what4 <- if (!is.null(ring_name)) ring_name else
      sub("^Outer ring = ", "", ring_title)
    text[[7]] <- hdr(isec, "Outer ring", what4)
    cols <- unique(rings[, c("group", "fill")])
    cols <- cols[match(ring_groups, cols$group), , drop = FALSE]
    cols <- cols[!is.na(cols$group), , drop = FALSE]
    k <- nrow(cols)
    step <- min(0.10 * lim, (hb - bottom - pad) / max(k, 1))
    ys <- cy + (k - 1) * step / 2 - (seq_len(k) - 1) * step
    sq <- 0.028 * lim
    rex <- 0.06 * wsec
    # automatic ring: the Events colour is the event/n count of the labels
    glab <- if (isTRUE(auto_ring))
      ifelse(cols$group == "Events", "Events (event/n)", cols$group) else cols$group
    wtxt <- max(nchar(glab)) * 0.55 * 11 / 72 * (2 * lim) / 9
    blk <- 2 * rex * 1.5 + 0.06 * lim + 2 * sq + 0.03 * lim + wtxt
    bx0 <- cxs[isec] - blk / 2
    ex_c <- circle_poly(bx0 + rex * 1.5, cy, rex)
    ex_c$id <- "EX0"
    ex_c$fill <- dominant_fill(nodes$fill)
    circles[[4]] <- ex_c
    ang <- pi / 2 - 2 * pi * seq(0, 1, length.out = k + 1)
    ringex <- do.call(rbind, lapply(seq_len(k), function(i) {
      d <- sector_poly(bx0 + rex * 1.5, cy, rex * 1.06, rex * 1.5, ang[i], ang[i + 1])
      d$id <- paste0("EX", i)
      d$fill <- cols$fill[i]
      d
    }))
    sx0 <- bx0 + 2 * rex * 1.5 + 0.06 * lim
    squares <- data.frame(xmin = sx0, xmax = sx0 + 2 * sq,
                          ymin = ys - sq, ymax = ys + sq, fill = cols$fill,
                          stringsAsFactors = FALSE)
    text[[8]] <- data.frame(x = sx0 + 2 * sq + 0.03 * lim, y = ys,
                            label = glab, face = "plain", hjust = 0, sz = 1,
                            stringsAsFactors = FALSE)
  }

  list(boxes = boxes, headers = headers, dividers = dividers,
       circles = do.call(rbind, circles), lines = do.call(rbind, lines),
       dot = dot, dot_text = dot_text, squares = squares, ringex = ringex,
       text = do.call(rbind, text), inside = inside, ylo = bottom - gap,
       n_sec = n_sec)
}
