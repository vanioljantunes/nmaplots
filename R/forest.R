#' Forest plot of the relative effects of a gemtc network meta-analysis
#'
#' `nmaforest()` draws what [gemtc::mtc.run()] estimated: one row per
#' comparison, the posterior median and its credible interval, and, next to
#' them, the direct evidence behind that pair -- how many studies randomised
#' the two treatments against each other and how many patients they carried.
#' Treatment names are printed as names: underscores become spaces, and the
#' descriptions [nma_gemtc()] keeps are used when they are there.
#'
#' Three sets of comparisons, chosen with `comparisons`:
#' \describe{
#'   \item{`"panels"`}{every pairwise comparison, in one block per comparator,
#'     each headed "Compared with ..." (the default).}
#'   \item{`"reference"`}{one block, every treatment against `reference` --
#'     the layout [gemtc::forest()] draws.}
#'   \item{`"pairs"`}{one flat block, one row per unordered pair, "A vs B".}
#' }
#'
#' Two looks, chosen with `style`: `"jama"` (serif, open markers, hairline
#' rules) and `"revman"` (sans, filled squares sized by the direct sample
#' size, events over totals when the outcome is binary).
#'
#' @param x The result of [gemtc::mtc.run()], or a table from
#'   [gemtc::relative.effect.table()].
#' @param comparisons Which comparisons to draw: `"panels"`, `"reference"` or
#'   `"pairs"`. See Details.
#' @param reference Treatment the rows are compared with when
#'   `comparisons = "reference"`, and the first block otherwise. Defaults to
#'   the first treatment of the network. The name as you use it is accepted.
#' @param order Character vector putting the treatments in the order you want;
#'   the ones you leave out keep their network order.
#' @param level Credible level of the interval. Ignored, with a warning, when
#'   `x` is a relative effect table, which only holds the 95% interval.
#' @param sm Name of the effect measure for the column head. Defaults to what
#'   the likelihood and link imply ("Risk Ratio", "Odds Ratio", ...).
#' @param exponentiate Logical. Back-transform the estimates. Defaults to
#'   `TRUE` for logarithmic links (logit, log, cloglog) and `FALSE` otherwise.
#' @param style `"jama"` or `"revman"`.
#' @param labels Optional character vector of display names for the
#'   treatments, named by treatment or in the order of the network.
#' @param clean_labels Logical. Turn underscores in the names into spaces.
#' @param label_wrap Wrap the names at this many characters, `NULL` for no
#'   wrapping. Wrapped names need a larger `row_height`.
#' @param columns Which count columns to show: any of `"studies"`, `"n"` and
#'   `"events"`, or `"none"`. `"events"` replaces `"n"` with events over
#'   total, and is dropped when the network has no event counts. The counts
#'   are the direct evidence for that pair alone; a pair with only indirect
#'   evidence shows a dash.
#' @param column_labels Optional column heads, one per column shown, replacing
#'   the defaults.
#' @param digits Decimal places of the effect and its interval.
#' @param sort Order of the rows inside a block: `"none"`, `"effect"` or
#'   `"label"`.
#' @param xlim Limits of the effect axis, on the reporting scale (for example
#'   `c(0.5, 4)`). Defaults to the range of the intervals, cut at `clip`.
#' @param xticks Tick positions on the reporting scale.
#' @param clip Widest interval to draw, as a multiple of (or distance from)
#'   the null value; intervals running past it get an arrowhead. `Inf` draws
#'   everything.
#' @param ref_line Position of the vertical null line. Defaults to 1 on a
#'   ratio scale and 0 otherwise. `NA` leaves it out.
#' @param favours Two strings printed under the axis, left and right, such as
#'   `c("Favours treatment", "Favours comparator")`.
#' @param outcome One line under the title naming the outcome.
#' @param title,subtitle,caption Title block. `subtitle` defaults to
#'   `outcome`.
#' @param title_size,title_color,title_align Title block size (points),
#'   colour and alignment (`"left"` or `"center"`).
#' @param font_family Font family. Defaults to `"serif"` for the JAMA style
#'   and `"sans"` for RevMan.
#' @param text_size Font size (points) of the rows.
#' @param label_color Colour of the row text.
#' @param point_color,point_fill,point_size Marker colour, fill and size. The
#'   RevMan style sizes the marker by the direct sample size instead.
#' @param line_width Width of the interval lines.
#' @param band_fill Fill of the alternating row bands, `NA` for none.
#' @param rule_color Colour of the axis and the rules.
#' @param row_height Height of one row, in inches, used when `height` is not
#'   given.
#' @param label_width,column_width,effect_width Widths of the name column, of
#'   one count column and of the effect column, as fractions of the figure
#'   width. What is left over is the plotting band. `NULL` picks a width that
#'   suits the columns shown.
#' @param background Background colour.
#' @param file Path(s) to write: `.png`, `.pdf`, `.tiff`. `NULL` only returns
#'   the plot.
#' @param width,height,dpi Size in inches and resolution of the saved figure.
#'   `height` defaults to the number of rows times `row_height`.
#'
#' @return A `ggplot` object. The tables used for drawing are in
#'   `p$nmaforest$rows` and `p$nmaforest$pairs`.
#'
#' @seealso [nmaplot()] for the evidence network, [nma_gemtc()] for building
#'   the network with the treatment names as they are.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("gemtc", quietly = TRUE) &&
#'     requireNamespace("rjags", quietly = TRUE)) {
#'   data(mash)
#'   d <- mash$fib_improvement_alldoses
#'   network <- nma_gemtc(d[, c("study", "treatment", "responders", "sampleSize")])
#'   model <- gemtc::mtc.model(network, likelihood = "binom", link = "log")
#'   result <- gemtc::mtc.run(model, n.adapt = 1000, n.iter = 4000)
#'   nmaforest(result, comparisons = "reference", reference = "Placebo",
#'             outcome = "Fibrosis improvement")
#' }
#' }
#' @export
nmaforest <- function(x,
                      comparisons = c("panels", "reference", "pairs"),
                      reference = NULL,
                      order = NULL,
                      level = 0.95,
                      sm = NULL,
                      exponentiate = NULL,
                      style = c("jama", "revman"),
                      labels = NULL,
                      clean_labels = TRUE,
                      label_wrap = NULL,
                      columns = c("studies", "n"),
                      column_labels = NULL,
                      digits = 2,
                      sort = c("none", "effect", "label"),
                      xlim = NULL,
                      xticks = NULL,
                      clip = 10,
                      ref_line = NULL,
                      favours = NULL,
                      outcome = NULL,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      title_size = 18,
                      title_color = "#1F2A44",
                      title_align = c("left", "center"),
                      font_family = NULL,
                      text_size = 10,
                      label_color = "#1F2A44",
                      point_color = "#1F2A44",
                      point_fill = NULL,
                      point_size = 2.4,
                      line_width = 0.5,
                      band_fill = "#F4F6F8",
                      rule_color = "#8A939B",
                      row_height = 0.26,
                      label_width = NULL,
                      column_width = NULL,
                      effect_width = 0.19,
                      background = "white",
                      file = NULL,
                      width = 10,
                      height = NULL,
                      dpi = 300) {

  if (!requireNamespace("gemtc", quietly = TRUE)) {
    stop("Package 'gemtc' is needed for nmaforest(): install.packages(\"gemtc\").",
         call. = FALSE)
  }
  comparisons <- match.arg(comparisons)
  style <- match.arg(style)
  sort <- match.arg(sort)
  title_align <- match.arg(title_align)
  if (identical(columns, "none") || is.null(columns)) columns <- character(0)
  if (length(columns)) {
    columns <- match.arg(columns, c("studies", "n", "events"),
                         several.ok = TRUE)
  }
  if (is.null(font_family)) font_family <- if (style == "jama") "serif" else "sans"
  if (is.null(point_fill)) point_fill <- if (style == "jama") "white" else point_color

  input <- forest_input(x)

  # names as you use them: the gemtc descriptions, or anything given in `labels`
  if (is.null(labels) && !is.null(input$labels)) labels <- input$labels
  to_id <- function(v) {
    if (is.null(v) || is.null(input$labels)) return(v)
    hit <- match(v, input$labels)
    ifelse(is.na(hit), v, names(input$labels)[hit])
  }
  reference <- to_id(reference)
  order <- to_id(order)

  trts <- complete_order(order, input$trts)
  if (is.null(reference)) reference <- trts[1]
  if (!reference %in% trts) {
    stop("`reference` is not a treatment of the network: ", reference,
         call. = FALSE)
  }
  if (comparisons != "pairs") trts <- c(reference, setdiff(trts, reference))
  name <- forest_labels(trts, labels, clean_labels, label_wrap)

  if (is.null(exponentiate)) exponentiate <- isTRUE(input$log_scale)
  log_scale <- isTRUE(exponentiate) && isTRUE(input$log_scale)
  null_value <- if (log_scale) 1 else 0
  if (is.null(ref_line)) ref_line <- null_value
  if (is.null(sm)) sm <- input$sm

  pairs <- forest_pairs(comparisons, trts, reference)
  pairs <- cbind(pairs, forest_estimates(input, pairs, level, exponentiate))

  arms <- gemtc_arms(input$network)
  pairs <- cbind(pairs, direct_counts(arms, pairs))
  pairs$idx <- seq_len(nrow(pairs))

  has_events <- !is.null(arms) && nrow(arms) && !all(is.na(arms$events))
  if ("events" %in% columns && !has_events) columns <- setdiff(columns, "events")
  if ("events" %in% columns) columns <- setdiff(columns, "n")

  # ---- rows ------------------------------------------------------------------
  blocks <- if (comparisons == "pairs") list(pairs) else
    split(pairs, factor(pairs$comparator, unique(pairs$comparator)))

  parts <- list()
  y <- 0
  for (b in seq_along(blocks)) {
    d <- blocks[[b]]
    if (sort == "effect") d <- d[order(d$est), , drop = FALSE]
    if (sort == "label") d <- d[order(name[d$treatment]), , drop = FALSE]
    if (comparisons != "pairs") {
      y <- y - 1
      parts[[length(parts) + 1]] <- data.frame(
        y = y, kind = "header", idx = NA_integer_,
        label = paste("Compared with", name[d$comparator[1]]),
        stringsAsFactors = FALSE)
      lab <- unname(name[d$treatment])
    } else {
      lab <- paste(unname(name[d$treatment]), "vs", unname(name[d$comparator]))
    }
    parts[[length(parts) + 1]] <- data.frame(
      y = y - seq_len(nrow(d)), kind = "row", idx = d$idx, label = lab,
      stringsAsFactors = FALSE)
    y <- y - nrow(d)
    if (b < length(blocks)) y <- y - 0.4
  }
  rows <- do.call(rbind, parts)
  body <- rows[rows$kind == "row", , drop = FALSE]
  body <- cbind(body, pairs[body$idx, c("comparator", "treatment", "est",
                                        "lower", "upper", "k", "n1", "n2",
                                        "e1", "e2")])

  # ---- geometry --------------------------------------------------------------
  cols <- character(0)
  if ("studies" %in% columns) cols <- c(cols, "k")
  if ("n" %in% columns) cols <- c(cols, "n1", "n2")
  if ("events" %in% columns) cols <- c(cols, "en1", "en2")

  if (is.null(label_width)) label_width <- if (length(cols)) 0.30 else 0.40
  # events over totals need a wider column than a plain count
  if (is.null(column_width)) {
    column_width <- if ("events" %in% columns) 0.11 else 0.085
  }
  gap <- 0.012
  col_right <- if (length(cols))
    label_width + seq_along(cols) * column_width - gap else numeric(0)
  plot_x0 <- label_width + length(cols) * column_width + gap
  plot_x1 <- 1 - effect_width
  if (plot_x1 - plot_x0 < 0.12) {
    stop("No room left for the plotting band: lower `label_width` (",
         label_width, "), `column_width` (", column_width, ") or ",
         "`effect_width` (", effect_width, ").", call. = FALSE)
  }

  tx <- function(v) if (log_scale) log(v) else v
  lim <- if (!is.null(xlim)) range(tx(xlim)) else
    forest_limits(body$lower, body$upper, null_value, clip, log_scale)
  map_x <- function(v) plot_x0 + (tx(v) - lim[1]) / diff(lim) * (plot_x1 - plot_x0)
  map_t <- function(v) plot_x0 + (v - lim[1]) / diff(lim) * (plot_x1 - plot_x0)

  body$cut_lo <- tx(body$lower) < lim[1]
  body$cut_hi <- tx(body$upper) > lim[2]
  body$xlo <- map_t(pmax(tx(body$lower), lim[1]))
  body$xhi <- map_t(pmin(tx(body$upper), lim[2]))
  body$x <- map_t(pmin(pmax(tx(body$est), lim[1]), lim[2]))

  body$psize <- point_size
  if (style == "revman") {
    tot <- body$n1 + body$n2
    if (any(is.finite(tot))) {
      fill_in <- ifelse(is.finite(tot), tot, stats::median(tot, na.rm = TRUE))
      body$psize <- rescale_range(sqrt(fill_in),
                                  c(point_size * 0.6, point_size * 1.7))
    }
  }

  # ---- text ------------------------------------------------------------------
  body$eff_txt <- format_effect(body$est, body$lower, body$upper, digits)
  txt <- list(k = format_count(body$k),
              n1 = format_count(body$n1), n2 = format_count(body$n2),
              en1 = format_events(body$e1, body$n1),
              en2 = format_events(body$e2, body$n2))

  head_default <- c(k = "Studies",
                    n1 = "Patients\nintervention", n2 = "Patients\ncontrol",
                    en1 = "Events/total\nintervention",
                    en2 = "Events/total\ncontrol")
  heads <- unname(head_default[cols])
  if (!is.null(column_labels)) {
    if (length(column_labels) != length(cols)) {
      stop("`column_labels` needs one entry per column shown (", length(cols),
           ").", call. = FALSE)
    }
    heads <- as.character(column_labels)
  }
  label_head <- if (comparisons == "pairs") "Comparison" else "Treatment"
  eff_head <- paste0(sm, " (", round(100 * level), "% CrI)")

  y_head <- 0.9
  y_bottom <- min(rows$y)
  y_axis <- y_bottom - 1.1
  y_favours <- y_axis - 1.2
  y_min <- if (!is.null(favours)) y_favours - 0.5 else y_axis - 1.0

  # ---- plot ------------------------------------------------------------------
  p <- ggplot()

  if (!is.na(band_fill) && style == "jama" && nrow(body)) {
    band <- body[seq(1, nrow(body), by = 2), , drop = FALSE]
    p <- p + geom_rect(data = band,
                       aes(xmin = 0, xmax = 1,
                           ymin = .data$y - 0.5, ymax = .data$y + 0.5),
                       fill = band_fill, colour = NA)
  }

  head_df <- data.frame(
    x = c(0, col_right, 1),
    label = c(label_head, heads, eff_head),
    hjust = c(0, rep(1, length(cols)), 1),
    stringsAsFactors = FALSE)
  p <- p +
    geom_text(data = head_df,
              aes(x = .data$x, y = y_head, label = .data$label,
                  hjust = .data$hjust),
              vjust = 0.5, size = text_size * 0.95 / ggplot2::.pt,
              fontface = "bold", colour = label_color, family = font_family,
              lineheight = 0.95) +
    geom_segment(aes(x = 0, xend = 1, y = y_head - 0.55, yend = y_head - 0.55),
                 colour = rule_color, linewidth = 0.4)

  if (!is.na(ref_line)) {
    p <- p + geom_segment(aes(x = map_x(ref_line), xend = map_x(ref_line),
                              y = y_bottom - 0.4, yend = y_head - 0.6),
                          colour = rule_color,
                          linewidth = if (style == "jama") 0.35 else 0.45)
  }

  head_rows <- rows[rows$kind == "header", , drop = FALSE]
  if (nrow(head_rows)) {
    p <- p + geom_text(data = head_rows,
                       aes(x = 0, y = .data$y, label = .data$label),
                       hjust = 0, vjust = 0.5, fontface = "bold",
                       size = text_size / ggplot2::.pt, colour = label_color,
                       family = font_family)
  }

  name_x <- if (comparisons == "pairs") 0 else 0.015
  p <- p + geom_text(data = body,
                     aes(x = name_x, y = .data$y, label = .data$label),
                     hjust = 0, vjust = 0.5, size = text_size / ggplot2::.pt,
                     colour = label_color, family = font_family,
                     lineheight = 0.95)
  for (i in seq_along(cols)) {
    cd <- data.frame(x = col_right[i], y = body$y, label = txt[[cols[i]]],
                     stringsAsFactors = FALSE)
    p <- p + geom_text(data = cd,
                       aes(x = .data$x, y = .data$y, label = .data$label),
                       hjust = 1, vjust = 0.5, size = text_size / ggplot2::.pt,
                       colour = label_color, family = font_family)
  }
  p <- p + geom_text(data = body,
                     aes(x = 1, y = .data$y, label = .data$eff_txt),
                     hjust = 1, vjust = 0.5, size = text_size / ggplot2::.pt,
                     colour = label_color, family = font_family)

  plain <- body[!body$cut_lo & !body$cut_hi, , drop = FALSE]
  if (nrow(plain)) {
    p <- p + geom_segment(data = plain,
                          aes(x = .data$xlo, xend = .data$xhi,
                              y = .data$y, yend = .data$y),
                          colour = point_color, linewidth = line_width)
  }
  # an interval cut by `clip` gets an arrowhead on the side that was cut
  for (side in c("first", "last", "both")) {
    cut <- switch(side,
                  first = body[body$cut_lo & !body$cut_hi, , drop = FALSE],
                  last = body[!body$cut_lo & body$cut_hi, , drop = FALSE],
                  both = body[body$cut_lo & body$cut_hi, , drop = FALSE])
    if (!nrow(cut)) next
    p <- p + geom_segment(data = cut,
                          aes(x = .data$xlo, xend = .data$xhi,
                              y = .data$y, yend = .data$y),
                          colour = point_color, linewidth = line_width,
                          arrow = ggplot2::arrow(length = unit(0.055, "in"),
                                                 type = "open", ends = side,
                                                 angle = 22))
  }
  p <- p +
    ggplot2::geom_point(data = body,
                        aes(x = .data$x, y = .data$y, size = .data$psize),
                        shape = if (style == "jama") 21 else 22,
                        colour = point_color, fill = point_fill, stroke = 0.7) +
    ggplot2::scale_size_identity()

  ticks <- if (!is.null(xticks)) xticks else
    forest_ticks(lim, null_value, log_scale)
  ticks <- ticks[tx(ticks) >= lim[1] & tx(ticks) <= lim[2]]
  tick_df <- data.frame(x = map_x(ticks),
                        label = format(ticks, trim = TRUE, drop0trailing = TRUE),
                        stringsAsFactors = FALSE)
  p <- p +
    geom_segment(aes(x = plot_x0, xend = plot_x1, y = y_axis, yend = y_axis),
                 colour = rule_color, linewidth = 0.4) +
    geom_segment(data = tick_df,
                 aes(x = .data$x, xend = .data$x, y = y_axis,
                     yend = y_axis - 0.18),
                 colour = rule_color, linewidth = 0.4) +
    geom_text(data = tick_df,
              aes(x = .data$x, y = y_axis - 0.6, label = .data$label),
              size = text_size * 0.9 / ggplot2::.pt, colour = label_color,
              family = font_family, vjust = 0.5)

  if (!is.null(favours)) {
    if (length(favours) != 2) {
      stop("`favours` must be two strings, left and right of the null line.",
           call. = FALSE)
    }
    fav <- data.frame(x = c(plot_x0, plot_x1), label = as.character(favours),
                      hjust = c(0, 1), stringsAsFactors = FALSE)
    p <- p + geom_text(data = fav,
                       aes(x = .data$x, y = y_favours, label = .data$label,
                           hjust = .data$hjust),
                       size = text_size * 0.9 / ggplot2::.pt, vjust = 0.5,
                       colour = label_color, family = font_family,
                       fontface = "italic")
  }

  if (is.null(subtitle)) subtitle <- outcome
  p <- p +
    ggplot2::coord_cartesian(xlim = c(-0.01, 1.01),
                             ylim = c(y_min, y_head + 0.9),
                             expand = FALSE, clip = "off") +
    labs(title = title, subtitle = subtitle, caption = caption) +
    nmaplot_theme(grid = FALSE, grid_color = "grey88", background = background,
                  title_size = title_size, title_color = title_color,
                  title_align = title_align, font_family = font_family)

  p$nmaforest <- list(rows = body, pairs = pairs, limits = lim, ticks = ticks,
                      sm = sm, log_scale = log_scale)

  if (!is.null(file)) {
    if (is.null(height)) {
      height <- row_height * (y_head + 0.9 - y_min) + 0.6 +
        (if (!is.null(title) || !is.null(subtitle)) 0.5 else 0)
    }
    for (f in file) save_nmaplot(p, f, width, height, dpi, background)
  }
  p
}
