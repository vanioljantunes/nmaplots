# Internal helpers for nmaforest(). Nothing here is exported.

# Effect measure implied by the gemtc likelihood and link. Names and the
# logarithmic flag follow what gemtc prints itself (scale.name.*, scale.log.*).
gemtc_scale <- function(likelihood, link) {
  key <- paste(likelihood, link, sep = ".")
  tab <- list(
    binom.logit = list(name = "Odds Ratio", log = TRUE),
    binom.log = list(name = "Risk Ratio", log = TRUE),
    binom.cloglog = list(name = "Hazard Ratio", log = TRUE),
    poisson.log = list(name = "Hazard Ratio", log = TRUE),
    normal.identity = list(name = "Mean Difference", log = FALSE),
    normal.smd = list(name = "Standardised Mean Difference", log = FALSE)
  )
  if (!is.null(tab[[key]])) tab[[key]] else list(name = "Effect", log = FALSE)
}

# Normalise the input of nmaforest() to what the forest needs: the result that
# holds the posterior samples, its model and network, the treatment ids, the
# display names gemtc keeps in treatments$description, and the effect measure.
forest_input <- function(x) {
  tbl <- NULL
  result <- NULL
  if (inherits(x, "mtc.relative.effect.table")) {
    tbl <- x
    model <- attr(x, "model")
  } else {
    if (inherits(x, "mtc.network") || inherits(x, "mtc.model")) {
      stop("`x` carries no estimates: nmaforest() needs the result of ",
           "gemtc::mtc.run(), not the network or the model. nmaplot() is the ",
           "one that draws the network itself.", call. = FALSE)
    }
    if (!inherits(x, "mtc.result")) {
      stop("`x` must be a 'gemtc' result (gemtc::mtc.run()) or a relative ",
           "effect table (gemtc::relative.effect.table()).", call. = FALSE)
    }
    result <- x
    model <- x[["model"]]
  }
  if (is.null(model)) {
    stop("The gemtc object carries no model, so the effect measure and the ",
         "treatments cannot be read.", call. = FALSE)
  }
  network <- model[["network"]]
  trts <- as.character(network[["treatments"]][["id"]])
  sc <- gemtc_scale(model[["likelihood"]], model[["link"]])

  labels <- NULL
  desc <- network[["treatments"]][["description"]]
  if (!is.null(desc)) {
    desc <- as.character(desc)
    bad <- is.na(desc) | !nzchar(desc)
    desc[bad] <- trts[bad]
    if (!identical(desc, trts)) labels <- stats::setNames(desc, trts)
  }

  list(result = result, table = tbl, model = model, network = network,
       trts = trts, labels = labels, sm = sc$name, log_scale = sc$log)
}

# The pairs to draw, one row per line of the forest. `comparator` is the
# treatment the row is compared with, `treatment` the one the estimate is for.
forest_pairs <- function(comparisons, trts, reference) {
  if (comparisons == "reference") {
    other <- setdiff(trts, reference)
    return(data.frame(comparator = rep(reference, length(other)),
                      treatment = other, stringsAsFactors = FALSE))
  }
  if (comparisons == "panels") {
    rows <- lapply(trts, function(cmp) {
      other <- setdiff(trts, cmp)
      data.frame(comparator = rep(cmp, length(other)), treatment = other,
                 stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }
  cm <- utils::combn(trts, 2)
  data.frame(comparator = cm[1, ], treatment = cm[2, ],
             stringsAsFactors = FALSE)
}

# Posterior median and credible interval for every pair, on the reporting
# scale. One gemtc::relative.effect() call per comparator returns the samples
# of every treatment against it at once, in columns d.<comparator>.<treatment>.
forest_estimates <- function(input, pairs, level, exponentiate) {
  probs <- c((1 - level) / 2, 0.5, 1 - (1 - level) / 2)
  q <- matrix(NA_real_, nrow = nrow(pairs), ncol = 3)

  if (!is.null(input$table)) {
    if (!isTRUE(all.equal(level, 0.95))) {
      warning("A relative effect table only holds the 95% interval, so ",
              "`level` is ignored. Pass the mtc.run() result for other levels.",
              call. = FALSE)
    }
    for (i in seq_len(nrow(pairs))) {
      q[i, ] <- as.numeric(input$table[pairs$comparator[i],
                                       pairs$treatment[i], ])
    }
  } else {
    for (cmp in unique(pairs$comparator)) {
      idx <- which(pairs$comparator == cmp)
      re <- gemtc::relative.effect(input$result, cmp, preserve.extra = FALSE)
      s <- as.matrix(re$samples)
      want <- paste("d", cmp, pairs$treatment[idx], sep = ".")
      hit <- match(want, colnames(s))
      if (anyNA(hit)) {
        stop("The gemtc result has no samples for the comparison(s): ",
             paste(want[is.na(hit)], collapse = ", "), call. = FALSE)
      }
      q[idx, ] <- t(apply(s[, hit, drop = FALSE], 2, stats::quantile,
                          probs = probs))
    }
  }

  out <- data.frame(lower = q[, 1], est = q[, 2], upper = q[, 3])
  if (isTRUE(exponentiate)) out <- exp(out)
  out
}

# Direct evidence for each pair: the studies that randomised both treatments,
# and the arm sizes and events summed over those studies only. A pair with no
# such study (indirect evidence only) comes back with k = 0 and NA counts.
direct_counts <- function(arms, pairs) {
  out <- data.frame(k = rep(0L, nrow(pairs)), n1 = NA_real_, n2 = NA_real_,
                    e1 = NA_real_, e2 = NA_real_)
  if (is.null(arms) || !nrow(arms)) return(out)
  by_study <- split(arms, factor(arms$study, unique(arms$study)))
  arm_sum <- function(d, trt, col) {
    v <- d[[col]][d$treatment == trt]
    if (!length(v)) NA_real_ else sum(v)
  }
  for (i in seq_len(nrow(pairs))) {
    a <- pairs$treatment[i]
    b <- pairs$comparator[i]
    hit <- Filter(function(d) all(c(a, b) %in% d$treatment), by_study)
    if (!length(hit)) next
    d <- do.call(rbind, hit)
    out$k[i] <- length(hit)
    out$n1[i] <- arm_sum(d, a, "n")
    out$n2[i] <- arm_sum(d, b, "n")
    out$e1[i] <- arm_sum(d, a, "events")
    out$e2[i] <- arm_sum(d, b, "events")
  }
  out
}

# Display name per treatment: the description gemtc keeps, a `labels` vector
# of your own, and underscores turned into spaces unless asked otherwise.
forest_labels <- function(trts, labels, clean_labels, wrap) {
  if (is.null(labels)) {
    name <- trts
  } else if (!is.null(names(labels))) {
    name <- unname(labels[trts])
  } else {
    name <- as.character(labels)
  }
  if (length(name) != length(trts) || anyNA(name)) {
    stop("`labels` must have one entry per treatment.", call. = FALSE)
  }
  if (isTRUE(clean_labels)) name <- gsub("_", " ", name, fixed = TRUE)
  if (!is.null(wrap)) name <- wrap_labels(name, wrap)
  stats::setNames(name, trts)
}

# Effect and interval as one string, "1.60 (0.68, 3.80)".
format_effect <- function(est, lower, upper, digits) {
  f <- function(v) formatC(v, format = "f", digits = digits)
  ifelse(is.na(est), "", paste0(f(est), " (", f(lower), ", ", f(upper), ")"))
}

# En dash, built from its code point so the source stays ASCII.
dash <- function() intToUtf8(8211)

# Counts as text, with a dash where the pair has no direct evidence.
format_count <- function(v) {
  ifelse(is.na(v) | v == 0, dash(), format_int(v))
}

format_events <- function(e, n) {
  ifelse(is.na(e) | is.na(n), dash(),
         paste0(format_int(e), "/", format_int(n)))
}

# Limits of the effect axis, in plotting units (log of the estimate when the
# scale is logarithmic). Intervals wider than `clip` folds of the null value
# are cut, and the rows that were cut get an arrowhead.
forest_limits <- function(lower, upper, null_value, clip, log_scale) {
  tx <- function(v) if (log_scale) log(v) else v
  null_t <- tx(null_value)
  lo <- tx(lower)
  hi <- tx(upper)
  keep <- is.finite(lo) & is.finite(hi)
  if (!any(keep)) return(c(null_t - 1, null_t + 1))
  span <- if (is.null(clip) || !is.finite(clip)) Inf else
    if (log_scale) log(clip) else clip
  lim <- c(max(min(lo[keep]), null_t - span), min(max(hi[keep]), null_t + span))
  if (diff(lim) <= 0) lim <- c(null_t - 1, null_t + 1)
  pad <- 0.04 * diff(lim)
  c(lim[1] - pad, lim[2] + pad)
}

# Tick positions inside the limits. Round values on a log scale, pretty()
# otherwise. Returned on the reporting scale, not the plotting scale.
forest_ticks <- function(lim, null_value, log_scale) {
  if (!log_scale) {
    tk <- pretty(lim, n = 5)
    return(tk[tk >= lim[1] & tk <= lim[2]])
  }
  cand <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.33, 0.5,
            0.67, 1, 1.5, 2, 3, 4, 5, 10, 20, 50, 100, 200, 500, 1000)
  tk <- cand[log(cand) >= lim[1] & log(cand) <= lim[2]]
  if (length(tk) > 7) {
    coarse <- c(0.001, 0.01, 0.1, 0.25, 0.5, 1, 2, 4, 10, 100, 1000)
    tk2 <- coarse[log(coarse) >= lim[1] & log(coarse) <= lim[2]]
    if (length(tk2) >= 3) tk <- tk2
  }
  if (!length(tk)) tk <- null_value
  tk
}
