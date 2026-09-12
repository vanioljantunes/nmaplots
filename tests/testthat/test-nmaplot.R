skip_if_not_installed("netmeta")

suppressPackageStartupMessages(library(netmeta))

make_net <- function() {
  data(smokingcessation, package = "netmeta", envir = environment())
  p <- pairwise(treat = list(treat1, treat2, treat3),
                event = list(event1, event2, event3),
                n = list(n1, n2, n3),
                data = smokingcessation, sm = "OR")
  suppressWarnings(netmeta(p, reference.group = "A"))
}

make_net_no_n <- function() {
  data(Senn2013, package = "netmeta", envir = environment())
  suppressWarnings(netmeta(TE, seTE, treat1, treat2, studlab,
                           data = Senn2013, sm = "MD", reference.group = "plac"))
}

test_that("nmaplot returns a ggplot with node and edge data", {
  net <- make_net()
  p <- nmaplot(net)
  expect_s3_class(p, "ggplot")
  expect_named(p$nmaplot, c("nodes", "edges", "rings", "multiarm"))
  expect_equal(nrow(p$nmaplot$nodes), length(net$trts))
  expect_true(all(p$nmaplot$edges$studies >= 1))
  # every direct comparison in the object is an edge
  key <- paste(pmin(net$treat1, net$treat2), pmax(net$treat1, net$treat2))
  expect_equal(nrow(p$nmaplot$edges), length(unique(key)))
})

test_that("labels carry sample sizes when available", {
  net <- make_net()
  p <- nmaplot(net)
  expect_true(all(grepl("n = ", p$nmaplot$nodes$label)))
  expect_equal(p$nmaplot$nodes$fill[p$nmaplot$nodes$trt == "A"], "#8A939B")
  p2 <- nmaplot(net, show_n = FALSE)
  expect_equal(p2$nmaplot$nodes$label, net$trts)
  expect_true(all(grepl("\nn = .*[(][0-9]+%[)]", p$nmaplot$nodes$label)))
  net2 <- make_net_no_n()
  p3 <- nmaplot(net2, label_wrap = NULL)
  expect_true(all(grepl("k = ", p3$nmaplot$nodes$label)))
})

test_that("layouts produce one coordinate per treatment", {
  net <- make_net()
  for (lay in c("multi", "circle", "star")) {
    p <- nmaplot(net, layout = lay)
    expect_equal(nrow(p$nmaplot$nodes), length(net$trts), info = lay)
    expect_false(any(is.na(p$nmaplot$nodes$x)), info = lay)
  }
  star <- nmaplot(net, layout = "star", reference = "A")
  ref <- star$nmaplot$nodes[star$nmaplot$nodes$trt == "A", ]
  expect_equal(unname(c(ref$x, ref$y)), c(0, 0))
  # circle: most connected treatment sits at the top
  circ <- nmaplot(net, layout = "circle")
  top <- circ$nmaplot$nodes[which.max(circ$nmaplot$nodes$y), ]
  expect_equal(top$degree, max(circ$nmaplot$nodes$degree))
})

test_that("custom layout matrix is accepted", {
  net <- make_net()
  m <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE)
  rownames(m) <- net$trts
  p <- nmaplot(net, layout = m)
  expect_equal(nrow(p$nmaplot$nodes), 4)
  expect_error(nmaplot(net, layout = m[1:2, ]), "custom `layout`")
})

test_that("order and highlight are respected", {
  net <- make_net()
  p <- nmaplot(net, order = c("D", "A"), highlight = "D", highlight_color = "red")
  nodes <- p$nmaplot$nodes
  expect_equal(nodes$fill[nodes$trt == "D"], "red")
  # D placed at the top of the circle (first in order)
  expect_equal(unname(nodes$y[nodes$trt == "D"]), 1)
  expect_error(nmaplot(net, order = "ZZZ"), "not in the network")
})

test_that("node_fill options work", {
  net <- make_net()
  p <- nmaplot(net, node_fill = "auto", palette = "Viridis")
  expect_equal(length(unique(p$nmaplot$nodes$fill)), 4)
  p <- nmaplot(net, node_fill = c(A = "red", B = "blue", C = "green", D = "black"))
  expect_equal(p$nmaplot$nodes$fill[p$nmaplot$nodes$trt == "B"], "blue")
  expect_error(nmaplot(net, node_fill = c("red", "blue")), "node_fill")
})

test_that("min_studies drops thin edges and edge_style multi builds", {
  net <- make_net()
  p_all <- nmaplot(net, min_studies = 1)
  p_thin <- nmaplot(net, min_studies = 3)
  expect_lt(nrow(p_thin$nmaplot$edges), nrow(p_all$nmaplot$edges))
  expect_true(all(p_thin$nmaplot$edges$studies >= 3))
  p <- nmaplot(net, edge_style = "multi", max_lines = 4)
  expect_s3_class(p, "ggplot")
  expect_error(nmaplot(net, layout = "spring"), "should be one of")
})

test_that("multi-arm polygons are detected", {
  net <- make_net()
  p <- nmaplot(net, multiarm = TRUE)
  expect_true(!is.null(p$nmaplot$multiarm))
  expect_true(nrow(p$nmaplot$multiarm) >= 3)
  p2 <- nmaplot(net, multiarm = FALSE)
  expect_null(p2$nmaplot$multiarm)
})

test_that("netgraph compatibility arguments are mapped", {
  net <- make_net()
  expect_message(
    p <- nmaplot(net, col.points = "darkblue", number.of.studies = FALSE,
                 plastic = FALSE, foo = 1),
    "ignoring unsupported argument"
  )
  expect_true(all(p$nmaplot$nodes$fill == "darkblue"))
})

test_that("files are written in png, pdf and tiff", {
  net <- make_net()
  td <- tempfile("nmaplot")
  dir.create(td)
  files <- file.path(td, c("net.png", "net.pdf", "net.tiff"))
  out <- nmaplot(net, file = files, width = 5, height = 5, dpi = 72)
  expect_s3_class(out, "ggplot")
  for (f in files) expect_true(file.exists(f), info = f)
  expect_true(all(file.size(files) > 0))
  expect_error(nmaplot(net, file = file.path(td, "net.svg")), "must end in")
  unlink(td, recursive = TRUE)
})

test_that("non-netmeta input is rejected", {
  expect_error(nmaplot(list()), "'netmeta' object .* 'gemtc'")
})

test_that("custom labels keep the sample size suffix", {
  net <- make_net()
  p <- nmaplot(net, labels = c(A = "No contact", B = "Self-help",
                               C = "Individual", D = "Group"), label_wrap = NULL)
  expect_true(all(grepl("^(No contact|Self-help|Individual|Group)\nn = ", p$nmaplot$nodes$label)))
  p2 <- nmaplot(net, labels = c("a", "b", "c", "d"), show_n = FALSE)
  expect_equal(p2$nmaplot$nodes$label, c("a", "b", "c", "d"))
  expect_error(nmaplot(net, labels = c("a", "b")), "one entry per treatment")
})

test_that("ring accepts long and wide input and normalises to 100 percent", {
  net <- make_net()
  long <- data.frame(treatment = rep(net$trts, each = 2),
                     group = rep(c("Low", "High"), 4),
                     value = c(3, 1, 2, 2, 0, 4, 5, 5))
  p <- nmaplot(net, ring = long, legend = TRUE)
  r <- p$nmaplot$rings
  expect_true(all(abs(tapply(r$prop, r$treatment, sum) - 1) < 1e-9))
  expect_equal(nrow(r), 7)  # the zero segment is dropped
  wide <- matrix(c(3, 1, 2, 2, 0, 4, 5, 5), ncol = 2, byrow = TRUE,
                 dimnames = list(net$trts, c("Low", "High")))
  p2 <- nmaplot(net, ring = wide, ring_colors = c(Low = "blue", High = "red"))
  expect_equal(unique(p2$nmaplot$rings$fill[p2$nmaplot$rings$group == "High"]), "red")
  expect_error(nmaplot(net, ring = data.frame(a = 1)), "`ring` must be")
  expect_warning(nmaplot(net, ring = data.frame(treatment = "ZZ", group = "Low", value = 1)),
                 "not in the network")
})

test_that("legend panel extends the window downwards", {
  net <- make_net()
  p0 <- nmaplot(net, legend = FALSE)
  p1 <- nmaplot(net, legend = TRUE)
  expect_lt(p1$coordinates$limits$y[1], p0$coordinates$limits$y[1])
})

test_that("mash dataset loads and plots with a ring from its design column", {
  data(mash, package = "nmaplots", envir = environment())
  expect_named(mash, c("fib_improvement", "mash_resolution",
                       "fib_improvement_alldoses", "mash_resolution_alldoses"))
  d <- mash$fib_improvement_alldoses
  expect_equal(names(d), c("study", "treatment", "responders", "sampleSize",
                           "rob", "incrr", "design"))
  expect_true(all(d$design %in% c("RCT", "PSM")))
  p <- pairwise(treat = treatment, event = responders, n = sampleSize,
                studlab = study, data = d, sm = "RR")
  net <- suppressWarnings(netmeta(p, reference.group = "Placebo"))
  g <- nmaplot(net, ring = table(d$treatment, d$design), ring_name = "Study design")
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$nmaplot$nodes), length(net$trts))
  expect_true(all(g$nmaplot$rings$group %in% c("RCT", "PSM")))
})

test_that("binary networks get an event-rate ring by default", {
  data(mash, package = "nmaplots", envir = environment())
  d <- mash$fib_improvement_alldoses
  pw <- pairwise(treat = treatment, event = responders, n = sampleSize,
                 studlab = study, data = d, sm = "RR")
  net <- suppressWarnings(netmeta(pw, reference.group = "Placebo"))
  g <- nmaplot(net)
  expect_false(is.null(g$nmaplot$rings))
  expect_setequal(unique(g$nmaplot$rings$group), c("Events", "No event"))
  # the ring proportion of the Events segment is events over participants
  ev <- g$nmaplot$rings[g$nmaplot$rings$group == "Events", ]
  i <- match(ev$treatment, names(net$events.trts))
  expect_equal(ev$prop, unname(net$events.trts[i] / net$n.trts[i]), tolerance = 1e-8)
  # only the event segment is labelled
  expect_true(all(g$nmaplot$rings$show_pct == (g$nmaplot$rings$group == "Events")))
  # ring = FALSE turns it off
  expect_null(nmaplot(net, ring = FALSE)$nmaplot$rings)
})

test_that("continuous networks get no ring and no event count", {
  net <- make_net_no_n()
  g <- nmaplot(net)
  expect_null(g$nmaplot$rings)
  sub <- g$labels$subtitle
  expect_false(!is.null(sub) && grepl("events", sub))
})

# ---- gemtc -------------------------------------------------------------------

mash_gemtc <- function() {
  data(mash, package = "nmaplots", envir = environment())
  d <- mash$fib_improvement_alldoses
  ids <- gsub("[^A-Za-z0-9_]", "_", d$treatment)
  trt <- unique(data.frame(id = ids, description = d$treatment,
                           stringsAsFactors = FALSE))
  list(d = d, network = gemtc::mtc.network(
    data.ab = data.frame(study = d$study, treatment = ids,
                         responders = d$responders, sampleSize = d$sampleSize),
    treatments = trt))
}

test_that("gemtc networks, models and results match the netmeta plot", {
  skip_if_not_installed("gemtc")
  g <- mash_gemtc()
  pw <- pairwise(treat = treatment, event = responders, n = sampleSize,
                 studlab = study, data = g$d, sm = "RR")
  net <- suppressWarnings(netmeta(pw, reference.group = "Placebo"))
  ref <- nmaplot(net)$nmaplot

  model <- suppressWarnings(gemtc::mtc.model(g$network, likelihood = "binom",
                                             link = "log", linearModel = "fixed"))
  result <- structure(list(model = model), class = "mtc.result")
  for (obj in list(g$network, model, result)) {
    p <- nmaplot(obj)
    expect_s3_class(p, "ggplot")
    nodes <- p$nmaplot$nodes
    # display names come from the description column
    expect_setequal(sub("\n.*", "", nodes$label), sub("\n.*", "", ref$nodes$label))
    i <- match(gsub("[^A-Za-z0-9_]", "_", ref$nodes$trt), nodes$trt)
    expect_equal(nodes$n[i], ref$nodes$n)
    expect_equal(nodes$k[i], ref$nodes$k)
    expect_equal(nrow(p$nmaplot$edges), nrow(ref$edges))
    expect_equal(sum(p$nmaplot$edges$studies), sum(ref$edges$studies))
    expect_setequal(unique(p$nmaplot$rings$group), c("Events", "No event"))
    expect_equal(length(p$nmaplot$multiarm), length(ref$multiarm))
  }
})

test_that("gemtc relative-effect networks plot without sample sizes", {
  skip_if_not_installed("gemtc")
  re <- data.frame(study = c(1, 1, 2, 2, 3, 3, 3),
                   treatment = c("A", "B", "A", "C", "A", "B", "C"),
                   diff = c(NA, -0.3, NA, 0.2, NA, -0.1, 0.4),
                   std.err = c(NA, 0.2, NA, 0.3, 0.1, 0.25, 0.3))
  nw <- gemtc::mtc.network(data.re = re)
  p <- nmaplot(nw)
  expect_equal(nrow(p$nmaplot$nodes), 3)
  expect_true(all(grepl("k = ", p$nmaplot$nodes$label)))
  expect_null(p$nmaplot$rings)
  expect_equal(p$nmaplot$edges$studies[p$nmaplot$edges$treat1 == "A" &
                                       p$nmaplot$edges$treat2 == "B"], 2)
  # study 3 is three-armed
  expect_named(nma_network(as_nma_input(nw))$multiarm, "3")
})
