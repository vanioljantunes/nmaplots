test_that("nma_clean converts decimal commas and Unicode minus signs", {
  d <- data.frame(TE = c("-0,68", "−1,5", "0,25", NA),
                  seTE = c("0,1", "1.234,5", " 2 ", "–3"),
                  trt = c("A", "B", "C", "D"),
                  note = c("-", "-", NA, "5-ALA"),
                  n = 1:4, stringsAsFactors = FALSE)
  out <- nma_clean(d)
  expect_equal(out$TE, c(-0.68, -1.5, 0.25, NA))
  expect_equal(out$seTE, c(0.1, 1234.5, 2, -3))
  expect_identical(out$trt, d$trt)
  expect_identical(out$note, d$note)
  expect_identical(out$n, d$n)
  expect_identical(nma_clean(d, "TE")$seTE, d$seTE)
  expect_error(nma_clean(d, "nope"), "not in `data`")
  expect_error(nma_clean(1:3), "data frame")
})

test_that("recurrence example: pairwise rows with decimal commas plot and fit", {
  f <- system.file("extdata", "recurrence_pairwise.csv", package = "nmaplots")
  d <- read.csv(f, sep = ";", fileEncoding = "UTF-8")
  expect_type(d$TE, "character")

  # nmaplot() takes the pairwise rows directly and cleans them
  g <- nmaplot(d)
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$nmaplot$nodes), 11)
  expect_true(all(grepl("n = ", g$nmaplot$nodes$label)))
  expect_setequal(unique(g$nmaplot$rings$group), c("Events", "No event"))

  skip_if_not_installed("netmeta")
  expect_error(netmeta::netmeta(TE, seTE, treat1, treat2, studlab, data = d,
                                sm = "RR"), "Non-numeric")
  dc <- nma_clean(d)
  expect_type(dc$TE, "double")
  net <- suppressWarnings(netmeta::netmeta(TE, seTE, treat1, treat2, studlab,
                                           data = dc, sm = "RR", n1 = n1,
                                           n2 = n2, event1 = event1,
                                           event2 = event2))
  ref <- nmaplot(net)$nmaplot
  i <- match(ref$nodes$trt, g$nmaplot$nodes$trt)
  expect_equal(g$nmaplot$nodes$n[i], ref$nodes$n)
  expect_equal(g$nmaplot$nodes$k[i], ref$nodes$k)
  expect_equal(nrow(g$nmaplot$edges), nrow(ref$edges))
})
