skip_if_not_installed("gemtc")

arms <- data.frame(
  study = c("S1", "S1", "S2", "S2", "S3", "S3"),
  treatment = c("Drug A + B", "Placebo", "5-FU", "Placebo", "Drug A + B", "5-FU"),
  responders = c(10, 5, 8, 6, 9, 7),
  sampleSize = c(50, 50, 40, 40, 45, 45),
  stringsAsFactors = FALSE
)

test_that("nma_gemtc takes treatment names that gemtc rejects", {
  expect_error(gemtc::mtc.network(data.ab = arms), "invalid")
  nw <- nma_gemtc(arms)
  expect_s3_class(nw, "mtc.network")
  expect_setequal(as.character(nw$treatments$description),
                  c("Drug A + B", "Placebo", "5-FU"))
  expect_true(all(grepl("^[A-Za-z0-9_]+$", nw$treatments$id)))
})

test_that("nmaplot shows the names and accepts them for reference and highlight", {
  nw <- nma_gemtc(arms)
  p <- nmaplot(nw, reference = "Drug A + B", highlight = "5-FU")
  nodes <- p$nmaplot$nodes
  expect_setequal(nodes$name, c("Drug A + B", "Placebo", "5-FU"))
  id_of <- function(nm) as.character(nw$treatments$id[nw$treatments$description == nm])
  expect_equal(nodes$fill[nodes$trt == id_of("Drug A + B")], "#8A939B")
  expect_equal(nodes$fill[nodes$trt == id_of("5-FU")], "#E4572E")
})

test_that("gemtc ids are valid and unique; bad input is reported", {
  expect_equal(gemtc_ids(c("A + B", "A-B", " x ", "+")),
               c("A_B", "A_B_1", "x", "treatment"))
  expect_error(nma_gemtc(), "Supply")
  expect_error(nma_gemtc(data.frame(study = 1, trt = "A")), "treatment")
})
