#' Build a gemtc network from treatment names as they are
#'
#' [gemtc::mtc.network()] only accepts treatment ids made of letters, digits
#' and underscores, so names such as `"Cilofexor + Firsocostat"` or
#' `"5-FU"` are rejected. `nma_gemtc()` takes the data with the names you
#' already use, makes valid ids internally and stores the original names in
#' the treatments' `description` column. [nmaplot()] shows those names on the
#' plot, and its `reference`, `order` and `highlight` arguments accept them
#' too, so the ids never need to be seen.
#'
#' @param data.ab Arm-level data, one row per study arm: `study`, `treatment`
#'   and the outcome columns gemtc expects (`responders` and `sampleSize` for
#'   binary outcomes; `mean`, `std.dev` and `sampleSize` for continuous ones).
#' @param data.re Optional relative-effect data: `study`, `treatment`, `diff`
#'   and `std.err`.
#' @param ... Further arguments for [gemtc::mtc.network()], such as
#'   `description` or `studies`.
#' @return An `mtc.network` object. Its `treatments` table has `id` (the valid
#'   id) and `description` (the name as given).
#'
#' @examples
#' if (requireNamespace("gemtc", quietly = TRUE)) {
#'   data(mash)
#'   d <- mash$fib_improvement_alldoses
#'   network <- nma_gemtc(d[, c("study", "treatment", "responders", "sampleSize")])
#'   network$treatments
#'   nmaplot(network, reference = "Placebo")
#' }
#' @export
nma_gemtc <- function(data.ab = NULL, data.re = NULL, ...) {
  if (!requireNamespace("gemtc", quietly = TRUE)) {
    stop("Package 'gemtc' is needed for nma_gemtc(): install.packages(\"gemtc\").",
         call. = FALSE)
  }
  if (is.null(data.ab) && is.null(data.re)) {
    stop("Supply `data.ab` (arm-level data) and/or `data.re`.", call. = FALSE)
  }
  check <- function(d, arg) {
    if (is.null(d)) return(NULL)
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    miss <- setdiff(c("study", "treatment"), names(d))
    if (length(miss)) {
      stop("`", arg, "` needs the column(s): ", paste(miss, collapse = ", "),
           call. = FALSE)
    }
    d$treatment <- as.character(d$treatment)
    d
  }
  data.ab <- check(data.ab, "data.ab")
  data.re <- check(data.re, "data.re")

  trt_names <- unique(c(data.ab$treatment, data.re$treatment))
  ids <- gemtc_ids(trt_names)
  map <- stats::setNames(ids, trt_names)
  if (!is.null(data.ab)) data.ab$treatment <- unname(map[data.ab$treatment])
  if (!is.null(data.re)) data.re$treatment <- unname(map[data.re$treatment])

  args <- list(treatments = data.frame(id = ids, description = trt_names,
                                       stringsAsFactors = FALSE), ...)
  if (!is.null(data.ab)) args$data.ab <- data.ab
  if (!is.null(data.re)) args$data.re <- data.re
  do.call(gemtc::mtc.network, args)
}

# Valid, unique gemtc ids: runs of other characters become one underscore,
# no leading or trailing underscore, duplicates numbered.
gemtc_ids <- function(x) {
  id <- gsub("[^A-Za-z0-9_]+", "_", x)
  id <- gsub("^_+|_+$", "", id)
  id[!nzchar(id)] <- "treatment"
  make.unique(id, sep = "_")
}
