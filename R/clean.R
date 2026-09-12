#' Convert numbers stored as text
#'
#' Files written in locales that use a decimal comma (`write.csv2()`, most
#' spreadsheets) come back from `read.csv()` with effect sizes such as
#' `"-0,684"` stored as text, which `netmeta::netmeta()` rejects
#' ("Non-numeric value for argument 'TE'"). `nma_clean()` turns every text
#' column that holds only numbers into a numeric column.
#'
#' A comma is read as the decimal mark; when a value has both a dot and a
#' comma (`"1.234,5"`) the dot is taken as a thousands separator. The Unicode
#' minus sign and the dashes that word processors and spreadsheets substitute
#' for `-` are accepted, and spaces (including non-breaking ones) are ignored.
#' Columns with any non-numeric entry (treatment names, notes, a lone `"-"`)
#' and columns that are already numeric are returned unchanged.
#'
#' [nmaplot()] applies it for you when given pairwise data.
#'
#' @param data A data frame, for example the rows of [meta::pairwise()] read
#'   back from a file.
#' @param columns Optional names of the columns to convert. Defaults to all.
#' @return `data`, with the numeric-looking text columns converted to numbers.
#'
#' @examples
#' f <- system.file("extdata", "recurrence_pairwise.csv", package = "nmaplots")
#' d <- read.csv(f, sep = ";", fileEncoding = "UTF-8")
#' class(d$TE)
#' d <- nma_clean(d)
#' class(d$TE)
#' @export
nma_clean <- function(data, columns = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (is.null(columns)) columns <- names(data)
  bad <- setdiff(columns, names(data))
  if (length(bad)) {
    stop("`columns` not in `data`: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  for (col in columns) {
    v <- data[[col]]
    if (is.factor(v)) v <- as.character(v)
    if (!is.character(v)) next
    num <- text_to_number(v)
    if (!is.null(num)) data[[col]] <- num
  }
  data
}

# Numeric version of a character vector, or NULL when any entry is not a
# number (or every entry is missing).
text_to_number <- function(v) {
  s <- gsub("[[:space:]   ]", "", v)
  s <- gsub("[−‐‑‒–—﹣－]", "-", s)
  na <- is.na(s) | s %in% c("", "NA", "NaN")
  both <- grepl(",", s, fixed = TRUE) & grepl(".", s, fixed = TRUE)
  s[both] <- gsub(".", "", s[both], fixed = TRUE)
  s <- sub(",", ".", s, fixed = TRUE)
  ok <- grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", s)
  if (all(na) || !all(ok | na)) return(NULL)
  out <- rep(NA_real_, length(s))
  out[ok] <- as.numeric(s[ok])
  out
}

# Pairwise rows (meta::pairwise() output, possibly read back from a file) ->
# the netmeta fields nma_network() reads. Sample sizes and events come from
# n1/n2 and event1/event2, one value per study arm.
pairwise_input <- function(d) {
  d <- nma_clean(d)
  t1 <- as.character(d$treat1)
  t2 <- as.character(d$treat2)
  studlab <- as.character(d$studlab)
  keep <- !is.na(t1) & !is.na(t2) & !is.na(studlab)
  if (!any(keep)) {
    stop("The pairwise data has no rows with `treat1`, `treat2` and `studlab`.",
         call. = FALSE)
  }
  d <- d[keep, , drop = FALSE]
  t1 <- t1[keep]
  t2 <- t2[keep]
  studlab <- studlab[keep]

  get <- function(col) if (col %in% names(d))
    suppressWarnings(as.numeric(d[[col]])) else rep(NA_real_, nrow(d))
  arms <- data.frame(study = c(studlab, studlab), treatment = c(t1, t2),
                     n = c(get("n1"), get("n2")),
                     events = c(get("event1"), get("event2")),
                     stringsAsFactors = FALSE)
  arms <- arms[!duplicated(arms[c("study", "treatment")]), , drop = FALSE]
  trts <- sort(unique(arms$treatment))
  totals <- arm_totals(arms, trts)

  list(trts = trts, treat1 = t1, treat2 = t2, studlab = studlab,
       n.trts = totals$n.trts, events.trts = totals$events.trts, k.trts = NULL,
       reference.group = NULL, labels = NULL)
}
