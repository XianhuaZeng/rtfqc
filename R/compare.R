# Compare an RTF table with a QC dataset ----------------------------------

.nums <- function(x) {
  m <- regmatches(x, gregexpr("-?[0-9]+(\\.[0-9]+)?", x))[[1]]
  m
}
.mask <- function(x) gsub("\\s", "", gsub("-?[0-9]+(\\.[0-9]+)?", "#", x))
.dec <- function(s) { p <- strsplit(s, ".", fixed = TRUE)[[1]]; if (length(p) > 1) nchar(p[2]) else 0L }

#' Classify the difference between two table cells
#'
#' @param a,b Cell texts (RTF and QC). Vectorised.
#' @param min_trunc Shortest common prefix (characters) for a difference to be
#'   reported as truncation rather than a value difference.
#' @return Character vector: `""` (same; trailing white space and line breaks
#'   inside the cell are ignored), `"spacing"` (only the spaces differ: inside
#'   the text, e.g. `9(31.0)` vs `9 ( 31.0)`, or leading spaces / indentation,
#'   e.g. `"  n"` vs `"n"`), `"truncated"` (one text is a prefix of the other), `"format"`
#'   (same numbers written differently, e.g. `5` vs `5.0`), `"rounding"` (the
#'   numbers differ by at most one unit in the last decimal shown; whole
#'   numbers such as counts never count as rounding) or `"diff"`.
#' @examples
#' rtf_cell_diff(c("12 (40.0)", "12 (40.0)", "3.46", "Headache and migr"),
#'               c("12 (40.0)", "12 (40.00)", "3.45", "Headache and migraine"))
#' @export
rtf_cell_diff <- function(a, b, min_trunc = 8L) {
  # only trailing white space is ignored (SAS pads character values); leading spaces
  # (indentation, number alignment) and spaces inside a cell must match
  a <- .strict(ifelse(is.na(a), "", as.character(a)))
  b <- .strict(ifelse(is.na(b), "", as.character(b)))
  n <- max(length(a), length(b))
  a <- rep_len(a, n); b <- rep_len(b, n)
  out <- character(n)
  for (k in seq_len(n)) out[k] <- .cell_diff1(a[k], b[k], min_trunc)
  out
}

.strict <- function(x) sub("\\s+$", "", gsub("\u00a0", " ", x, fixed = TRUE))
.lead <- function(x) nchar(x) - nchar(sub("^ +", "", x))

.cell_diff1 <- function(x, y, min_trunc = 8L) {
  if (identical(x, y)) return("")
  if (identical(gsub("\\s", "", x), gsub("\\s", "", y))) {
    if (.lead(x) != .lead(y)) return("spacing")   # indentation / alignment differs
    # a line break (RTF \\line / \\par, ODS @n / @w) is where the text wraps in the
    # cell, not a real character: differences around it do not count
    if (grepl("\n", x, fixed = TRUE) || grepl("\n", y, fixed = TRUE)) return("")
    return("spacing")
  }
  x <- .norm(x); y <- .norm(y)
  if (identical(x, y)) return("")
  if (nzchar(x) && nzchar(y) && (startsWith(y, x) || startsWith(x, y)) &&
      min(nchar(x), nchar(y)) >= min_trunc) return("truncated")
  sx <- .nums(x); sy <- .nums(y)
  if (length(sx) && length(sx) == length(sy) && identical(.mask(x), .mask(y))) {
    nx <- as.numeric(sx); ny <- as.numeric(sy)
    if (all(nx == ny)) return("format")
    d <- pmax(vapply(sx, .dec, 0L), vapply(sy, .dec, 0L))
    # rounding = numbers that differ only in the last decimal shown; whole numbers
    # (counts such as 30 vs 29) that differ are real differences
    chg <- nx != ny
    if (all(d[chg] > 0) && all(abs(nx - ny)[chg] <= 10^(-d[chg]) * 1.0001)) return("rounding")
  }
  "diff"
}

.read_qc <- function(qc) {
  if (is.data.frame(qc)) return(qc)
  if (is.character(qc) && length(qc) == 1) {
    if (!file.exists(qc)) stop("QC dataset not found: ", qc, call. = FALSE)
    if (grepl("\\.sas7bdat$", qc, ignore.case = TRUE)) {
      if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' is needed to read sas7bdat files.", call. = FALSE)
      return(as.data.frame(haven::read_sas(qc)))
    }
    if (grepl("\\.xpt$", qc, ignore.case = TRUE)) {
      if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' is needed to read xpt files.", call. = FALSE)
      return(as.data.frame(haven::read_xpt(qc)))
    }
    if (grepl("\\.rds$", qc, ignore.case = TRUE)) return(readRDS(qc))
    if (grepl("\\.csv$", qc, ignore.case = TRUE)) return(utils::read.csv(qc, colClasses = "character", check.names = FALSE))
  }
  stop("`qc` must be a data frame or a path to a .sas7bdat / .xpt / .rds / .csv file.", call. = FALSE)
}

# SAS ODS inline formatting in QC text (ods escapechar, e.g. "@{super 1}",
# "^{sub 2}", "(*ESC*){unicode 03B1}") -> the text the RTF shows
.ods_inline <- function(x) {
  esc <- "(?:\\(\\*ESC\\*\\)|[^A-Za-z0-9\\s{}])"
  x <- gsub(paste0(esc, "\\{(?i:super|sub)\\s+([^{}]*)\\}"), "\\1", x, perl = TRUE)
  x <- gsub(paste0(esc, "\\{(?i:newline)(?:\\s+\\d+)?\\}"), "\n", x, perl = TRUE)
  x <- gsub(paste0(esc, "\\{(?i:nbspace)(?:\\s+\\d+)?\\}"), " ", x, perl = TRUE)
  x <- gsub(paste0(esc, "\\{(?i:style)\\s*\\[[^]]*\\]\\s*([^{}]*)\\}"), "\\1", x, perl = TRUE)
  m <- gregexpr(paste0(esc, "\\{(?i:unicode)\\s+([0-9A-Fa-f]{2,6})\\}"), x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    if (!length(v)) return(v)
    vapply(v, function(t) intToUtf8(strtoi(sub(".*\\s([0-9A-Fa-f]+)\\}$", "\\1", t), 16L)), "")
  })
  x <- gsub(paste0(esc, "\\{(?i:dagger)\\}"), "\u2020", x, perl = TRUE)
  # short forms (only the usual escape characters @ ^ ~ and (*ESC*)): n = line break,
  # _ = non-breaking space, w = wrap point and m = alignment marker (removed)
  short <- "(?:\\(\\*ESC\\*\\)|[@^~])"
  x <- gsub(paste0(short, "-?[0-9]*n"), "\n", x, perl = TRUE)
  x <- gsub(paste0(short, "_"), " ", x, perl = TRUE)
  x <- gsub(paste0(short, "[wm]"), "", x, perl = TRUE)
  x
}

# QC data frame -> character matrix in display order (COL1..COLn when present)
.qc_matrix <- function(df, cols = NULL) {
  if (is.null(cols)) {
    cols <- grep("^COL[0-9]+$", names(df), ignore.case = TRUE, value = TRUE)
    if (length(cols)) cols <- cols[order(as.integer(sub("^COL", "", cols, ignore.case = TRUE)))]
    else cols <- names(df)[vapply(df, is.character, TRUE)]
  }
  if (!length(cols)) stop("No character columns (COL1, COL2, ...) found in the QC data.", call. = FALSE)
  m <- vapply(cols, function(cn) { v <- as.character(df[[cn]]); v[is.na(v)] <- ""; .ods_inline(v) }, character(nrow(df)))
  if (is.null(dim(m))) m <- matrix(m, ncol = length(cols))
  keep <- apply(m, 1, function(r) any(nzchar(.norm(r))))
  list(m = m[keep, , drop = FALSE], cols = cols)
}

# Rows of the RTF side as a list of character vectors
.rtf_side <- function(rtf) {
  if (is.character(rtf) && length(rtf) == 1) rtf <- rtf_read(rtf)
  if (inherits(rtf, "rtf_doc")) {
    r <- rtf$rows[!rtf$rows$header, , drop = FALSE]
    rows <- r$cells; page <- r$page
    ind <- if ("indent" %in% names(r)) r$indent else NULL
  } else if (is.data.frame(rtf)) {
    # e.g. rtf_table() or artful::rtf_to_df() output
    df <- rtf[, setdiff(names(rtf), c(".page", "indent")), drop = FALSE]
    page <- if (".page" %in% names(rtf)) rtf$.page else rep(NA_integer_, nrow(rtf))
    rows <- lapply(seq_len(nrow(df)), function(i) { v <- as.character(unlist(df[i, ], use.names = FALSE)); v[is.na(v)] <- ""; v })
    ind <- NULL
  } else stop("`rtf` must be an RTF path, an rtf_doc or a data frame.", call. = FALSE)
  keep <- vapply(rows, function(cc) any(nzchar(.norm(cc))), TRUE)
  list(rows = rows[keep], page = page[keep], ind = if (!is.null(ind)) ind[keep] else NULL)
}

# Indentation written as RTF paragraph indent (\\li) instead of leading spaces is turned
# into leading spaces, so it can be compared with the QC text. Per column, the smallest
# indent is cell padding (the baseline); each larger indent level is matched by rank to
# the leading-space counts the QC uses in that column (if the number of levels differs,
# by the ratio of the first level; with no indented QC cells, 2 spaces per level).
.apply_li <- function(rows, ind, qm) {
  if (is.null(ind) || !length(rows)) return(rows)
  w <- max(lengths(rows))
  for (j in seq_len(w)) {
    tw <- vapply(seq_along(rows), function(i) {
      if (j <= length(rows[[i]]) && j <= length(ind[[i]]) && nzchar(trimws(rows[[i]][j]))) as.integer(ind[[i]][j]) else NA_integer_
    }, 0L)
    ok <- !is.na(tw)
    if (!any(ok)) next
    d <- tw - min(tw[ok])
    lev <- sort(unique(d[ok & d > 0]))
    if (!length(lev)) next
    qs <- integer(0)
    if (j <= ncol(qm)) {
      v <- qm[, j]; v <- v[nzchar(trimws(v))]
      n <- .lead(v); qs <- sort(unique(n[n > 0]))
    }
    sp <- if (length(qs) == length(lev)) qs
      else if (length(qs)) pmax(1L, as.integer(round(lev / (lev[1] / qs[1]))))
      else 2L * seq_along(lev)
    for (i in which(ok & d > 0)) rows[[i]][j] <- paste0(strrep(" ", sp[match(d[i], lev)]), rows[[i]][j])
  }
  rows
}

# Longest common subsequence on a logical match matrix H (n x m): matched (i, j) pairs
.lcs <- function(H) {
  n <- nrow(H); m <- ncol(H)
  if (!n || !m || as.numeric(n) * m > 4e6) return(matrix(integer(0), 0, 2))
  L <- matrix(0L, n + 1, m + 1)
  for (i in n:1) for (j in m:1) L[i, j] <- if (H[i, j]) L[i + 1, j + 1] + 1L else max(L[i + 1, j], L[i, j + 1])
  i <- 1L; j <- 1L; oi <- integer(0); oj <- integer(0)
  while (i <= n && j <= m) {
    if (H[i, j] && L[i, j] == L[i + 1, j + 1] + 1L) { oi <- c(oi, i); oj <- c(oj, j); i <- i + 1L; j <- j + 1L }
    else if (L[i + 1, j] >= L[i, j + 1]) i <- i + 1L else j <- j + 1L
  }
  cbind(oi, oj)
}

# Do two row labels name the same row: equal, equal apart from white space, or one a
# truncation of the other (at least 8 characters)
.label_match <- function(A, B) {
  H <- outer(A, B, "==") | outer(gsub("\\s", "", A), gsub("\\s", "", B), "==")
  cand <- which(!H & outer(substr(A, 1, 8), substr(B, 1, 8), "==") & outer(nchar(A) >= 8, nchar(B) >= 8, "&"), arr.ind = TRUE)
  if (nrow(cand)) for (r in seq_len(nrow(cand))) {
    i <- cand[r, 1]; j <- cand[r, 2]
    H[i, j] <- startsWith(A[i], B[j]) || startsWith(B[j], A[i])
  }
  H
}

# Listings often leave repeated values out (PROC REPORT shows the group / subject only on
# the first row). Fill leading empty cells from the row above, on both sides.
.fill_down <- function(rows) {
  prev <- NULL
  lapply(rows, function(r) {
    nz <- which(nzchar(.norm(r)))
    if (!is.null(prev) && length(nz) && nz[1] > 1) for (c in seq_len(nz[1] - 1)) r[c] <- if (c <= length(prev)) prev[c] else ""
    prev <<- r
    r
  })
}

# Row alignment: (1) rows whose whole content is identical are matched first (LCS) as
# anchors; (2) between anchors the remaining rows are matched on the first column
# (the row label); (3) what is still left is paired in order (same row, content changed),
# and only the surplus counts as present on one side only. Listings often have the same
# first column on every row (e.g. the group), so aligning on it alone shifts rows.
.align <- function(RA, QB) {
  full <- function(rows) vapply(rows, function(r) gsub("\\s", "", paste(.norm(r), collapse = "|")), "")
  FA <- full(RA); FB <- full(QB)
  anchors <- .lcs(outer(FA, FB, "=="))
  if (length(RA) * as.numeric(length(QB)) > 4e6) {
    k <- seq_len(max(length(RA), length(QB)))
    return(cbind(ifelse(k <= length(RA), k, NA), ifelse(k <= length(QB), k, NA)))
  }
  oi <- integer(0); oj <- integer(0)
  push <- function(i, j) { oi <<- c(oi, i); oj <<- c(oj, j) }
  gap <- function(i0, i1, j0, j1) { # rows i0..i1-1 and j0..j1-1
    ia <- if (i1 > i0) i0:(i1 - 1) else integer(0)
    jb <- if (j1 > j0) j0:(j1 - 1) else integer(0)
    ka <- .norm(vapply(RA[ia], function(r) if (length(r)) r[1] else "", ""))
    kb <- .norm(vapply(QB[jb], function(r) if (length(r)) r[1] else "", ""))
    sub <- if (length(ka) && length(kb)) .lcs(.label_match(ka, kb)) else matrix(integer(0), 0, 2)
    pi <- i0; pj <- j0
    rest <- function(ie, je) {
      while (pi < ie && pj < je) { push(pi, pj); pi <<- pi + 1L; pj <<- pj + 1L }
      while (pi < ie) { push(pi, NA); pi <<- pi + 1L }
      while (pj < je) { push(NA, pj); pj <<- pj + 1L }
    }
    for (r in seq_len(nrow(sub))) { rest(i0 + sub[r, 1] - 1L, j0 + sub[r, 2] - 1L); push(pi, pj); pi <- pi + 1L; pj <- pj + 1L }
    rest(i1, j1)
  }
  pi <- 1L; pj <- 1L
  for (r in seq_len(nrow(anchors))) {
    gap(pi, anchors[r, 1], pj, anchors[r, 2]); push(anchors[r, 1], anchors[r, 2])
    pi <- anchors[r, 1] + 1L; pj <- anchors[r, 2] + 1L
  }
  gap(pi, length(RA) + 1L, pj, length(QB) + 1L)
  cbind(oi, oj)
}

#' Compare an RTF output with its QC dataset
#'
#' Leading empty cells (values a listing leaves out because they repeat the row
#' above) are filled from the row above on both sides. Rows are then aligned:
#' identical rows first, then on the first column (the row labels), then the
#' remaining rows in order, so an extra or missing row does not shift every row
#' after it. Every cell is classified with [rtf_cell_diff()].
#'
#' @param rtf Path to the RTF file, an `rtf_doc` from [rtf_read()], or a data
#'   frame such as the result of [rtf_table()] or `artful::rtf_to_df()`.
#' @param qc QC data: a data frame or a path to `.sas7bdat`, `.xpt`, `.rds` or
#'   `.csv`. Columns `COL1`, `COL2`, ... are used in that order when present,
#'   otherwise all character columns.
#' @param qc_cols Optional character vector of QC columns to use, in display order.
#' @return An object of class `rtf_compare`: a list with `rows` (one entry per
#'   aligned row: `status`, RTF cells, QC cells, per-cell classes), `count`
#'   (number of rows per worst status) and some metadata. Use
#'   `as.data.frame()` for the cell-level differences.
#' @examples
#' f <- system.file("extdata", "demo.rtf", package = "rtfqc")
#' qc <- data.frame(COL1 = c("Age (years)", "  n", "  Mean (SD)"),
#'                  COL2 = c("", "30", "45.2 (10.11)"),
#'                  COL3 = c("", "31", "47.0 (9.80)"))
#' res <- rtf_compare(f, qc)
#' res
#' as.data.frame(res)
#' @export
rtf_compare <- function(rtf, qc, qc_cols = NULL) {
  src <- if (is.character(rtf) && length(rtf) == 1) rtf else NA_character_
  R <- .rtf_side(rtf)
  Q <- .qc_matrix(.read_qc(qc), qc_cols)
  R$rows <- .fill_down(.apply_li(R$rows, R$ind, Q$m))
  QB <- .fill_down(lapply(seq_len(nrow(Q$m)), function(k) unname(Q$m[k, ])))
  P <- .align(R$rows, QB)

  status_order <- c("diff", "truncated", "rounding", "format", "spacing")
  rows <- list()
  count <- c(same = 0L, rounding = 0L, format = 0L, spacing = 0L, truncated = 0L, diff = 0L, only_rtf = 0L, only_qc = 0L)
  k <- 1L; np <- nrow(P)
  while (k <= np) {
    i <- P[k, 1]; j <- P[k, 2]
    a <- if (!is.na(i)) R$rows[[i]] else NULL
    b <- if (!is.na(j)) QB[[j]] else NULL
    if (is.null(a) || is.null(b)) {
      st <- if (is.null(a)) "only_qc" else "only_rtf"
      count[st] <- count[st] + 1L
      rows[[length(rows) + 1L]] <- list(status = st, rtf_row = i, qc_row = j,
        page = if (!is.na(i)) R$page[i] else NA_integer_, rtf = a, qc = b, cells = character(0))
    } else {
      w <- max(length(a), length(b))
      aa <- a; length(aa) <- w; aa[is.na(aa)] <- ""
      bb <- b; length(bb) <- w; bb[is.na(bb)] <- ""
      cells <- rtf_cell_diff(aa, bb)
      worst <- status_order[status_order %in% cells][1]
      # diff / truncated / rounding / format are counted per CELL (one row can have several);
      # same is counted per row
      for (ty in c("diff", "truncated", "rounding", "format", "spacing")) count[ty] <- count[ty] + sum(cells == ty)
      if (is.na(worst)) { count["same"] <- count["same"] + 1L; st <- "" } else st <- worst
      rows[[length(rows) + 1L]] <- list(status = st, rtf_row = i, qc_row = j,
        page = R$page[i], rtf = unname(aa), qc = unname(bb), cells = cells)
    }
    k <- k + 1L
  }
  structure(list(
    rtf = src, rows = rows, count = count,
    rtf_rows = length(R$rows), qc_rows = nrow(Q$m), qc_cols = Q$cols
  ), class = "rtf_compare")
}

#' @export
summary.rtf_compare <- function(object, ...) {
  object$count
}

#' @export
print.rtf_compare <- function(x, n = 10, ...) {
  cnt <- x$count
  issues <- sum(cnt[c("diff", "truncated", "rounding", "format", "spacing", "only_rtf", "only_qc")])
  cat(sprintf("<rtf_compare>%s\n", if (!is.na(x$rtf)) paste0(" ", basename(x$rtf)) else ""))
  cat(sprintf("  RTF rows: %d   QC rows: %d   QC columns: %s\n", x$rtf_rows, x$qc_rows, paste(x$qc_cols, collapse = ", ")))
  cat(sprintf("  rows: same %d | only in RTF %d | only in QC %d\n  cells: diff %d | truncated %d | rounding %d | format %d | spacing %d\n",
    cnt["same"], cnt["only_rtf"], cnt["only_qc"], cnt["diff"], cnt["truncated"], cnt["rounding"], cnt["format"], cnt["spacing"]))
  if (!issues) { cat("  \u2714 RTF matches QC\n"); return(invisible(x)) }
  d <- as.data.frame(x)
  if (nrow(d)) {
    cat("\n")
    # shorten long text so each difference fits on one line; as.data.frame() has the full text
    show <- utils::head(d, n)
    cut <- function(v, w) ifelse(nchar(v) > w, paste0(substr(v, 1, w - 1), "\u2026"), v)
    show$label <- cut(show$label, 24)
    # show long text from where the two sides start to differ (the common start becomes an ellipsis)
    from_diff <- function(a, b) {
      a <- ifelse(is.na(a), "", a); b <- ifelse(is.na(b), "", b)
      p <- vapply(seq_along(a), function(k) {
        x <- strsplit(a[k], "")[[1]]; y <- strsplit(b[k], "")[[1]]; m <- min(length(x), length(y))
        i <- which(x[seq_len(m)] != y[seq_len(m)])[1]; if (is.na(i)) m + 1L else i
      }, 0L)
      st <- pmax(1L, p - 8L)
      list(a = ifelse(st > 1, paste0("\u2026", substring(a, st)), a), b = ifelse(st > 1, paste0("\u2026", substring(b, st)), b))
    }
    fd <- from_diff(show$rtf, show$qc)
    show$rtf <- cut(fd$a, 25)
    show$qc <- cut(fd$b, 25)
    show$qc_row <- NULL
    print(show, row.names = FALSE, right = FALSE)
    if (nrow(d) > n) cat(sprintf("  ... %d more (as.data.frame() for all)\n", nrow(d) - n))
  }
  invisible(x)
}

# cell text for display: line breaks shown as spaces, leading / inner spaces kept
.show <- function(x) sub("\\s+$", "", gsub("\n", " ", gsub("\u00a0", " ", x, fixed = TRUE), fixed = TRUE))

#' @export
as.data.frame.rtf_compare <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- lapply(x$rows, function(r) {
    if (r$status %in% c("only_rtf", "only_qc")) {
      return(data.frame(status = r$status, page = r$page, rtf_row = r$rtf_row, qc_row = r$qc_row,
        column = NA_integer_, label = if (!is.null(r$rtf)) .norm(r$rtf[1]) else .norm(r$qc[1]),
        rtf = paste(.norm(r$rtf), collapse = " | "), qc = paste(.norm(r$qc), collapse = " | "),
        stringsAsFactors = FALSE, row.names = NULL))
    }
    k <- which(r$cells %in% c("diff", "truncated", "rounding", "format", "spacing"))
    if (!length(k)) return(NULL)
    data.frame(status = r$cells[k], page = r$page, rtf_row = r$rtf_row, qc_row = r$qc_row,
      column = k, label = unname(.norm(r$rtf[1])), rtf = unname(.show(r$rtf[k])), qc = unname(.show(r$qc[k])),
      stringsAsFactors = FALSE, row.names = NULL)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(status = character(0), page = integer(0), rtf_row = integer(0), qc_row = integer(0),
      column = integer(0), label = character(0), rtf = character(0), qc = character(0))
  }
  rownames(out) <- NULL
  out
}
