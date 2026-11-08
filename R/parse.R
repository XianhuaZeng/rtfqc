# RTF parsing ---------------------------------------------------------------
#
# A small RTF reader written for the layout of clinical TFL outputs. It walks
# the RTF tokens once and collects, per page, the header/footer text (where
# SAS ODS RTF and r2rtf put titles and "Page x of y"), the body text, and the
# body table rows (\trowd ... \cell ... \row), with the \trhdr flag that marks
# repeated column headers. No Word, LibreOffice or Pandoc needed.

.skip_dest <- c(
  "fonttbl", "colortbl", "stylesheet", "info", "pict", "listtable",
  "listoverridetable", "rsidtbl", "generator", "xmlnstbl", "themedata",
  "colorschememapping", "latentstyles", "datastore", "object", "fldinst",
  "filetbl", "revtbl", "pgdsctbl", "nonshppict", "mmathPr", "bkmkstart",
  "bkmkend",
  # old result of PAGE / NUMPAGES fields: not real page text
  "fldrslt"
)
.hf_dest <- "^(header|footer)[lrf]?$"

.rtf_encoding <- function(src) {
  cp <- regmatches(src, regexpr("\\\\ansicpg[0-9]+", src))
  cp <- if (length(cp)) sub("\\\\ansicpg", "", cp) else "1252"
  switch(cp,
    "936" = "GBK", "950" = "BIG5", "65001" = "UTF-8",
    "932" = "CP932", "949" = "CP949", "1252" = "CP1252",
    paste0("CP", cp)
  )
}

.read_rtf_text <- function(file) {
  if (!file.exists(file)) stop("RTF file not found: ", file, call. = FALSE)
  raw <- readBin(file, "raw", n = file.info(file)$size)
  raw <- raw[raw != as.raw(0)]
  # RTF is 7-bit by spec; any stray 8-bit bytes are kept as Latin-1 characters
  txt <- rawToChar(raw)
  Encoding(txt) <- "latin1"
  enc2utf8(txt)
}

.norm <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}

# Walk the RTF once. Returns list(pages = data.frame(page, head, body),
# rows = data.frame(page, row, header, ncell) + cells list).
.rtf_walk <- function(src) {
  enc <- .rtf_encoding(substr(src, 1, 5000))
  tok <- regmatches(src, gregexpr(
    "\\\\'[0-9a-fA-F]{2}|\\\\[a-zA-Z]+-?[0-9]* ?|\\\\[^a-zA-Z]|[{}]|[\r\n]+|[^\\\\{}\r\n]+",
    src, perl = TRUE
  ))[[1]]

  # state
  stack_skip <- logical(0); stack_hf <- logical(0); stack_uc <- integer(0)
  # paragraph indent (\\li left indent, \\fi first-line indent, in twips): some
  # programs indent with it instead of leading spaces
  stack_li <- integer(0); stack_fi <- integer(0); li <- 0L; fi <- 0L
  skip <- FALSE; hf <- FALSE; uc <- 1L; pend <- 0L; first <- FALSE
  bytes <- integer(0)
  # PAGE / NUMPAGES fields: Word fills the numbers in when it opens the file, so the
  # RTF holds no digits. Keep a placeholder {PAGE} / {NUMPAGES} in the text instead.
  fldcap <- FALSE; fld_depth <- 0L; fldbuf <- character(0)

  heads <- list(character(0)); bodies <- list(character(0)); pg <- 1L
  row_cells <- list(); row_page <- integer(0); row_hdr <- logical(0)
  cells <- character(0); cell <- character(0); hdr <- FALSE
  row_ind <- list(); inds <- integer(0)

  decode <- function(b) {
    s <- rawToChar(as.raw(b))
    out <- iconv(s, from = enc, to = "UTF-8", sub = "?")
    if (is.na(out)) iconv(s, from = "latin1", to = "UTF-8") else out
  }
  put <- function(t) {
    if (skip) return(invisible())
    if (hf) {
      heads[[pg]] <<- c(heads[[pg]], t)
    } else {
      bodies[[pg]] <<- c(bodies[[pg]], t)
      cell <<- c(cell, t)
    }
  }
  flush <- function() {
    if (length(bytes)) {
      t <- decode(bytes)
      bytes <<- integer(0)
      put(t)
    }
  }
  new_page <- function() {
    flush()
    has <- any(nzchar(trimws(c(heads[[pg]], bodies[[pg]]))))
    if (!has) return(invisible())
    pg <<- pg + 1L
    heads[[pg]] <<- character(0); bodies[[pg]] <<- character(0)
  }

  for (t in tok) {
    c1 <- substr(t, 1, 1)
    # characters to skip after \uN (the ANSI fallback)
    if (pend > 0L && c1 != "{" && c1 != "}") {
      if (startsWith(t, "\\'")) { pend <- pend - 1L; next }
      if (c1 != "\\" && !grepl("^[\r\n]", t)) {
        n <- nchar(t)
        if (n <= pend) { pend <- pend - n; next }
        t <- substr(t, pend + 1L, n); pend <- 0L
        c1 <- substr(t, 1, 1)
      }
    }
    if (c1 == "{") {
      flush()
      stack_skip <- c(stack_skip, skip); stack_hf <- c(stack_hf, hf); stack_uc <- c(stack_uc, uc)
      stack_li <- c(stack_li, li); stack_fi <- c(stack_fi, fi)
      first <- TRUE
      next
    }
    if (c1 == "}") {
      flush()
      k <- length(stack_skip)
      if (k) {
        skip <- stack_skip[k]; hf <- stack_hf[k]; uc <- stack_uc[k]
        li <- stack_li[k]; fi <- stack_fi[k]
        stack_skip <- stack_skip[-k]; stack_hf <- stack_hf[-k]; stack_uc <- stack_uc[-k]
        stack_li <- stack_li[-k]; stack_fi <- stack_fi[-k]
      }
      if (fldcap && length(stack_skip) < fld_depth) {
        fldcap <- FALSE
        word <- toupper(sub("^\\s*([A-Za-z]+).*$", "\\1", paste(fldbuf, collapse = "")))
        fldbuf <- character(0)
        if (word %in% c("PAGE", "NUMPAGES", "SECTIONPAGES")) put(paste0("{", word, "}"))
      }
      first <- FALSE
      next
    }
    if (c1 == "\r" || c1 == "\n") next
    if (c1 != "\\") { flush(); if (fldcap) fldbuf <- c(fldbuf, t); put(t); first <- FALSE; next }

    # control symbol / word
    if (startsWith(t, "\\'")) { bytes <- c(bytes, strtoi(substr(t, 3, 4), 16L)); next }
    c2 <- substr(t, 2, 2)
    if (c2 == "*") { skip <- TRUE; next }
    if (c2 %in% c("\\", "{", "}")) { flush(); put(c2); next }
    if (c2 == "~") { flush(); put(" "); next }
    if (!grepl("^[a-zA-Z]$", c2)) next
    w <- sub("^\\\\([a-zA-Z]+).*$", "\\1", t)
    num <- sub("^\\\\[a-zA-Z]+(-?[0-9]*) ?$", "\\1", t)
    if (first && w == "fldinst") { fldcap <- TRUE; fld_depth <- length(stack_skip); fldbuf <- character(0) }
    if (first && w %in% .skip_dest) skip <- TRUE
    else if (first && grepl(.hf_dest, w)) hf <- TRUE
    first <- FALSE
    flush()

    if (w == "u" && nzchar(num)) {
      code <- as.integer(num); if (code < 0L) code <- code + 65536L
      put(intToUtf8(code)); pend <- uc; next
    }
    if (w == "uc" && nzchar(num)) { uc <- as.integer(num); next }
    if (skip) next
    if (w == "pard") { li <- 0L; fi <- 0L; next }
    if (w %in% c("li", "lin") && nzchar(num)) { li <- as.integer(num); next }
    if (w == "fi" && nzchar(num)) { fi <- as.integer(num); next }
    if (!hf) {
      if (w == "trowd") {
        # text before a row starts (titles in the body) is not part of its first cell;
        # some writers (Word) put \trowd after the cells, so only reset when the row has no cells yet
        hdr <- FALSE
        if (!length(cells)) cell <- character(0)
        next
      }
      if (w == "trhdr") { hdr <- TRUE; next }
      if (w == "cell") {
        cells <- c(cells, paste(cell, collapse = "")); cell <- character(0)
        inds <- c(inds, max(0L, li + fi))
        bodies[[pg]] <- c(bodies[[pg]], " "); next
      }
      if (w == "row") {
        if (length(cells)) {
          row_cells[[length(row_cells) + 1L]] <- sub("\\s+$", "", gsub("\u00a0", " ", cells, fixed = TRUE))
          row_page <- c(row_page, pg); row_hdr <- c(row_hdr, hdr)
          row_ind[[length(row_ind) + 1L]] <- inds
        }
        cells <- character(0); cell <- character(0); inds <- integer(0)
        bodies[[pg]] <- c(bodies[[pg]], " "); next
      }
      if (w %in% c("page", "sect")) { new_page(); next }
    }
    # a line break inside a cell is kept as "\\n": rtf_cell_diff() treats it as a wrap point
    if (w %in% c("par", "line")) { put("\n"); next }
    if (w %in% c("tab", "cell", "row")) { put(" "); next }
    if (w == "emdash") put("\u2014")
    else if (w == "endash") put("\u2013")
    else if (w == "bullet") put("\u2022")
    else if (w %in% c("lquote", "rquote")) put("'")
    else if (w %in% c("ldblquote", "rdblquote")) put("\"")
  }
  flush()

  head <- vapply(heads, function(x) .norm(paste(x, collapse = "")), "")
  body <- vapply(bodies, function(x) .norm(paste(x, collapse = "")), "")
  n <- length(head)
  if (n > 1 && !nzchar(head[n]) && !nzchar(body[n])) { head <- head[-n]; body <- body[-n] }
  list(
    pages = data.frame(page = seq_along(head), head = head, body = body, stringsAsFactors = FALSE),
    rows = row_cells, row_page = row_page, row_hdr = row_hdr, row_ind = row_ind
  )
}

#' Read an RTF output
#'
#' Parses a clinical RTF table, listing or figure file into pages and table
#' rows. The file is read directly; Word, LibreOffice and Pandoc are not needed.
#'
#' @param file Path to an `.rtf` file.
#' @return An object of class `rtf_doc`: a list with
#'   * `file`: the path;
#'   * `pages`: data frame with one row per page: `page`, `head` (header and
#'     footer text, which is where SAS ODS RTF and r2rtf put titles and
#'     "Page x of y"), `body` (all body text);
#'   * `rows`: data frame of body table rows: `page`, `row`, `header` (`TRUE`
#'     for repeated column-header rows, RTF `\\trhdr`), `cells` (list column)
#'     and `indent` (list column: paragraph indent of each cell in twips,
#'     RTF `\\li` + `\\fi`).
#' @examples
#' f <- system.file("extdata", "demo.rtf", package = "rtfqc")
#' doc <- rtf_read(f)
#' doc$pages
#' @export
rtf_read <- function(file) {
  w <- .rtf_walk(.read_rtf_text(file))
  rows <- data.frame(
    page = w$row_page, row = seq_along(w$row_page), header = w$row_hdr,
    stringsAsFactors = FALSE
  )
  rows$cells <- w$rows
  rows$indent <- w$row_ind
  structure(list(file = file, pages = w$pages, rows = rows), class = "rtf_doc")
}

#' @export
print.rtf_doc <- function(x, ...) {
  cat(sprintf("<rtf_doc> %s\n", basename(x$file)))
  cat(sprintf("  %d page(s), %d table row(s) (%d header rows)\n",
    nrow(x$pages), nrow(x$rows), sum(x$rows$header)))
  if (nrow(x$pages)) cat("  Page 1 header: ", substr(x$pages$head[1], 1, 120), "\n", sep = "")
  invisible(x)
}

#' Body table of an RTF output as a data frame
#'
#' Rebuilds the body table across pages: repeated column-header rows are
#' dropped (or used as column names) and empty rows are removed.
#'
#' @param x An `rtf_doc` from [rtf_read()] or a path to an RTF file.
#' @param header `"auto"` uses the first header row (`\\trhdr`) as column names,
#'   `"none"` names the columns `COL1`, `COL2`, ... (the usual QC dataset layout).
#' @param keep_indent If `TRUE`, adds an `indent` column with the number of
#'   leading spaces of the first column (useful for SOC / PT hierarchies).
#' @return A data frame of character columns, plus `.page`.
#' @examples
#' f <- system.file("extdata", "demo.rtf", package = "rtfqc")
#' rtf_table(f)
#' rtf_table(f, header = "auto", keep_indent = TRUE)
#' @export
rtf_table <- function(x, header = c("none", "auto"), keep_indent = FALSE) {
  header <- match.arg(header)
  if (is.character(x)) x <- rtf_read(x)
  r <- x$rows
  body <- r[!r$header, , drop = FALSE]
  keep <- vapply(body$cells, function(cc) any(nzchar(cc)), TRUE)
  body <- body[keep, , drop = FALSE]
  if (!nrow(body)) return(data.frame())
  w <- max(lengths(body$cells))
  m <- t(vapply(body$cells, function(cc) { length(cc) <- w; cc[is.na(cc)] <- ""; cc }, character(w)))
  if (w == 1) m <- matrix(m, ncol = 1)
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  names(out) <- paste0("COL", seq_len(w))
  if (header == "auto" && any(r$header)) {
    h <- r$cells[[which(r$header)[1]]]
    h <- make.unique(ifelse(nzchar(h), h, paste0("COL", seq_along(h))))
    if (length(h) == w) names(out) <- h
  }
  if (keep_indent) {
    raw1 <- vapply(body$cells, function(cc) cc[1], "")
    out$indent <- nchar(raw1) - nchar(sub("^\\s+", "", raw1))
  }
  out$.page <- body$page
  rownames(out) <- NULL
  out
}
