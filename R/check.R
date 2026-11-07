# Page and text checks ------------------------------------------------------

.issue <- function(check, page, message) {
  data.frame(check = check, page = page, message = message, stringsAsFactors = FALSE)
}
.as_doc <- function(x) if (inherits(x, "rtf_doc")) x else rtf_read(x)

#' Check page numbering and blank pages
#'
#' Looks for the page number in the header, footer and body of every page.
#' It can be written as text ("Page x of y", "Page x/y" or Chinese
#' the Chinese "di x ye gong y ye" form): then page k must say "Page k of n", where n is the real
#' number of pages. Or it can be Word PAGE / NUMPAGES fields, the usual SAS ODS
#' RTF and r2rtf output: Word fills the numbers in when it opens the file, so
#' they only need to be present on every page. Also reports pages
#' that have a header but no body content.
#'
#' @param x Path to an RTF file or an `rtf_doc`.
#' @param require If `TRUE` (default), a file with no page number at all is
#'   reported too; set `FALSE` for outputs that are meant to have none.
#' @return A data frame of issues (`check`, `page`, `message`); zero rows when
#'   everything is fine.
#' @export
rtf_check_pages <- function(x, require = TRUE) {
  doc <- .as_doc(x)
  p <- doc$pages
  n <- nrow(p)
  out <- list()
  pat <- "(?i)(?:page\\s*(\\d+)\\s*(?:of|/)\\s*(\\d+))|(?:\u7b2c\\s*(\\d+)\\s*\u9875\\s*[,\uff0c/]?\\s*\u5171\\s*(\\d+)\\s*\u9875)"
  txt <- paste(p$head, p$body)
  m <- regmatches(txt, regexec(pat, txt, perl = TRUE))
  found <- vapply(m, length, 0L) > 0
  # SAS ODS RTF / r2rtf usually write "Page {PAGE} of {NUMPAGES}" as Word fields: the
  # numbers only exist once Word opens the file, so they are always right as long as
  # both fields are there.
  fld <- grepl("{PAGE}", txt, fixed = TRUE) & (grepl("{NUMPAGES}", txt, fixed = TRUE) | grepl("{SECTIONPAGES}", txt, fixed = TRUE))
  pfld <- grepl("{PAGE}", txt, fixed = TRUE)
  if (!any(found | fld | pfld) && require) {
    out[[length(out) + 1]] <- .issue("page_text", NA_integer_, "No page number anywhere in this file (neither \"Page x of y\" text nor PAGE / NUMPAGES fields)")
  }
  if (any(found | fld | pfld)) {
    for (k in seq_len(n)) {
      if (!found[k]) {
        if (!fld[k]) out[[length(out) + 1]] <- .issue("page_text", k, if (pfld[k])
          "Page number has no total: PAGE field present but NUMPAGES field missing (\"Page x of\")"
          else "No page number (\"Page x of y\" or PAGE / NUMPAGES fields) on this page")
        next
      }
      g <- m[[k]]
      cur <- as.integer(if (nzchar(g[2])) g[2] else g[4])
      tot <- as.integer(if (nzchar(g[3])) g[3] else g[5])
      if (cur != k || tot != n) {
        out[[length(out) + 1]] <- .issue("page_text", k, sprintf("Says \"Page %d of %d\" but is page %d of %d", cur, tot, k, n))
      }
    }
  }
  if (any(nzchar(p$body))) {
    for (k in which(!nzchar(p$body))) out[[length(out) + 1]] <- .issue("blank_page", k, "Blank page: header/footer only, no content")
  }
  res <- if (length(out)) do.call(rbind, out) else .issue(character(0), integer(0), character(0))
  structure(res, class = c("rtf_check", "data.frame"), file = doc$file)
}

.squash <- function(x) tolower(gsub("[^[:alnum:]]", "", .norm(x)))

#' Check titles and footnotes against the table of contents
#'
#' Every expected title and footnote line must appear on every page. The
#' comparison ignores case, spaces and punctuation, so line breaks and
#' different dash characters do not count as differences. Footnotes are also
#' checked to be the same on all pages.
#'
#' @param x Path to an RTF file or an `rtf_doc`.
#' @param titles Character vector of expected title lines (for example from
#'   your TOC / shell). `NULL` skips the title check.
#' @param footnotes Character vector of expected footnote lines. `NULL` skips it.
#' @param toc Optional data frame (or path to an `.xlsx` / `.csv` file) with one
#'   row per output, used instead of `titles` / `footnotes`. It needs a column
#'   naming the output (`output`, `name`, `display` or `table`, matched to the
#'   RTF file name without extension) and title / footnote columns (names
#'   starting with `title` or `footnote`).
#' @return A data frame of issues (`check`, `page`, `message`).
#' @export
rtf_check_titles <- function(x, titles = NULL, footnotes = NULL, toc = NULL) {
  doc <- .as_doc(x)
  if (!is.null(toc)) {
    e <- .toc_entry(toc, doc$file)
    if (is.null(e)) {
      res <- .issue("toc", NA_integer_, sprintf("%s not found in the TOC", basename(doc$file)))
      return(structure(res, class = c("rtf_check", "data.frame"), file = doc$file))
    }
    titles <- e$titles; footnotes <- e$footnotes
  }
  p <- doc$pages
  page_txt <- .squash(paste(p$head, p$body))
  out <- list()
  chk <- function(lines, what) {
    lines <- lines[!is.na(lines) & nzchar(trimws(lines))]
    for (ln in lines) {
      s <- .squash(ln)
      if (!nzchar(s)) next
      miss <- which(!grepl(s, page_txt, fixed = TRUE))
      if (length(miss) == length(page_txt)) {
        out[[length(out) + 1]] <<- .issue(what, NA_integer_, sprintf("%s not found on any page: \"%s\"", tools_title(what), .norm(ln)))
      } else if (length(miss)) {
        out[[length(out) + 1]] <<- .issue(what, miss[1], sprintf("%s missing on page(s) %s: \"%s\"", tools_title(what), paste(utils::head(miss, 10), collapse = ", "), .norm(ln)))
      }
    }
  }
  if (!is.null(titles)) chk(titles, "title")
  if (!is.null(footnotes)) chk(footnotes, "footnote")
  res <- if (length(out)) do.call(rbind, out) else .issue(character(0), integer(0), character(0))
  structure(res, class = c("rtf_check", "data.frame"), file = doc$file)
}

tools_title <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

.read_toc <- function(toc) {
  if (is.data.frame(toc)) return(toc)
  if (is.character(toc) && grepl("\\.xlsx?$", toc, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is needed to read an Excel TOC.", call. = FALSE)
    return(as.data.frame(readxl::read_excel(toc, col_types = "text")))
  }
  if (is.character(toc) && grepl("\\.csv$", toc, ignore.case = TRUE)) return(utils::read.csv(toc, colClasses = "character", check.names = FALSE))
  stop("`toc` must be a data frame or a path to .xlsx / .csv", call. = FALSE)
}

.toc_entry <- function(toc, file) {
  toc <- .read_toc(toc)
  nm <- tolower(names(toc))
  key <- which(nm %in% c("output", "name", "display", "display name", "table", "output name", "file"))[1]
  if (is.na(key)) stop("TOC needs a column named output / name / display / table.", call. = FALSE)
  id <- tolower(sub("\\.rtf$", "", basename(file), ignore.case = TRUE))
  hit <- which(tolower(sub("\\.rtf$", "", trimws(as.character(toc[[key]])), ignore.case = TRUE)) == id)[1]
  if (is.na(hit)) return(NULL)
  row <- toc[hit, , drop = FALSE]
  tcols <- which(startsWith(nm, "title")); fcols <- which(startsWith(nm, "footnote"))
  split_lines <- function(cols) {
    v <- unlist(lapply(cols, function(k) as.character(row[[k]])))
    v <- unlist(strsplit(v[!is.na(v)], "\r?\n"))
    v[nzchar(trimws(v))]
  }
  list(titles = split_lines(tcols), footnotes = split_lines(fcols))
}

#' Run all checks on one RTF output
#'
#' @inheritParams rtf_check_titles
#' @param qc Optional QC data (see [rtf_compare()]); when given, value
#'   differences are added to the issues.
#' @return A data frame of issues with columns `check`, `page`, `message`.
#' @export
rtf_check <- function(x, qc = NULL, titles = NULL, footnotes = NULL, toc = NULL) {
  doc <- .as_doc(x)
  parts <- list(rtf_check_pages(doc))
  if (!is.null(titles) || !is.null(footnotes) || !is.null(toc)) parts[[2]] <- rtf_check_titles(doc, titles, footnotes, toc)
  if (!is.null(qc)) {
    d <- as.data.frame(rtf_compare(doc, qc))
    if (nrow(d)) parts[[length(parts) + 1]] <- .issue(paste0("qc_", d$status), d$page,
      ifelse(is.na(d$column), sprintf("%s: \"%s\"", d$status, d$label),
        sprintf("Row \"%s\", column %d: RTF \"%s\" vs QC \"%s\"", d$label, d$column, d$rtf, d$qc)))
  }
  res <- do.call(rbind, lapply(parts, as.data.frame))
  rownames(res) <- NULL
  structure(res, class = c("rtf_check", "data.frame"), file = doc$file)
}

#' @export
print.rtf_check <- function(x, ...) {
  f <- attr(x, "file")
  cat(sprintf("<rtf_check>%s: ", if (!is.null(f)) paste0(" ", basename(f)) else ""))
  if (!nrow(x)) { cat("\u2714 no issues\n"); return(invisible(x)) }
  cat(sprintf("%d issue(s)\n", nrow(x)))
  print(as.data.frame(unclass(x), stringsAsFactors = FALSE), row.names = FALSE)
  invisible(x)
}
