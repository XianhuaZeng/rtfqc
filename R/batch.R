# Folder-level QC and reports ---------------------------------------------

#' Compare every RTF in a folder with its QC dataset
#'
#' Pairs `<main_dir>/<name>.rtf` with `<qc_dir>/<prefix><name>.sas7bdat`
#' (by default the common `v_` naming, e.g. `t_14_1_1.rtf` with
#' `v_t_14_1_1.sas7bdat`), compares each pair and runs the page checks.
#'
#' @param main_dir Folder with the RTF outputs.
#' @param qc_dir Folder with the QC datasets.
#' @param pattern Regular expression for the RTF files to include.
#' @param prefix Prefix of the QC dataset names.
#' @param ext QC dataset extension (`"sas7bdat"`, `"xpt"`, `"rds"` or `"csv"`).
#' @param toc Optional TOC (see [rtf_check_titles()]) for title / footnote checks.
#' @return A data frame with one row per RTF: counts of each difference type,
#'   number of page / title issues, and an `error` column. The cell-level
#'   differences of all outputs are in `attr(x, "details")`.
#' @export
rtf_qc_dir <- function(main_dir, qc_dir, pattern = "\\.rtf$", prefix = "v_", ext = "sas7bdat", toc = NULL) {
  files <- list.files(main_dir, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  files <- files[!grepl("^~\\$", basename(files))]
  details <- list()
  rows <- lapply(files, function(f) {
    name <- sub("\\.rtf$", "", basename(f), ignore.case = TRUE)
    qcf <- file.path(qc_dir, paste0(prefix, tolower(name), ".", ext))
    if (!file.exists(qcf)) {
      alt <- list.files(qc_dir, pattern = paste0("^", prefix, name, "\\.", ext, "$"), ignore.case = TRUE, full.names = TRUE)
      if (length(alt)) qcf <- alt[1]
    }
    base <- data.frame(output = name, qc = if (file.exists(qcf)) basename(qcf) else NA_character_,
      same = NA_integer_, diff = NA_integer_, truncated = NA_integer_, rounding = NA_integer_, format = NA_integer_, spacing = NA_integer_,
      only_rtf = NA_integer_, only_qc = NA_integer_, page_issues = NA_integer_, title_issues = NA_integer_,
      status = NA_character_, error = NA_character_, stringsAsFactors = FALSE)
    tryCatch({
      doc <- rtf_read(f)
      base$page_issues <- nrow(rtf_check_pages(doc))
      if (!is.null(toc)) base$title_issues <- nrow(rtf_check_titles(doc, toc = toc))
      if (file.exists(qcf)) {
        cmp <- rtf_compare(doc, qcf)
        d <- as.data.frame(cmp)
        if (nrow(d)) details[[name]] <<- cbind(output = name, d, stringsAsFactors = FALSE)
        base[names(cmp$count)] <- as.list(unname(cmp$count))
        bad <- sum(cmp$count[c("diff", "truncated", "only_rtf", "only_qc")])
        base$status <- if (bad) "differences" else if (sum(cmp$count[c("rounding", "format", "spacing")])) "minor" else "match"
      } else base$status <- "no QC"
      base
    }, error = function(e) { base$status <- "error"; base$error <- conditionMessage(e); base })
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  rownames(out) <- NULL
  # cell-level differences of all outputs, for rtf_compare_report() / attr(x, "details")
  attr(out, "details") <- if (length(details)) { z <- do.call(rbind, details); rownames(z) <- NULL; z } else NULL
  out
}

#' Write a comparison report
#'
#' @param x An `rtf_compare` from [rtf_compare()] or the data frame from [rtf_qc_dir()].
#' @param file Output path ending in `.xlsx` (needs openxlsx2) or `.csv`.
#' @return `file`, invisibly.
#' @export
rtf_compare_report <- function(x, file) {
  sheets <- if (inherits(x, "rtf_compare")) {
    list(Summary = data.frame(item = names(x$count), rows = unname(x$count)), Differences = as.data.frame(x))
  } else {
    det <- attr(x, "details")
    sm <- x; attr(sm, "details") <- NULL
    if (is.null(det)) list(Summary = as.data.frame(sm)) else list(Summary = as.data.frame(sm), Differences = det)
  }
  if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) stop("Package 'openxlsx2' is needed for .xlsx reports; use a .csv file name instead.", call. = FALSE)
    wb <- openxlsx2::wb_workbook()
    for (nm in names(sheets)) {
      wb <- openxlsx2::wb_add_worksheet(wb, nm)
      wb <- openxlsx2::wb_add_data(wb, nm, sheets[[nm]], with_filter = TRUE, na.strings = "")
      wb <- openxlsx2::wb_freeze_pane(wb, nm, first_row = TRUE)
    }
    openxlsx2::wb_save(wb, file, overwrite = TRUE)
  } else {
    utils::write.csv(sheets[[length(sheets)]], file, row.names = FALSE, na = "")
  }
  invisible(file)
}
