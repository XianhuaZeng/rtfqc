# rtfqc 0.1.15

* The example files `inst/extdata/test.rtf` and `v_test.sas7bdat` now hold
  invented numbers and generic withdrawal reasons only.

# rtfqc 0.1.14

* Preparing the first CRAN submission: examples for every exported function
  (files are only written to `tempdir()`), software names quoted in
  DESCRIPTION.

# rtfqc 0.1.13

* Leading spaces must now match as well: indentation (`  Female` vs `Female`)
  and number alignment (` 29` vs `29`) are reported as `spacing`. Only trailing
  spaces (SAS pads character values) and line breaks inside a cell are ignored.
* Indentation written as RTF paragraph indent (`\li` / `\fi`) instead of
  leading spaces is recognised: per column, each indent level is converted to
  the leading-space count the QC dataset uses for that level. `rtf_read()`
  returns the indent of every cell in the new `rows$indent` column.
* `as.data.frame()` of an `rtf_compare` keeps the spaces in the `rtf` / `qc`
  cell text, so spacing differences are visible.

# rtfqc 0.1.12

* Spaces inside a cell must now match: `9(31.0)` vs `9 ( 31.0)` is reported as
  `spacing`. Leading / trailing spaces and line breaks inside a cell (RTF
  `\line`, ODS `@n` / `@w`) are still ignored.

# rtfqc 0.1.11

First public version.

## Reading RTF outputs
* `rtf_read()` reads RTF files directly (no Word, LibreOffice or Pandoc):
  pages, header/footer text, body text and body table rows, with repeated
  column-header rows (`\trhdr`) flagged.
* Code pages are detected from `\ansicpg` (e.g. GBK for Chinese outputs);
  `\uN` Unicode characters are decoded.
* Word PAGE / NUMPAGES fields are kept as `{PAGE}` / `{NUMPAGES}` placeholders.
* `rtf_table()` rebuilds the body table as a data frame (`COL1`, `COL2`, ...)
  across pages, optionally with the indentation level.

## Comparing with QC data
* `rtf_compare()` compares an RTF output with its QC dataset (data frame,
  `.sas7bdat`, `.xpt`, `.rds` or `.csv`) cell by cell and classifies each
  difference: `diff`, `truncated`, `rounding`, `format`, or white space only
  (not reported). Whole numbers that differ (counts) are never "rounding".
* Rows are aligned in three steps (identical rows, then row labels, then in
  order), so an extra or missing row does not shift the rows after it.
* Leading cells that a listing leaves empty because they repeat the row above
  are filled in on both sides before comparing.
* SAS ODS inline formatting in QC text (`@{super 1}`, `^{sub 2}`, `@n`, `@w`,
  `(*ESC*){unicode 03B1}`, ...) is resolved to what the RTF shows.
* `rtf_compare()` also accepts a data frame, e.g. from `artful::rtf_to_df()`.

## Checks
* `rtf_check_pages()`: "Page x of y" text or PAGE / NUMPAGES fields on every
  page, numbering, blank pages, files without any page number.
* `rtf_check_titles()`: titles and footnotes against expected text or a TOC.
* `rtf_check()` runs all checks, optionally with the QC comparison.

## Folders and reports
* `rtf_qc_dir()` pairs `x.rtf` with `v_x.sas7bdat` in a folder and returns one
  summary row per output; cell-level details are in `attr(x, "details")`.
* `rtf_compare_report()` writes an Excel report (Summary + Differences sheets)
  or CSV.
