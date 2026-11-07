# rtfqc

<!-- badges: start -->
[![R-CMD-check](https://github.com/XianhuaZeng/rtfqc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/XianhuaZeng/rtfqc/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Read clinical RTF outputs and check them against QC data — in R, without
Word, LibreOffice or Pandoc.**

In double programming, the production program writes the table, listing or
figure to RTF and the QC programmer writes an independent dataset with the
same content. Comparing the two usually means opening the RTF and checking by
eye. rtfqc reads the RTF itself and compares it with the QC dataset cell by
cell, so you see every difference — and only real differences.

## Features

* **Read RTF directly.** Pages, titles and footnotes in the header/footer,
  body table rows, repeated column headers, Chinese / other code pages
  (`\ansicpg`), Unicode characters and Word page-number fields.
* **Compare with QC** (`rtf_compare()`). Each differing cell is classified:

  | Class | Example (RTF vs QC) |
  |---|---|
  | `diff` | `30` vs `29`, `thema` vs `themselves` |
  | `truncated` | `Headache and migr` vs `Headache and migraine` |
  | `rounding` | `51.6` vs `51.7` (last decimal shown; never for counts) |
  | `format` | `20.70` vs `20.7` (same number, different format) |
  | `spacing` | `9(31.0)` vs `9 ( 31.0)` (spaces inside the text) |

  Leading / trailing spaces and line breaks inside a cell are ignored.

* **Robust row alignment.** Identical rows are matched first, then rows with
  the same label, then the rest in order — so one extra or missing row does
  not shift every row after it. Values a listing leaves out because they repeat
  the row above are filled in before comparing.
* **Understands SAS ODS inline formatting** in QC data: `@{super 1}`,
  `^{sub 2}`, `@n`, `@w`, `(*ESC*){unicode 03B1}`, `@{style [...] text}`.
* **Page checks** (`rtf_check_pages()`): "Page x of y" on every page,
  PAGE / NUMPAGES fields, blank pages.
* **Title and footnote checks** (`rtf_check_titles()`) against your TOC.
* **Whole folders** (`rtf_qc_dir()`): pairs `x.rtf` with `v_x.sas7bdat` and
  writes an Excel report with a summary and all differences.

## Installation

```r
# install.packages("remotes")
remotes::install_github("XianhuaZeng/rtfqc")
```

Reading `.sas7bdat` / `.xpt` needs [haven](https://haven.tidyverse.org/);
Excel reports need [openxlsx2](https://janmarvin.github.io/openxlsx2/).

## Usage

### One output

```r
library(rtfqc)

res <- rtf_compare("t_14_1_1_1.rtf", "v_t_14_1_1_1.sas7bdat")
res
#> <rtf_compare> t_14_1_1_1.rtf
#>   RTF rows: 12   QC rows: 12   QC columns: COL1, COL2, COL3, COL4
#>   rows: same 11 | only in RTF 0 | only in QC 0
#>   cells: diff 1 | truncated 0 | rounding 0 | format 0
#>
#>  status page rtf_row column label                    rtf        qc
#>  diff   1     9      1      Withdraw from the study… …cts thema …cts themselves

as.data.frame(res)   # all differences with full text
```

### Page, title and footnote checks

```r
rtf_check_pages("t_14_1_1_1.rtf")
rtf_check_titles("t_14_1_1_1.rtf", toc = "TOC.xlsx")
rtf_check("t_14_1_1_1.rtf", qc = "v_t_14_1_1_1.sas7bdat", toc = "TOC.xlsx")  # everything
```

### A whole folder

```r
r <- rtf_qc_dir("01Main/03Table/04Files", "02QC/03Table/01Data")
r                                       # one row per output
attr(r, "details")                      # every difference
rtf_compare_report(r, "tfl_qc_summary.xlsx")
```

### Reading only

```r
doc <- rtf_read("t_14_1_1_1.rtf")
doc$pages          # page, header/footer text, body text
rtf_table(doc)     # body table as COL1, COL2, ... (repeated headers removed)
```

## QC dataset layout

The QC dataset holds the table as displayed: one row per table row, character
columns `COL1`, `COL2`, ... in display order (other column names work too: all
character columns are used, or pass `qc_cols`). Leading spaces, repeated
column-header rows and ODS inline formatting are handled.

## How it relates to other packages

| Package | Purpose |
|---|---|
| [r2rtf](https://merck.github.io/r2rtf/), rtf, pharmaRTF | *write* RTF |
| [striprtf](https://github.com/kota7/striprtf) | extract plain text from RTF |
| [artful](https://github.com/BristolMyersSquibb/artful) | RTF table → data frame / ARD-like data (via Pandoc) |
| [diffdf](https://cran.r-project.org/package=diffdf), dataCompareR | compare two data frames |
| **rtfqc** | read RTF outputs and QC them: cell-level comparison with the QC dataset, page / title / footnote checks |

`rtf_compare()` also accepts a data frame, but pass the RTF path whenever you
can. Tools that convert RTF through HTML (e.g. Pandoc, as `artful::rtf_to_df()`
does) collapse repeated spaces inside a cell, so the alignment spaces in
`28 ( 96.6)` are lost and every such cell is reported as `spacing`; they may
also drop the last body row. rtfqc reads the RTF itself and keeps the cell
text exactly as written, because those spaces are part of the layout.

## Limitations

* Tables and listings are compared; figures (images) are not.
* The RTF must contain real table rows (`\trowd … \row`), as produced by
  SAS ODS RTF, r2rtf and most reporting tools.
* Very large outputs (more than about 2,000 × 2,000 rows) are aligned by
  position only.

## Contributing

Issues and pull requests are welcome. Please run `devtools::test()` and
`devtools::check()` before submitting, and never commit real study data —
use small synthetic RTF files in tests.

## License

GPL (>= 3)
