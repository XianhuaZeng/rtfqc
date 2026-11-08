# SAS ODS RTF output (disposition table) and its QC dataset; the numbers are invented
rtf <- system.file("extdata", "test.rtf", package = "rtfqc")
qc  <- system.file("extdata", "v_test.sas7bdat", package = "rtfqc")

test_that("disposition table is compared with its sas7bdat QC dataset", {
  skip_if_not_installed("haven")
  res <- rtf_compare(rtf, qc)
  d <- as.data.frame(res)
  st <- function(lab, col) d$status[startsWith(d$label, lab) & d$column %in% col]

  expect_equal(st("Safe dose administration", 4), "rounding")      # 98.3 vs 98.4
  expect_equal(st("Withdrawal", 2), "spacing")                     # 8 ( 26.7) vs 8(26.7)
  expect_equal(st("Subject withdrew", 1), "diff")                # consnt vs consent
  expect_equal(st("Other reasons", 1), "truncated")
  expect_equal(st("Other reasons", 2), "format")                   # 16.70 vs 16.7
  expect_true(any(d$status == "only_rtf" & startsWith(d$label, "Complete study")))
  expect_false(any(d$status == "only_qc"))
})
