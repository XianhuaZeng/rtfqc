# Real SAS ODS RTF output (Table 14.1.1.1 disposition) and its QC dataset
rtf <- system.file("extdata", "test.rtf", package = "rtfqc")
qc  <- system.file("extdata", "v_test.sas7bdat", package = "rtfqc")

test_that("disposition table is compared with its sas7bdat QC dataset", {
  skip_if_not_installed("haven")
  res <- rtf_compare(rtf, qc)
  d <- as.data.frame(res)
  st <- function(lab, col) d$status[startsWith(d$label, lab) & d$column %in% col]

  expect_equal(st("Safe dose administration", 4), "rounding")      # 98.3 vs 98.4
  expect_equal(st("Withdrawal", 2), "spacing")                     # 9 ( 31.0) vs 9(31.0)
  expect_equal(st("Withdraw from the study by subjects", 1), "diff")  # thema vs themselves
  expect_equal(st("Other conditions", 1), "truncated")
  expect_equal(st("Other conditions", 2), "format")                # 20.70 vs 20.7
  expect_true(any(d$status == "only_rtf" & startsWith(d$label, "Complete study")))
  expect_false(any(d$status == "only_qc"))
})
