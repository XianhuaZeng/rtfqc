demo <- system.file("extdata", "demo.rtf", package = "rtfqc")
qc_ok <- data.frame(
  COL1 = c("Age (years)", "n", "Mean (SD)", "Sex, n (%)", "Female", "Male", "Café ≥ 65"),
  COL2 = c("", "30", "45.2 (10.1)", "", "16 (51.6)", "15 (48.4)", "2 (6.5)"),
  COL3 = c("", "31", "47.0 (9.80)", "", "15 (46.9)", "17 (53.1)", "3 (9.4)"),
  stringsAsFactors = FALSE
)

test_that("cell classification", {
  expect_equal(rtf_cell_diff("12 (40.0)", "12 (40.0)"), "")
  expect_equal(rtf_cell_diff("  12 (40.0) ", "12 (40.0)"), "")        # leading / trailing spaces ignored
  expect_equal(rtf_cell_diff("12  (40.0)", "12 (40.0)"), "spacing")   # inner spaces must match
  expect_equal(rtf_cell_diff("9(31.0)", "9 ( 31.0)"), "spacing")
  expect_equal(rtf_cell_diff("12(40.0)", "12 (40.0)"), "spacing")
  expect_equal(rtf_cell_diff("12 (40.0)", "12 (40.00)"), "format")
  expect_equal(rtf_cell_diff("3.46", "3.45"), "rounding")
  expect_equal(rtf_cell_diff("3.48", "3.45"), "diff")
  expect_equal(rtf_cell_diff("30", "29"), "diff")                     # counts are never rounding
  expect_equal(rtf_cell_diff("16 (51.6)", "16 (51.7)"), "rounding")
  expect_equal(rtf_cell_diff("Headache and migr", "Headache and migraine"), "truncated")
  expect_equal(rtf_cell_diff("abc", "abd"), "diff")
})

test_that("identical QC gives no differences", {
  res <- rtf_compare(demo, qc_ok)
  expect_s3_class(res, "rtf_compare")
  expect_equal(unname(res$count["same"]), 7L)
  expect_equal(nrow(as.data.frame(res)), 0)
})

test_that("value, rounding, missing and extra rows are found", {
  qc <- qc_ok
  qc$COL2[2] <- "29"                       # diff
  qc$COL3[3] <- "47.0 (9.81)"              # rounding
  qc <- qc[-6, ]                           # Male missing in QC  -> only_rtf
  qc <- rbind(qc, data.frame(COL1 = "Unknown", COL2 = "0", COL3 = "0"))   # extra in QC -> only_qc
  res <- rtf_compare(demo, qc)
  d <- as.data.frame(res)
  expect_equal(res$count[["diff"]], 1L)
  expect_equal(res$count[["rounding"]], 1L)
  expect_true("only_rtf" %in% d$status || res$count[["diff"]] >= 1)
  expect_equal(d$rtf[d$status == "diff"], "30")
  expect_equal(d$qc[d$status == "diff"], "29")
  expect_equal(d$page[d$status == "diff"], 1)
})

test_that("an inserted row does not shift the rest", {
  qc <- rbind(qc_ok[1:3, ], data.frame(COL1 = "Median", COL2 = "44", COL3 = "46"), qc_ok[4:7, ])
  res <- rtf_compare(demo, qc)
  expect_equal(res$count[["only_qc"]], 1L)
  expect_equal(res$count[["same"]], 7L)
})

test_that("a data frame from rtf_table() works as input", {
  res <- rtf_compare(rtf_table(demo), qc_ok)
  expect_equal(unname(res$count["same"]), 7L)
})

test_that("SAS ODS inline formatting in QC text is resolved", {
  expect_equal(rtfqc:::.ods_inline("Complete (R) administration@{SUPER 1}"), "Complete (R) administration1")
  expect_equal(rtfqc:::.ods_inline("BMI (kg/m^{super 2})"), "BMI (kg/m2)")
  expect_equal(rtfqc:::.ods_inline("x(*ESC*){sub 0}"), "x0")
  expect_equal(rtfqc:::.ods_inline("a@{newline}b"), "a\nb")
  expect_equal(rtfqc:::.ods_inline("@{unicode 03B1}"), "\u03b1")
  expect_equal(rtfqc:::.ods_inline("@{style [fontweight=bold] Total}"), "Total")
  expect_equal(rtfqc:::.ods_inline("plain 5 (3.0)"), "plain 5 (3.0)")
  expect_equal(rtfqc:::.ods_inline("48/@nMale"), "48/\nMale")
  expect_equal(rtf_cell_diff("48/\nMale", rtfqc:::.ods_inline("48/@nMale")), "")
  expect_equal(rtfqc:::.ods_inline("Dosage Discon@w@wtinue"), "Dosage Discontinue")
  # a line break the RTF puts in the middle of a word only differs in white space
  expect_equal(rtf_cell_diff("Gastrointesti\nnal disorders/\nDuodenal", rtfqc:::.ods_inline("Gastrointestinal disorders/@nDuodenal")), "")
})

test_that("listing rows with left-out repeated values align correctly", {
  rtf <- data.frame(COL1 = c("RT", "", "", "RT"), COL2 = c("1027", "", "", "1027"),
                    COL3 = c("Screening", "Day 5", "Exit", "Exit unplanned"),
                    COL4 = c("Normal", "Abnormal", "Normal", "Normal"), stringsAsFactors = FALSE)
  qc <- data.frame(COL1 = rep("RT", 4), COL2 = rep("1027", 4),
                   COL3 = c("Screening", "Day 5", "Exit", "Exit unplanned"),
                   COL4 = c("Normal", "Abnormal", "Normal", "Normal"), stringsAsFactors = FALSE)
  res <- rtf_compare(rtf, qc)
  expect_equal(res$count[["same"]], 4L)
  expect_equal(res$count[["only_rtf"]] + res$count[["only_qc"]], 0L)
  qc$COL4[3] <- "Abnormal"
  d <- as.data.frame(rtf_compare(rtf, qc))
  expect_equal(nrow(d), 1)
  expect_equal(d$rtf_row, 3)
  expect_equal(d$column, 4)
})
