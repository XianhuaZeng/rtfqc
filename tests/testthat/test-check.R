demo <- system.file("extdata", "demo.rtf", package = "rtfqc")

test_that("correct page numbers give no issues", {
  expect_equal(nrow(rtf_check_pages(demo)), 0)
})

test_that("wrong page text is reported", {
  f <- tempfile(fileext = ".rtf")
  txt <- readLines(demo, warn = FALSE)
  txt <- sub("Page 2 of 2", "Page 2 of 3", txt, fixed = TRUE)
  writeLines(txt, f)
  iss <- rtf_check_pages(f)
  expect_equal(nrow(iss), 1)
  expect_equal(iss$page, 2)
})

test_that("titles and footnotes against expected text", {
  ok <- rtf_check_titles(demo,
    titles = c("Table 14.1.1 Demographic Characteristics", "Safety Population"),
    footnotes = c("Note: Percentages are based on N.", "Source: ADSL"))
  expect_equal(nrow(ok), 0)
  bad <- rtf_check_titles(demo, titles = "Table 14.1.1 Baseline Characteristics")
  expect_equal(nrow(bad), 1)
})

test_that("TOC data frame is matched by file name", {
  toc <- data.frame(output = "demo", title1 = "Table 14.1.1 Demographic Characteristics",
                    title2 = "Full Analysis Set", footnote1 = "Source: ADSL")
  iss <- rtf_check_titles(demo, toc = toc)
  expect_equal(nrow(iss), 1)
  expect_match(iss$message, "Full Analysis Set")
})

test_that("folder mode pairs RTF with v_ datasets", {
  d <- tempfile(); dir.create(d)
  file.copy(demo, file.path(d, "t_demo.rtf"))
  q <- tempfile(); dir.create(q)
  qc <- data.frame(COL1 = c("Age (years)", "n"), COL2 = c("", "30"), COL3 = c("", "31"))
  utils::write.csv(qc, file.path(q, "v_t_demo.csv"), row.names = FALSE)
  res <- rtf_qc_dir(d, q, ext = "csv")
  expect_equal(nrow(res), 1)
  expect_equal(res$only_rtf, 5L)
  expect_equal(res$status, "differences")
  det <- attr(res, "details")
  expect_equal(nrow(det), 5)
  expect_equal(unique(det$output), "t_demo")
})

test_that("PAGE / NUMPAGES fields count as correct page numbers", {
  f <- tempfile(fileext = ".rtf")
  pg <- "{\\header\\pard\\qr {Page {\\field{\\*\\fldinst{page}}} of {\\field{\\*\\fldinst{numpages}}}}\\par}\\pard Body\\par"
  writeLines(paste0("{\\rtf1\\ansi ", pg, "\\sect ", pg, "}"), f)
  doc <- rtf_read(f)
  expect_match(doc$pages$head[1], "Page {PAGE} of {NUMPAGES}", fixed = TRUE)
  expect_equal(nrow(rtf_check_pages(doc)), 0)
  # one page without the fields
  writeLines(paste0("{\\rtf1\\ansi ", pg, "\\sect \\pard Body 2\\par}"), f)
  iss <- rtf_check_pages(f)
  expect_equal(nrow(iss), 1)
  expect_equal(iss$page, 2)
})

test_that("a file without any page number is reported", {
  f <- tempfile(fileext = ".rtf")
  writeLines("{\\rtf1\\ansi {\\header\\pard Sponsor: X\\par}\\pard Body\\par}", f)
  iss <- rtf_check_pages(f)
  expect_equal(nrow(iss), 1)
  expect_true(is.na(iss$page))
  expect_equal(nrow(rtf_check_pages(f, require = FALSE)), 0)
})

test_that("PAGE field without NUMPAGES is reported", {
  f <- tempfile(fileext = ".rtf")
  writeLines("{\\rtf1\\ansi {\\header\\pard {Page {\\field{\\fldinst{page}}} of }\\par}\\pard Body\\par}", f)
  iss <- rtf_check_pages(f)
  expect_equal(nrow(iss), 1)
  expect_equal(iss$page, 1)
  expect_match(iss$message, "NUMPAGES field missing")
})
