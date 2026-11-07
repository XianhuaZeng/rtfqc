demo <- system.file("extdata", "demo.rtf", package = "rtfqc")

test_that("pages, header text and table rows are read", {
  doc <- rtf_read(demo)
  expect_s3_class(doc, "rtf_doc")
  expect_equal(nrow(doc$pages), 2)
  expect_match(doc$pages$head[1], "Table 14.1.1 Demographic Characteristics")
  expect_match(doc$pages$head[2], "Page 2 of 2")
  expect_false(grepl("Courier", doc$pages$body[1]))          # font table skipped
  expect_equal(sum(doc$rows$header), 2)                       # \trhdr repeated on both pages
  expect_equal(doc$rows$page[!doc$rows$header], c(1, 1, 1, 2, 2, 2, 2))
})

test_that("special characters are decoded", {
  doc <- rtf_read(demo)
  last <- doc$rows$cells[[nrow(doc$rows)]]
  expect_equal(last[1], "Café ≥ 65")                # \'e9 and 蠅 with its ? fallback skipped
})

test_that("rtf_table drops header rows and keeps indentation", {
  tb <- rtf_table(demo, keep_indent = TRUE)
  expect_equal(nrow(tb), 7)
  expect_equal(names(tb)[1:3], c("COL1", "COL2", "COL3"))
  expect_equal(tb$indent[tb$COL1 == "  n"], 2)
  tb2 <- rtf_table(demo, header = "auto")
  expect_equal(names(tb2)[1], "Parameter")
})
