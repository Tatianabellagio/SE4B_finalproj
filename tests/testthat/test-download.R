test_that("getData.R works", {
  tmp_path <- file.path(tempdir(), 'Nowicki_C_SPP_Males_data_ESM.csv')
  download.file("https://datadryad.org/stash/downloads/file_stream/368459",
                tmp_path)
  expect_true(file.exists(tmp_path))
})

test_that("data has correct number of columns", {
  tmp_path <- file.path(tempdir(), 'Nowicki_C_SPP_Males_data_ESM.csv')
  download.file("https://datadryad.org/stash/downloads/file_stream/368459",
                tmp_path)
  expect_true(file.exists(tmp_path))
})
