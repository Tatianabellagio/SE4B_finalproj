test_that("check number of rows", {
  expect_true(nrow(buttR_neuro) == 414)
})

test_that("data has correct columns", {
  expect_true("count" %in% colnames(buttR_neuro))
  expect_true("gene" %in% colnames(buttR_neuro))
  expect_true("sample" %in% colnames(buttR_neuro))
  expect_true("individual" %in% colnames(buttR_neuro))
  expect_true("sex" %in% colnames(buttR_neuro))
  expect_true("species" %in% colnames(buttR_neuro))
  expect_true("social" %in% colnames(buttR_neuro))
  expect_true("region_mammal" %in% colnames(buttR_neuro))
})

test_that("columns have correct data type", {
  expect_type(buttR_neuro$count,"double")
  expect_type(buttR_neuro$gene,"integer")
  expect_type(buttR_neuro$sample,"integer")
  expect_type(buttR_neuro$individual,"integer")
  expect_type(buttR_neuro$sex,"integer")
  expect_type(buttR_neuro$species,"integer")
  expect_type(buttR_neuro$social,"integer")
  expect_type(buttR_neuro$region_mammal,"integer")
})

test_that("columns have correct class", {
  expect_true(is.numeric(buttR_neuro$count))
  expect_true(is.factor(buttR_neuro$gene))
  expect_true(is.integer(buttR_neuro$sample))
  expect_true(is.factor(buttR_neuro$individual))
  expect_true(is.factor(buttR_neuro$sex))
  expect_true(is.factor(buttR_neuro$species))
  expect_true(is.factor(buttR_neuro$social))
  expect_true(is.factor(buttR_neuro$region_mammal))
})

test_that("gene column has correct control", {
  expect_true("r18S" %in% buttR_neuro$gene)
})

test_that("all sexes are male", {
  expect_true(length(unique(buttR_neuro$sex))==1)
  expect_true(unique(buttR_neuro$sex)[1]=="Male")
})

test_that("all species are there", {
  expect_true("C.bar" %in% buttR_neuro$species)
  expect_true("C.lun" %in% buttR_neuro$species)
  expect_true("C.rainf" %in% buttR_neuro$species)
  expect_true("C.trif" %in% buttR_neuro$species)
  expect_true("C.vag" %in% buttR_neuro$species)
})

test_that("all brain regions are meAMY.BNST", {
  expect_true(length(unique(buttR_neuro$region_mammal))==1)
  expect_true(unique(buttR_neuro$region_mammal)[1]=="meAMY.BNST")
})

#have a test set for inputs and outputs to functions
# -right inputs
# -make sure packages are running well
# -make sure models are running well
# -right outputs
