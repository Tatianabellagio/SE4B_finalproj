test_that("check load_data()", {
  load_data()
  expect_equal(levels(buttR_neuro$species)[1],'C.bar')
})

test_that("check run_model() and otr_expression_plot()", {
  model = run_model(buttR_neuro)
  expect_s3_class(model,"MCMCglmm")
  expect_error(otr_expression_plot(model,buttR_neuro),NA)
})
