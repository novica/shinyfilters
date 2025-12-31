# tests/testthat/test-serverFilterInput.R

test_that("serverFilterInput works with data.frames", {
	testServer(app_shiny(), {
		res <- serverFilterInput(test_df, input)
		test_df_filt <- apply_filters(test_df, res$input_values)
		expect_s3_class(test_df_filt, "data.frame")
		expect_lte(nrow(test_df_filt), nrow(test_df))
	})
})
