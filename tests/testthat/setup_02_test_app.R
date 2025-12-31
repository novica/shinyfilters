library(DT)
library(shiny)
library(shinytest2)

app_shiny <- local({
	._app_shiny_cache <- NULL
	function() {
		if (is.null(._app_shiny_cache)) {
			._app_shiny_cache <<- shinyApp(
				ui = fluidPage(
					sidebarLayout(
						sidebarPanel(
							filterInput(test_df)
						),
						mainPanel(
							verbatimTextOutput("input_choices"),
							DTOutput("df_filt")
						)
					)
				),
				server = function(input, output, session) {
					test_df_inputs <- serverFilterInput(test_df, input)
					output$input_choices <- renderPrint(test_df_inputs$input_values)
					output$df_filt <- renderDT(datatable(
						apply_filters(test_df, test_df_inputs$input_values)
					))
				}
			)
		}
		._app_shiny_cache
	}
})
