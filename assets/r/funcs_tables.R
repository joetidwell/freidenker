print_table <- function(data, element_id, width=900, pagesize=5) {
	htmltools::browsable(
	  tagList(
			reactable(
				data,
			  defaultColDef = colDef(
			    header = function(value) gsub(".", " ", value, fixed = TRUE),
			    cell = function(value) format(value, nsmall = 1),
			    align = "center",
			    minWidth = 70,
			    headerStyle = list(background = "#f7f7f8")
			  ),
			  elementId  			= element_id,
			  searchable 			= TRUE,
			  style 		 			= list(fontFamily = "Work Sans, sans-serif",
			  							    		 fontSize = "0.875rem"),
			  defaultPageSize = pagesize,
			  bordered 				= TRUE,
			  highlight 			= TRUE,
				width 					= width
				),

	    tags$button(
	      tagList(fontawesome::fa("download"), "Download as CSV"),
	      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s.csv')", 
												  element_id,
												  element_id)
	    )
	  )
	)
}

print_table_html <- function(data, element_id, width=900, pagesize=5) {
	tags <- htmltools::tagList(
		reactable(
			data,
		  defaultColDef = colDef(
		    header = function(value) gsub(".", " ", value, fixed = TRUE),
		    cell = function(value) format(value, nsmall = 1),
		    align = "center",
		    minWidth = 70,
		    headerStyle = list(background = "#f7f7f8")
		  ),
		  elementId  			= element_id,
		  searchable 			= TRUE,
		  style 		 			= list(fontFamily = "Work Sans, sans-serif",
		  							    		 fontSize = "0.875rem"),
		  defaultPageSize = pagesize,
		  bordered 				= TRUE,
		  highlight 			= TRUE,
			width 					= width
			),

    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s.csv')", 
											  element_id,
											  element_id)
    )
  )

	htmlPreserve(tags)
}